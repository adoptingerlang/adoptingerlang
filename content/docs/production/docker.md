+++
title = "Docker"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
date = 2019-09-28T09:32:00+00:00
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:09+00:00
draft = false
[menu]
  [menu.main]
    weight = 2003
    identifier = "docker"
    parent = "production"
+++

[Docker](https://docker.com) helped popularize Linux containers through its ease of use and registry of pre-built images, and became a word often used interchangeably with "Linux container".

Docker images contain multiple layers that are merged at runtime to make up the filesystem of the container. Docker creates the layers by running commands found in a `Dockerfile`, each command creates a new layer. Layers are shared between images, saving space, and can be used as a cache for speeding up the building of images. Additional space is saved, compared to other options like a virtual machine (VM), by not including the Linux kernel in the image. The size of the image is little larger than the size of the packaged Erlang release we are deploying.

The small size and ability to run like a regular Linux process (a new kernel isn't booted) makes for quicker start times and less resource consumption than using a traditional VM for isolation. Having little overhead means that the advantages of isolation when packaging and running a program can be standard practice instead of the burden it would be to have to run a VM per program.

Advantages of containers running with filesystem and network isolation are not having to perform operations that are common when programs are not isolated:

-   Pre-installing shared libraries
-   Updating configuration
-   Finding an open port
-   Finding a unique name for node name

{{% admonition note %}}
You might notice we will not be using the <code>latest</code> tag at all when using Docker. This tag is commonly misunderstood and misused. It is assigned to the last image used without a specific tag, it is not the latest created image. It should rarely, if ever, be relied on, unless you really don't care what version of an image will be used.
{{% /admonition %}}

In this chapter we will cover efficiently building images for running the [service_discovery](https://adoptingerlang.org/gh/servicediscovery) project as well as images for running tests and dialyzer. Then, we will update the continuous integration pipeline to build and publish new images.

The minimum Docker version required for this chapter is 19.03 with [buildx](https://github.com/docker/buildx) installed. Installing `buildx` can be done with the following commands:

```shell
$ export DOCKER_BUILDKIT=1
$ docker build --platform=local -o . git://github.com/docker/buildx
$ mv buildx ~/.docker/cli-plugins/docker-buildx
```


## Building Images {#building-images}

[Official Erlang Docker images](https://hub.docker.com/_/erlang/) are published for each new OTP release. They include Rebar3 and come in [Alpine](https://alpinelinux.org/) and [Debian](https://www.debian.org/) flavors -- the images are updated for new releases of Rebar3 and Alpine/Debian as well. Because the tagged images are updated for new releases it is recommended to both use the `sha256` digest of the image and to mirror the images used to your own repository, even if your repository is also on Docker Hub. Having a copy ensures the base image does not change without developer intervention and having a mirror in a registry separate from Docker Hub means you are not dependent on its availability. This best practice is why in the examples to follow and the `service_discovery` repository use images from `ghcr.io/adoptingerlang/service_discovery/` and `us.gcr.io/adoptingerlang/`.


### Private Dependencies {#private-dependencies}

The first stumbling block many in a work environment encounter when building Docker images is access to private dependencies. If you have private git repos or [Hex organization packages](https://hex.pm/docs/rebar3_private) as dependencies they will not be able to be fetched in the Docker container during the build. Often this leads people to not include `_build` in `.dockerignore` and risk polluting the build with local artifacts, possibly not being reproducible elsewhere, so the dependencies could be fetched with Rebar3 before running `docker build`. The other option is copying the host SSH credentials and/or Hex apikey into the build container, but this is not recommended because it will be kept in the Docker layer and leaked anywhere you push the image. Instead, in recent Docker releases (18.06 and later) the abilities to mount secrets and SSH agent connections or keys in a secure manner. The data does not leak to the final image or any commands it is not explicitly mounted to.

Since `service_discovery` has no private dependencies we will look at how to support them separately, before getting started with building the images for `service_discovery`.


#### Hex Dependencies {#hex-dependencies}

Rebar3 keeps the access keys for private Hex dependencies in a file `~/.config/rebar3/hex.config`. Using the experimental Dockerfile syntax `--mount=type=secret` the config can be mounted into the container for just the compile command. The file is mounted to a separate tmpfs filesystem and excluded from the build cache:

```dockerfile
# syntax=docker/dockerfile:1.2
RUN --mount=type=secret,id=hex.config,target=/root/.config/rebar3/hex.config rebar3 compile
```

To mount the host's `hex.config` when running `docker build` simply pass a secret with a matching `id` and the `src` path to the file:

```shell
$ docker build --secret id=hex.config,src=~/.config/rebar3/hex.config .
```


#### Git Dependencies {#git-dependencies}

You could use the secret mount from the last section for mounting SSH keys, but Docker added a better solution with a mount type specifically for dealing with SSH. A `RUN` command that needs SSH access can use `--mount=type=ssh`:

```dockerfile
# syntax=docker/dockerfile:1.2
RUN apt install --no-cache openssh-client git && \
    mkdir -p -m 0600 ~/.ssh && \
    ssh-keyscan github.com >> ~/.ssh/known_hosts && \
    git config --global url."git@github.com:".insteadOf "https://github.com/"
WORKDIR /src
COPY rebar.config rebar.lock .
RUN --mount=type=ssh rebar3 compile
```

First, a `RUN` command installs the necessary dependencies, SSH and git. Then, `ssh-keyscan` is used to download the current public key for Github and add it to `known_hosts`. The public key being in `known_hosts` means SSH will not attempt to prompt to ask if you accept the host's public key. Next, the git config setting ensures that even if in the `rebar.config` the git url is using `https` it will instead use SSH. If the private repos are not on Github this url replacement has to be changed for the appropriate location.

Along with adding the previous snippet to the Dockerfiles we'll see later in this chapter you'll also need to add `--ssh default` to the build command when run and set `DOCKER_BUILDKIT`:

```shell
$ export DOCKER_BUILDKIT=1
$ docker build --ssh default .
```

Additional information and options for the SSH mount type can be found [in the Moby documentation](https://github.com/moby/buildkit/blob/master/frontend/dockerfile/docs/experimental.md#run---mounttypessh) -- Moby is the name of the project that makes up the core functionality of Docker.


### Efficient Caching {#efficient-caching}


#### Basic Instruction Ordering {#basic-instruction-ordering}

The order of commands in a Dockerfile is very important to the build time and size of the images it creates. Each command in the Dockerfile creates a layer which is then reused in future builds to skip a command if nothing has changed. With Rebar3 we take advantage of this by creating a layer containing the built dependencies of our project:

```dockerfile
COPY rebar.config rebar.lock .
RUN rebar3 compile
```

The `COPY` command will only invalidate the cache of the command that runs `rebar3 compile` (and subsequent commands in the file) if `rebar.config` or `rebar.lock` are different from a previously created layer. Since none of the project's code was copied and Rebar3 only builds the dependencies, this results in a layer containing only the built dependencies under `_build/default/lib`.

After the dependencies are built and cached we can copy in the rest of the project and compile it:

```dockerfile
COPY . .
RUN rebar3 compile
```

Because of the order of operations in the Dockerfile each run of `docker build
.` only compiles the project's source, assuming there is a change, otherwise an
existing layer is used here as well. Any command that does not need to be rerun
when there are changes to the project need to come before either of the `COPY`
commands. For example, installing Debian packages, `RUN apt install git` and `WORKDIR /app/src` for setting the working directory.

Use of `COPY . .` is discouraged because it makes it more likely to invalidate the cache. If possible it is better to copy only the files and directories needed for the build or having a `.dockerignore` file filter out files that are not needed. The `.git` directory is one that because of its size and the fact changes to the contents in it do not effect the build artifacts is good to ignore. However, in `service_discovery` we rely on git commands for setting the version of the release and the applications that make up the release. In projects that do not rely on this feature of Rebar3 it is recommended to add `.git` to `.dockerignore`.


#### Experimental Mount Syntax {#experimental-mount-syntax}

Copying files into an image and caching layers are no longer the only options for efficiency when building a Docker image. Caching the built dependencies in a layer is all well and good but that layer also contains the Hex package cache Rebar3 creates under `~/.cache/rebar3/hex`. Any change to `rebar.config` or `rebar.lock` will result in all the packages having to not only be rebuilt but also be re-fetched from Hex. Additionally, the instruction to copy in the whole project, creating an additional layer with all the source is wasteful since we only care about the build artifacts.

These issues are resolved as of Docker 19.03 through an experimental syntax for
mounting files to the context of a `RUN` command. To enable the experimental
syntax the environment variable `DOCKER_BUILDKIT` must be set or
`{"features":{"buildkit": true}}` be set in `/etc/docker/daemon.json`, and `#
syntax=docker/dockerfile:1.2` used as the first line of the Dockerfile:

```dockerfile
# syntax=docker/dockerfile:1.2

[...]

WORKDIR /app/src

ENV REBAR_BASE_DIR /app/_build

# build and cache dependencies as their own layer
COPY rebar.config rebar.lock .
RUN --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 compile

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 compile
```

In this new set of instructions the build sets the `WORKDIR` to `/app/src`, this is then the current working directory of the commands that follow. And an environment variable `REBAR_BASE_DIR` is set to `/app/_build`. The base directory is where Rebar3 will output all build artifacts and by default is a directory `_build/` at the root of the project, in this case without the environment variable it would have been `/app/src/_build`.

The `COPY` of the Rebar3 configuration and lock file remains the same but the following `RUN` has changed to include a `--mount` option with type `cache`. This tells Docker to create a cache directory separate from the Docker layers and stored locally on the host. This cache will remain between runs of `docker build`, so future local runs of `docker build`, even if the config or lock file has changed, will mount this cache and only newly needed packages will be fetched from Hex.

Next, unlike the previous instructions for building the rest of the project the instruction `COPY . .` has been removed. Instead, a mount of type `bind` (the default) is used with a `target` of `.`. A bind mount, as opposed to the `cache` mount, means Docker will mount from the build context into the container, giving us the same result as `COPY . .`, but without creating a layer from copying in the files, making the build faster and smaller. With the `COPY` command there are two copies made from the host: the build context and the copy in the build container. Each run of `build` with `COPY` requires copying the whole project from the build context into the build container again.

By default the mount is immutable, meaning the build will error out if anything is written to `/app/src`, and that is why the Rebar3 base directory has been configured to `/app/_build`. There is an option to mount in `read-write` mode but the writes are not persisted and it removes optimizations around not having to make copies of the data from the build context for the build container. More can be read about the mount options in the Buildkit docs for [Dockerfile frontend experimental syntax](https://github.com/moby/buildkit/blob/master/frontend/dockerfile/docs/experimental.md).

The end result is a layer containing `/app/_build` with the compiled dependencies (along with `/app/src/rebar.*`, but they add very little to the size of the layer) followed by a layer containing `/app/_build` with the compiled project but nothing in `/app/src`. Separately there is a cache of all the hex packages that were downloaded.


#### Local vs Remote Cache {#local-vs-remote-cache}

There are a couple types of caches we have used in this section and both have relied the builds being done on the same host to have access to the cache. In the case of the `hex-cache` that is mounted during `RUN` this is solely a feature for local caching and can not be exported or imported from a registry. However, the layers built from each instruction in the Dockerfile can be imported from a registry.

Having a build be setup to use a remote cache is particularly useful when working with continuous integration or any sort of build server. Unless there is only a single node running builds, it will end up wasting time rebuilding every step of a Dockerfile. To resolve this issue `docker build` can be told where to look for layers, including images in a remote registry, with `--cache-from`.

There are two versions of `--cache-from`, and because the newer one is still technically part of an experimental "tech preview" called [buildx](https://github.com/docker/buildx), we will cover both. However, since `buildx` is so much more efficient, easier to use and appears stable within the bounds of the features we require, it will be the default used by the `service_discovery` project.

The old `--cache-from` is not "multi-stage aware", meaning it requires the user manually building and pushing each stage in a multi-stage Dockerfile individually. When building stages which build on the earlier stage its image can be referenced through `--cache-from` and it will be pulled from the registry.

With `buildx` a cache manifest is built that will include information about previous stages in a multi-stage build. The argument `--cache-to` allows for this cache to be exported in various ways. We will use the `inline` option which writes the cache manifest directly to the image's metadata. The image can be pushed to the registry and then referenced in a later build with `--cache-from`. The unique part about the new cache manifest is that only layers that are cache hits will be downloaded, in the old form the entire image of the previous stage was downloaded when referenced by `--cache-from`.

{{% admonition danger "Caching and Security Updates" %}}
<p>There is a security concern to keep in mind when using layer caching. For example, since the <code>RUN</code> command only reruns if the text of the command changes, or a previous layer invalidated the cache, any system package installed will remain the same version even if a security fix has been released. For this reason it is good to occasionally run Docker with <code>--no-cache</code> which will not reuse any layers when building the image.</p>
{{% /admonition %}}


### Multi-Stage Build {#multi-stage-build}

For an Erlang project we are going to need an image with the built release and this image should not contain anything not required for running the release. Tools like Rebar3, the Erlang/OTP version used to build the project, git that was used for fetching dependencies from github, etc all must go. Instead of removing items after the build is complete, a multi-stage Dockerfile can be used to copy the final release, which bundles the Erlang runtime, from the stage it was built to a stage with a Debian base and only the shared libraries necessary to run the release, such as OpenSSL.

We will step through the stages in the `service_discovery` project's [Dockerfile](https://github.com/adoptingerlang/service_discovery/blob/docker-chapter/Dockerfile). The first stage is named `builder`:

```dockerfile
# syntax=docker/dockerfile:1.2
FROM ghcr.io/adoptingerlang/service_discovery/erlang:26.0.2 as builder

WORKDIR /app/src
ENV REBAR_BASE_DIR /app/_build

RUN rm -f /etc/apt/apt.conf.d/docker-clean

# Install git for fetching non-hex depenencies.
# Add any other Debian libraries needed to compile the project here.
RUN --mount=target=/var/lib/apt/lists,id=apt-lists,type=cache,sharing=locked \
    --mount=type=cache,id=apt,target=/var/cache/apt \
    apt update && apt install --no-install-recommends -y git

# build and cache dependencies as their own layer
COPY rebar.config rebar.lock .
RUN --mount=id=hex-cache,type=cache,target=/root/.cache/rebar3 \
    rebar3 compile

FROM builder as prod_compiled

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,target=/root/.cache/rebar3 \
    rebar3 as prod compile
```

The `builder` stage starts with the base image `erlang:26.0.2`. `as builder` names the stage so we can use it as a base image to `FROM` in later stages.

{{% admonition info "Old Docker Caching" %}}
<p>For remote caching using the old <code>--cache-from</code>, as described in the previous section,  the <code>builder</code> stage would be built and tagged with an identifier that lets us reference the image based on the Rebar3 dependencies it contains. To do this we can use the command <code>cksum</code> on <code>rebar.config</code> and <code>rebar.lock</code>. This acts similarly to what Docker does before choosing to invalidate its cache or not.</p>

<code><pre>
$ CHKSUM=$(cat rebar.config rebar.lock | cksum | awk '{print $1}')
$ docker build --target builder -t service&#95;discovery:builder-${CHKSUM} .
$ docker push service&#95;discovery:builder-${CHKSUM}
</pre></code>

<p>When building any stage that uses <code>FROM builder</code> we would include <code>--cache-from=service&#95;discovery:builder-${CHKSUM}</code> to pull the previously built dependencies.</p>

<p>It is common for developers to be working on concurrent branches of the same project, potentially with varying dependencies, using a checksum of the current Rebar3 config and lock files when defining the image to use as a cache allows for multiple sets of dependencies for a project to be cached and the correct set to be used when building.</p>
{{% /admonition %}}

The next stage, named `releaser`, uses the `prod_compiled` image as its base:

```dockerfile
FROM prod_compiled as releaser

WORKDIR /app/src

# create the directory to unpack the release to
RUN mkdir -p /opt/rel

# build the release tarball and then unpack
# to be copied into the image built in the next stage
RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,target=/root/.cache/rebar3 \
    rebar3 as prod tar && \
    tar -zxvf $REBAR_BASE_DIR/prod/rel/*/*.tar.gz -C /opt/rel
```

This stage builds a tarball of the release using the `prod` profile:

<a id="code-snippet--rebar.config"></a>
```erlang
{profiles, [{prod, [{relx, [{dev_mode, false},
                            {include_erts, true},
                            {include_src, false},
                            {debug_info, strip}]}]
            }]}.
```

The profile having `include_erts` set to `true` means the tarball contains the Erlang runtime and can be run on a target that doesn't have Erlang installed. At the end the tarball is unpacked to `/opt/rel` so the stage that will copy the release out of the `releaser` stage does not need to have `tar` installed.

{{% admonition question "Why tar the release at all?" %}}
You might notice that a tarball of the release is created only to be untarred immediately. This is done, instead of copying the contents of the release directory, for two reasons. First, it ensures only what is explicitly defined to be in this version of the release is used. This is less important when building in a docker image since no previous release builds will be in the <code>&#95;build/prod/rel</code> directory, but still good to do. Second, there are some changes made to the release when tarring that are required to use tools like <code>release&#95;handler</code>, for example the boot script is renamed from <code>RelName.boot</code> to <code>start.boot</code>. For more details see the <a href='http://erlang.org/doc/man/systools.html#make&#95;tar-1'>systools</a> documentation.
{{% /admonition %}}

Finally, the deployable image uses a regular OS image (`debian:bullseye`) as the base instead of a prior stage. Any shared libraries needed to run the release are installed first and then the unpacked release from the `releaser` stage is copied to `/opt/service_discovery`:

```dockerfile
FROM ghcr.io/adoptingerlang/service_discovery/debian:bullseye as runner

WORKDIR /opt/service_discovery

ENV COOKIE=service_discovery \
    # write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp \
    # service_discovery specific env variables to act as defaults
    DB_HOST=127.0.0.1 \
    LOGGER_LEVEL=debug \
    SBWT=none

RUN rm -f /etc/apt/apt.conf.d/docker-clean

# openssl needed by the crypto app
RUN --mount=target=/var/lib/apt/lists,id=apt-lists,type=cache,sharing=locked \
    --mount=type=cache,id=apt,sharing=locked,target=/var/cache/apt \
    apt update && apt install --no-install-recommends -y openssl ncurses-bin

COPY --from=releaser /opt/rel .

ENTRYPOINT ["/opt/service_discovery/bin/service_discovery"]
CMD ["foreground"]
```

In the `ENV` command we set some useful defaults for environment variables used when the release is run. `RELX_OUT_FILE_PATH=/tmp` is used by the release start script as the directory to output any files created by the script. This is done because when the release is run `sys.config` and `vm.args` need to be generated from their respective `.src` files and by default these are placed in the same directory as the original `.src` files. We do not want these files written to the release directory, where the `.src` files are, because it is a best practice for the container filesystem to not be written to. This image can be run as any user if it writes to `/tmp`, but it would have to run as `root` if it needed to write anywhere under `/opt/service_discovery`. So writing to `/tmp` allows another best practice to be followed, not running the container as `root`. We can go even further and make the runtime filesystem read-only, we will see that in [Running a Container](#running-a-container).

`/opt/service_discovery` is owned by root and it is recommended to not run the container as root.   If `RELX_OUT_FILE_PATH` is set, its location will be used instead. Here, the `ENV` command is used to ensure the environment variable `RELX_OUT_FILE_PATH` is set to `/tmp` when the container is run.

```shell
$ docker buildx build -o type=docker --target runner --tag service_discovery:$(git rev-parse HEAD) .
```

Or using the script `service_discovery` contains for building and pushing images from CircleCI:

```shell
ci/build_images.sh -l
```

This script will also tag the image twice, once with the git ref, `git rev-parse HEAD`, as done in the manual command, and again with the name of the branch, `git symbolic-ref --short HEAD`. The branch tag is used referencing as a build manifest cache with `--cache-from`. The script will use an image tagged for the `master` branch and the current branch as caches when they are available, and to do so must include `--cache-to=type=inline` in the build command.

Using the current branch name and `master` as the images to inspect for cache hits is less exact than the use of a checksum of `rebar.config` and `rebar.lock` on an image only containing the stage that builds dependencies. There is nothing stopping a build from still making and pushing an explicit image tagged with the checksum and using it also as one of the `--cache-from` images. But, at least with this project, the ease of not needing to juggle additional images, since the Buildkit cache manifest keeps track of all stages, outweighs the potential for a cache miss on dependencies where one wouldn't happen with the other scheme.

Lastly, a difference to note with how the script is used in CircleCI, see [Building and Publishing Images in CI](#building-and-publishing-images-in-ci), compared to here is the `-l` option. In CI we only care about getting the image to the remote registry, so time can be saved by not loading the built image in to the Docker daemon. When building images locally we likely want to then run it, and we will in the next section [Running a Container](#running-a-container), so loading into the Docker daemon is necessary.


## Running a Container {#running-a-container}

Now that we have the image, `docker run` can be used to start the release for local verification and testing. By default, the `CMD`, `foreground`, is passed to the release start script, configured through `ENTRYPOINT` as `/opt/service_discovery/bin/service_discovery`. `CMD` can be overridden if with the last argument passed to `docker run`. Using the `console` command results in an interactive shell when the container is run:

```shell
$ docker run -ti service_discovery console
[...]
(service_discovery@localhost)1>
```

The `-ti` options tell `docker` we want an interactive shell. This is useful for local testing of the image where you want a shell for inspecting the running release. The default, set by `CMD` in the Dockerfile `runner` stage will use `foreground`. There is no use for `-ti` here so they can be dropped as well and the command is simply:

```shell
$ docker run service_discovery
Exec: /opt/service_discovery/erts-10.5/bin/erlexec -noshell -noinput +Bd -boot /opt/service_discovery/releases/8ec119fc36fa702a8c12a8c4ab0349b392d05515/start -mode embedded -boot_var ERTS_LIB_DIR /opt/service_discovery/lib -config /tmp/sys.config -args_file /tmp/vm.args -- foreground
Root: /opt/service_discovery
/opt/service_discovery
```

To prevent accidental shutdown you will not be able to stop this container with `Ctrl-c` so to stop the container use `docker kill <container id>`.

Note that `foreground` is the default because this is how it should be run in production, though it would be in the background:

```shell
$ docker run -d service_discovery
3c45b7043445164d713ab9ecc03e5dbfb18a8d801e1b46e291e1167ab91e67f4
```

Running with `-d` is short for `--detach` and the output is the container id. Logs that are written to `stdout` will be viewable with `docker log <container id>` and we will see in the [next chapter](/docs/production/kubernetes) how in Kubernetes the logs can be routed to your log store of choice. And even when

The running node can also be attached to with `docker exec`, the container id (seen in the output of `docker run -d` or use `docker ps` to find this) and command `remote_console`. It doesn't matter if the container was started with `console` or `foreground`, but this is, of course, most useful when you need a shell for a node you didn't already start with `console`. Because `exec` does not use the `ENTRYPOINT` defined in the image the command to run must start with the release start script, `bin/service_discovery`:

```shell
$ docker exec -ti 3c45 bin/service_discovery remote_console
[...]
(service_discovery@localhost)1>
```

Alternatively, a Linux shell could be run with `docker exec -ti 3c45 /bin/sh`, which will drop you into `/opt/service_discovery` where you could then connect with a `remote_console` or inspect other aspect of the running container.

To exit the remote console do not run `q().`, this will shutdown the Erlang node and the Docker container. Use `Ctrl-g` and enter `q`. `Ctrl-g` enters the shell into what is called the Job Control Mode. To read more about how this shell mode can be used see the [Job Control Mode (JCL Mode) documentation](http://erlang.org/doc/man/shell.html#jcl-mode).

In some cases, such as if the release will not boot, it can be useful to override the `ENTRYPOINT` and get a shell in a container that will attempt to boot the release:

```shell
$ docker run -ti --entrypoint /bin/sh service_discovery
/opt/service_discovery #
```

Lastly, in the previous section we saw how `RELX_OUT_FILE_PATH` was set to `/tmp` so no files would attempt to write to the release directory, which should remain read-only. Docker has a `diff` command that can will show the difference between the image filesystem and the current running container's filesystem:

```shell
$ docker container diff 3c45
C /tmp
A /tmp/sys.config
A /tmp/vm.args
```

This can be a useful command for easily inspecting what your release is writing to disk if you run into problems or want to verify you release is not doing something it shouldn't. And in cases where there are a lot of writes to disk from the release it is best to mount a [volume](https://docs.docker.com/storage/volumes/) and point all writes to it, but for the 2 small configuration files this is not necessary. Unless, when creating the config files from the templates there is sensitive data being used or you want to run the container with `--read-only`. In those cases a [tmpfs](https://docs.docker.com/storage/tmpfs/) mount is recommended. On Linux simply add `--tmpfs /tmp` to the `docker run` command. Then `/tmp` will not be part of the writable layer of the container but a separate volume that only exists in memory and is destroyed when the container is stopped.

{{% admonition danger "Beware Zombies!" %}}
<p>As of Erlang/OTP 19.3 an Erlang node will gracefully shutdown with <code>init:stop()</code> when a <code>TERM</code> signal is received, as is used by Docker and Kubernetes for shutting down containers.</p>

<p>But there are still potential issues you will encounter with zombies in a container. When using <code>docker exec</code> to run <code>remote&#95;console</code>, or any other release script command like <code>ping</code>, will leave zombie processes behind unless the container is started with the argument <code>--init</code> that Docker offers for starting a small init before your entrypoint.</p>

<p>This is usually not an issue, but like with atoms, it certainly can be if the uses aren't limited. An example of something to avoid for this reason, unless running with <code>--init</code> or some other tiny init as pid 0, would be using <code>ping</code> as a health check which is run periodically during the life of a container by the container runtime. A long running container like this will eventually have the kernel process table run out of slots and it will not be possible to create new processes.</p>
{{% /admonition %}}


## Building and Publishing Images in CI {#building-and-publishing-images-in-ci}

Since it would be tedious to manually build and publish images to the registry for each release, it is common to include image building as part of the Continuous Integration process. Usually this is restricted to only occur on a merge to master or a new tag being created, but it can sometimes be useful to also build branch images for testing purposes. In this section we will cover a couple options for automating this process, but whatever CI tool you are already using will be capable of something similar.


### CircleCI {#circleci}

In the [Testing](/docs/development/testing) chapter (coming soon...) we covered [CircleCI](https://circleci.com) for running tests. To build and publish Docker images for `service_discovery` a new job is added named `docker-build-push`. It uses a VM instead of a Docker image as the executor and first installs the latest Docker version, at the time of writing this the version available by default does not support features used in the `service_discovery` `Dockerfile`.

```yaml
jobs:
  docker-build-and-push:
    executor: docker/machine
    steps:
      - run:
          name: Install latest Docker
          command: |
            sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"
            sudo apt-get update

            # upgrade to latest docker
            sudo apt-get install docker-ce
            docker version

            # install buildx
            mkdir -p ~/.docker/cli-plugins
            curl https://github.com/docker/buildx/releases/download/v0.3.0/buildx-v0.3.0.linux-amd64  --output ~/.docker/cli-plugins/docker-buildx
            chmod a+x ~/.docker/cli-plugins/docker-buildx
      - checkout
      - gcp-gcr/gcr-auth
      - run:
          name: Build and push images
          command: |
            ci/build_image.sh -p -t runner -r gcr.io/adoptingerlang
```

After installing the latest Docker the code `service_discovery` repository is checked out and since this is using Google Cloud for the registry and Kubernetes it authenticates with the registry. Lastly, a script found in the `ci/` directory of `service_discovery` is called to build and publish the images. The script uses the `docker build` commands discussed earlier in the chapter to build the individual stages and use `--cache-from` to reference the stages as caches during each build.

To run this job only after tests pass it can be added to the CircleCI workflow with a `requires` constraint on `rebar3/ct`.

```yaml
workflows:
  build-test-maybe-publish:
    jobs:

      [...]

    - docker-build-and-push:
        requires:
        - rebar3/ct
```


### Google Cloud Build {#google-cloud-build}

In 2018 Google released a image build tool [Kaniko](https://github.com/GoogleContainerTools/kaniko) that runs in userspace and does not depend on a daemon, these features enable container image building in environments like a Kubernetes cluster. Kaniko is meant to be run as an image, `gcr.io/kaniko-project/executor`, and can be used as a `step` in Google Cloud Build.

Kaniko offers remote caching for each layer created by a `RUN` command. Builds check the layer cache in the image registry for matches before building any layer. However, the Buildkit features we used in the `service_discovery` Dockerfile are not available in Kaniko, so a separate Dockerfile, `ci/Dockerfile.cb`, is used in the Google Cloud Build configuration, `cloudbuild.yaml`:

```yaml
steps:
- name: 'gcr.io/kaniko-project/executor:latest'
  args:
  - --target=runner
  - --dockerfile=./ci/Dockerfile.cb
  - --build-arg=BASE_IMAGE=$_BASE_IMAGE
  - --build-arg=RUNNER_IMAGE=$_RUNNER_IMAGE
  - --destination=gcr.io/$PROJECT_ID/service_discovery:$COMMIT_SHA
  - --cache=true
  - --cache-ttl=8h

substitutions:
  _BASE_IMAGE: gcr.io/$PROJECT_ID/erlang:22
  _RUNNER_IMAGE: gcr.io/$PROJECT_ID/alpine:3.10
```

Because Kaniko works by checking for each instruction in a cache directory of the registry there is no need for instructing it to use a specific image as a cache like we did with Docker's `--cache-from`. Docker's build cache can act more like Kaniko's by setting it to export the cache metadata to the registry and to export layers for all stages, `--cache-to=type=registry,mode=max`, but this is not supported by most registries at the time of this being written so is not covered.

For more details on using Kaniko in Google Cloud Build see their [Using Kaniko cache](https://cloud.google.com/cloud-build/docs/kaniko-cache) documentation.


## Next Steps {#next-steps}

In this chapter we built images for our service and finished off by creating a CI pipeline for continually building and publishing those images when changes are made to the repository. In the next chapter we will cover how to build a deployment to Kubernetes from these images. After we are running in Kubernetes the following chapter covers observability, such as connecting to a running node, well structured logs, reporting metrics and distributed traces.

<div class="pagination">
  <div><a href="/docs/production/releases/">← Prev</a></div>
  <div><a href="/docs/production/kubernetes/">Next →</a></div>
</div>
