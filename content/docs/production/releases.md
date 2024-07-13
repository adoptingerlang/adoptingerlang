+++
title = "Releases"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
date = 2019-09-24T07:43:00+00:00
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:09+00:00
draft = false
[menu]
  [menu.main]
    weight = 2002
    identifier = "releases"
    parent = "production"
+++

In [OTP at a High Level](/docs/development/otp_high_level/) we took a bird's-eye view of how applications are combined into a release and in the following chapters of [Development](/docs/development/) we built and tested OTP applications. We have discussed how an Erlang system is setup differently from many other languages which have a single entry point the project's code, like a `main()` function that is called when starting the program. Ultimately starting an Erlang node that runs your code is similar enough to other languages and runtimes, and can be packaged up in a Docker container to run like any other container, and does not appear so different once it is being run by booting a container. But getting to the point that you have a bundled up release with an easy to use shell script, or a Docker image ready to run, can be confusing at first and give the impression Erlang is too different or too difficult to be running in your environment.

This chapter will give some low level details about how a release is constructed, but the main purpose is to use the [service_discovery](https://adoptingerlang.org/gh/servicediscovery) project to show how to use `rebar3` for building releases suited for local development, then a production ready release, and finally how to use the tools provided by `rebar3` to configure and operate a release. We will show that the advantages from the flexibility of a system based on booting applications which run concurrently does not have to come with the cost of esoteric operations.


## The Nitty Gritty {#the-nitty-gritty}

Going back to the operating system comparison, starting an Erlang release is similar to an operating system boot sequence that is followed by an init system starting services. An Erlang node starts by running instructions found in a [boot file](http://erlang.org/doc/man/script.html). The instructions load modules and start applications. Erlang provides functions for generating a boot script from a list of all required applications, defined in a release resource file with extension ".rel", and the corresponding application resource file, the ".app" file each application has. The application resource file defines the modules the boot script must load and the dependencies of each application so that the boot script will start applications in the correct order. When only the applications used in a release are bundled together with the boot script, to be copied and installed on a target, it is called a **target system**.

In the earlier days of Erlang/OTP, there was only `systools` and its functions for generating boot scripts from release resource files. Back then, release handling was a manual process around which users built their own tooling. Then came `reltool`, a release management tool that ships with Erlang/OTP and was meant to ease the creation of releases—it even has a GUI. While creating and installing target systems has never been provided outside of an example module found in the `sasl` application, `sasl/examples/src/target_system.erl`.

Releases continued to be mysterious and difficult to build to many users. relx was created with the goal of making release creation and management so simple that users no longer felt it was a burden best not undertaken -- this is done in part through requiring minimal configuration to get started and by including tools for runtime management in the generated release. When Rebar3 was started, it bundled relx to provide its release building functionality.

Along with building and packaging `relx` comes with a shell script for starting a release and interacting with the running release. While running a release can be as simple as `erl` (which itself runs a boot script built from `start.script` you can find in the `bin/` directory of your Erlang install) the provided script handles setting appropriate arguments to point to configuration files, attaching a remote console to a running node, running functions on a running node and more.

In the following sections we will dissect release building for the [service_discovery](https://adoptingerlang.org/gh/servicediscovery) project. The focus is on real world usage of the tools and not the low level details of constructing and running a release.


## Building a Development Release {#building-a-development-release}

The first thing to look at in [service_discovery](https://adoptingerlang.org/gh/servicediscovery) is the `relx` section of `rebar.config`:

<a id="code-snippet--rebar.config"></a>
```erlang
{relx, [{release, {service_discovery, {git, long}},
         [service_discovery_postgres,
          service_discovery,
          service_discovery_http,
          service_discovery_grpc,
          recon]},

        {sys_config, "./config/dev_sys.config"},
        {vm_args, "./config/dev_vm.args"},

        {dev_mode, true},
        {include_erts, false},

        {extended_start_script, true},

        {overlay, [{copy, "apps/service_discovery_postgres/priv/migrations/*", "sql/"}]}]}.
```

The `release` tuple in the `relx` configuration list defines a release's name, version and the applications
included in the release. The version here is `{git, long}` which tells `relx` to use the full git sha reference of the current commit as the version of the release. The applications the release will boot includes the Postgres storage backend, the application that is the main interface to the services and the DNS setup, HTTP and grpc frontends and a tool useful for inspecting production nodes, `recon`.

The order the applications are listed is important here. When constructing the boot script a stable sort on all applications based on their dependencies is done to decide the order to start them. However, when the dependency order is not specific enough to make a decision, the order defined in the list is kept. Each of `service_discovery_postgres`, `service_discovery_http` and `service_discovery_grpc` depend on `service_discovery`, resulting in the latter being the first to start. The next one will then be `service_discovery_postgres` because it is listed first. This is important because we need the storage backend available before the HTTP and grpc services are usable.

`rebar3 release` runs `relx` with a configuration based on the `relx` section of `rebar.config` and the Rebar3 project structure, allowing `relx` to find the necessary applications for building the release.

```shell
$ rebar3 release
===> Verifying dependencies...
===> Compiling service_discovery_storage
===> Compiling service_discovery
===> Compiling service_discovery_http
===> Compiling service_discovery_grpc
===> Compiling service_discovery_postgres
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /app/src/_build/default/lib
          /app/src/apps
          /root/.cache/erls/otps/OTP-22.1/dist/lib/erlang/lib
          /app/src/_build/default/rel
===> Resolved service_discovery-c9e1c805d57a78d9eb18af1124962960abe38e70
===> Dev mode enabled, release will be symlinked
===> release successfully created!
```

The first `relx` step seen in the output is "Resolving OTP Applications" followed by a list of directories it will search for built applications. For each application in the `relx` release's configuration, in this case `service_discovery_postgres`, `service_discovery`, `service_discovery_http`, `service_discovery_grpc`, and `recon`, `relx` will find the directory of the built application and then do the same for any application listed in its `.app` file.

{{% admonition note %}}
The application <code>sasl</code> is not included in the <code>relx</code> config's list of applications. By default in the Rebar3 template it is in the list. This is because <code>sasl</code> is required for certain release operations. The <code>sasl</code> application includes <code>release&#95;handler</code> which provides functionality for performing release upgrades and downgrades. Since we are focused here on creating containers which are replaced instead of live upgraded <code>sasl</code> does not need to be included.
{{% /admonition %}}

Because this release is built with `{dev_mode, true}`, symlinks are created in the release's lib directory that point to each application instead of being copied:

```shell
$ ls -l _build/default/rel/service_discovery/lib
lrwxrwxrwx ... service_discovery-c9e1c80 -> .../_build/default/lib/service_discovery
```

The same is also done for the runtime configuration files `dev_sys.config` and `dev_vm.args`:

```shell
$ ls -l _build/default/rel/service_discovery/releases/c9e1c805d57a78d9eb18af1124962960abe38e70
lrwxrwxrwx [...] sys.config -> [...]/config/dev_sys.config
lrwxrwxrwx [...] vm.args -> [...]/config/dev_vm.args
```

This allows for a faster feedback loop when running our release for local testing. Simply stopping and starting the release again will pick up any changes to beam files or configuration, no need to run `rebar3 release`.

{{% admonition warning %}}
On Windows the <code>dev&#95;mode</code> of relx won't necessary work but it will fallback to copying.
{{% /admonition %}}

The full filesystem tree of the development release now looks like:

```shell
_build/default/rel/service_discovery/
├── bin/
│   ├── install_upgrade.escript
│   ├── nodetool
│   ├── no_dot_erlang.boot
│   ├── service_discovery
│   ├── service_discovery-c9e1c805d57a78d9eb18af1124962960abe38e70
│   └── start_clean.boot
├── lib/
│   ├── acceptor_pool-1.0.0 -> /app/src/_build/default/lib/acceptor_pool
│   ├── base32-0.1.0 -> /app/src/_build/default/lib/base32
│   ├── chatterbox-0.9.1 -> /app/src/_build/default/lib/chatterbox
│   ├── dns-0.1.0 -> /app/src/_build/default/lib/dns
│   ├── elli-3.2.0 -> /app/src/_build/default/lib/elli
│   ├── erldns-1.0.0 -> /app/src/_build/default/lib/erldns
│   ├── gproc-0.8.0 -> /app/src/_build/default/lib/gproc
│   ├── grpcbox-0.11.0 -> /app/src/_build/default/lib/grpcbox
│   ├── hpack-0.2.3 -> /app/src/_build/default/lib/hpack
│   ├── iso8601-1.3.1 -> /app/src/_build/default/lib/iso8601
│   ├── jsx-2.10.0 -> /app/src/_build/default/lib/jsx
│   ├── recon-2.4.0 -> /app/src/_build/default/lib/recon
│   ├── service_discovery-c9e1c80 -> /app/src/_build/default/lib/service_discovery
│   ├── service_discovery_grpc-c9e1c80 -> /app/src/_build/default/lib/service_discovery_grpc
│   ├── service_discovery_http-c9e1c80 -> /app/src/_build/default/lib/service_discovery_http
│   └── service_discovery_storage-c9e1c80 -> /app/src/_build/default/lib/service_discovery_storage
├── releases/
│   ├── c9e1c805d57a78d9eb18af1124962960abe38e70
│   │   ├── no_dot_erlang.boot
│   │   ├── service_discovery.boot
│   │   ├── service_discovery.rel
│   │   ├── service_discovery.script
│   │   ├── start_clean.boot
│   │   ├── sys.config.src -> /app/src/config/sys.config.src
│   │   └── vm.args.src -> /app/src/config/vm.args.src
│   ├── RELEASES
│   └── start_erl.data
└── sql
    ├── V1__Create_services_endpoints_table.sql
    └── V2__Add_updated_at_trigger.sql
```

The last directory, `sql`, is created by an `overlay`, `{copy, "apps/service_discovery_postgres/priv/migrations/*", "sql/"}`. An `overlay` tells `relx` about files to include that are outside of the regular release structure of applications and boot files. It supports making directories, basic templating with `mustache` and copying files. In this case we only need to copy the `service_discovery_postgres` application's migrations to a top level directory of the release `sql`. The migrations would be included anyway because they are in the `priv` of an application that is part of the release, but since we intend to only have one release available at a time (see notice box for more information) it simplifies the migration scripts we will see later to keep them in a known top level directory.

{{% admonition info %}}
The OTP release structure supports having multiple versions of the same release. They share the same <code>lib</code> directory but could have different versions of each application and even different <code>erts</code>. This is why there is a <code>releases</code> directory with a version number directory under it and the file <code>RELEASES</code>. Having the SQL files in a directory shared by all versions of the release could be problematic in such an environment. But since we are focused on building self contained releases that will run separately from any other in a container we can assume there is only ever one version.
{{% /admonition %}}

Since the release uses the Postgres storage backend,a database needs to be running and accessible before starting the release. Running Docker Compose at the top level of the project will bring up a database and run the migrations:

```shell
$ docker-compose up
```

{{% admonition tip %}}
You don't have to start everything to get an Erlang shell from your release. Every release built with Rebar3 comes with a boot script called <code>start&#95;clean</code> which only starts the <code>kernel</code> and <code>stdlib</code> applications. This can be run with the command <code>console&#95;clean</code> and can be useful for debugging purposes.
{{% /admonition %}}

To boot the development release to an interactive Erlang shell run the extended start script with command `console`:

```shell
$ _build/default/rel/service_discovery/bin/service_discovery console
Erlang/OTP 22 [erts-10.5] [source] [64-bit] [smp:1:1] [ds:1:1:10] [async-threads:30] [hipe]

(service_discovery@localhost)1>
```

With `service_discovery` running, `curl` can be used to access the HTTP interface. The following commands create a service `service1`, verify it was created by listing all services, registers an endpoint with the service at IP `127.0.0.3` and  adds a named port `http` on port 8000.

```shell
$ curl -v -XPUT http://localhost:3000/service \
    -d '{"name": "service1", "attributes": {"attr-1": "value-1"}}'
$ curl -v -XGET http://localhost:3000/services
[{"attributes":{"attr-1":"value-1"},"name":"service1"}]
$ curl -v -XPUT http://localhost:3000/service/service1/register \
    -d '{"ip": "127.0.0.3", "port": 8000, "port_name": "http", "tags": []}'
$ curl -v -XPUT http://localhost:3000/service/service1/ports \
    -d '{"http": {"protocol": "tcp", "port": 8000}}'
```

`service_discovery` DNS server is running on port 8053, use `dig` to see that `service1` is a registered endpoint at the correct IP and that a service (SRV) DNS query returns the port and DNS name for the service.

```shell
$ dig -p8053 @127.0.0.1 A service1
;; ANSWER SECTION:
service1.		3600	IN	A	127.0.0.3
$ dig -p8053 @127.0.0.1 SRV _http._tcp.service1.svc.cluster.local
;; ANSWER SECTION:
_http._tcp.service1.svc.cluster.local. 3600 IN SRV 1 1 8000 service1.svc.cluster.local.
```


## Building a Production Release {#building-a-production-release}

Preparing a release to be deployed to production requires different options than what is best used during local development. Rebar3 profiles allow us to override and add to the `relx` configuration. This profile is commonly named `prod`:

<a id="code-snippet--rebar.config"></a>
```erlang
{profiles, [{prod, [{relx, [{sys_config_src, "./config/sys.config.src"},
                            {vm_args_src, "./config/vm.args.src"},
                            {dev_mode, false},
                            {include_erts, true},
                            {include_src, false},
                            {debug_info, strip}]}]
            }]}.
```

We have two overridden configuration values in the `prod` profile. `dev_mode` is set to `false` so all content is copied into the release directory, we can't utilize symlinks to the `_build` directory from another machine and a production release should be an immutable snapshot of the project. `include_erts` copies the Erlang runtime and the Erlang/OTP applications depended on by the release into the release directory and configures the boot script to point to this copy of the runtime.

The entries added to the configuration are setting `include_src` to `false`, `debug_info` to `strip`, and `_src` versions of the configuration files. Running the release in production doesn't require the source code, so we set `include_src` to false in order to drop it from the final release to save on space. Additional space is saved by stripping debug information from the beam files with `debug_info` set to `strip`. Debug information is used by tools like the debugger, `xref` and `cover` but in a release those tools won't be used and, unless explicitly included, won't even be available.

`sys_config_src` and `vm_args_src` take precedence over the entries `sys_config` and `vm_args` we had in the default profile, these two will be discussed more in the following section [Runtime Configuration](#runtime-configuration). There will be a warning printed when the production release is built in case the user did not intend for this, but we are doing it on purpose so the warning can be ignored.

Building with the production profile enabled results in artifacts being written to the profile directory `_build/prod/`:

```shell
$ rebar3 as prod release
===> Verifying dependencies...
===> Compiling service_discovery_storage
===> Compiling service_discovery
===> Compiling service_discovery_http
===> Compiling service_discovery_grpc
===> Compiling service_discovery_postgres
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /app/src/_build/prod/lib
          /app/src/apps
          /root/.cache/erls/otps/OTP-22.1/dist/lib/erlang/lib
===> Resolved service_discovery-c9e1c805d57a78d9eb18af1124962960abe38e70
===> Both vm_args_src and vm_args are set, vm_args will be ignored
===> Both sys_config_src and sys_config are set, sys_config will be ignored
===> Including Erts from /root/.cache/erls/otps/OTP-22.1/dist/lib/erlang
===> release successfully created!
```

Viewing the tree of the new `prod` profile's release directory we see:

<a id="code-snippet--prod-rel-structure"></a>
```sh
_build/prod/rel/service_discovery
├── bin
│   ├── install_upgrade.escript
│   ├── nodetool
│   ├── no_dot_erlang.boot
│   ├── service_discovery
│   ├── service_discovery-c9e1c805d57a78d9eb18af1124962960abe38e70
│   └── start_clean.boot
├── erts-10.5
│   ├── bin
│   ├── doc
│   ├── include
│   ├── lib
│   └── man
├── lib
│   ├── acceptor_pool-1.0.0
│   ├── asn1-5.0.9
│   ├── base32-0.1.0
│   ├── chatterbox-0.9.1
│   ├── crypto-4.6
│   ├── dns-0.1.0
│   ├── elli-3.2.0
│   ├── erldns-1.0.0
│   ├── gproc-0.8.0
│   ├── grpcbox-0.11.0
│   ├── hpack-0.2.3
│   ├── inets-7.0.8
│   ├── iso8601-1.3.1
│   ├── jsx-2.10.0
│   ├── kernel-6.5
│   ├── mnesia-4.16
│   ├── public_key-1.6.7
│   ├── recon-2.4.0
│   ├── service_discovery-c9e1c80
│   ├── service_discovery_grpc-c9e1c80
│   ├── service_discovery_http-c9e1c80
│   ├── service_discovery_storage-c9e1c80
│   ├── ssl-9.3.1
│   └── stdlib-3.10
├── releases
│   ├── c9e1c805d57a78d9eb18af1124962960abe38e70
│   │   ├── no_dot_erlang.boot
│   │   ├── service_discovery.boot
│   │   ├── service_discovery.rel
│   │   ├── service_discovery.script
│   │   ├── start_clean.boot
│   │   ├── sys.config.src
│   │   └── vm.args.src
│   ├── RELEASES
│   └── start_erl.data
└── sql
    ├── V1__Create_services_endpoints_table.sql
    └── V2__Add_updated_at_trigger.sql
```

There are no symlinks under `lib` and OTP applications like `stdlib-3.10` are included. At the top of the tree is `erts-10.5` which contains the Erlang runtime, `bin/beam.smp`, along with executables like `erlexec`, `erl` and `escript` required for running and interacting with a release.

To build the target system of the release we run the `tar` command in the `prod` profile:

```shell
$ rebar3 as prod tar
```

Now we have a tarball `_build/prod/rel/service_discovery/service_discovery-c9e1c805d57a78d9eb18af1124962960abe38e70.tar.gz` that can be copied to any compatible host, unpacked and run. But before we do that we need to know how to configure `sys.config.src` and `vm.args.src` when the release is booted.


## Runtime Configuration {#runtime-configuration}

In the `service_discovery` project we have two files for configuration under the `config/` directory that are included in the production release: `vm.args.src` and `sys.config.src`. These files act as templates to be filled in at runtime based on environment variables. There are two separate files because running a release involves two levels of configuration. First, there is the underlying Erlang virtual machine's settings. These values have to be set before the VM has started, so they cannot be part of a term file like `sys.config` which requires a running VM to read and parse the Erlang term file. Instead, the VM arguments are passed directly to `erl` -- `erl` being the command used to boot a release. To simplify this, the `erl` command has an `-args_file` argument to allow command-line arguments to be read from a plain text file. This file is commonly named `vm.args`.

The second level is the configuration for the Erlang applications that make up the release. This is done with a file passed to `erl` through the `-config` argument. The file is a list of 2-tuples where the first element is the name of the application to set the environment of and the second element is a list of key-value pairs to set in the environment.

Of course static files can be pretty limiting and it has become common to want to set configuration through OS environment variables. To offer flexibility and support for environment variable configuration the release start script generated when using Rebar3 can replace variables of the form `${FOO}` with the value found in the current environment. This is done automatically if the files end with extension `.src`.

In the `relx` configuration we use `vm_args_src` and `sys_config_src` to include the files and signal that they are templates -- this is necessary so that the release building does not attempt to verify that `sys.config` is a proper list of Erlang terms, which, for example, `{port, ${PORT}}` is not:

```erlang
{relx, [...
        {sys_config_src, "config/sys.config.src"},
        {vm_args_src, "config/vm.args.src"},
        ...
       ]}.
```

In the Docker and Kubernetes chapters we will discuss why we need to set `sbwt`, but for now we just care about how it would be done in `vm.args.src`:

```shell
+sbwt ${SBWT}
```

In `sys.config.src` we will make the `logger` level a variable as well, so we could, for example, turn on `debug` or `info` level logging to get more details when investigating the deployed service:

```erlang
{kernel, [{logger_level, ${LOGGER_LEVEL}}]}
```

Now when running the release we must set these variables or the release will fail to start.

```shell
$ DB_HOST=localhost LOGGER_LEVEL=debug SBWT=none \
  _build/prod/rel/service_discovery/bin/service_discovery console
Erlang/OTP 22 [erts-10.5] [source] [64-bit] [smp:1:1] [ds:1:1:10] [async-threads:30] [hipe]

(service_discovery@localhost)1>
```

The errors produced when running without the necessary environment variables set can be confusing. There is currently no validation done to check that all environment variables used in the configuration files are set and then print out which are missing. Instead the missing variables are replaced with empty strings. If the variable is used in a string, like `"${DB_HOST}"` the applications will start but be unable to connect to the database. When the missing variable creates a `sys.config` that is not able to be parsed, like in the case of `${LOGGER_LEVEL}`, there will be a syntax error:

```shell
$ DB_HOST=localhost SBWT=none \
  _build/prod/rel/service_discovery/bin/service_discovery console
{"could not start kernel pid",application_controller,"error in config file \"/app/src/_build/prod/rel/service_discovery/releases/71e109d8f34ef5e5ccfcd666e0d9e544836044f1/sys.config\" (48): syntax error before: ','"}
could not start kernel pid (application_controller) (error in config file "/home/tristan/Devel/service_discovery/_build/prod/rel/service_discovery/releases/71e109d8f34ef5e5ccfcd666e0d9e544836044f1/sys

Crash dump is being written to: erl_crash.dump...done
```

When a value in `vm.args` is missing you will likely see an error about the argument given to a flag. For `+sbwt ${SBWT}` it results in attempting to use the next line as the argument to `+sbwt`, in this case it is `+C multi_time_warp` producing a boot error that `+C` is not a valid :

```shell
$ DB_HOST=localhost LOGGER_LEVEL=debug \
  _build/prod/rel/service_discovery/bin/service_discovery console
bad scheduler busy wait threshold: +C
Usage: service_discovery [flags] [ -- [init_args] ]
The flags are:
...
```

Not all failures from missing variables are so simple and they can result in very odd crashes, or no crash on boot but failures within the application that depends on the value. We hope to improve this in future Rebar3 versions to enable a check before startup that will fail with a list of missing environment variables. But for now if you are seeing odd behavior, or a crash that you can't understand, checking the environment variables is a good place to start.


## Coming Up {#coming-up}

To get a feel for what we will be doing in the next chapters, copy `_build/prod/rel/service_discovery/service_discovery-c9e1c805d57a78d9eb18af1124962960abe38e70.tar.gz` to `/tmp/service_discovery`, unpack it and start the node.

```shell
$ export LOGGER_LEVEL=debug
$ export SBWT=none
$ export DB_HOST=localhost
$ mkdir /tmp/service_discovery
$ cp _build/prod/rel/service_discovery/service_discovery-c9e1c805d57a78d9eb18af1124962960abe38e70.tar.gz /tmp/service_discovery
$ cd /tmp/service_discovery
$ tar -xvf service_discovery-c9e1c805d57a78d9eb18af1124962960abe38e70.tar.gz && rm service_discovery-c9e1c805d57a78d9eb18af1124962960abe38e70.tar.gz
...
$ ls
bin  erts-10.5  lib  releases
$ bin/service_discovery console
...
(service_discovery@localhost)1>
```

Running with `console` gives an interactive Erlang shell. To run the release without an interactive shell use `foreground`, which is how we will be running the release in a Docker container in the next chapter. With the release running with `foreground` or `console`, open a separate terminal and try the same script with the argument `remote_console`:

```shell
$ export LOGGER_LEVEL=debug
$ export SBWT=none
$ export DB_HOST=localhost
$ bin/service_discovery remote_console
...
(service_discovery@localhost)1>
```

`remote_console` uses `-remsh` [erl](http://erlang.org/doc/man/erl.html#flags) flag for connecting an interactive shell to the running release. The command knows which running Erlang node to connect to based on the same configuration, from `vm.args`, that sets the name and cookie for the node that was run originally. This means any environment variables used to populate `vm.args` when the node is started must be set the same when connecting the remote console.

<div class="pagination">
  <div><a href="/docs/production/">← Prev</a></div>
  <div><a href="/docs/production/docker/">Next →</a></div>
</div>
