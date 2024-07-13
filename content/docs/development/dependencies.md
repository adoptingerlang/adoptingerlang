+++
title = "Dependencies"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
date = 2019-10-26T15:03:00+00:00
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:09+00:00
draft = false
[menu]
  [menu.main]
    weight = 2006
    identifier = "dependencies"
    parent = "development"
+++

There have been large shifts in how people approached programming over the last 20 years. Whereas material of yesterday focused a lot on writing reusable and extendable components within each project, the current trend is to make small, isolated projects that each individually ends up being easy to throw away and replace. Current microservice implementations and Javascript dependency trees may lead us to believe no project is too small to be discardable.

Ironically, the shift towards strong isolation and clearly-defined boundaries—whether they be interfaces, network APIs, or protocols—that was necessary for small single-purpose components has given rise to what is possibly the greatest amount of code reuse we've ever seen. Reuse materialized from shared needs across various people and projects, not from the mythical ability of providing a perfectly extendable class hierarchy. There is so much reuse today that we have reached the point where folks are starting to ask if maybe, just maybe, we aren't reusing more code than we should: each library comes with risks and liabilities, and we're exposing ourselves to a lot of it for the sake of moving fast.

Erlang, for its own part, has always lived and breathed strong isolation with clearly defined message-passing protocols wrapped in functional interfaces. Its small community size of, generally, experienced developers probably made it lag behind other communities when it comes to libraries and packaging. Despite prior efforts on package managers like [CEAN](https://web.archive.org/web/20111026040038/http://cean.process-one.net/) by ProcessOne and Faxien, both of which installed OTP applications globally, it was not until 2009 that it really became easy to install libraries from other people. This was made possible through the first version of `rebar`, which favored per-project dependency installation, and was the first to come as an [escript](http://erlang.org/doc/man/escript.html), a portable script that required no installation by the developer.

Erlang libraries proliferated all over GitHub and similar hosted version control services haphazardly, and at some point, it became possible to find over twelve different versions of the same PostgreSQL driver, which all had the same name and similar versions, but all did a few things differently.

It took a decent push in the back from Elixir, which was bringing in newer perspectives and tools such as `mix` and hex.pm, for the Erlang community to collect themselves and get to a more understandable ecosystem. In modern days, Erlang's community is still small, but it has adopted better practices, and now shares its package and library infrastructure with Elixir and half a dozen smaller languages on the same virtual machine.

In this chapter, we'll see how modern library usage and community integration is done within the Erlang world by answering questions like:

-   What is a library?
-   How can I use one as a dependency?
-   What is a dependency's lifecycle?
-   How can I use Elixir dependencies?
-   What do I do if my work uses a monorepo?


## Using Open Source Libraries {#using-open-source-libraries}

Erlang's open source dependencies are just OTP applications, like every other library in a release. As such, all that's required to use an open source library is to have that OTP application visible to the Erlang toolchain. This is conceptually simple, but every language and community takes slightly different approaches here. We have choices to make between installing libraries globally on a computer, within shared environments, or locally within each project. Then, there are norms regarding versioning and publishing, which also must be adhered to. This section will show how that's all done, but first we'll take a detour through the expectations of Rebar3 regarding your project's lifecycle.


### Rebar3 Expectations {#rebar3-expectations}

Some of the tricky decisions around open source work have been enshrined in Rebar3, and it's often easier to go with the flow than fight it, especially when you're first starting. Rebar3 has initially been built in a mid-size corporation writing services in a semi-private mode: open-source dependencies are both used and published, but some of the code is to remain private forever. Multiple unrelated services are developed at the same time, and not all of them will necessarily be on the same Erlang version with the same libraries. Some programs will only be deployed through rolling restarts (as is usual everywhere in the cloud) but some systems will absolutely require hot code loading. Rebar3 was also developed at a time where versioning practices were downright messy in the community: many libraries had different versions in their `.app` files than their git tag on GitHub, and documentation yet again mentioned another version. Back then, roughly 4 out of 5 libraries on hex.pm had not even reached version 1.0.0 either.

As such, Rebar3 has the following properties:

-   Rebar3 is a declarative build tool. You provide a configuration, and it executes code to satisfy that configuration. If you want to run custom scripts and extend the build, it specifically provides [hooks](https://www.rebar3.org/docs/configuration/configuration/#hooks) or [plugins](https://www.rebar3.org/docs/configuration/plugins/) interfaces to do so. There are also ways to gain [Dynamic Configuration](https://www.rebar3.org/docs/configuration/config_script/) to more flexibly populate config files.
-   As part of that declarative approach, commands in Rebar3 are defined with dependency sequences. For example, the `rebar3 compile` task relies on `rebar3 get-deps` (or rather, a private form of it that locks dependencies), and will run it for you. The command `rebar3 tar` will implicitly call a sequence that includes `get-deps -> compile -> release -> tar` for you. Rebar3 therefore knows that to compile your project, its dependencies are needed.
-   Rebar3 defines its own working area within the `_build` directory; it expects to have control over what goes in there, and for you not to track that directory in source control nor rely on its internal structure. When it creates an artifact the user will want to use directly, it outputs the path to that artifact in the terminal. For example `rebar3 escriptize` will print the path to the generated escript in `_build/default/bin/` after each run, `rebar3 ct` prints the path to the Common Test HTML output if tests fail and `rebar3 tar` prints the path to the release tarball.
-   All projects are built locally within their own directory based on the Erlang runtime currently loaded in your environment, i.e. the `erl` found in `$PATH`.
-   All dependencies are fetched to the project's `_build` directory.
-   All dependencies can define their own dependencies, and Rebar3 will recognize this and fetch them to the root project's `_build` directory as well.
-   Since version numbers are unreliable (even with semantic versioning), we consider versions as information for humans, not build tools. In case of libraries that end up declared multiple times, the one closest to the project root is chosen, while [warnings](https://www.rebar3.org/docs/configuration/dependencies) are emitted during the first build (it knows it is the "first" because there is no lock for the dependency) to let the user know versions not being used, with the assumption that libraries declared closer to the root project are used more thoroughly.
-   Circular dependencies are forbidden.
-   Rebar3 supports composable per-project [<span class="underline">profiles</span>](https://www.rebar3.org/docs/configuration/profiles) that let you segment or combine configuration settings, such as dependencies that are to only be used for tests or only in a specific environment (say on a specific target OS). There are no restrictions on the number or names of profiles, but there are four profiles, `default`, `test`, `docs`, and `prod`, which are used by Rebar3 automatically for specific tasks.
-   Builds are meant to be repeatable. Dependencies are locked, and only upgraded when specifically asked to. The version in `rebar.lock` takes precedence of whatever version is declared in `rebar.config` until `rebar3 upgrade` is called to update the lock file.
-   Dependencies are always built with the `prod` profile applied. Rebar3 will always check that they match the lock file and that build artifacts are present, but will not detect code changes done by hand within a dependency.
-   Rebar3 assumes that you'll sometimes need to tweak the configuration of libraries you do not control and supports [overrides](https://www.rebar3.org/docs/configuration#section-overrides) for this purpose.
-   The tool assumes you're developing with a source control mechanism such as `git` or `hg` (mercurial), which means that switching branches may switch dependency versions in the lock file. Since Rebar3 verifies dependencies before each build, it will automatically re-fetch libraries to get the locked version for the current branch if there is a change when switching branches.
-   To ease contribution and publishing, Rebar3 does not natively support ways to use relative paths to declare libraries, since this could make builds brittle, non-repeatable, and non-portable when publishing code.
-   Knowing relative paths are very common when making changes within the dependencies of a project, [`_checkouts`](https://www.rebar3.org/docs/configuration/dependencies/#checkout-dependencies) allow for an automatic way to temporary override of a dependency with a local copy. For other use cases [plugins](https://www.rebar3.org/docs/configuration/plugins/#recommended-plugins) allow the creation of [custom resource types](https://www.rebar3.org/docs/extending/custom_dep_resources/).
-   Rebar3 is not an installer or runner of end-user applications and does not support anything related to that; it aims to generate build artifacts that you can then install through the correct dedicated channels. Rebar3 does not expect or need to ever be on a production device or server.
-   Rebar3 is not a sandbox. While it will make sure all dependencies you download match the right signatures and provides repeatable builds, it cannot guarantee that script files invoked during builds or [parse transforms](https://stackoverflow.com/questions/2416192/is-there-a-good-complete-tutorial-on-erlang-parse-transforms-available) run during compilation are ever going to be safe, and has no intention of taking on that responsibility. Plugins are also not locked automatically, and it is up to library authors to pin versions in cases where they impact compilation.

That's a lot of information, but we find that it's useful to know about it before getting in too deep with dependencies. If you operate under assumptions that Rebar3 works like Javascript's `npm`, Elixir's `mix`, Go's toolchain, or even the original `rebar`, you might find some behaviours confusing. With that being said, let's use dependencies.


### Declaring Dependencies {#declaring-dependencies}

Since dependencies are all project-local, they have to be declared in the `rebar.config` file of your project. This will let Rebar3 know it needs to fetch them, build them, and make them available to your project. All dependencies must be individual OTP applications, so that they can be versioned and handled independently from each other.

The following formats are valid:

<a id="code-snippet--dep-types"></a>
```erlang
{deps, [
    %% git dependencies
    {AppName, {git, "https://host.tld/path/to/app", {tag, "1.2.0"}}},
    {AppName, {git, "https://host.tld/path/to/app", {branch, "master"}}},
    {AppName, {git, "https://host.tld/path/to/app", {ref, "aed12f..."}}},
    %% similar format for mercurial deps
    {AppName, {hg, "https://host.tld/path/to/app", {RefType, Ref}}}
    %% hex packages
    AppName, % latest known version (as per `rebar3 update`)
    {AppName, "1.2.0"},
    {AppName, "~> 1.2.0"}, % latest version at 1.2.0 or above, and below 1.3.0
    {AppName, "1.2.0", {pkg, PkgName}}, % when application AppName is published with package name PkgName
]}.
```

Additionally, plugins allow to define [custom resource definitions](https://www.rebar3.org/docs/extending/custom_dep_resources/) that let you add new types of dependencies to projects.

Let's see how this would work with a project we've created specifically for this book, [service_discovery](https://adoptingerlang.org/gh/servicediscovery). Open up the `rebar.config` file and you'll see:

<a id="code-snippet--service-discovery.deps"></a>
```erlang
...

{deps, [
    {erldns,
     {git, "https://github.com/tsloughter/erldns.git",
     {branch, "revamp"}}},
    {dns,
     {git, "https://github.com/tsloughter/dns_erlang.git",
     {branch, "hex-deps"}}},

    recon,
    eql,
    jsx,
    {uuid, "1.7.5", {pkg, uuid_erl}},
    {elli, "~> 3.2.0"},
    {grpcbox, "~> 0.11.0"},
    {pgo,
     {git, "https://github.com/tsloughter/pgo.git",
     {branch, "master"}}}
]}.

...
```

Both git and hex dependencies can work together for most projects. The only exception is hex packages, which can only depend on other hex packages. Let's compile the whole project and step through what is going on:

<a id="code-snippet--service-discovery.compile"></a>
```sh
$ rebar3 compile
===> Fetching covertool v2.0.1
===> Downloaded package, caching at /Users/ferd/.cache/rebar3/hex/hexpm/packages/covertool-2.0.1.tar
===> Compiling covertool
...
===> Verifying dependencies...
===> Fetching dns (from {git,"https://github.com/tsloughter/dns_erlang.git",
               {ref,"abc562548e8a232289eec06cf96ce7066261cc9d"}})
===> Fetching provider_asn1 v0.2.3
===> Downloaded package, caching at /Users/ferd/.cache/rebar3/hex/hexpm/packages/provider_asn1-0.2.3.tar
===> Compiling provider_asn1
===> Fetching elli v3.2.0
...
===> Fetching rfc3339 v0.9.0
===> Version cached at /Users/ferd/.cache/rebar3/hex/hexpm/packages/rfc3339-0.9.0.tar is up to date, reusing it
===> Compiling quickrand
===> Compiling uuid
===> Compiling recon
...
===> Compiling service_discovery_storage
===> Compiling service_discovery
===> Compiling service_discovery_http
===> Compiling service_discovery_grpc
===> Compiling service_discovery_postgres
```

Running this build, you can see multiple things going on:

1.  Plugins (such as `covertool`) are fetched and compiled before anything else goes on
2.  Actual dependencies of the project (such as `dns` and `elli`) are fetched
3.  Dependencies are compiled (`quickrand` and others)
4.  The main applications are compiled

If you were to try again and run things from scratch while deleting the `rebar.lock` file, things would be a bit different. You might see something looking like this as part of the output:

<a id="code-snippet--service-discovery.resolve"></a>
```nil
...
===> Fetching dns (from {git,"https://github.com/tsloughter/dns_erlang.git",
               {branch,"hex-deps"}})
...
===> Skipping dns (from {git,"git://github.com/dnsimple/dns_erlang.git",
               {ref,"b9ee5b306acca34b3d866d183c475d5f12b313a5"}}) as an app of the same name has already been fetched
...
===> Skipping jsx v2.9.0 as an app of the same name has already been fetched
===> Skipping recon v2.4.0 as an app of the same name has already been fetched
...
```

Those are little notices and warnings that happen during dependency resolution, which is not necessary when a `rebar.lock` file is available. They inform the library maintainer that a conflict was detected, and a certain version of a library was skipped. To finish an audit of your build, you can inspect the final dependency resolving by calling `rebar3 tree`:

<a id="code-snippet--service-discovery.deps-tree"></a>
```sh
$ rebar3 tree
===> Verifying dependencies...
├─ service_discovery─e4b7061 (project app)
├─ service_discovery_grpc─e4b7061 (project app)
├─ service_discovery_http─e4b7061 (project app)
├─ service_discovery_postgres─e4b7061 (project app)
└─ service_discovery_storage─e4b7061 (project app)
   ├─ dns─0.1.0 (git repo)
   │  └─ base32─0.1.0 (hex package)
   ├─ elli─3.2.0 (hex package)
   ├─ eql─0.2.0 (hex package)
   ├─ erldns─1.0.0 (git repo)
   │  ├─ iso8601─1.3.1 (hex package)
   │  ├─ opencensus─0.9.2 (hex package)
   │  │  ├─ counters─0.2.1 (hex package)
   │  │  └─ wts─0.3.0 (hex package)
   │  │     └─ rfc3339─0.9.0 (hex package)
   │  └─ telemetry─0.4.0 (hex package)
   ├─ grpcbox─0.11.0 (hex package)
   │  ├─ acceptor_pool─1.0.0 (hex package)
   │  ├─ chatterbox─0.9.1 (hex package)
   │  │  └─ hpack─0.2.3 (hex package)
   │  ├─ ctx─0.5.0 (hex package)
   │  └─ gproc─0.8.0 (hex package)
   ├─ jsx─2.10.0 (hex package)
   ├─ pgo─0.8.0+build.91.refaf02392 (git repo)
   │  ├─ backoff─1.1.6 (hex package)
   │  └─ pg_types─0.0.0+build.24.ref32ed140 (git repo)
   ├─ recon─2.4.0 (hex package)
   └─ uuid─1.7.5 (hex package)
      └─ quickrand─1.7.5 (hex package)
```

This listing starts with the `Verifying dependencies ...` line, which is Rebar3 validating that all dependencies are resolved before printing the tree. Following it are all the top-level applications (those we write in the repository), and the dependencies we just fetched are all below them. You can see the entire resolution tree that way, find which versions have been fetched and which application brought them in. This can prove useful to understand why a version was selected when a transitive dependency is included by two or more applications.

If you take a look within `_build/default/lib`, you will see all these applications within their own directory:

<a id="code-snippet--service-discovery.deps-ls"></a>
```sh
$ ls _build/default/lib
acceptor_pool  gproc       rfc3339
backoff        grpcbox     service_discovery
base32         hpack       service_discovery_grpc
chatterbox     iso8601     service_discovery_http
counters       jsx         service_discovery_postgres
ctx            opencensus  service_discovery_storage
dns            pgo         telemetry
elli           pg_types    uuid
eql            quickrand   wts
erldns         recon
```

Each of these is an OTP application with similar directory structures. This layout is rather similar to the project structure described for releases in [OTP at a High Level]({{< relref "otp_high_level" >}}), but this is still just a staging area.


### Building a Project with Dependencies {#building-a-project-with-dependencies}

Building the OTP applications in a project requires more than just fetching its dependencies and compiling them. As mentioned in [What Makes a Lib an App]({{< relref "otp_applications" >}}), the Erlang run-time system expects to find run-time definitions of dependencies within the `.app` file. Not putting them there tells Rebar3 they are build-time dependencies, not runtime ones. This means they will not be included in some tasks and environments: releases will ignore them, and `rebar3 dialyzer` will avoid including them in its analysis, for example.

Open up `apps/service_discovery/src/service_discovery.app.src` and look at the values in the `applications` tuple:

<a id="code-snippet--service-discovery.app-src"></a>
```erlang
{application, service_discovery,
 [{description, "Core functionality for service discovery service"},
  {vsn, {git, short}},
  {registered, []},
  {mod, {service_discovery_app, []}},
  {applications,
   [kernel,
    stdlib,
    erldns,
    service_discovery_storage
   ]},
  {env,[]},
  {modules, []},

  {licenses, ["Apache 2.0"]},
  {links, []}
 ]}.
```

You can see `erldns` and `service_discovery_storage` have been added. Specifying these dependencies ensures they are available at runtime and in releases. Not putting them there can result in broken builds.

If you have ever worked with other build tools in the Erlang ecosystem, you likely never had to do this. These tools (erlang.mk or Mix in elixir) end up copying dependencies in the project configuration into the `applications` tuple for you. It sounds like a huge hindrance to have to do this by hand instead, but it ends up following OTP standards to support build-time dependencies. Other tools reach similar results through additional options within configuration files. Let's see a few scenarios where Rebar3's approach can give a critical bit of control.

The first case where this is important is downloading a dependency that you want to include in a release to help you debug it, but on which none of your OTP applications depend. Examples of this include `recon`, `redbug` or custom [`logger`](https://ferd.ca/erlang-otp-21-s-new-logger.html) [handlers](http://erlang.org/doc/apps/kernel/logger_chapter.html#handlers). You would want these applications to be available in a release, but since the `applications` tuple lets releases know in which order applications must be booted or started, you do not necessarily want these to be part of your dependency chain. Why should a debugging tool installed just in case be up and running for a website to work? It's not necessary at all. You wouldn't want a malfunctioning debug tool to prevent your actual application from booting.

In such cases you'd want to have a project configuration that looks like this part of `service_discovery`:

<a id="code-snippet--service-discovery.relx-cfg"></a>
```erlang
{relx, [
    {release, {service_discovery, {git, long}},
     [service_discovery_postgres,
      service_discovery,
      service_discovery_http,
      service_discovery_grpc,
      recon]},
    ...
]}.
```

You can see that, along with our applications, the `recon` debugging tool is explicitly included in the release's list of applications. All their transitive dependencies will be included (according to the `applications` tuple in the `.app` file), but the various OTP applications are handled in a disjoint manner.

Let's focus on these 4 `service_discovery` applications just a bit. These represent the second type of situation where we want to split the declaration of dependencies for builds in `rebar.config`, and the declaration of dependencies for runtime in `.app.src` files.

You can see that at the top-level, all the dependencies for all the libraries are declared in a single `rebar.config` file. That makes it easy for developers to handle and update all versions required. However, if you go look into the `.app.src` files of both `service_discovery` and `service_discovery_http`, you will find this:

<a id="code-snippet--service-discovery.app-src-compare"></a>
```erlang
%% service_discovery.app.src
...
  {applications,
   [kernel,
    stdlib,
    erldns,
    service_discovery_storage
   ]},
...
%% service_discovery_http.app.src
...
  {applications,
   [kernel,
    stdlib,
    service_discovery,
    jsx,
    elli
   ]},
...
```

Here, `service_discovery_http` depends on a web server (`elli`), but `service_discovery` does not. This allows for cleaner boot and shutdown scenarios, where you don't actually need the HTTP server to be up and running to actually start booting the back-end of the system.

For a third scenario, you might also imagine a small app `service_discovery_mgmt` that is used only to generate an escript that lets you do system administration tasks to interact with the system and send commands around.

If the run-time dependencies were shared across all applications depending on the same `rebar.config` file, then even if `service_discovery_mgmt` were to <span class="underline">not</span> be included in the release (it'd just be a script on the side), its dependencies would still risk being pushed to production in it through other apps getting them automatically inserted in there. Possibly worse, all the dependencies of the `service_discovery` release would risk being bundled with the script as well! We could end up with a small admin tool that contains web servers and database drivers because the build tool tried to be nice.

The Rebar3 maintainers therefore just decided to keep a clear distinction between the applications that need fetching for the project to build or run (in `rebar.config`), and the run-time dependencies of each OTP application (in the `.app` file) which may be part of the default OTP install, and would therefore not be included in `rebar.config`. Other build tools in the ecosystem let you achieve similar results, but they default to including everything at run-time whereas Rebar3 asks of developers to always be specific in their intent.

With this all in place, all we have to do is groom and clean our sets of dependencies.


## Dependency Lifecycles {#dependency-lifecycles}

When it comes to initianalizing a project, the dependency resolution and fetching is where the heaviest work is done. The result is stored in a lock file, and all the results after that are handled through shorter partial changes done through Rebar3. This first phase is important to understand, even if you need to do it rather infrequently.

The Rebar3 lock file is created at the root of your project, the directory from which you called Rebar3. It is saved as `rebar.lock` and you should track it in your source control of choice. You can open the file and look at its content, but there shouldn't be any need to ever edit it by hand. You'll find that it mostly contains version numbers, application names, and various hashes. It might be interesting to audit it from time to time, but you'll end up doing that indirectly as you maintain your dependency tree.

The lock file represents the flattened tree of all dependencies as desired at the time of the build. It will not be modified unless you ask for it to be changed, or unless you delete it, forcing a new resolution to be done from scratch. This strictness is on purpose, and is part of how Rebar3 can guarantee repeatable builds under all circumstances.

The hashes in the file mean that even if the dependencies are fetched by multiple layered [mirrors](https://www.rebar3.org/docs/configuration/configuration/#hex-repos-and-indexes), and some malevolent person alters the packages in one or more of the various hex indices or git sources you might use, Rebar3 will be able to find that the information is not as expected, and error out because of it.

You should, therefore, only update the lock file as required. You can do so by using the following operations:

-   `rebar3 unlock <appname>` removes an unused dependency from the lockfile. You will usually want to call this <span class="underline">after</span> you have already removed it from your `rebar.config` file, to tell Rebar3 that it's really gone or has been downgraded to a transitive dependency.
-   `rebar3 upgrade <appname>` tells Rebar3 to disregard the locked version for that application, and re-build the dependency tree from the version currently specified in `rebar.config` (if any). This will generate a new lock file and re-resolve all transitive dependencies that might have changed.
-   `rebar3 update`, while not strictly about the lock file, updates the local <span class="underline">snapshot</span> of remote hex packages, a kind of cache that prevents each build from pinging the package server. If you find yourself calling `rebar3 upgrade` on an app and it isn't upgrading to the latest version you know is available in Hex, you will want to `update` first. This is because Rebar3 limits the use of the network by trying to resolve dependencies with the local index cache. `rebar3 update` will fetch the latest index entries for each package already in the index and then another run of `upgrade` will see the latest versions. Note that if you specify an exact version to upgrade to Rebar3 will automatically fetch the updated index because it is unable to satisfy the dependency locally.
-   `rebar3 tree` prints out the dependency tree that was built and is represented by the lock file.
-   `rebar3 deps` lists out dependencies and annotate those that could be updated. Do note that there are strong limitations in what it considers worth updating: a branch in git, a tag whose reference shifted, or hex versions that weren't specified. But if you specified a package to have version `"1.2.3"` and `1.2.4` is available, it won't tell you anything.

There are other commands that are a bit more drastic, namely `rebar3 unlock` and `rebar3 upgrade` (without any arguments). These will just get rid of the lock file for the next build of the project. But overall, all these commands will do everything you need to manage most dependencies.

In general your workflow might look like this:

1.  set up the initial project, compile once, and track the lock file in source control
2.  find out you have a top-level dependency you want to change
3.  change the dependency definition in the rebar.config file (or optionally if using a non-versioned hex package, call `rebar3 update`)
4.  call `rebar3 upgrade <app>` to update the application and its transitive dependencies

And that's it, you're done.


## Checkout Dependencies {#checkout-dependencies}

Rebar3 wants to make the developers life easier, while also staying safe and repeatable. `_checkouts` are a feature that goes <span class="underline">against</span> repeatability and concept such as lock files, but that provides quick feedback and a better experience around local changes to dependencies.

If you're just adopting Erlang, whether for fun or at work, chances are you'll have few projects with dependencies that live in separate repositories. Working with them won't be too hard. But sooner or later, if you have to start patching dependencies or if you're working in a corporate environment with dozens and dozens of repositories, working with Rebar3 might become frustrating.

The problem would be that every time you want to try a modification of a dependency you will have to commit and publish the change somewhere Rebar3 can fetch it, because it will not build changes made to source files under `_build` if there is already a corresponding `.beam` file for the module. This can become annoying really fast when you have to work across repository boundaries and just want to test a change.

So there's a little trickshot of a feature called <span class="underline">checkout dependencies</span>. Checkout dependencies work as follows:

-   you have your main project somewhere on your file system
-   the dependency is declared in the rebar.config file of the main project
-   you also have that dependency somewhere on your file system, as a standalone project
-   you add a `_checkouts/` directory to the main project
-   you either copy or symlink the dependency's directory in the `_checkouts/` directory

From that point on, every time the main app is built, it will add an `ebin/` directory to the dependency's directory in checkouts, and re-compile it as if it were a top-level application within the main project.

You can then test the changes to your dependency within the main project until they're ready. Once you're done, remove the `ebin/` directory from the dependency, commit and publish your code in the dependency, remove it from the `_checkouts` directory, and `rebar3 upgrade` it.

This lets you do a lot of small iterative changes locally on a dependency, within the context of the main project, without having to push the dependency's changes nor changing configuration files to point a dependency to some local directory. It massively reduces the overhead of per-application repositories as an overall development strategy.


## Using Elixir Dependencies {#using-elixir-dependencies}

For many years, the Elixir and Erlang road was a one way street. You could include Erlang dependencies in your Elixir project, but the opposite wasn't true. Since then, and thanks to the support of the Erlang Ecosystem Foundation, changes have been made to Rebar3 to give it a brand new compiler management structure. This structure has made it possible to write compiler plugins that can let Erlang users use Elixir code.

The way to do it is to first install Elixir. For this you might want to follow the steps [on the official Elixir website](https://elixir-lang.org/install.html). Most Elixir developers use `asdf` as a tool to manage versions. As described in [Setup]({{< relref "setup" >}}), `kerl` options for Erlang can be used with the Erlang plugin for `asdf`, so that can give you a complete setup.

With Elixir in place, add the [rebar_mix](https://www.rebar3.org/docs/configuration/plugins/#elixir-dependencies) plugin to your library or project:

<a id="code-snippet--rebar-mix.config"></a>
```erlang
{plugins, [rebar_mix]}.
{provider_hooks, [{post, [{compile, {mix, consolidate_protocols}}]}]}.

{relx, [
    ...
    {overlay, [
        {copy, "{{base_dir}}/consolidated", "releases/{{release_version}}/consolidated"}
    ]
}.
```

And the following line to your `vm.args.src` file if you have one:

<a id="code-snippet--rebar-mix.args"></a>
```sh
-pa releases/${REL_VSN}/consolidated
```

From that point on, you will be able to install any hex dependency containing Elixir code without a problem. For now, the plugin only supports hex dependencies that also only rely on other hex dependencies; support for transitive dependencies using git should be coming soon, however.

Do note that mixed Rebar3 projects using both Erlang and Elixir within the same library are not currently supported since more work both on the Rebar3 and Elixir side would need to be done to make this possible. On the other hand, Mix does support this pattern if you need it.


## Corporate Environments {#corporate-environments}

Corporate environments tend to have all kinds of weird restrictions regarding what can or cannot be done, and development tools can be very idiosyncratic. Rebar3 was mostly developed to fit an open-sourced world and mainly focuses on enforcing project structure, fetching dependencies, and using both together as a great pretext to wrap a bunch of standard tools.

As such, it is perhaps unsurprising that fitting a corporate environment with the tool can prove to be a bit challenging. In this section, we'll cover some standard tools that are common to corporate environments and may make your life easier when adopting Erlang.


### Proxy Support {#proxy-support}

Many workplaces enforce very strict firewall rules, to the point where all incoming and outgoing data must be intercepted and monitored. Generally, these places will not be totally isolated from the public Internet, but will require the use of proxy servers to make outgoing connections.

It is rather standard for programs to respect both the `HTTP_PROXY` and `HTTPS_PROXY` environment variable. When those are set in your development environment Rebar3 will make sure that all the communications it makes talking to the outside world uses these proxies.

This should let you properly work in line with your IT department's policies. In some cases, that won't even be enough.


### Private Hex Mirrors {#private-hex-mirrors}

Some corporations go a step further and segment their internal network from the public Internet. All data that comes on site has to be inspected and hosted independently, without a chance to talk to a common code repository like github, gitlab, or hex. Another interesting case is build servers, where you might want to prevent all connectivity to the outside world both for safety and for repeatability reasons.

For such setups, two approaches tend to be used: vendoring in a monorepo, which will be covered in the next section, and through privately hosted package indexes, which we'll cover here.

The idea behind a privately hosted index is that all packages and dependencies to be used in a project need to be fully vetted. You might want to give it a technical review for code quality, a security assessment, or have corporate lawyers look at code for licensing or patent issues. Then, only the packages of the right version can be used. This kind of index is frequently accepted for hermetic builds with the assumption that it either runs locally on each build server, or within a private network that is as tightly monitored as build servers.

Rebar3 supports this use case. If you want to enable it, you will need to first set up a private hex instance. This can be done through the [minirepo](http://blog.plataformatec.com.br/2019/07/announcing-minirepo-a-minimal-hex-server/) project. By following the instructions [on the project page](https://github.com/wojtekmach/mini_repo), you will end up running your own private hex server, with either a local filesystem or S3-based storage, and the ability to both mirror other indexes and publish your own packages privately.

You will need to [tweak your global Rebar3 config](https://github.com/wojtekmach/mini_repo#usage-with-rebar3) to make use of it, but once that's done, you're good to go.


### About Monorepos {#about-monorepos}

If you work in a corporate environment with a monorepo where all private libraries, OTP applications, and dependencies are treated on equal footing (it's not just an umbrella release), Rebar3 is not the best tool for the job. Mostly, this comes down to the fact that most companies using a monorepo have a lot of custom tooling with very custom workflows, large codebases, and a very strong propension to never share access with maintainers of Rebar3. Until it becomes possible for maintainers to get access, little will be possible to do on that front.

Various commercial users who rely on Rebar3 despite using monorepos have reported obtaining successful builds with a combination of using `_checkouts` dependencies, along with re-configuring the `_build` directory. However, we cannot recommend this approach at this time, and no official support is provided for monorepos.

Another option is to vendor dependencies, which can be done through plugins such as [`rebar3_path_deps`](https://www.rebar3.org/docs/configuration/plugins/#vendoring-dependencies).

With all this in place, you should be set to manage the lifecycle of your project's dependencies.

<div class="pagination">
  <div><a href="/docs/development/supervision_trees/">← Prev</a></div>
  <div><a href="/docs/development/umbrella_projects/">Next →</a></div>
</div>
