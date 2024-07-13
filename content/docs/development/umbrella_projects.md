+++
title = "Multi-App Projects"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
date = 2020-01-15T20:00:00+00:00
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:09+00:00
draft = false
[menu]
  [menu.main]
    weight = 2007
    identifier = "multi-app-projects"
    parent = "development"
+++

Multi-application projects, more frequently called <span class="underline">umbrella projects</span>, are how most projects are structured in a business environment, mostly because they make it easier to maintain multiple OTP Applications within a single repository. In this chapter, we'll cover their structure, when they prove most and least useful, some subtleties of this new structure, and finally, give tips to help properly split up a monolithic library into multiple OTP Applications.


## Organizing Multi-App Projects {#organizing-multi-app-projects}

One example of an umbrella project is the [service_discovery](https://adoptingerlang.org/gh/servicediscovery) repository we have been using before. The tell-tale sign that lets you know right away it's an umbrella is visible right in the directory listing:

```shell
$ ls
apps  cloudbuild.yaml  deployment          Dockerfile  README.md     rebar.lock  Tiltfile
ci    config           docker-compose.yml  LICENSE     rebar.config  test
```

Multi-app projects require a directory in which all the top-level applications' source code is located and this is in the `apps/` directory (the `libs/` directory is also supported). Whenever you see `apps/` or `libs/` with a `rebar.config` file, you can be pretty certain this is an umbrella project.

Take a look at that directory and you can get a pretty good idea about what the project is all about:

```shell
$ ls apps
service_discovery       service_discovery_http      service_discovery_storage
service_discovery_grpc  service_discovery_postgres
```

There are no hard rules about how to name the OTP applications in your project. The rules we have found to work well with experience were to always use some sort of namespacing, since the VM doesn't support anything like that. In the previous listing, we can see that we have one `service_discovery` app, and then a bunch of `service_discovery_<thing>` applications.

This tells us that all these applications are related. The main one is probably `service_discovery` and the others are helpers: the `_grpc` and `_http` apps are likely front-ends or client libraries (spoiler: they're front-ends), the `_storage` one should clearly handle storage, and the `_postgres` one probably also handles some sort of storage.

If you go dig in the code you'll find that `_storage` is a kind of generic storage API (see its `.app.src` file's `description` field) while `_postgres` is one specific implementation: it has been written for extensibility.

It's possible for applications to have wildly different names in that directory. For example, we could decide to write or vendor in some sort of authentication library, and so we could also have something like `authlib` and `authlib_http` in terms of applications in there.

This type of pattern is why namespacing can be useful in larger projects. As projects grow, they tend to gain more and more APIs, endpoints, clients, and ways to interact with them, and while cumbersone during writing, such manual namespaces allow very clear separations when necessary.

In all cases, this multi-app directory is the biggest structural difference between single-app and multi-app projects. There's another small variation though: you can have multiple rebar.config and test directories. In the case of `service_discovery` in particular, you can see that it has a top-level `rebar.config` file and a `test/` directory. But if you look into all individual applications, there's more:

```shell
$ ls apps/service_discovery*
apps/service_discovery:
src

apps/service_discovery_grpc:
proto  rebar.config  src

apps/service_discovery_http:
src

apps/service_discovery_postgres:
priv  src

apps/service_discovery_storage:
src
```

All apps maintain the basic need for OTP applications of having a `src/` directory, but they're free to add their own test directories, `priv/` directories, or any other they need, along with new `rebar.config` files.

Let's take a look at the `service_discovery_grpc`'s config:

<a id="code-snippet--service-discovery-grpc-config"></a>
```erlang
{grpc, [{protos, "proto"},
        {gpb_opts, [{descriptor, true},
                    {module_name_prefix, "sdg_"},
                    {module_name_suffix, "_pb"}]}]}.
```

This configuration is meant specifically for the `grpcbox_plugin` that is declared in the top-level's `rebar.config`, but allows the plugin to only run for the OTP applications that do require it.

In short, this creates a multi-tiered dynamic to building and structuring applications:

-   everything at the top level is shared across all top-level applications (even tests)
-   each application is allowed to be more specific by creating a local version of directories or test files

There are a few exceptions to these. For example, dependencies are shared for the project: while each top-level app can specify its own dependencies, Erlang and Rebar3 only allow one version of a library to be loaded at a time (live code upgrades aside). This means that dependency resolution will pick a single winning application for all conflicting versions, and considering them shared therefore makes sense. At the opposite, directories such as `priv/` are meant for a single app's private files, and while anyone can read their content, the same directory cannot be owned by multiple applications.

Another small subtlety is around hooks; some hooks can be defined both for a single OTP application and for a whole project. For example, a `compile` hook defined at the top level will run before or after <span class="underline">all</span> applications are built, and the same hook defined for a single application in `apps/` will run only before or after that one application is compiled.


## Should You Migrate {#should-you-migrate}

The main benefit of an umbrella project compared to dozens of single-app repositories is that most of your code development gets centered in one place where you can easily have one big shared configuration for a lot of tools. They also make it easy to track everything in one repository in terms of reviews, migrations, and history. It sounds like a pretty straightforward decision to make, but it's not always that easy.

There are two big caveats to switching to multi-app projects. The first one is that the only dependencies that an Erlang project can have with Rebar3 need to be single-app repositories, at least until new features are added to permit it. If you intend to write libraries that are to be used across multiple projects within your workplace, doing it in a multi-app project is not going to work unless all development also moves into the multi-app project as well.

The second caveat is that moving all your development into the big multi-app project is not that simple either. Most of the tooling assumes you might be building one (or not more than a few) releases per project, and as such will not hesitate to run code analysis or to rebuild on all the top-level applications at once.

This means that if instead of having a few moderately sized repositories you have a gigantic one, you might see common commands take a lot more time, simply because they expected gigantic projects to be rarer.

Until Rebar3 (and other tools) can manage to catch-up to monorepos, you might want to structure things as follows:

-   Make one multi-app repository per larger project, such as a service or micro-service
-   All your common libraries that are shared across larger projects are maintained and published individually, and pulled in when required by specific projects
-   Use [templates](https://www.rebar3.org/docs/tutorials/templates/#section-plugin-templates) stored in some general [plugin](https://www.rebar3.org/docs/configuration/plugins/) that your teammates will install globally to automate the layout and specification of services, web APIs, CI configurations, and so on

If at some point a library from a multi-app project becomes useful to other users within your organization, it becomes easy to just take it out into its own repository, publish it, and reimport it as a dependency. Similarly, orphaned libraries or forked libraries can then be maintain locally within each project.

This structure is also helpful when your organization intends to patch or develop, and then publish open source code, and makes it somewhat less costly to make changes in a library without having to synchronize all of its users at once.

It also keeps things somewhat straightforward when it comes to developing specific scripts around deployments and CI for each project; open source tools tend to keep working well. However, it has a higher cost when you're in an organization that already has monorepos and tooling developed for these. Another common roadblock is that it requires that CI and build servers for one project have read-access to those of dependencies, which isn't necessarily in place in all organizations.


## Cutting Up Apps {#cutting-up-apps}

Regardless of the approach you prefer—single-app, multi-app, or monorepos—you have to figure out how to best cut code up into manageable chunks.

This has always been challenging, regardless of what you're writing. The same way there are countless blog posts and articles on the perfect size of a function, how much a module or a class should contain and expose, and exactly how small or large should a micro-service be, there is no single canonical reference on what's the best OTP application size.

Rather than providing hard and fast rules, we tend to structure them according to some sort of gut feeling of what good isolation feels like. This is often the result of experience that is difficult to teach, but here are a few questions we like to ask that simplifies decision-making:

-   Is the specific piece of functionality something other projects could eventually need? If so, it may help to give it its own OTP application so that it is easy to extract and share
-   Does it contain code that is very specific to the concerns of the project? For service discovery, does it have to do with tracking services, or it's something general like "storing data"? The more specific, the closer it should be to the main app; the more general, the easier it is to imagine as a stand-alone OTP application
-   Does it require some peculiar configuration values or domain knowledge? If so, it might be a good idea to bundle all the calls into a restricted set of modules in their own OTP application so that others can use them as a good source of abstraction (for example, handling authentication or specific protocols)
-   Can you imagine it booting only in some specific contexts or builds? If so, making it a distinct app will make things easier down the road. A good example for this could be healthcheck or monitoring endpoints that could depend on your main app, but wouldn't be needed for their tests or for specific builds

All of these questions are proxies that should lead you to more easily gauge just how much coupling exists between your business logic (which tends to always live in the top-level repo and apps) and the rest of the code you're currently writing.

Tracing such a line in the sand is often a useful exercise to figure out how to structure things.

To use `service_discovery` as an example, there would technically be no strong requirement to make `service_discovery_storage` a distinct application from the main `service_discovery` application. However, we felt that it would make sense that the main application does not worry about specifics of the storage layer, whether it's generic, backed to disk, on another service, or in memory. It does not care. All that complexity and indirection can be handled in a very narrowly-defined application (`service_discovery_storage`), which can be configured with specific plugins (such as the `service_discovery_postgres` library).

We simply felt that this isolation could be beneficial in more clearly ensuring that the main app never gets to be concerned about storage-specific woes outside of calling them as a general abstraction. The complexity of switchable backends still exists, but has been isolated in one clearly-labelled area of the code, which we hoped would make maintenance simpler.

In the end, the best way to structure and organize code and repositories is to pick the one you and your team finds the most effective given your current organisational context. The way we (the authors) prefer things is one way we've found to be a reasonable compromise across multiple organizations we have worked for over the last decade or so, but it might not be as effective as fully embracing what is currently provided to you.

Solve one problem at a time; if your team is learning Erlang for the first time, it may make sense to start with everything on the path of least resistance given your organization's existing deployment and build systems, knowing fully you'll rework them later to be more comfortable. Trying to tackle all the problems at once and trying to solve them all perfectly the first time around can be counterproductive when you have limited energy, time, or resources.

<div class="pagination">
  <div><a href="/docs/development/dependencies">← Prev</a></div>
  <div><a href="/docs/development/hard_to_get_right/">Next →</a></div>
</div>
