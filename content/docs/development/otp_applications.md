+++
title = "OTP Applications"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
date = 2019-08-08T08:05:00+00:00
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:08+00:00
draft = false
[menu]
  [menu.main]
    weight = 2004
    identifier = "otp-applications"
    parent = "development"
+++

Since every component to be shipped in an Erlang/OTP release needs to be an OTP Application, it will do you a great good to understand what they are and how they work. In this chapter, we'll go over the basic structure of an OTP application, and what that means for your project.


## Project Structure {#project-structure}

We'll start by using the Rebar3 templates, since they will allow us to create brand new projects that properly respect the directory structures expected by Erlang/OTP. Let's see which templates are available:

<a id="code-snippet--rebar3-new"></a>
```sh
$ rebar3 new
app (built-in): Complete OTP Application structure.
cmake (built-in): Standalone Makefile for building C/C++ in c_src
escript (built-in): Complete escriptized application structure
lib (built-in): Complete OTP Library application (no processes) structure
plugin (built-in): Rebar3 plugin project structure
release (built-in): OTP Release structure for executable programs
umbrella (built-in): OTP structure for executable programs
                     (alias of 'release' template)
```

Here's a table showing when they might be used:

| Type of Project                    | Template to use | Comments                                                                             |
|------------------------------------|-----------------|--------------------------------------------------------------------------------------|
| script or command line tool        | escript         | Requires Erlang to be installed by the user                                          |
| a library (collection of modules)  | lib             | Can be used as a dependency                                                          |
| a library (stateful processes)     | app             | Can be used as a dependency                                                          |
| full executable program            | umbrella or app | Can be turned into a full release, the recommended deploy mechanism                  |
| a collection of multiple libraries | umbrella        | Cannot be used as a git dependency but each individual app could be published to hex |
| Rebar3 extension                   | plugin          |                                                                                      |
| compiling C code                   | cmake           | Also see the "pc" plugin for a portable way to compile C/C++                         |

You can see the details of a given template by calling `rebar3 new help <template>`. See for example:

<a id="code-snippet--rebar3-new-lib"></a>
```sh
$ rebar3 new help lib
lib:
  built-in template
  Description: Complete OTP Library application (no processes) structure
  Variables:
    name="mylib" (Name of the OTP library application)
    desc="An OTP library" (Short description of the app)
    date="2019-03-15"
    datetime="2019-03-15T19:52:31+00:00"
    author_name="Fred Hebert"
    author_email="mononcqc@ferd.ca"
    copyright_year="2019"
    apps_dir="apps" (Directory where applications will be created if needed)
```

The values can be modified as desired on the command line, but those are the default variables. Let's see what we get by writing our own:

<a id="code-snippet--rebar3-new-mylib"></a>
```sh
$ rebar3 new lib mylib desc="Checking out OTP libs"
===> Writing mylib/src/mylib.erl
===> Writing mylib/src/mylib.app.src
===> Writing mylib/rebar.config
===> Writing mylib/.gitignore
===> Writing mylib/LICENSE
===> Writing mylib/README.md
```

Go to the `mylib` directory, and call `rebar3 compile` right away:

<a id="code-snippet--rebar3-mylib-compile"></a>
```sh
$ rebar3 compile
===> Verifying dependencies...
===> Compiling mylib
```

If you look at your directory structure, you should now have something like this in your project:

<a id="code-snippet--lib-structure"></a>
```sh
mylib/
├─ _build/
│  └─ default/
│     └─ lib/
│        └─ mylib/
│           ├─ ebin/
│           │  ├─ mylib.app
│           │  └─ mylib.beam
│           ├─ include/
│           ├─ priv/
│           └─ src/
│              └─ ...
├─ .gitignore
├─ LICENSE
├─ README.md
├─ rebar.config
├─ rebar.lock
└─ src/
   ├─ mylib.app.src
   └─ mylib.erl
```

The `_build/` directory is the build tool's playground, where it can stash all the artifacts it needs. You should never have to touch what is in there by hand, but should feel free to blow it away when you want. This directory is nonetheless interesting because it shows how Rebar3 structures things.

Everything in `_build/` is split by [profile](https://www.rebar3.org/docs/configuration/profiles/), which lets Rebar3 build things differently (with different sets of dependencies and compiler options) whether they are built in the `default`, `test`, or `prod` profile—in fact, you can define as many profiles as you want, and compose them together. The Rebar3 documentation explains how this works.

Within each profile, the `lib/` directory contains all the OTP applications that your project may use, outside of the standard distribution's libraries. You can see our `mylib` library replicated right there, but its directory structure is a bit different from what's directly at the project root:

-   compiled `.erl` files are moved to the `ebin/` directory and now have the `.beam` extension
-   there is a `mylib.app` file created, whereas the source application had `mylib.app.src`
-   two symlinks have been added to `include/` and `priv/`. These will refer to matching directories at the root of the project, if they exist. The `include/` directory is meant for [header files](http://erlang.org/doc/reference_manual/macros.html#file-inclusion) (`.hrl`), and the `priv/` directory for any file that must be copied over and made available in production
-   All other files at the root of the project have been discarded

If we had any dependencies (see [The Dependencies chapter](/docs/development/dependencies)), they would also be placed in the `_build/<profile>/lib/` directory.

In general, you will want to ignore the `_build/` directory entirely and avoid tracking it in your source control: if you look at the `.gitignore` file, you will see that it automatically ignores `_build/` for you.

Rebar3 chooses a license for you by default (because you should always choose a license if you plan on doing open source work), going for the [Apache 2.0](https://en.wikipedia.org/wiki/Apache_License#Version_2.0) license that Erlang ships with. Feel free to replace it as required. Rebar3 also sets up a `README` file that you might want to fix up and update with all the relevant contents. Don't be a jerk, write documentation!

Then we get to two interesting files, `rebar.config` and `rebar.lock`. The lock file is used by Rebar3 to track which versions you were using for any dependency in the project, and should therefore be checked into source control. The [Dependencies chapter](/docs/development/dependencies) contains more details.

The `rebar.config` file is a complete declarative configuration file that exposes options for all the Erlang tools that Rebar3 integrates with. [The official documentation](https://www.rebar3.org/docs/configuration) explains all the values possible, but by default it is quite empty. In fact, if you only want default values with no dependencies, you can just delete the file. As long as your project is structured like an OTP application, Rebar3 will figure out what needs to be done.

Let's see what the standards are for that to happen.


## What Makes a Lib an App {#what-makes-a-lib-an-app}

As with any other framework, there are some things you have to do to conform to its expectations. You've possibly guessed it, but the directory structure is one of the basic requirements of a framework like OTP. As long as your library has an `ebin/` directory once compiled with an `<appname>.app` file in it, the Erlang runtime system will be able to load your modules and run your code.

This basic requirement guides the project structure of the entire Erlang ecosystem. Let's look at what a built `.app` file looks like:

<a id="code-snippet--mylib.app"></a>
```erlang
$ cat _build/default/lib/mylib/ebin/mylib.app
{application, mylib, [
  {description, "Checking out OTP libs"},
  {vsn, "0.1.0"},     % version number (string)
  {registered, []},   % name of registered processes, if any
  {applications, [    % List of OTP application names on which
    kernel, stdlib    % yours depends at run-time. kernel and
  ]},                 % stdlib are ALWAYS needed
  {env, []},          % default configuration values ({Key, Val} pairs)
  {modules, [mylib]}, % list of all the modules in the application
  %% content below is optional, and for package publication only
  {licenses, ["Apache 2.0"]},
  {links, []}         % relevant URLs
]}.
```

This is essentially a metadata file that describes everything about the application. We've taken the time to annotate it for you, so check it out. A lot of the content in there is annoying to write by hand so if you look at the source file (`src/mylib.app.src`), you'll see that the fields are mostly pre-populated when you apply the Rebar3 template. You may also notice that `modules` is empty. That's on purpose: Rebar3 will populate the list for you when compiling your code.

By far, the most critical field to keep up to date in there is the `applications` tuple. It lets Erlang libraries know the order in which OTP applications must be started to work, and also allows build tools to build a dependency graph between all available OTP applications to know which to keep and which to remove from the distribution when building a release.

A more subtle thing to notice is that even if what we have here is a <span class="underline">library</span>, and it therefore has no processes to run, we still have the ability to define some configuration values (to be covered in the not yet written Configuration chapter), and dependencies must be respected. It is possible, for example, that our library is stateless, but uses a stateful HTTP client: the Erlang VM will then need to know when your code may or may not be safe to call.

For now, let's focus on what exactly is the difference between a stateless and a stateful application.


## What Makes a Runnable App an App {#what-makes-a-runnable-app-an-app}

To make a runnable application, we're going to use the "app" template in Rebar3, and see what are the differences with a stateless application.

So let's grab your command line tool and run the following:

<a id="code-snippet--rebar3-new-myapp"></a>
```sh
$ rebar3 new app myapp
===> Writing myapp/src/myapp_app.erl
===> Writing myapp/src/myapp_sup.erl
===> Writing myapp/src/myapp.app.src
===> Writing myapp/rebar.config
===> Writing myapp/.gitignore
===> Writing myapp/LICENSE
===> Writing myapp/README.md
$ cd myapp
```

If you're careful, you'll see that we now have two modules instead of `<appname>.erl`: we have `<appname>_app.erl` and `<appname>_sup.erl`. We'll study them real soon, but first, let's focus on the top-level metadata file for the application, the `myapp.app.src` file:

<a id="code-snippet--myapp.app.src"></a>
```erlang
$ cat src/myapp.app.src
{application, myapp,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, {myapp_app, []}},               % this is new!
  {applications, [kernel, stdlib]},
  {env,[]},
  {modules, []},

  {licenses, ["Apache 2.0"]},
  {links, []}
 ]}.
```

The only new line here is the `{mod, {<appname>_app, []}}` tuple. This tuple specifies a special module that can be called (`<appname>_app`) with some specific arguments (`[]`). When called, it is expected that this module will return the <span class="underline">process identifier</span> (the <span class="underline">pid</span>) of a [supervision tree](/docs/development/supervision_trees).

If you go visit the `myapp_app` module, you will see what these callbacks are:

<a id="code-snippet--myapp-app.erl"></a>
```erlang
%%%-------------------------------------------------------------------
%% @doc myapp public API
%% @end
%%%-------------------------------------------------------------------

-module(myapp_app).
-behaviour(application).
%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    myapp_sup:start_link().

stop(_State) ->
    ok.
```

The `start/2` callback is called when the application is booted by the Erlang runtime system, at which point all of its dependencies—as defined in the `applications` tuple in the .app file—have already been started. This is where you can do one-time bits of initialization. In the template application, the only thing done is starting the root supervisor for the application.

The `stop/1` callback is called <span class="underline">after</span> the whole supervision tree has been taken down once someone, somewhere, has decided to shut down the OTP application.

But all in all, this little additional `mod` line in the app file and the presence of a supervision structure are what differentiates a runnable application from a library application.

{{% admonition tip %}}
If you do not want to care for a supervision tree and are only interested in getting a <code>main()</code> function to get going like you would in most other programming languages, <code>escript</code> might be a good option for you.

<code>escript</code> is a special C program that wraps the Erlang virtual machine. In wrapping it, it also introduces a small shim that retrofits the idea of a <code>main()</code> function to the release structure, by calling your code into the root Erlang process of the virtual machine.

The net result is that you can run interpreted code without having to bother about releases, OTP Applications, or supervision tree. You can read more about escripts in <a href='http://erlang.org/doc/man/escript.html'>the official Erlang documentation</a>. Rebar3 also <a href='https://www.rebar3.org/docs/commands#escriptize'>has a command to create complex escript bundles</a>.
{{% /admonition %}}

You now understand most of the weird stuff about Erlang/OTP's project structure and everything that has to do with these mysterious "OTP Applications". Starting with next chapter, we'll start digging a bit in supervision trees, so that you know how to set things up in a stateful runnable application.

<div class="pagination">
  <div><a href="/docs/development/otp_high_level">← Prev</a></div>
  <div><a href="/docs/development/supervision_trees">Next →</a></div>
</div>
