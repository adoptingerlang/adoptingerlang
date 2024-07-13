+++
title = "OTP at a High Level"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
date = 2019-08-08T08:05:00+00:00
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:08+00:00
draft = false
[menu]
  [menu.main]
    weight = 2003
    identifier = "otp-at-a-high-level"
    parent = "development"
+++

Erlang/OTP is different from most programming environments out there, even those that also use a virtual machine. Erlang has a strong opinion about how your applications should be structured, the level of isolation they should have, and a separation between what Erlang's VM can do, and what your software can do. It's not just a programming language, it's a whole framework for building systems. Understanding its core principles is the key to getting started fast without having to rewrite everything later: it ensures that all applications can fit well together, that updates can be done live, and that your code is easy to instrument and make observable.

In this chapter, we'll cover the Erlang virtual machine and the core concepts of OTP at the highest level.


## The Erlang Run-Time System {#the-erlang-run-time-system}

The foundational block for everything is the Erlang virtual machine itself, called BEAM. BEAM is technically a single implementation of the Erlang virtual machine, as there could be others. For example, Erllvm is an implementation over LLVM (using some custom patches to make everything possible), and an older implementation in the 90s was called JAM. The Erlang VM is implemented in C, and contains a lot of fancy stuff: schedulers to run processes, garbage collection, memory allocators, a timer wheel for events, a bunch of smart switches to abstract over operating system features and provide unified interfaces (such as over time management, file-handling drivers, and so on), a few built-in functions that go faster than what Erlang can do on its own (BIFs) and an interface for functions implemented natively in other languages (NIFs) along with special schedulers for them. There's obviously a lot more, but you can think of all that stuff the way you would with the kernel in BSD or Linux: low level stuff that you need in order to build fancier stuff.

If all you have is the virtual machine with nothing else, you can't run Erlang code. You don't have a standard library, you don't have libraries to even load code. To get it all going, there's some tricky bootstrapping going on that we don't need to understand. Just know that there's a limited set of pre-loaded Erlang modules that ship with the virtual machine, and those can be used to set up networking and file-handling stuff, which is in turn used to further load and run modules. If you're interested in knowing more though, please consult [The BEAM Book](https://happi.github.io/theBeamBook/) or [BEAM Wisdoms](http://beam-wisdoms.clau.se/).

If you take the virtual machine and the pre-loaded stuff, along with all the little utilities that make code-loading possible, you have what is essentially called the <span class="underline">Erlang Run-Time System</span> (ERTS). The Run-Time System, when starting, follows the instructions of a thing called a <span class="underline">boot script</span> (which nobody writes by hand) that specifies what to start.

Erlang, by default, provides boot scripts that load a minimal amount of code required to start a shell and write your own applications. Once this is done, we can start thinking about Erlang, and not just the virtual machine.


## Erlang/OTP {#erlang-otp}

What we have described so far is equivalent to an operating system's kernel. We now need the foundational blocks for the userspace components. In Erlang, this is essentially what OTP is about. OTP specifies how "components" that run on the virtual machine should be structured. There's more to the language than just "processes and messages": there's one well-defined way to structure your code.

{{% admonition note %}}
OTP stands for _Open Telecom Platform_, which is literally a meaningless name that was used to get the stuff open-sourced back in the old days of Erlang at Ericsson.
{{% /admonition %}}

Erlang/OTP systems are structured through components named <span class="underline">OTP Applications</span>. Every Erlang version you have installed or system built with it that you use ships with a few OTP Applications. There are basically two variants of OTP applications: <span class="underline">Library Applications</span>, which are just collections of modules, and <span class="underline">Runnable Applications</span>, which contain a collection of modules, but also specify a stateful process structure stored under a supervision tree. For the sake of clarity, we're going to use the following terminology for OTP Applications for this entire book:

-   <span class="underline">Library Applications</span>: stateless collections of modules
-   <span class="underline">Runnable Applications</span>: OTP applications that start stateful supervision tree structures with processes running in them
-   <span class="underline">OTP Applications</span>: either <span class="underline">Library</span> or <span class="underline">Runnable Applications</span>, interchangeably

By default, the two OTP applications everyone includes are called `stdlib`, which is a library application that contains the core standard library modules such as `list` or `maps`, and `kernel`, which is a runnable application and sets up the core structure for an Erlang system that relies on OTP applications to work.

When a node boots, the modules from all required OTP applications are loaded in memory. Then `kernel` is started. `kernel` manages the lifecycle of the system from this point on. All other OTP applications and their configuration are handled through it, and so are unique features like distribution and hot code updates. If we go back to the operating system comparison, you can think of the `kernel` OTP application a bit like you could think of `systemd` for the Linux kernel (or `init` if you hate `systemd` or use a BSD -- Windows users can think of it as the service that runs other services)

In fact, `kernel` and `stdlib` are the only two applications you need for a basic working Erlang shell. When you type in `erl` (or start `werl` on Windows), this boots up the VM, along with kernel, with `stdlib` pre-loaded. Everything else is optional and can be loaded at a later time.

The standard Erlang distribution contains applications such as:

-   kernel
-   stdlib
-   crypto (cryptographic primitives)
-   ssl (TLS termination library)
-   inets (network services such as FTP or HTTP clients)
-   ct (Common Test framework)
-   wx (graphic toolkit)
-   observer (a control panel to manage your Erlang node, building on `wx`)
-   compiler (the Erlang compiler to build your own project)
-   and so on

All of these are put together into what is called an Erlang <span class="underline">release</span>. A release is a collection of OTP applications, possibly bundled together with a full copy of the virtual machine. As such, when you download and install Erlang, you just get a release whose name is something like <span class="underline">Erlang/OTP-21.3.4</span>. You're free to build your own releases, which will take some of the OTP applications in the standard distribution, and then bundle them with some of your own apps.

So if we were to write an app named `proxy` that relies on `ssh` and `ssl` (which themselves depend on `public_key`, `crypto`, `stdlib`, and `kernel`), we would make a release with all of these components in it:

-   ERTS
-   kernel
-   stdlib
-   crypto
-   public_key
-   ssl
-   ssh
-   proxy

A visual representation of this can be seen in Figure [1](#figure--fig:proxy-release).

<a id="figure--fig:proxy-release"></a>

{{< figure src="/img/proxy_release_draft.png" caption="<span class=\"figure-number\">Figure 1: </span>Visual representation of building the `proxy` release" >}}

Essentially, building an Erlang system is re-bundling the VM, along with some standard applications provided with the default distribution, together with your own apps and libraries.


## Living in Erlang/OTP {#living-in-erlang-otp}

Standard tools developed and used by the community such as Rebar3 operate on the idea that what you write and publish are OTP applications, and as such contain all the functionality required to deal with them. That's a big shift from a lot of programming languages that only ask of you to have a function named `main()` somewhere in one of your files. This is why the programming language is often called `Erlang/OTP` rather than just 'Erlang': it's not just a programming language, it's a general development framework that mandates some basic structure for everything you do.

And everyone follows it, whether they are writing embedded software, blockchain systems, or distributed databases. It's OTP or nothing. Whereas other languages usually mandate nothing specific to get started, but then add some requirements later on (such as when integrating with a package manager), Erlang--and its entire community--expects you to just write OTP applications, which the rest of the tools can handle.

So the key to getting started fast in Erlang is to know the framework, which is often kept as more advanced material. Here we're going to do things upside down and start from a fully functional release, and then dig down into its structure. The next chapters will be dedicated to understanding how to work within these requirements.

<div class="pagination">
  <div><a href="/docs/development/setup">← Prev</a></div>
  <div><a href="/docs/development/otp_applications">Next →</a></div>
</div>
