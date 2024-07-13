+++
title = "Setup"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
date = 2019-08-08T08:05:00+00:00
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:08+00:00
draft = false
[menu]
  [menu.main]
    weight = 2002
    identifier = "setup"
    parent = "development"
+++

You can't write Erlang without having Erlang installed, so the unsurprising first steps covered in this chapter will be to go through the basic steps required to install Erlang/OTP on most major platforms. The instructions will aim for a basic set-up on most platforms, but you'll find out that real world Erlang development is rarely done with just the basic instructions.

In fact, as teams grow and they accumulate various projects, chances are that not all services, libraries, or bits of code will all support the same exact version of Erlang, and won't be upgraded all at once. If you talk to developers who use Erlang professionally, most (aside from Windows users) will tell you that they just compile their own copies with the options they need, and with a tool that lets them switch between multiple versions. So we'll go through that.

You will also see how to install Rebar3, the official build tool for the Erlang community, and base configurations for various text editors. In later chapters, we'll additionally talk about other tools that aren't specific to Erlang such as Kubernetes or Prometheus, but let's get started with Just Erlang for now.


## Installing Erlang/OTP {#installing-erlang-otp}

The first step is to get a proper install of Erlang/OTP in place. This is not going to be a uniform experience on all platforms, but we'll at least make sure everyone following these steps has a fully functioning setup for any work environment.


### Choosing a Version {#choosing-a-version}

Erlang/OTP is released on a fairly stable and predictable schedule, with well-defined criteria for backwards-incompatible changes.

Erlang versions are numbered according to a `<Major>.<Minor>.<Patch>` scheme, as described in the [Erlang/OTP system principles](http://erlang.org/doc/system_principles/versions.html#version_scheme). In some rare circumstances, other digits are bolted on as "branched" versions, which you likely won't have to care about.

Here are some example possible versions:

-   22.0
-   22.0-rc3
-   21.3
-   21.2.3
-   21.1
-   19.3
-   17.0
-   R16B03 (this is a legacy version format that hasn't been used since 2014)

As you can see, the `Patch` version is not mentioned when no patch is required. The release schedule for Erlang goes a bit like this:

1.  Once per year, around February or March, a release candidate for the next major version is announced (with a suffix such as `-rc1`, or `-rc2`). This release candidate is made available for users who want to build from source, in order to test that their applications and system will work well with it
2.  A few months later (April to June), the major release is cut and made public. Major releases contain large new features that require bigger virtual machine changes, and are also allowed to introduce backwards-incompatible changes
3.  At a frequency of every three or four months, a minor release is made public, which usually includes stability fixes and minor feature additions in individual libraries
4.  If a critical bug has been found in some circumstances, either for security or stability reasons, a patch release may be announced.

{{% admonition info %}}
Backwards incompatible changes are usually going through a cycle of deprecation before being removed, which tends to leave ample time to adapt. The policy is described in the <a href='http://erlang.org/doc/system_principles/misc.html'>Support, Compatibility, Deprecations, and Removal</a> document published by the OTP team at Ericsson.
{{% /admonition %}}

In some rare scenarios, hard-and-fast deprecations do happen (mostly by accident), and it may take a few weeks for the community to come up with workarounds.

A team that adopts Erlang will therefore likely want to adopt a maintenance schedule that fits the main releases if they want to avoid falling too far behind. While it is possible to only upgrade occasionally, you will find that it is often easier to do a bit of maintenance here and there than a lot of maintenance all at once.

Do note that patch-level releases are often only announced on the [mailing lists](http://erlang.org/mailman/listinfo/erlang-questions) and tagged on [the main git repository on GitHub](https://github.com/erlang/otp), but are otherwise not packaged on the main website.


### Windows {#windows}

If you are a Windows user, it is recommended that you use Windows 10 for any Erlang development. Prior versions can work, but community tools such as Rebar3 are only tested on Windows 10, for example.

Building on Windows from source has been notoriously difficult, and it is therefore recommended that you stick to the pre-built copies.

If you are a user of [Chocolatey](https://chocolatey.org), you can grab the [Erlang](https://chocolatey.org/packages/erlang) packages, and install them as you wish, with commands such as:

<a id="code-snippet--choco"></a>
```sh
choco install erlang                   # for the latest
choco install erlang --version 21.2 -m # allow many versions
choco install erlang --version 20.1 -m # and one more versions
```

This will add all the versions you want to your `PATH` variable, which you will then need to maintain in the right order.

Without Chocolatey, use binaries distributed on [www.erlang.org/downloads](https://www.erlang.org/downloads), or alternatively those built by [Erlang Solutions Ltd.](https://www.erlang-solutions.com/resources/download.html).

The installer for these versions comes with a wizard that will take you through all the required steps.

Do not forget to add Erlang/OTP to your `PATH` variable to contain your Erlang/OTP installation, since this will let you call it from the command-line:

1.  In the start menu, search for "system environment variables" and select the "Edit the System and Environment Variables (Control Panel)" option
2.  At the bottom of the "System Properties" window that has just open, press the "Environment Variables..." button
3.  Select the `Path` variable (or create it if it does not exist) and click the "Edit" button
4.  Add an entry for Erlang/OTP that matches the installation path, usually something like `C:\Program Files\erl10.2\bin`. The entries put earlier in the list will be loaded first.
5.  Save the options
6.  Close and restart any terminal you were running.

If you do development in the long term, you will be able to install multiple versions that way. You can control which one is used by changing and modifying the `PATH` variable's priorities in paths.

If you are a purist when it comes to Windows development, you may be quite comfortable in an environment such as Visual Studio, where pretty much everything can be done from within the IDE. Erlang comes from a different environment, and a lot of the instructions we'll use in this book are focused on using the command line to build everything.

If you are looking for a terminal to run the command line on Windows, various options are available:

-   Use PowerShell as a terminal. Most commands in this book should work fine with it, but some edge cases may exist.
-   Download and install [git for Windows](https://git-scm.com/download/win), which will come with a `git-bash` shell that will work well with all tooling and most commands in this book
-   Try [ConEmu](https://www.fosshub.com/ConEmu.htm) as a nicer terminal emulator to work with
-   Use [Cmder](https://cmder.net/) which is a Windows console emulator that packages most of the above options rather well
-   Use [Cygwin](https://www.cygwin.com) at your own risk; you will need to rebuild your software from source to work well with it, and tools like Rebar3 dynamically figure out they're on Windows, which historically has caused a few path problems when interacting with Cygwin

You can then use the editor or IDE of your choosing to work with Erlang components.


### OSX {#osx}

While OSX makes it possible to use [Homebrew](https://brew.sh/) or [Erlang Solutions Ltd. packages](https://www.erlang-solutions.com/resources/download.html) to install pre-built versions of Erlang/OTP, you should only do so if you're trying things out the first time around. If you're planning on doing actual development for the longer haul, you'll instead want to be able to handle multiple versions at once.

The most commonly supported tool for this is [kerl](https://github.com/kerl/kerl). Kerl is a wrapper around downloading, compiling, and loading various Erlang/OTP versions on a single system, and will abstract away most annoying operations.

You can install Kerl from homebrew by calling `$ brew install kerl`, or by following the instructions in its [README file](https://github.com/kerl/kerl#downloading).

Before installing Erlang, we will need to install and update a few dependencies, the main ones being to make sure you have [XCode](https://developer.apple.com/xcode/) installed and to then install OpenSSL (since OSX has terribly outdated copies of SSL by default):

<a id="code-snippet--openssl-osx"></a>
```sh
$ brew install openssl
...
$ ls /usr/local/Cellar/openssl/
1.0.2q
```

Note the full path this gives you for the local openssl install, here being `/usr/local/Cellar/openssl/1.0.2q/`

You can set the following options in your environment:

<a id="code-snippet--kerlcfg-osx"></a>
```sh
SSL_PATH=/usr/local/Cellar/openssl/1.0.2q/
export KERL_BUILD_BACKEND="git"
export KERL_CONFIGURE_OPTIONS="--without-javac \
                               --with-dynamic-trace=dtrace \
                               --with-ssl=${SSL_PATH}"
```

And ensure it's active (for example, call `source ~/.bashrc`). These options specify what is accepted or expected from the build tool. The one here disables Java bindings, and uses the new SSL install we've made. You can look at the [Build Instructions](https://github.com/erlang/otp/blob/master/HOWTO/INSTALL.md#configuring-1) for more configuration options.

If you want to add more content, such as `Wx` (which lets you use and build GUIs), the [Build instructions for OSX](https://github.com/erlang/otp/blob/master/HOWTO/INSTALL.md#os-x-darwin) contain further details to guide you.

From that point on, you can download and install your own Erlang/OTP versions:

<a id="code-snippet--kerl-osx"></a>
```sh
$ kerl update releases
...
# kerl build <release> <build name>
$ kerl build 21.3 21.3
...
# kerl install <build name> <target path>
$ kerl install 21.3 ~/bin/erls/21.3/
...
# make that version active
$ . ~/bin/erls/21.3/activate
# or alternatively
$ source ~/bin/erls/21.3/activate
```

Any installed version can then be activated on-demand. If you want to set a default version, you can put the activation command in your `.bashrc` configuration file (or any shell profile you might have).

If you are planning on using both Erlang and Elixir on your development machine, you might want to take a look at [`asdf`](https://asdf-vm.com/#/core-manage-asdf-vm). It is a plugin-based installer for multiple programming languages, and can handle both Elixir and Erlang at once. You may need to install the `autoconf` package to make it work.

To use it with Erlang, install the [Erlang plugin](https://github.com/asdf-vm/asdf-erlang) by calling `asdf plugin-add erlang https://github.com/asdf-vm/asdf-erlang.git`. This plugin wraps `kerl` and reuses all of its options, but transfers the builds under `asdf`'s control. As such, the previous configuration instructions remain the same. You just have to change the sequence of calls for:

<a id="code-snippet--asdf-linux"></a>
```sh
# asdf install erlang <version>
$ asdf install erlang 21.3
...
# asdf global <name> <version> [<version>...]
# asdf local <name> <version> [<version>...]
# export ASDF_ERLANG_VERSION=<version>
```

The main difference between `kerl` and `asdf` from there on is that `kerl` will use environment variables to know which version to run, and `asdf` will optionally use a `.tool-versions` file to trigger the change on a per-directory basis.


### Linux {#linux}

Linux distributions pretty much all have package managers that let you install pre-built copies of Erlang, or you can still use [Erlang Solutions Ltd. packages](https://www.erlang-solutions.com/resources/download.html). Much like with OSX though, you should only do so if you're trying things out the first time around. If you're planning on doing actual development for the longer haul, you'll instead want to be able to handle multiple versions at once.

The most commonly supported tool for this is [kerl](https://github.com/kerl/kerl). Kerl is a wrapper around downloading, compiling, and loading various Erlang/OTP versions on a single system, and will abstract away most annoying operations.

You can install kerl by calling:

<a id="code-snippet--linux-kerl"></a>
```sh
$ curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
$ chmod a+x kerl
```

And then moving kerl to your path. Kerl will automatically check and warn you about missing dependencies you might be needing when building libraries, so you can just go ahead and run the following commands, and listen to its directions as you go.

First, you can set options as follows in your environment:

<a id="code-snippet--kerlcfg-linux"></a>
```sh
export KERL_BUILD_BACKEND="git"
export KERL_CONFIGURE_OPTIONS="--without-javac \
                               --with-dynamic-trace=systemtap"
```

And ensure it's active (for example, call `source ~/.bashrc`). These options specify what is accepted or expected from the build tool. The one here disables Java bindings, but they would be skipped automatically anyway. You can look at the [Build Instructions](https://github.com/erlang/otp/blob/master/HOWTO/INSTALL.md#configuring-1) for more configuration options.

If you want to add more content, such as `Wx` (which lets you use and build GUIs), the [Build instructions for Wx](https://github.com/erlang/otp/blob/master/HOWTO/INSTALL.md#building-with-wxerlang) contain further details to guide you.

From that point on, you can download and install your own Erlang/OTP versions:

<a id="code-snippet--kerl-linux"></a>
```sh
$ kerl update releases
...
# kerl build <release> <build name>
$ kerl build 21.3 21.3
...
# kerl install <build name> <target path>
$ kerl install 21.3 ~/bin/erls/21.3/
...
# make that version active
$ . ~/bin/erls/21.3/activate
# or alternatively
$ source ~/bin/erls/21.3/activate
```

Any installed version can then be activated on-demand. If you want to set a default version, you can put the activation command in your `.bashrc` configuration file (or any shell profile you might have).

If you are planning on using both Erlang and Elixir on your development machine, you might want to take a look at [`asdf`](https://asdf-vm.com/#/core-manage-asdf-vm). It is a plugin-based installer for multiple programming languages, and can handle both Elixir and Erlang at once. You may need to install the `autoconf` package to make it work.

To use it with Erlang, install the [Erlang plugin](https://github.com/asdf-vm/asdf-erlang) by calling `asdf plugin-add erlang https://github.com/asdf-vm/asdf-erlang.git`. This plugin wraps `kerl` and reuses all of its options, but transfers the builds under `asdf`'s control. As such, the previous configuration instructions remain the same. You just have to change the sequence of calls for:

<a id="code-snippet--asdf-osx"></a>
```sh
# asdf install erlang <version>
$ asdf install erlang 21.3
...
# asdf global <name> <version> [<version>...]
# asdf local <name> <version> [<version>...]
# export ASDF_ERLANG_VERSION=<version>
```

The main difference between `kerl` and `asdf` from there on is that `kerl` will use environment variables to know which version to run, and `asdf` will optionally use a `.tool-versions` file to trigger the change on a per-directory basis.


### FreeBSD {#freebsd}

On FreeBSD, the experience with `kerl` (as reported in other sections) has been hit and miss. Sometimes, some patches are required to make things work as smoothly as on other platforms. The good news is that if you use either the BSD [ports](https://www.freebsd.org/doc/en/books/handbook/ports-using.html) or [packages](https://www.freebsd.org/doc/en/books/handbook/pkgng-intro.html), it will all work fine out of the box.

This is the easiest way forwards, but makes switching across versions a bit trickier since you don't get an Erlang version manager for free. However, BSD ports and packages do let you build any version supported at your liking.

For example you can call any of the following:

<a id="code-snippet--bsd-install"></a>
```sh
# pkg install erlang # default copy
# pkg install erlang-runtime20  # OTP-20.x
# ls /usr/ports/lang/erlang* # source install: pick the version directory
erlang/
...
erlang-runtime20/
erlang-runtime21/
erlang-wx/
# cd /usr/ports/lang/erlang-runtime21/
# make config-recursive     # configure all the deps
# make install
```

FreeBSD maintainers are generally good about ensuring things keep working fine on the main supported architectures, so if you're sticking to x86 and avoid ARM, you should have no major issues.


### Making things Nice {#making-things-nice}

Before you're done, you should go to your shell or terminal profile, and add a few environment variables. Specifically, you can use `ERL_AFLAGS` or `ERL_ZFLAGS` to add configuration switches to the `erl` executable at all times.

We'll use `ERL_AFLAGS` to turn on two neat features: outputting strings with Unicode support by default, and enabling shell history so that the Erlang shell remembers your commands between invocations. Add the following to your environment:

<a id="code-snippet--erl-aflags"></a>
```sh
export ERL_AFLAGS="+pc unicode -kernel shell_history enabled"
```

Things will feel a bit more modern that way.


## Installing Rebar3 {#installing-rebar3}

Rebar3 is the standard build tool within the Erlang community. It essentially bundles all of the other tools shipping with Erlang along with a few open-source ones, and makes them all work under a unified project structure.

There are a few ways to install Rebar3: from a pre-built binary, or from source, and then a last variant for a faster-running local install. Do note that in all cases, you need Erlang to have been installed already.


### Pre-Built Binaries {#pre-built-binaries}

Pre-built binaries can be found at [www.rebar3.org](https://www.rebar3.org/). There's a big "Download" button with the latest stable version, but if you like to live more dangerously, you can grab [the latest <span class="underline">nightly</span> build](https://s3.amazonaws.com/rebar3-nightly/rebar3) as well.

It is common to create a directory `~/bin/` to place commands line utilities like `rebar3`, which is where you might want to put the version you just downloaded. Call `chmod +x rebar3` on it to make sure it can run, and add it to your path with `export PATH=~/bin/:$PATH` in your `~/.bashrc`, `~/.zshrc` or equivalent.

Windows users who want to use the code from PowerShell or cmd.exe (rather than a terminal emulator) must ensure that a `rebar3.cmd` file is added:

<a id="code-snippet--rebar.cmd"></a>
```sh
@echo off
setlocal
set rebarscript=%~f0
escript.exe "%rebarscript:.cmd=%" %*
```


### Building From Source {#building-from-source}

First make sure that you have git installed, and checkout the repository to build it:

<a id="code-snippet--rebar-bootstrap"></a>
```sh
$ git clone https://github.com/erlang/rebar3.git
$ cd rebar3
$ ./bootstrap
```

This will create a `rebar3` script file (along with a `rebar3.cmd` file on Windows).


### Local Install {#local-install}

The local install form will let you take any of the previously built Rebar3 versions, and unpack them to a local directory from which the tool will be able to self-update at a later time:

<a id="code-snippet--rebar-local"></a>
```sh
$ ./rebar3 local install  # starting from a rebar3 not in PATH
===> Extracting rebar3 libs to ~/.cache/rebar3/lib...
===> Writing rebar3 run script ~/.cache/rebar3/bin/rebar3...
===> Add to $PATH for use: export PATH=$PATH:~/.cache/rebar3/bin
$ export PATH=$PATH:~/.cache/rebar3/bin
$ rebar3 local upgrade # this can be used to update to the latest stable copy
...
```


## Configuring Editors {#configuring-editors}


### Editor-agnostic (via a Language Server) {#editor-agnostic--via-a-language-server}

A [language server](https://langserver.org) is an editor-agnostic solution which provides language features such as code completion, jump to definition and inline diagnostics. The [Erlang LS](https://erlang-ls.github.io/) language server implements those features for the Erlang programming language. It integrates with Emacs, VS Code, Sublime Text 3, Vim and probably many more text editors and IDEs which adhere to the [LSP protocol](https://microsoft.github.io/language-server-protocol/).

To get started with Erlang LS with a specific text editor, please refer to the "editors" section of the [documentation](https://erlang-ls.github.io/).


### Visual Studio Code {#visual-studio-code}

The [Erlang extension](https://marketplace.visualstudio.com/items?itemName=pgourlain.erlang) by Pierrick Gourlain is recommended.

To configure the extension, go to the `Preferences` and then `Settings` menu. Within the VS Code window, unroll the `Extensions` menu until the `erlang configuration` section. Make sure that all the values are right, particularly the Erlang path and the Rebar3 path. With this in place, you can mix and match all the other extensions you'd like and things should be ready to go.

The code formatter may feel a bit janky; it respects the official Erlang repository's old rules of mixing tabs and spaces, and expects each tab is 8 spaces wide. This is not really use anywhere else, and if your Visual Studio Code is not configured that way (using 4 spaces for example), it will just look off.

Otherwise, that extension covers all the major features: jumping around code definitions, build tool support (although only `compile`, `eunit`, and `dialyzer` are supported in the command palette, you can still call `rebar3` directly from the terminal), intellisense, warnings as you type, and CodeLens features. If you look at the extension's documentation, you'll also find debugger support instructions.

All you've got to do then is configure themes and more general extensions to your liking.


### Emacs {#emacs}

Erlang/OTP comes with an Emacs mode in the `tools` application, `lib/tools/emacs/`. The authors of this book who use Emacs stick to using this mode by having Emacs load directly from the latest Erlang version installed. There are a number of options for alternative modes and addons to use for fancier support in `erlang-mode`, here we will only discuss [Ivy](https://oremacs.com/swiper/) completions and [Flycheck](https://www.flycheck.org/en/latest/) syntax checking. But first we need [use-package](https://jwiegley.github.io/use-package/) which is a tool for isolating package configuration. Code for automatically installing `use-package` can be [found here](https://github.com/CachesToCaches/getting_started_with_use_package/blob/7d260ddf7b15160c027915340ff0c70ce05ea315/init-use-package.el). Include that code in your `~/.emacs.d/init.el` so that on startup `use-package` is installed. Or use the [installation instructions](https://jwiegley.github.io/use-package/installation/) found on the `use-package` website.

This following bit of `elisp` code can be used to setup just `erlang-mode`, no Ivy or Flycheck involved, and have it load for Rebar3, Relx and other Erlang configuration files:

```elisp
(use-package erlang
  :load-path ("<PATH TO OTP>/lib/erlang/lib/tools-3.0/emacs/")
  :mode (("\\.erl?$" . erlang-mode)
         ("rebar\\.config$" . erlang-mode)
         ("relx\\.config$" . erlang-mode)
         ("sys\\.config\\.src$" . erlang-mode)
         ("sys\\.config$" . erlang-mode)
         ("\\.config\\.src?$" . erlang-mode)
         ("\\.config\\.script?$" . erlang-mode)
         ("\\.hrl?$" . erlang-mode)
         ("\\.app?$" . erlang-mode)
         ("\\.app.src?$" . erlang-mode)
         ("\\Emakefile" . erlang-mode)))
```

For using Ivy to get completion support add the `ivy-erlang-complete` package, set a custom Erlang root for it to use and run its `init` when the Erlang mode is configured:

```elisp
(use-package ivy-erlang-complete
  :ensure t)

(use-package erlang
  :load-path ("<PATH TO OTP>/lib/erlang/lib/tools-3.0/emacs/")
  :hook (after-save . ivy-erlang-complete-reparse)
  :custom (ivy-erlang-complete-erlang-root "<PATH TO OTP>/lib/erlang/")
  :config (ivy-erlang-complete-init)
  :mode (("\\.erl?$" . erlang-mode)
         ("rebar\\.config$" . erlang-mode)
         ("relx\\.config$" . erlang-mode)
         ("sys\\.config\\.src$" . erlang-mode)
         ("sys\\.config$" . erlang-mode)
         ("\\.config\\.src?$" . erlang-mode)
         ("\\.config\\.script?$" . erlang-mode)
         ("\\.hrl?$" . erlang-mode)
         ("\\.app?$" . erlang-mode)
         ("\\.app.src?$" . erlang-mode)
         ("\\Emakefile" . erlang-mode)))
```

Flycheck comes with Rebar3 support and can automatically detect a Rebar3 project, so all that is needed is the `flycheck` package:

```elisp
(use-package delight
  :ensure t)

(use-package flycheck
  :ensure t
  :delight
  :config (global-flycheck-mode))
```

The `:config (global-flycheck-mode)` argument to `use-package` will enable Flycheck for all code you edit in Emacs, the expressions given with `:config` are run after the package has been loaded. The `:delight` argument tells `use-package` to use the `delight` utility to disable showing Flycheck in the mode line. Keeping it out of the mode line saves space and especially since it is enabled globally we don't need it being called out as currently enabled in the mode line.

If you like using Flycheck then [hydra](https://github.com/abo-abo/hydra) is worth checking out for stepping through and viewing the full list of errors. The Hydra macro sets up short keybindings that work only when the initial Hydra binding has been run. The following code will setup basic bindings for viewing Flycheck errors when `C-c f` is called:

```elisp
(use-package hydra
  :defer 2
  :bind ("C-c f" . hydra-flycheck/body))

(defhydra hydra-flycheck (:color blue)
  "
  ^
  ^Errors^
  ^──────^
  _<_ previous
  _>_ next
  _l_ list
  _q_ quit
  ^^
  "
  ("q" nil)
  ("<" flycheck-previous-error :color pink)
  (">" flycheck-next-error :color pink)
  ("l" flycheck-list-errors))
```

Lastly, a couple packages that are not Erlang specific but are worth calling out as very useful when developing on a project:

-   [magit](https://magit.vc/): An Emacs interface for [Git](https://git-scm.com/). Magit does not just allow for calling Git from Emacs but provides a streamlined interface for everything from staging changes to interactive rebases.
-   `counsel-rg`: [counsel](https://github.com/abo-abo/swiper/) is a collection of commands that utilize Ivy, the package we used earlier for Erlang completions. `counsel-rg` uses  [ripgrep](https://github.com/BurntSushi/ripgrep) to search for strings across the files in a project -- when in a git project they act like `git grep`, only searching in files in the git repo and honoring `.gitignore`. Since ripgrep is an external commands it must be installed separately, for example on Ubuntu or Debian run `sudo apt-get install ripgrep`. When `ripgrep` is installed it will also be used by `ivy-erlang-complete` for faster searches.
-   [swiper](https://github.com/abo-abo/swiper/): An alternative to `isearch` for searching in a buffer that uses Ivy.
-   [company-mode](https://company-mode.github.io/): When combined with `ivy-erlang-complete` through [company-erlang](https://github.com/s-kostyaev/company-erlang) this mode will provide a popup of completions automatically, rather than requiring `C-:` to bring up completions in the minibuffer.
-   [flycheck-inline](https://github.com/flycheck/flycheck-inline), [flycheck-pos-tip](https://github.com/flycheck/flycheck-pos-tip) or [flycheck-popup-tip](https://github.com/flycheck/flycheck-popup-tip): These packages offer different options for displaying Flycheck errors at the position of the error instead of in the minibuffer.


### Vim {#vim}

Although absolutely fancy support for Erlang is possible in Vim—as the [vim-erlang group on Github](https://github.com/vim-erlang) allows—the authors of this book who use it tends to stick with the most minimal configuration possible.

Simply stick with the default syntax highlighting in your `.vimrc` file, and make sure it's used in all the right file types:

```vim
"also erlang
autocmd BufRead,BufNewFile *.erl,*.es.*.hrl,*.xrl,*.config setlocal expandtab noautoindent
au BufNewFile,BufRead *.erl,*.es,*.hrl,*.xrl,*.config setf erlang
```

This is the very basic stuff, obviously. Fancier integration is possible, but the one author who uses vim mostly uses only this, and relies on Rebar3 in a terminal to deal with the rest of the language.

<div class="pagination">
  <div><a href="/docs/development">← Prev</a></div>
  <div><a href="/docs/development/otp_high_level">Next →</a></div>
</div>
