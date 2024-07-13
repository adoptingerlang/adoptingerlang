+++
label = "Index"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
date = 2019-08-08T08:05:00+00:00
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:08+00:00
draft = false
title = "Introduction"
[menu]
  [menu.main]
    weight = 1001
    identifier = "introduction"
+++

<header>
  <h1>Introduction</h1>
  <h5>
    <strong>August 3, 2019</strong>
  </h5>
</header>

So you've been checking Erlang out, and you've read a few tutorials and a bunch of books, but you're finding that nothing really tells you how you should set up a modern project for you and your team, and you have then found this text.  The Erlang community has collectively spent years writing introductory content, and a lot of it is still really good. So what's the hope for yet another book?

Frankly, most of the material out there is really solid to teach you the Erlang basics, and we have no pretension of replacing them, nor a desire to re-explain the same content they contain once more. Instead, we the authors felt like while they are rock solid in a lot of areas, we could help cover some blind spots.

For example, here are a bunch of interesting books:

-   [Programming Erlang](https://www.goodreads.com/book/show/808814.Programming_Erlang), by Joe Armstrong is great to get into the philosophy behind Erlang
-   [Erlang Programming](https://www.goodreads.com/book/show/4826120-erlang-programming), by Cesarini &amp; Thompson is a very well-rounded practical approach to learning the language and bits of OTP
-   [Erlang and OTP in Action](https://www.goodreads.com/book/show/7438968-erlang-and-otp-in-action), by Logan, Merritt &amp; Carlsson is the first Erlang book that really tries to teach you OTP-first and hints at broader system design
-   [Learn You Some Erlang](https://learnyousomeerlang.com/), by Fred Hebert, is possibly the friendliest introduction to both Erlang and OTP that tries to cover everything from basic Erlang to the design principles underpinning OTP, releases, and the whole ordeal
-   [Études for Erlang](https://www.goodreads.com/book/show/17984681-tudes-for-erlang) by J. David Eisenberg is a fantastic companion exercise book that works with a bunch of other Erlang books, as a kind of practical complement
-   [Concurrent Programming ERLANG](https://www.goodreads.com/book/show/808815.Concurrent_Programming_ERLANG) by Williams &amp; Armstrong is a great piece of history from the 90s, showing earlier iterations of Erlang and how it could be applied to real world problems
-   [Introducing Erlang](https://www.goodreads.com/book/show/15811999-introducing-erlang) by Simon St. Laurent is the most concise taste of Erlang you can get in book form
-   [Erlang in Anger](https://www.erlang-in-anger.com/) by Fred Hebert is the only book that really contains a complete guide to debugging your Erlang systems in production
-   [Designing for Scalability with Erlang/OTP](https://www.goodreads.com/book/show/18324312-designing-for-scalability-with-erlang-otp) by Cesarini &amp; Vinoski is likely the most modern approach to Erlang/OTP systems with a real-world slant to it.
-   [The BEAM Book](https://blog.stenmans.org/theBeamBook/) by Erik Stenman (and a lot of community contributors) is the most advanced resource on the virtual machine internals
-   [Property-Based Testing with PropEr, Erlang, and Elixir](https://propertesting.com/) by Fred Hebert is the one book that teaches Property-based testing for Erlang

And there are some more too.

We intend to replace none of these. One huge omission in most (if not all) of these books, is that they tend to focus on Erlang/OTP on its own. In fact, many of these were written before massive shifts in how the community works. For example, <span class="underline">Learn You Some Erlang</span>, while very complete, was being written before <span class="underline">any</span> community-driven build tool would see massive adoption, and before concepts such as OTP Releases would see widespread use. None of them have really been written under the new age of containerized platforms currently in place. And pretty much none of them mention how you should structure your projects to fit well within the open source Erlang ecosystem.

So this is what <span class="underline">Adopting Erlang</span> is all about. This book (and website!) is all about filling in the niche that other books and manuals have not yet managed to properly cover. What you will learn in these pages will contain actually super useful stuff like:

-   How to set yourself up to use multiple Erlang versions, because in the real world, you end up having to run multiple Erlang versions for the multiple projects your workplace or group of friends will end up using.
-   We also cover how to set up editors and other tools, because chances are you may not have a good Erlang setup going even if you've already seen the basics
-   How to approach OTP systems from the top. Most resources out there take a bottom-up approach, but we want you to be able to have the right project structure from day one, and then fill in the gaps with other resources as you need them
-   What's needed for a good project, including dependency handling, some testing practices, handling configuration and documentation, and so on
-   How to set up a good <span class="underline">Continuous Integration</span> (CI) pipeline on common open platforms so that code reviews and automated testing can get the best support they can
-   How to handle a bunch of difficult stuff nobody really teaches properly, like dealing with strings, specifically Unicode, time and proper SSL/TLS configuration
-   How to deploy your Erlang systems as a self-executable bundle of files
-   How to properly package your Erlang systems as Docker images, and showing how to manage their lifecycle with Kubernetes
-   How to plan on setting up operations and get metrics going <span class="underline">outside</span> of what the VM provides; think of things like logging and distributed tracing, platforms to get metrics dashboards going, and so on
-   How to build a team that will start using Erlang in commercial projects
-   How to interview your first Erlang experts or developers
-   How to structure your practices for things such as code reviews, experience sharing, and so on.

By opposition, we will <span class="underline">not</span> cover things like basic Erlang, core OTP behaviours, and so on. They have been covered multiple times in other resources in the past, many of which are freely available. We still have put together an appendix of cheat sheets you can refer to if you need a refresher.

Essentially, we hope for <span class="underline">Adopting Erlang</span> to be the missing link between all the various starter books  and the more advanced material like <span class="underline">Erlang in Anger</span> that lets you debug stuff in production. We want this book to teach you how to go from "Okay, I think I got the basics" to "let's get this project going, and let's do it right." After reading this book, you should be able to know exactly what the best practices are to fit right in with the rest of the Erlang community.

<div class="pagination">
  <div><a href="/">← Prev</a></div>
  <div><a href="/docs/introduction/about_the_authors">Next →</a></div>
</div>
