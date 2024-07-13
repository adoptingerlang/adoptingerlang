+++
title = "How to Do Polyglot Right"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:10+00:00
draft = false
[menu]
  [menu.main]
    weight = 2005
    identifier = "how-to-do-polyglot-right"
    parent = "team_building"
+++

You're adopting Erlang; unless you're a new company with no existing code base, that means that you're planning to become a polyglot organisation. Even if your roadmap plans for a total replacement of all existing code, there will be a transition period during which you will have to be polyglot. Therefore, you should plan to properly support polyglot workflows.

In this chapter, we'll cover common polyglot myths and misguided ideas, propose to impose standards for language viability, and raise a few challenging points your organisation will likely go through.


## Polyglot Myths {#polyglot-myths}

There are plenty of beliefs and arguments that get brought up when engineers start discussing the idea of changing an organisation to be polyglot. Let's look at common complaints:

-   build engineering becomes more complex as more tools and build chains need support
-   more dependencies need auditing and upkeep to keep everything current
-   as time goes on, some languages become uncool and require more effort to train, hire for, or rewrite
-   we're already doing polyglot and there's this one component nobody wants to touch because a dev once wrote it in a language nobody else knows

Build engineering is a legitimate concern, in the same way many organizations with a dedicated IT department ship out a single type of computer for every developer to in order to reduce the number of configurations that must be supported. That being said, this is a less prevalent issue with the advent of pervasive containerization. Docker images and runtimes such as Kubernetes already provide a one-size-fits-all interface that extends from local development to production runs. Maintaining build images with the help of people familiar with the programming languages on board mostly reduces the complexity of everything involved over time.

There isn't necessarily a great way to work around dependency explosion and monitoring, short of using tools that scan and report issues for you. Dependency surface area is mostly a problem of managing complexity, and isn't necessarily tied to how many languages you're using. For example, we've seen a single Javascript demo project with a dependency tree larger than established Erlang software used for entire departments in a company. It's also becoming more common for other tools, such as dependency vulnerability scanners, to support Hex packages.

If your company has a procedure where each dependency must be vetted by the security team, the legal department (for licensing purposes), and engineers (for code quality), you'll find out that in practice, it doesn't work very well. People will either stick to a pre-approved list of libraries or will only review top-level dependencies. In some cases, they'll never update outdated libraries because they prefer to avoid all the red tape. In other cases the developers will just write the dependency into their project in order to avoid having to cross many departments of reviews if they feel they gotta ship things fast, and you'll end up with code that barely gets reviewed at all, and possibly of poor quality because they may have rewritten established libraries. We consider this to be an organisational issue, not a programming language issue.

Uncool languages or languages that are "hard to hire for" will always be a problem. Unfortunately, this isn't a problem you can  actually solve, because no one knows the future.  Is it worse to have a single main language people stop liking and then none of your stack is easy to staff, or to have only half of it being hard to hire for? What about cases like Python, where the migration from 2.x to 3.x has been so hard that libraries and components essentially consider them to be distinct languages? We consider that organisations that do not marry themselves to a single language but instead make a habit of switching between a few of them (and not just a ton of them) train themselves to better adjust to variations in the market of programming languages.

Then we have the complaint about the lone developer not caring about what the organisation does or knows, and who starts shipping things in a language nobody else knows. That developer might be doing this as part of resume padding, and then leave everyone behind after hopping to a new job. There's no protection against that, even when you mandate a single language. Hell, a developer could do the same by using your company's main language but by trying a new framework or new paradigm supported by the language or one of its libraries. Having a single unsupported [Lambda](https://aws.amazon.com/lambda/) could be trickier than a new language within a known deployment context.

We mostly consider this negative behaviour, letting such work take place in complete isolation, a failure of process that must be addressed, rather than blaming the tool for being used.

Finally, there's one major argument to counter. It's commonly accepted that having many languages creates barriers, and that you will have an easier time moving developers around if you use one language and one framework. In fact, we would call this the biggest myth against polyglot organisations.

It's misguided because it assumes developers are like mechanical parts. As long as the interface is the same, they can be interchangeable. It ignores everything not related to the language. Different teams have different habits and dynamics you need to get used to. They work on projects with different histories and challenges, in possibly distinct domains, with different focuses and compromises done in how they engineer things. To add to this, even within a single language, there is a cost of migration: different frameworks, libraries, toolchains, or versions of these toolchains all require some adaptation.

We argue that together, these factors represent a challenge that is often as significant—if not more so—than learning a new programming language. Even if it's significant, the cost of learning new languages is less than it seems, mostly because it encompasses all of these underlying issues, which may still exist within a single language. It looks like the cost is high <span class="underline">because</span> we traditionally don't provide support for any of the challenging things and they must be learned from scratch when switching ecosystems or teams.


## Minimum Viable Language {#minimum-viable-language}

Based on these myths and common complaints mentioned earlier, we suggest an approach that addresses these underlying issues directly, regardless of the programming language being used. We also propose minimal standards which must be met in order for a programming language to be adopted. Although it may be tempting to initially propose these factors as hard rules to be enforced, we believe that letting engineers participate in their creation will provide better buy-in and self-policing than the alternatives.

The first perspective to keep in mind in deciding what represents a <span class="underline">Minimum Viable Language</span> (MVL) is that you will want to help your organization become a learning organization, where your developers train each other to continuously adapt. The objective is to guide learning and provide boundaries around the maturity levels of a piece of technology to be considered worth checking out.

To do this, we suggest making the costs of longer-term support obvious. As long as your developers can provide a reasonable path forward for that support, there would be no reason to ban that language. For example, ask to have the following questions answered:

-   How do you deploy it?
-   How do you provide observability for it? What are the metrics, logs, and tracing support like, and how do you add them to a project?
-   Which build tools are going to be blessed for people to use, and how should developers set up their development environment?
-   What test frameworks are provided, and how do you reconcile them with the overall test standards you have?
-   How do you write documentation for it?
-   Who are we going to ask questions about it in the future? Do we have any local experts and advocates? How many, and what are we going to do if we fall below a given threshold?
-   If you do RPC, message passing, data serialization, use a specific database, which libraries should be used for each? Is there a need to write them?
-   How do you approach structuring, writing, and debugging code in the language? Are there basic guidelines or tools that should be provided?
-   How do you hire for these developers? How do you train those who do not know about the language?
-   Do we have any basic on-boarding documentation to help people get started with any of the factors above?

Add or remove questions as you see fit. You can contextualize some like memory footprint, requirements for front-end languages, mobile development, back-end languages, desktop software, and so on. Come up with a baseline your engineers think should be provided. Answer them in the abstract, without thinking of a single programming language. Classify some of these items as critical, nice to have, and so on. You might come up with a table looking like this:

| Item                    | Critical                           | Nice to Have                                               | In an Ideal World                                                            |
|-------------------------|------------------------------------|------------------------------------------------------------|------------------------------------------------------------------------------|
| HTTP Server             | bare server                        | web framework                                              | popular web framework with community support                                 |
| gRPC Library            | serialization/deserialization      | full gRPC client/server                                    | well-documented examples                                                     |
| Micro services          | sample implementation              | templates library                                          | full tutorial                                                                |
| Number of local Experts | 2                                  | 4                                                          | 10+                                                                          |
| Hireability             | sizeable local user group          | taught in college                                          | everyone knows it                                                            |
| On-boarding             | people pick things up on their own | some documentation exists                                  | tutorials and training provided                                              |
| Observability           | logging and metrics library        | guidelines on language- or framework-specific logs/metrics | OpenTelemetry support                                                        |
| Testing                 | unit testing framework             | integration testing framework                              | advanced testing frameworks (Property-Based Testing, mutation testing, etc.) |
| ...                     | ...                                | ...                                                        | ...                                                                          |

Establish a base criteria to hit, the critical items without which you can't adopt the language, and so on. Turn it into a rather objective scorecard you can use to validate decisions. Otherwise, what will happen is that a CTO or architect will push for whatever tech they fancy, without regards to actual support. The default reflex of organizations is actually to just find something someone higher up likes, and push for it and let the rest of the organization adjust to these whims. A score card (as simple or complex as you want it to be) aims to make people aware of the costs of adopting or changing technologies, and as a checklist of what they need to provide to help each other use it.

Run the scorecard against a language you want to adopt, and see if you could indeed adopt it successfully or what might be missing to do it well; run the scorecard against languages you already have adopted, and see if they would pass. If they don't, start by fixing the gaps. Grow a knowledge base that is guided by what your engineers feel they'd need to do their job more effectively. Specialize and adapt the requirements for some areas of the company, and see where this lands you.

Chances are that even in a company with a single language being mandated, you'll be missing plenty of the "nice to haves" or above points; When we choose to leave it to the community to handle that support, we end up underfunding internal support as well. That's how legacy codebases get created. You might also find out that different teams judge things very differently around their specific domain of expertise. Folks who talk to infrastructure components all the time may end up with very distinct requirements from teams who work on end-user features. You can start specializing score cards, or incorporating domain-specific elements to it.


## A Production Language {#a-production-language}

As you iterate over all of these criteria you have established, you should keep some end-goal in mind. For example, you might want to end up with the following things covered (note that many of these are covered in this text to make things easier for more Erlang users out there):

-   How to install and set-up your development environment with all the versions required
-   A blessed list of libraries that have been validated and that most projects are using
-   The procedures around starting new projects (setting up tests, which linters to use, how to get CI/CD going, code snippets and templates, and so on)
-   The places where you can get help (such as internal mailing lists, chatrooms, or the names of people to contact)
-   A library of learning resources, which can be books, tutorials, videos, or internal documents you found useful
-   Security guidelines
-   Checklists of things to do to make a service or application production grade
-   How to benchmark your code
-   How to instrument your code
-   Architecture and contribution guidelines
-   Runbooks and playbooks for common problems
-   Interview questions

Not all of these need to be expansive tutorials in wikis; some can be accomplished by pairing, lunch and learns, presentations, and so on. Do note that regardless of how you choose to help on-board people for these items, the developers you hire or move across teams already have to figure <span class="underline">all of these</span> out, many without explicit support. This invisible work is being done all the time. The aim is to make it visible.

In choosing to support polyglot languages and surfacing what your team believes is necessary to help them move from one project to the next (without creating orphaned technology), you're likely to actually make switching between teams easier than if you just had one programming language without that support. If you really want to help make such principles part of your culture, show that you value teams that participate in reaching and improving standard practices, and who do work to bring existing codebases up to the desired level of excellence. It can be tricky to get enough buy-in to progressively build these resources and keep them alive, so that last bit is particularly important.

In short, let's focus on how people learn and help them do it, rather than pretending they already know everything. Prepare starter guides around domain knowledge. Map out areas where you are currently under-serving your engineers and in which you possibly need to invest. Establish ways to socially keep language-specific knowledge and tooling alive. And possibly discover factors that you did not know were important but turn out to be critical to your teams doing their work.
