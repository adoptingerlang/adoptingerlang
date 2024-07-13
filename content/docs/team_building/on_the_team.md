+++
title = "Who to Put on The Team"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:10+00:00
draft = false
[menu]
  [menu.main]
    weight = 2002
    identifier = "who-to-put-on-the-team"
    parent = "team_building"
+++

In this chapter, we'll look into the best ways to set up your team for success when getting started. We're not talking about tools, but rather about who is going to be on that team. Don't worry, you shouldn't need to fire or replace anyone; however, you'll have to create expertise in a new area where you currently have little.

We'll give you two principal approaches: building around an expert or creating your expertise in-house. We'll also tackle the topic of remote teams.

One piece of advice that is valuable in all cases: start by favoring those who are willing and excited to learn about new technology, Erlang in this case. You can't really force people to learn against their will, but you can absolutely help those who are willing to do so.


## Building Around an Expert {#building-around-an-expert}

The easiest way to gain expertise into a new technology or a new business domain is often to just hire someone with that expertise. Unfortunately, this never truly solves your problem on its own; you then depend on your expert for everything. This ends up creating bottlenecks for all changes going through the system, resulting in massive slowdowns when your expert is on leave.

{{% admonition info "Hiring an Expert" %}}
For this section, we'll more or less assume you know how to find an expert on your own, and will focus on structuring the team and figuring out what such an expert will need to help you with. For help on actually hiring people, we'll add a "How to Hire" chapter that should help clear things up.
{{% /admonition %}}

At the time of this writing, the average tenure time in the industry is below 5 years. This means that a team willing to be sustainable would be hiring expert after expert with no end in sight. The secret is to train your own experts after having found your first one. We consider it healthier for a company to be able to consistently hire more junior employees and see them leave as experts over the years than it would be to just poach experts and senior-level employees all the time and see them leave all the same.

Make no mistake here, we're not arguing against expertise. We're arguing in favor of it: expertise is so important that you should be focusing on producing it in-house. Places that can't offer the environment that helps their employees grow are at the mercy of their competition doing it for them. As such, building around an expert implies that the expert is your partner in building the rest of your team. In fact, the rest of your team should also be aware of this so they get in the right mindset, and make sure to get the most they can out of it.

This should be a role of which they are fully aware, and which you discuss with them over time. The expectation should be that you start slow with your expert and the people in your organization who are most enthusiastic about learning Erlang. In your first few weeks, your expert's role will be to mostly give your folks the basic knowledge they need to get going. This will usually follow the pattern of most beginners' books: the basic syntax and data types, teaching recursion and writing small modules, showing them how to test their code, then slowly adding in multiprocessing and OTP behaviours.

Once your team starts feeling comfortable enough on the basics, you'll be able to have your people start working on their first prototype of the system you want them to write and maintain in the long term. An interesting approach there is to actually use that first prototype as a training setup and nothing more. Everyone on board knows the prototype is not meant to reach production, and if it ever does, the first thing on the roadmap is to replace it once you have the proper experience.

The prototype is important, though; by picking a problem they're familiar with, it becomes a test bed where your employees are free to experiment in order to get comfortable around the language and its peculiarities. This creates an opportunity for them to revisit their domain knowledge and to teach it to the expert, while the expert works with them to figure out how they could re-frame what they know within the context and semantics of Erlang. This will hopefully create a rapport and complicity between them, where the relationship shifts from "trainer and trainee" to just having a lot of people teaching things to each other.

As this prototyping work is being done, your expert should start guiding your team towards broader and more complex concepts: developing for fault tolerance (how do you structure supervision trees properly?), building and deploying OTP releases, and, finally, performance-related and production debugging concerns.

One of our favorite experiments to run is to have the expert guide the team into building their own supervision structure, ideally with the prototype the team has been working on:

1.  Start with the various components the team has identified (web servers, connection handlers, encoders/decoders, configuration managers, and so on), and draw them on a whiteboard
2.  Draw communication channels between the components. Highlight the places where messages are being passed or where network communication takes place.
3.  Ask of each communication channel "is this something we expect to fail often, what happens when it does?". Make the team aware of the potential for faults in the system
4.  For each of the faults, ask what should be happening with the state held by each component. Is it going to be transient? Is it allowed to be dropped? Can it be re-built or fetched from somewhere?
5.  Insert supervisors above the components that could be failing and assume that after each fault, each of the components starts from a predictable non-dynamic state (initial arguments passed by the supervisor)
6.  Iteratively rework the components and re-structure them; move the state you can't afford to lose out from the boxes where failure is far more easy to foresee, and think of how information gets transferred there on each restart.

This exercise is where we've seen the most "enlightenment" about Erlang. The team suddenly realizes they have amassed enough understanding to re-frame and re-structure their whole approach to designing their software. They start making use of features unique to the Erlang virtual machine and the OTP framework, and build differently, leveraging new affordances. Even if some of your developers drop out of the Erlang train and go back to their prior tasks, they will likely take that experience in how to structure things and apply it in older places. We've heard of people doing it and saying they now thought differently about their C#, Go, or even Ruby design once they tried that one.

Once you think they get it, ask the whole team to re-design the prototype with what they now know. They can possibly keep some of the components and reuse them, but they should not feel bad at all about throwing a lot of it away, and neither should you: you'll be rewriting a thing written by near-complete beginners for something written by people with some experience. As a retrospective, it's interesting to compare what they did in the prototype with what they ended up with in the real product.

Doing the full learning experiment may take a few months, but even after a few weeks you'll likely see some of your non-expert employees slowly gain confidence and start helping others. Your expert, knowing you want his expertise to spread, should actually encourage that behaviour, and start delegating on the more basic questions. Then the transition to a more mature and functional team will be taking place. People will be teaching each other about various topics; your expert's expertise will be less and less necessary—likely only for tricky issues that are more and more rare—and they'll then be free to either participate to the projects on more equal footing, or start moving to other teams you have that also want to adopt the technology.

{{% admonition tip %}}
Regardless of how you run things at that point, make sure that from time to time your expert sits down with the rest of the team, and asks them about the various points of friction they have. Figure out what annoys them about the toolings, the language, and so on. This will help everyone re-calibrate and improve things. It's otherwise frequent for newcomers to just assume "this is how it is" and they may end up living with far more annoyances than they otherwise would.
{{% /admonition %}}


## Building Without an Expert {#building-without-an-expert}

Building without an expert is definitely a bit trickier. It's not impossible (some people had to become experts on their own to get going) and there are a lot of resources out there—books, talks, forums, chat rooms, and so on—but it's definitely more of a challenge and it can be costlier. The biggest risk in building without an expert is that you may not know how long you're going to spend in wrong paths and bad avenues, and the bad learning you do early can stick for a longer time.

This is, ultimately, not too different from regular development, but you'll be headed that way willingly. As such, you may want to structure things a bit differently. Rather than picking one expert and your enthusiastic folks, you will want to explode the requirements and roles across everyone, to try and get the dynamics that an expert would have facilitated.

Identify people who have prior knowledge of functional languages; they won't know everything, but they already have a head start. Also identify some self-directed learners, those who enjoy sitting down and digging into something. Their job will be to read books and figure things out a few weeks ahead of everyone else; they're quite literally the piano teacher only ahead of you by a couple of weeks. You'll want to find someone who enjoys teaching, writing documentation, or preparing demos. That's often pretty hard, so if you can't find anyone, assign a rotating schedule to that responsibility. Put them in charge of doing broader information dissemination, maintaining docs, and a quick "getting started" track to what you do in your organization.

Have everyone do bits of development and getting used to the feel of the language, getting involved with things in the community, and leverage the code already existing out there. Here are a few ideas:

-   look into existing code, and do a show-and-tell of various libraries and their approaches; dig in to figure out how they are structured.
-   if you can't figure out why an open-source library does what it does, feel free to ask their authors in an issue, or people on slack, mailing lists, or a forum. Get the real answer, and show it out there
-   do code reviews on the code you have written, where the main objective is everyone discussing some things where you (so far) had not had agreements. This may include syntax and alignment, but will more likely include things like just how much do you document, the better testing approaches you'd prefer to normalize on, and so on.
-   when you get stumped, have a quick chat to figure out what approaches are possible to work around issues, what you will do to experiment, and finally figure out how to make a decision to move ahead.
-   do experiment reports; if a few people on your team worked on something tricky, have them report back and share their experience to other people.

You'll also want to ask people for help online. Answers might be slow, but they can eventually come. StackOverflow is alright, but has never really managed to foster a community for Erlang. More interactive areas ([slack](https://erlef.org/slack-invite/erlanger), IRC, [mailing lists](https://erlang.org/mailman/listinfo/erlang-questions), [forums](https://elixirforum.com/c/erlang-forum/84)) tend to have longer-lasting memberships with a better way to figure out what you actually need. Remember that newcomers to a technology are always at risk of suffering from the [XY problem](https://en.wikipedia.org/wiki/XY_problem), and so will your team.

If you do have issues that you have trouble moving past, or feel you need a pair of more expert eyes to make sure you're going the right way, another option is possibly to hire a consultant or a consulting company for a week or two to answer your biggest question and help fix your most glaring issues, which might be less trouble than finding and hiring an expert.


## To Remote or Not To Remote {#to-remote-or-not-to-remote}

The problem with Erlang and its community is that there are always more people looking for work than there are people hiring, and at the same time people hiring who can't find employees. This is essentially due to the fact that the geographical distribution of Erlang developers and Erlang companies does not necessarily align. The people who want Erlang jobs and the people who want Erlang developers don't tend to be at the same place at the same time.

Going for remote workers means that finding an expert or adept Erlang engineers will be a lot easier, and they will be able to tap into their own network to bring more on board. It does however come with change in habits, which following the 2020 global COVID-19 pandemic, might turn out to be something most tech companies will have tried for a while.

And so the final choice on this front will come from the preferences you have to run a team; direct availability might be something to push you towards remote workers to add expertise to your team more rapidly; most places in the world don't have a ready supply of Erlang experts waiting to work for them in their local market. There can be huge time and cost savings in getting someone out-of-market to help you quickly, particularly when comparing the alternative of doing everything from scratch and developing your own in-home expertise without direct help.

Such savings would need to be done by potentially changing the way your team works and communicates. If you currently do not do remote or distributed work and your expert would be the only remote person, this also represents a challenge on your expert; being the only remote person on a team can be particularly isolating, and keeping them happy may turn complicated.


## What Next {#what-next}

So we've covered broad strokes of how to get going and how to structure things when you start with the help of an expert, or without their help. In all cases, focus on learning, training, and growing your team. It's a healthy pattern, and it is healthy whether you are remote or all on-site.

With these broad lines covered, we can start exploring the processes and practices that you'll want to put in place regardless of which approach you have taken.
