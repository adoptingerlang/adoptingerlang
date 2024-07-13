+++
label = "Index"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
date = 2019-08-08T08:05:00+00:00
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:09+00:00
draft = false
title = "Production"
[menu]
  [menu.main]
    weight = 3001
    identifier = "production"
+++

<header>
  <h1>Production</h1>
  <h5>
    <strong>August 3, 2019</strong>
  </h5>
</header>

Erlang/OTP is certainly a unique language and runtime, but it is not as different as even some proponents would have you believe. In this part we will see how to build artifacts for deployment and how to operate the deployment in the same environment you would run any other service. In this part we'll build a deployable artifact (a release), create a docker image of the release and deploy to a Kubernetes cluster.

A common claim heard on forums and comment sections of popular tech news sites is, you have OTP, you don't need Kubernetes. Or the opposite, that having Kubernetes replaces OTP. Erlang does not replace Kubernetes, nor does Kubernetes replace Erlang. If an Erlang system didn't need to be monitored and restarted like any other runtime because it has supervision trees then it wouldn't come with [heart](http://erlang.org/doc/man/heart.html). Similarly, if restarting the entire program with `heart` was adequate there wouldn't be supervision trees.

[Kubernetes](https://kubernetes.io/) provides a scheduler for efficiently packing your programs, as containers, on physical or virtual machines. Using containers eases deployment by bundling all dependencies (think OpenSSL in an Erlang release's case) into a single image and isolation that protects against dependency conflicts between services running on a machine. Additionally, deployment and management becomes consistent across the services in your production environment no matter the language or runtime they are built with. When you integrate your Erlang service with common monitoring and tracing services, you will also ensure it's no longer the oddball your coworkers dread having to deal with.

However, containers and Kubernetes are not appropriate in all cases. Kubernetes can be overkill, particularly if using a hosted solution isn't an option, or your product could be an embedded device.

<div class="pagination">
  <div><a href="/docs/development/hard_to_get_right">← Prev</a></div>
  <div><a href="/docs/production/releases">Next →</a></div>
</div>
