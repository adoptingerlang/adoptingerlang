+++
title = "Kubernetes"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
date = 2020-02-04T10:21:00+00:00
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:09+00:00
draft = false
[menu]
  [menu.main]
    weight = 2004
    identifier = "kubernetes"
    parent = "production"
+++

Kubernetes is a container orchestration system. When a backend contains many distinct services that need to be kept running across nodes, with cross communication and scaling, an orchestration system becomes essential. And with the growing popularity of containers, using Kubernetes for orchestration has grown in popularity as well.

With containers and Kubernetes we have common methods of deployment, discovery, scaling and monitoring across all the languages and runtimes within our system. In this chapter we will go over the core components of Kubernetes and how to use them to deploy and manage the `service_discovery` service we built in the previous chapters.


## Running Kubernetes {#running-kubernetes}

In order to develop and test the Kubernetes deploy, we will need to run Kubernetes locally for testing purposes. The following sections were written with [microk8s](https://microk8s.io/) as the local test Kubernetes but should work on any of the local or cloud options for running Kubernetes. When necessary to give specific instructions around local testing, `microk8s` will be used. Instructions for Continuous Integration and deployment to production will use Google Kubernetes Engine. But the majority of the content should work easily with any of the offerings in the following two sections.


### Locally {#locally}

-   [microk8s](https://microk8s.io/): An offering from the company behind Ubuntu, Canonical, but runs on many [Linux distros](https://snapcraft.io/docs/installing-snapd), [Windows](https://multipass.run/#install) and [MacOS](https://multipass.run/#install). microk8s is simple to install and get started with, and it runs locally, not within a VM like minikube, so is less of a resource hog.
-   [minikube](https://github.com/kubernetes/minikube): The oldest and most flexible option, that is also now an official part of the Kubernetes project. `minikube` offers support for various hypervisors and has an option to run without creating a new virtual machine, but it still suggests doing so within a Linux VM.
-   [k3s](https://k3s.io/) and [k3d](https://github.com/rancher/k3d): k3s is lightweight Kubernetes from [Rancher Labs](https://rancher.com/). k3s removes legacy and non-default features to cut down on size and replaces etcd with SQLite3, making k3s a good option for CI and local testing. k3d is a helper for running k3s in Docker.
-   [kind](https://kind.sigs.k8s.io/): Kubernetes in Docker is a projected backed by a [Kubernetes SIG](https://github.com/kubernetes-sigs) for running Kubernetes in Docker originally for testing Kubernetes itself.
-   [Docker for Mac Kubernetes](https://docs.docker.com/docker-for-mac/#kubernetes): The easiest to get going with when running on MacOS.


### Production {#production}

All the big cloud providers offer managed Kubernetes clusters:

-   [Google Kubernetes Engine](https://cloud.google.com/kubernetes-engine/): Google Cloud was used for this chapter when not using microk8s, because it offers [$300 of credit when signing up as a new user](https://cloud.google.com/free/)
-   [Digital Ocean Kubernetes](https://www.digitalocean.com/products/kubernetes/)
-   [AWS Elastic Container Service for Kubernetes](https://aws.amazon.com/eks/)
-   [Azure Kubernetes Service](https://azure.microsoft.com/en-us/services/kubernetes-service/)

There are [many more options](https://kubernetes.io/docs/setup/pick-right-solution/#table-of-solutions) for deploying to the cloud or on-premise. Many factors go into play when choosing a solution, like in the case of an existing company, where are your services currently being hosted. Luckily with Kubernetes your deployment will not be locked in to any one provider.


## Deployment {#deployment}

In Kubernetes, containers are part of a `Pod`. Each `Pod` has 1 or more containers, and 0 or more `init-containers` which run once to completion before the other containers are started. For an application, a higher level abstraction called a `Deployment` is used, so individual `Pods` don't have to be manually created for deploying or scaling. A `Deployment` is a declarative way to create and update the `Pods` of an application.

Each Kubernetes resource is defined in a `yaml` file containing the `apiVersion`, the `kind` of resource, `metadata`, such as a name, and the resource's specification:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: service-discovery
spec:
[...]
```

The `Deployment` spec entries we will cover here are `selector`, `replicas`, and `template`. The `selector` is a specification for which `Pods` belong to a `Deployment`. `matchLabels` means that `Pods` with the label `app` with value `service-discovery`, within the same `Namespace`, will be considered part of the `Deployment`. `replicas` declares how many instances of the `Pod` should be running.

```yaml
spec:
  selector:
    matchLabels:
      app: service-discovery
  replicas: 1
  template:
[...]
```

The `template` section is a `Pod` template and defines the specification of the `Pods` to be run by the `Deployment`:

```yaml
template:
  metadata:
    labels:
      app: service-discovery
  spec:
    shareProcessNamespace: true
    containers:
    - name: service-discovery
      image: service_discovery
      ports:
      - containerPort: 8053
        protocol: UDP
        name: dns
      - containerPort: 3000
        protocol: TCP
        name: http
      - containerPort: 8081
        protocol: TCP
        name: grpc
```

First, the `Pod` metadata sets the label to match with the `selector` from the `Deployment` spec -- in the upcoming section [Using Kustomize to Simplify Deployment](#using-kustomize-to-simplify-deployment) we will see how the need to define the `labels` twice is removed. Then, the `Pod` spec has a list of containers. In this case there is one container that exposes ports for accessing through DNS, HTTP and GRPC.

{{% admonition danger "The Zombie Killer!" %}}
<p>The issue of zombie processes (processes who exit but their PID sticks around because the parent hasn't called <code>wait()</code>) discussed in the <a href="/docs/production/docker/#running-a-container">Docker chapter</a> applies to running containers with Kubernetes as well. One way to protect against them, and the one used in the <code>service&#95;discovery</code> project is to use a shared process namespace between containers in the Pod.</p>

<p>This is done with <code>shareProcessNamespace: true</code> in the Pod spec. This setting means the process started in the container will not be PID 1. Instead, PID 1 is the Kubernetes <a href="https://github.com/kubernetes/kubernetes/tree/master/build/pause">pause container</a>. The pause container is always the parent container in a Pod but with this setting, making it PID 1 for all processes in the Pod, it is able to reap zombies processes that might result from any container in the Pod.</p>

<p>Read more about <a href="https://kubernetes.io/docs/tasks/configure-pod-container/share-process-namespace/">shared process namespaces in the Kubernetes docs</a>.</p>
{{% /admonition %}}


### Container Resources {#container-resources}

Each container spec in the `Pod` spec can include resource requests and limits for memory and CPU. Requests are used to schedule the pod. Scheduling involves choosing a node that the requested CPU and memory is available (not already requested by other Pods on the node).

```yaml
resources:
  requests:
    memory: "250Mi"
    cpu: "500m"
  limits:
    memory: "1Gi"
    cpu: "2000m"
```

Additionally, CPU requests are translated to the cgroup property `cpu.shares`. Each CPU is considered 1024 slices and a CPU request tells the kernel how many of those slices to try to give to the process. But there is no upper bound to how much a process can ultimately take when only setting the shares. For throttling a process that is using too much CPU time we need the Kubernetes resource limits.

The limits are translated to CPU bandwidth controls for the Linux kernel scheduler to enforce on the process. Bandwidth control has a period, which is some number of microseconds, and a quota, the maximum number of microseconds a process can use within a period. The period is always 100000 microseconds, so in the case of the example above with CPU limit set to `2000m` the cgroup quota set will be `200_000`, meaning 2 CPUs can be used every `100_000` microseconds.

If the limit is exceeded during a period, the kernel will throttle the process by not allowing it to run again until the next period. This is why it is important to correctly set the number of Erlang VM schedulers that are used and to limit the amount of busy waiting done by the VM.

A busy wait is a tight loop an Erlang scheduler will enter waiting for more work to do before eventually going to sleep. This tight loop burns CPU just waiting to do actual work and can lead to much worse performance because your Erlang program will be likely to get throttled by the kernel scheduler. Then when there is actual work to be done, it may happen in a period the Erlang VM gets no CPU slices at all. This will be even worse if the VM is running more schedulers than CPUs it is allocated. The CPU limit of `2000m`, which we think of as meaning 2 CPU cores, does not actually restrict the process to 2 cores. If 8 schedulers are being used and there are the same number of cores on the node, those 8 will still spread across all the cores and run in parallel. But the quota is still `200000` and more likely to be exceeded when 8 schedulers are taking time on 8 cores. Even without the busy wait, a scheduler must do work of its own and can be unnecessary overhead when trying to stay within some CPU usage constraints.

In order to disable the scheduler busy waiting we set the VM arguments `+sbwt` in `vm.args.src` as shown here:

```shell
+sbwt ${SBWT}
```

{{% admonition note "As of OTP-23" %}}
<p>With OTP-23, released in 2020, the Erlang VM is "container aware" and will s
the proper number of active schedulers automatically based on the containers
allocated resources. Before OTP-23 the <code>+S</code> argument was needed to
set the number of active schedulers equal the container's CPU limit.</p>

<p>Additionally, <code>+sbwt</code> defaults to <code>very&#95;short</code>
since OTP-23. This is an improvement but it is likely you still want to set the value to <code>none</code> when running in Kubernetes, or similar environment. But, as always is the case, be sure to benchmark to find the optimal value for your particular workload.</p>
{{% /admonition %}}


### Container Environments and ConfigMaps {#container-environments-and-configmaps}

As we've seen in the Releases chapter, runtime configuration is done through environment variable substitution in `vm.args.src` and `sys.config.src`. We therefore have to insert those variables to the environment of the container. Each container in a Kubernetes Pod can have an `env` field declaring a set of environment variables. The simplest case is explicitly providing a `name` and a `value`:

```yaml
env:
- name: LOGGER_LEVEL
  value: error
- name: SBWT
  value: none
```

This configuration would result in the environment variable `LOGGER_LEVEL` with value `error`.

There are other environment variables that must be set based on the state of the container that is running. An example of this is setting the `NODE_IP` variable based on the IP of the Pod:

```yaml
env:
- name: NODE_IP
  valueFrom:
    fieldRef:
      fieldPath: status.podIP
```

The `status.podIP` declaration under `fieldRef` will return the current Pod's IP when it is created.

The user-defined environment variables like `LOGGER_LEVEL` can be better tracked in a Kubernetes resource specifically for configuration called a `ConfigMap`. A `ConfigMap` contains key-value pairs and can be populated from files, directories of files or literal values. Here we will use a literal value to set `LOGGER_LEVEL` to `error`:

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: configmap
data:
  LOGGER_LEVEL: error
```

Then back in the `Deployment` resource the `LOGGER_LEVEL` value can be made a reference to the `ConfigMap`:

```yaml
env:
- name: LOGGER_LEVEL
  valueFrom:
    configMapKeyRef:
      name: configmap
      key: LOGGER_LEVEL
```

A shortcut for bringing in all the variables defined in a ConfigMap as environment variables for the container can be done with `envFrom`:

```yaml
envFrom:
- configMapRef:
    name: configmap
```

Now all the variables defined in the ConfigMap will be added to the container without having to individually specify each one. For more on defining ConfigMaps see the upcoming section [Using Kustomize to Simplify Deployment](#using-kustomize-to-simplify-deployment).

{{% admonition tip %}}
Updating a value in a <code>ConfigMap</code> will <b>not</b> trigger the <code>Deployments</code> that have references to the <code>ConfigMap</code> to restart their containers with the new environment variables. Instead, the way to update the containers when the configuration changes is to create a new <code>ConfigMap</code> with the necessary changes and a new name, then modify the name of the <code>ConfigMap</code> referenced by the <code>Deployment</code> under <code>configMapRef</code>. The old <code>ConfigMaps</code> will eventually be garbage collected by Kubernetes since they are not referenced anywhere and the <code>Deployment</code> will recreate its <code>Pods</code> with the new configuration.
{{% /admonition %}}


### Init Containers {#init-containers}

Our release of `service_discovery` depends on a Postgres database for storing state, to ensure the database is properly setup before running our containers we will use an [Init Container](https://kubernetes.io/docs/concepts/workloads/pods/init-containers/). Each `Init Container` is a container that runs to completion before any of the main containers in a Pod are run. The database migration tool `flyway` we are using for running migrations is used to validate the database is up to date before allowing the main containers to run:

```yaml
volumes:
- name: migrations
  emptyDir:
    medium: Memory

initContainers:
- name: flyway
  image: flyway/flyway:9.22
  name: flyway-validate
  args:
  - "-url=jdbc:postgresql://$(POSTGRES_SERVICE):5432/$(POSTGRES_DB)"
  - "-user=$(POSTGRES_USER)"
  - "-password=$(POSTGRES_PASSWORD)"
  - "-connectRetries=60"
  - "-skipCheckForUpdate"
  - validate
  volumeMounts:
  - name: migrations
    mountPath: /flyway/sql
    env:
    - name: POSTGRES_SERVICE
      value: POSTGRES_SERVICE
    - name: POSTGRES_DB
      value: POSTGRES_DB
    - name: POSTGRES_USER
      value: POSTGRES_USER
    - name: POSTGRES_PASSWORD
      value: POSTGRES_PASSWORD

- name: service-discovery-sql
  image: service_discovery
  command: ["/bin/sh"]
  args: ["-c", "cp /opt/service_discovery/sql/* /flyway/sql"]
  volumeMounts:
    - name: migrations
      mountPath: /flyway/sql
```

The volume `migrations` exists only in memory and is used to copy the current SQL migration files from the release image to a shared directory that the `flyway` container will use to run validation. The `emptyDir` type of volume with `medium` `memory` is used because we are not using this volume to persist any data, only to share between two `Init Containers`. If `medium: Memory` was not included, the `emptyDir` is created on the host file system but it is still empty initially for each new `Pod`, and is removed when the `Pod` is deleted.


### Readiness, Liveness and Startup Probes {#readiness-liveness-and-startup-probes}

Each container in a `Pod` can define a `readiness`, a `liveness`, and a `startup` probe.
Probes are defined either as a command to execute in the container, as an HTTP
`GET` request made to a given port and path, or as an attempt to open a TCP
connection to a specific port.

While kubernetes will restart any failed container, there are some times where a
process becomes unresponsive, such as deadlocks. A `livenessProbe` can be
used for such cases, where stopping to respond prompts a restart. For this
reason if a `livenessProbe` is included it should be kept extremely simple. It is
more often than not safer to leave out the `livenessProbe` entirely and rely on
the node crashes, metric based alerting and auto-scaling.

One part of auto-scaling is based on the `readinessProbe`. The `readinessProbe`
tells Kubernetes the `Pod` is ready to receive traffic. When a `Pod` becomes `unready`
because a container is failing the `readinessProbe`, it is removed from the
`Service` resulting in traffic being directed at the remaining `Pods`, increasing
their load, then a new `Pod` will be started.

Not using the `livenessProbe` for this purpose means the containers are still
available, since they aren't killed, and the reason for failing the check can be
examined. This may mean attaching a shell if it isn't completely frozen or
forcing a crash dump to be written. In a very resource constrained environment
you may prefer to have the `livenessProbe` in order to have deadlocked containers
and their `Pods` cleaned up right away, instead of waiting for manual intervention
and the auto-scaler to scale down after load falls.

Another use for the `readinessProbe` is if the application has to periodically do work
where you don't want to be handling requests, or have to do some sort of
maintenance, it can return a `503` status response for readiness causing it to be
removed from the `Service` backends.

In the `Pod` configuration nothing special is needed to have a graceful shutdown,
whether it is during a deploy, `livenessProbe` failure or scale down. Erlang will
call `init:stop()` when `SIGTERM` is sent by Kubernetes to signal shutdown. As
discussed in [Releases]({{< relref "releases" >}}), each Applications will be stopped in the reverse they
were started and each Application's supervision tree terminates children in
reverse order.

By default the process can wait up to 30 seconds (configurable with the
`terminationGracePeriodSeconds` `PodSpec` option) before Kubernetes sends a `SIGKILL`
signal that will force the process to terminate.

An important note about deployments and `readinessProbe` is the behaviour if a
`readinessProbe` never passes. During deployment if the `Pod` never passes its
`readinessProbe` then, depending on the deployment strategy, `Pods` from the
previous version of the deployment that are `Ready` may remain until they do. This
is covered more in the follow section [Rolling Deployments](#rolling-deployments).

In the `service_discovery` project only `readinessProbe` is defined for the purpose
of removing the `Pod` from `Service` backends without killing the container:

```yaml
readinessProbe:
  httpGet:
    path: /ready
    port: http
  initialDelaySeconds: 0
  periodSeconds: 10
```

With this configuration we expect the initial check to fail at least once
because the actual startup time before the `service_discovery_http` Application
has bound to a port to listen for HTTP requests is more than `0` seconds. This
will result in a `connection refused` error handled by the `Probe`:

```nil
service-disc… │ [event: pod service-discovery-dev/service-discovery-dev-645659d699-plxfw] Readiness probe failed: Get "http://10.244.0.114:3000/ready": dial tcp 10.244.0.114:3000: connect: connection refused
```

Despite failing and `periodSeconds` being `10` it likely won't take 10 seconds for
the `Pod` to be ready. With an `initialDelaySeconds` of `0` the `readinessProbe` is
checked immediately during startup and does not wait the configured
`periodSeconds` to check a second time. Because the service does little on startup
it will pass the second check. If it doesn't it will then wait the `periodSeconds`
until it is checked again and the `Pod` becomes ready.

To be as simple as possible, the `readinessProbe` is defined in `service_discovery_http` as:

```erlang
handle('GET', [<<"ready">>], _Req) ->
    {ok, [], <<>>};
```

Meaning it matches on a `GET` request to `/ready` and returns a `200` response
immediately. There are no additional checks of functionality of the service
except that it is able to receive and respond to HTTP requests.


### Rolling Deployments {#rolling-deployments}

Kubernetes `Deployment` resources have a configurable [strategy](https://kubernetes.io/docs/concepts/workloads/controllers/deployment/#strategy) for what happens when a new version of an image is deployed. The `service_discovery` `Deployment` uses a rolling update strategy. An alternative strategy is `Recreate` which means Kubernetes first terminates all `Pods` in the `Deployment`, and then starts the new `Pods`. For the `RollingUpdate` strategy the Kubernetes `Deployment` controller will bring up new `Pods` and then terminate the old. There are two configuration variables to tell the controller how many `Pods` above the desired amount are allowed (`maxSurge`) and how many are allowed to be unavailable during the deploy (`maxUnavailable`).

```yaml
strategy:
  type: RollingUpdate
  rollingUpdate:
    maxUnavailable: 0
    maxSurge: 25%
```

`service_discovery` is configured with a `maxUnavailable` of `0` and `maxSurge` of `25%`. This means that if it has been scaled up to four replicas then during deployment one new `Pod` will be started, when the `readinessProbe` for that `Pod` is passing one of the old `Pods` will begin termination and a new `Pod` will be started. This process will continue four times.


## Service {#service}

A `Service` is a resource for defining how to expose an application on the network. Each `Pod` has its own IP address and can expose ports. A `Service` provides a single IP address (the `Service's` `ClusterIP`) and can map a port on that IP to the exposed port for each `Pod` of the application. In the following resource definition a `Service` named `service-discovery` is created with a selector of `app: service-discovery` to match the `Pods` created in the previous section:

```yaml
kind: Service
apiVersion: v1
metadata:
  name: service-discovery
spec:
  selector:
    app: service-discovery
  ports:
  - name: dns
    protocol: UDP
    port: 8053
    targetPort: dns
  - name: http
    protocol: TCP
    port: 3000
    targetPort: http
  - name: grpc
    protocol: TCP
    port: 8081
    targetPort: grpc
```

The `Deployment's` container exposed three ports and gave each one a name, `dns`, `http` and `grpc`. The `Service` uses those names as the `targetPort` for each port it exposes. With this resource applied to a Kubernetes cluster there will be an IP listening on those three ports that will proxy to one of the running `Pods`.

A proxy (`kube-proxy`) is used to route traffic to `Pods` instead of adding each `Pod` IP to a DNS record because of the rate of change for what `Pods` are actually running for a `Service`. If DNS were used it would require a low or zero time to live (TTL) and depend on the client to fully respect the TTL value. Having every request be routed through a proxy means as long as the proxy's data has been updated the right set of `Pod's` will be routed to.

Kubernetes-aware DNS services, like [CoreDNS](https://coredns.io/), will watch for new `Services` and create DNS records `[service-name].[namespace]` for each service. When using named ports, like the three ports in the `service-discovery` `Service`, there are also SRV records created of the form `_[name]._[protocol].[service-name].[namespace]`. Querying the DNS service for the SRV record, such as `_http._tcp.service-discovery.default` will return the `Service` DNS name `service-discovery.default` and port 3000.

In a later section on clustering we will see how to use a `Headless Service` (a `Service` with `ClusterIP` set to `None`) resource and named ports for connecting Erlang nodes over distributed Erlang without the [Erlang Port Mapper Daemon (epmd)](http://erlang.org/doc/man/epmd.html).


## Using Kustomize to Simplify Deployment {#using-kustomize-to-simplify-deployment}

Now that we have talked about the two main Kubernetes resources used for our application we can go into how we would actually write and deploy these resources. The reason the YAML shown in the previous sections for each resource is not used for deployment is that it is static, at times repetitive (like having to define the same set of labels in a `Deployment` twice) and requires manually modifying or having duplicates of the resources for deploying to different environments which require different configuration. We need something that makes it easy and clear to do changes like updating the image used in a container, using a different name or namespace for resources depending on the environment or using different values for a `ConfigMap` depending on the environment.

There are a few open source options for working with Kubernetes resources. The two most popular are [Helm](https://helm.sh/) and [Kustomize](https://kustomize.io/). Both are part of the Kubernetes project but have very different solutions to the problem. Helm uses [Go templates](https://golang.org/pkg/text/template/) for writing and rendering YAML. Some, this author included, find templating YAML to be overly complex, error prone and just plain annoying. Luckily, the other solution, `Kustomize,` does not rely on templates.

{{% admonition note "Helm 3" %}}
<p>Even if Helm is not used for deployment of your project itself it can still be useful for deployment of dependencies. There are a lot of <a href="https://github.com/helm/charts">Helm Charts</a> available and is likely how you would want to offer your project to the outside world if it was not simply for internal usage.</p>

<p>There were a number of issues with Helm aside from the templating that we won't get into here, except to say that they are being worked out in the latest major release, <a href="https://v3.helm.sh/">Helm v3</a>, that had its first stable release in November 2019. So I would still suggest taking a look at the latest offering from Helm for yourself.</p>
{{% /admonition %}}

[Kustomize](https://kustomize.io/) is a `kubectl` built-in tool (as of v1.14.0) that provides a template-free way to customize Kubernetes YAML resources. We will use it to create different configurations for various environments, starting with a development environment, `dev`, which will then be used in the next section, [Tilt for Local Development](#tilt-for-local-development), to run `service_discovery` locally.

The setup is a base configuration with overlays for different environments. Overlays can add additional resources and make modifications to the resources from the base layer. The directory layout for the base configuration and two overlays is:

```shell
$ tree deployment
deployment
├── base
│   ├── default.env
│   ├── deployment.yaml
│   ├── init_validation.yaml
│   ├── kustomization.yaml
│   ├── namespace.yaml
│   └── service.yaml
├── overlays
│   ├── dev
│   │   ├── dev.env
│   │   └── kustomization.yaml
│   └── stage
│       ├── kustomization.yaml
│       └── stage.env
└── postgres
    ├── flyway-job.yaml
    ├── kustomization.yaml
    ├── pgdata-persistentvolumeclaim.yaml
    ├── postgres-deployment.yaml
    └── postgres-service.yaml
```

The base `kustomization.yaml` includes the main resources of the `service_discovery` project, a `Namespace`, `Deployment` and `Service`:

```yaml
apiVersion: kustomize.config.k8s.io/v1beta1
kind: Kustomization
namespace: service-discovery
commonLabels:
  app: service-discovery
resources:
- namespace.yaml
- deployment.yaml
- service.yaml
```

The labels under `commonLabels` will be added to each resource and simplifies the `Deployment` configuration from the earlier section by being able to remove both the `labels` entry and the `selector`. So now the `Deployment` resource, found in `deployment/base/deployment.yaml` for `service_discovery` looks like:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: service-discovery
spec:
  replicas: 1
  template:
    spec:
      containers:
      - name: service-discovery
        image: service_discovery
[...]
```

Kustomize will insert the labels under the `metadata` when rendering the resources, as well as inserting the same labels under the `spec`'s `selector` field `matchLabels` automatically. To see what Kustomize generates run `kubectl kustomize` on the base and it will print the resources to stdout:

```yaml
$ kubectl kustomize deployment/base
[...]
apiVersion: apps/v1
kind: Deployment
metadata:
  labels:
    app: service-discovery
  name: service-discovery
  namespace: service-discovery
spec:
  replicas: 1
  selector:
    matchLabels:
      app: service-discovery
  template:
    metadata:
      labels:
        app: service-discovery
[...]
```

Another feature for improving creation of `Deployments` is generation of `ConfigMaps`. In the previous section on environment variables and ConfigMaps we ended with a `Deployment` that included environment variables from the data in a `ConfigMap`:

```yaml
envFrom:
- configMapRef:
    name: configmap
```

With Kustomize's `configMapGenerator` we can declare that the `ConfigMap` be generated from any file with lines like `VarName=VarValue`:

```yaml
configMapGenerator:
- name: configmap
  envs:
  - dev.env
```

The content of `dev.env` is:

```shell
LOGGER_LEVEL=debug
```

The resulting ConfigMap seen in the output for `kubectl kustomize deployment/overlays/dev` is:

```yaml
apiVersion: v1
data:
  LOGGER_LEVEL: debug
kind: ConfigMap
metadata:
  labels:
    overlay: dev
  name: configmap-dev-2hfc445577
  namespace: service-discovery-dev
```

Note that the name is no longer simply `configmap` but instead `configmap-dev-2hfc445577`. Kustomize will create a new `ConfigMap` with a different name if the content changes and it will update any reference to the `ConfigMap`. Updating the reference to the `ConfigMap` in the `Deployment` spec ensures that `Pods` are restarted when their configuration changes. Simply modifying a `ConfigMap` would not otherwise result in an update to the running `Pods`. So in the Kustomize output the `Deployment` will have:

```yaml
- envFrom:
  - configMapRef:
      name: configmap-dev-2hfc445577
```

The same is done for `Secrets`.

To apply the resources generated by Kustomize in one step the `-k` option can be passed to `kubectl apply`:

```shell
$ kubectl apply -k deployment/overlays/dev
```

This command will generate the resources based on the `dev` overlay and apply them to the Kubernetes cluster.


## Database Migrations {#database-migrations}


### Jobs {#jobs}

A Kubernetes `Job` creates one or more `Pods` and runs them until the specified number of them complete successfully. In the case of database migrations for `service_discovery` the `Job` is one `Pod` with a container running [Flyway](https://flywaydb.org/) sharing a `Volume` with an `initContainer` which copies over the `SQL` files from the image of `service_discovery` being deployed. Being `initContainers` means they must all run to completion and succeed (finish with a status code of 0) before the main containers of the `Pod` are started. So before `flyway migrate` can be run by the main container of the `Job`'s `Pod` the container with the migrations, it is actually the same image with the full `service_discovery` release we use to run the `Deployment`, must have successfully copied the migration to the shared `Volume` under directory `/flyway/sql`:

```yaml
apiVersion: batch/v1
kind: Job
metadata:
  labels:
    service: flyway
  name: flyway
spec:
  ttlSecondsAfterFinished: 0
  template:
    metadata:
      labels:
        service: flyway
    spec:
      restartPolicy: OnFailure
      volumes:
      - name: migrations
        emptyDir:
          medium: Memory

      containers:
      - args:
        - "-url=jdbc:postgresql://$(POSTGRES_SERVICE):5432/$(POSTGRES_DB)"
        - -user=$(POSTGRES_USER)
        - -password=$(POSTGRES_PASSWORD)
        - -connectRetries=60
        - -skipCheckForUpdate
        - migrate
        image: flyway/flyway:9.22
        name: flyway
        volumeMounts:
        - name: migrations
          mountPath: /flyway/sql
        env:
        - name: POSTGRES_SERVICE
          value: POSTGRES_SERVICE
        - name: POSTGRES_DB
          value: POSTGRES_DB
        - name: POSTGRES_USER
          value: POSTGRES_USER
        - name: POSTGRES_PASSWORD
          value: POSTGRES_PASSWORD

      initContainers:
      - name: service-discovery-sql
        image: service_discovery
        command: ["/bin/sh"]
        args: ["-c", "cp /opt/service_discovery/sql/* /flyway/sql"]
        volumeMounts:
          - name: migrations
            mountPath: /flyway/sql
```

The trouble with using a `Job` for migrations is applying a set of Kubernetes YAML resources can not update the image of a completed `Job` and cause it to be rerun. Applying a `Job` of the same name with a different `service_discovery` image will result in an error. There are a couple options to deal with this limitation.

One option, which is still an alpha feature as of Kubernetes 1.16, is setting `ttlSecondsAfterFinished: 0` in the `Job` specification. With this setting the `Job` will be eligible for deletion immediately after completion. Then, when the next deployment applies the new Kubernetes resources a new `Job` will be created, instead of attempting to update the `Job` from the last deployment.

The `ttlSecondsAfterFinished` options is still an alpha feature and requires manually enabling. You can try the feature on Google Cloud by creating an [alpha cluster](https://cloud.google.com/kubernetes-engine/docs/concepts/alpha-clusters), but these clusters can only live for 30 days. An alternative solution is to manually (or with a script that can run in a CI pipeline) run migrations and delete the `Job` after completing a deployment.

An advantage of running the `Job` separately from the main `kubectl apply` is the deployment can be stopped if the `Job` failed.


### Validating the Migration {#validating-the-migration}

If we want to be sure our `service_discovery` container does not start until the database has been successfully migrated the `flyway` command `validate` can be used. The container running `flyway validate` will succeed once the database it is checking has run all the migrations. The resulting container setup is essentially the same as in the last section when we did the migrations, but instead of `migrate` the command is `validate` and both the `flyway` container and the container copying the migrations to the shared volume are `initContainers` in this case:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: service-discovery
spec:
  replicas: 1
  template:
    spec:
      volumes:
      - name: migrations
        emptyDir: {}
      initContainers:
      - name: service-discovery-sql
        image: service_discovery
        volumeMounts:
          - name: migrations
            mountPath: /flyway/sql
        command: ["/bin/sh"]
        args: ["-c", "cp /opt/service_discovery/sql/* /flyway/sql"]

      - image: flyway/flyway:9.22
        name: flyway-validate
        args:
        - "-url=jdbc:postgresql://$(POSTGRES_SERVICE):5432/$(POSTGRES_DB)"
        - "-user=$(POSTGRES_USER)"
        - "-password=$(POSTGRES_PASSWORD)"
        - "-connectRetries=60"
        - "-skipCheckForUpdate"
        - validate
        volumeMounts:
        - name: migrations
          mountPath: /flyway/sql
        env:
        - name: POSTGRES_SERVICE
          value: POSTGRES_SERVICE
        - name: POSTGRES_DB
          value: POSTGRES_DB
        - name: POSTGRES_USER
          value: POSTGRES_USER
        - name: POSTGRES_PASSWORD
          value: POSTGRES_PASSWORD
```

Each init container is run in order to completion before the next is started, which makes the order in this case very important. If the container which copies the migration files to `/flyway/sql` were not first in the list of `initContainers`, and required by Kubernetes to run to successful completion before the next container is started, the migrations would not be copied to the volume before `flyway` is run.


## Tilt for Local Development {#tilt-for-local-development}

[Tilt](https://tilt.dev/) is a tool for deploying and updating Docker images and Kubernetes deploys locally for development purposes. By default it will only build and deploy services against a local Kubernetes, such as `microk8s`, `minikube`, etc. to safe guard against accidentally sending your development environment into production!

The easiest way to get started with Tilt is using a registry within the local Kubernetes cluster. For `microk8s` a registry can be enabled easily, we will need DNS as well so enable that at this time as well:

```shell
$ microk8s.enable registry
$ microk8s.enable dns
```

To allow the host Docker to publish to this registry it must be added to `/etc/docker/daemon.json`:

```json
{
  [...]
  "insecure-registries" : ["localhost:32000"]
  [...]
}
```

Tilt works off of a file named `Tiltfile` at the root of the project. Stepping through the `Tiltfile` at the root of [service_discovery](https://github.com/adoptingerlang/service_discovery) it begins with:

```python
default_registry('127.0.0.1:32000')
```

`default_registry` sets where to push Docker images built by Tilt, in this case the registry enabled in `microk8s` is used. This line is not actually required because Tilt will pick up that `microk8s` with an enabled registry is being used and automatically configure itself to use that registry.

Next the build of a couple of Docker images is configured, starting with `service_discovery_sql`:

```python
custom_build(
    'service_discovery_sql',
    'docker buildx build -o type=docker --target dev_sql --tag $EXPECTED_REF .',
    ['apps/service_discovery_postgres/priv/migrations'],
    entrypoint="cp /app/sql/* /flyway/sql"
)
```

This image is built from the `Dockerfile` target `dev_sql`:

```dockerfile
FROM busybox as dev_sql

COPY apps/service_discovery_postgres/priv/migrations/ /app/sql/
```

It only contains the SQL migration files and uses `busybox` so that the `entrypoint` can use `cp`. The third argument to `custom_build`, `['apps/service_discovery_postgres/priv/migrations']`, tells Tilt to rebuild the image if any file in that directory, in this case the SQL migration files, changes. We will see how this image is used when we get to the kustomize deployment later in the `Tiltfile`.

Notice the function [custom_build](https://docs.tilt.dev/custom_build.html) is used. Tilt offers a simpler function `docker_build` for building Docker images if that suites your needs. In the case of `service_discovery` we have specific `targets` in the `Dockerfile` to use and wanted to ensure the use of `buildx`.

The next image built is `service_discovery`, the main image used by the `Deployment`:

```python
custom_build(
    'service_discovery',
    'docker buildx build -o type=docker --target dev_release --tag $EXPECTED_REF .',
    ['.'],
    live_update=[
        sync('rebar.config', '/app/src/rebar.config'),
        sync('apps', '/app/src/apps'),
        run('rebar3 as tilt compile'),
        run('/app/_build/tilt/rel/service_discovery/bin/service_discovery restart')
    ],
    ignore=["rebar.lock", "apps/service_discovery_postgres/priv/migrations/"]
)
```

Note that `rebar.lock` is explicitly ignored, this is because it gets rewritten at times when it hasn't actually changed and Tilt does no comparison to verify a change has actually occurred, so would needlessly run the `live_update` instructions when Rebar3 is run locally. The migration files are also ignored because that is handled by the other Docker image.

The target is `dev_release` because we want the ability to do [live updates](https://blog.tilt.dev/2019/04/02/fast-kubernetes-development-with-live-update.html) simply by recompiling and restarting inside the running image:

```dockerfile
# image to use in tilt when running the release
FROM builder as dev_release

COPY . .
RUN rebar3 as tilt release

ENTRYPOINT ["/app/_build/tilt/rel/service_discovery/bin/service_discovery"]
CMD ["foreground"]
```

The `live_update` instructions will run in the container any time a file changes. The Rebar3 profile `tilt` uses `dev_mode` for the release building, meaning to update the compiled modules in the release requires only running `compile` and not having to rebuild the whole release -- see the [Releases Chapter](https://adoptingerlang.org/docs/production/releases/#building-a-development-release) for more details on `dev_mode` and release building -- so the update commands simply sync the `apps` directory to the running container, run `compile` and restart the release.

Finally, the Kubernetes resources to deploy are configured in the `TiltFile`, and we set a watcher to rerun it when any of the kustomize files change.

```python
k8s_yaml(kustomize('deployment/overlays/dev'))

watch_file('deployment/')
```

Tilt has built-in support for kustomize so we use `kustomize('deployment/overlays/dev')` to render the `dev` overlay and pass to `k8s_yaml` which tells Tilt what Kubernetes resources to deploy and track.

The important differences from the `base` overlay in the `dev` overlay's `kustomization.yaml` is the inclusion of the Postgres kustomize resources and a patch that is merged on the `base` resources:

```yaml
bases:
- ../../base
- ../../postgres
patchesStrategicMerge:
- flyway_job_patch.yaml
```

The `fly_job_patch.yaml` is used to configure the Flyway job to work with the Tilt setup:

```yaml
# For tilt we make an image named service_discovery_sql with the migrations.
# This patch replaces the image used in the flyway migration job to match.
apiVersion: batch/v1
kind: Job
metadata:
  labels:
    service: flyway
  name: flyway
spec:
  template:
    spec:
      initContainers:
      - name: service-discovery-sql
        image: service_discovery_sql
        volumeMounts:
          - name: migrations
            mountPath: /flyway/sql
        command: ["/bin/sh"]
        args: ["-c", "cp /app/sql/* /flyway/sql"]
```

This patch changes the image name in the `Job` resource to `service_discovery_sql`, the same name as the first `custom_build` image in the `Tiltfile`. Tilt will update the image with the latest tag, which it sets as `$EXPECTED_REF` in the environment of the Docker build command in `custom_build`, and reruns the `Job`. This way while `service_discovery` is running in the local Kubernetes if a new migration is added it will automatically be picked up and run on the database, keeping our development cluster in sync with our local development.

After running `tilt`:

```shell
$ tilt up
```

A console UI is brought up that shows the status of bringing up the difference resources that were passed to `k8s_yaml` in the `Tiltfile` and the logs associated with them:

{{< figure src="/img/tilt-console.png" >}}

Tilt will also automatically open a page in the browser showing the same information:

{{< figure src="/img/tilt-webui.png" >}}

Using `kubectl` the IP of `service_discovery` can be found:

```shell
$ kubectl get services --namespace=service-discovery-dev
NAME                    TYPE        CLUSTER-IP       EXTERNAL-IP   PORT(S)                      AGE
postgres-dev            ClusterIP   10.152.183.116   <none>        5432/TCP                     16m
service-discovery-dev   ClusterIP   10.152.183.54    <none>        8053/UDP,3000/TCP,8081/TCP   16m
```

And we can interact with the running service_discovery through `curl` and `dig` to verify it is functioning properly:

```shell
$ curl -v -XPUT http://10.152.183.54:3000/service \
    -d '{"name": "service1", "attributes": {"attr-1": "value-1"}}'
$ curl -v -XGET http://10.152.183.54:3000/services
[{"attributes":{"attr-1":"value-1"},"name":"service1"}]
```


## Clustering {#clustering}

Coming soon...


## StatefulSets {#statefulsets}

Coming soon...
