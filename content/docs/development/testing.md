+++
title = "Testing"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:09+00:00
draft = true
[menu]
  [menu.main]
    weight = 2011
    identifier = "testing"
    parent = "development"
+++

## Common Test {#common-test}


## Coverage {#coverage}


## Dialyzer {#dialyzer}

Example from `service_discovery`: adding `port_name` to the endpoints:

```erlang
-spec endpoint_from_json(unicode:unicode_binary(), map()) -> {ok, service_discovery:endpoint()} |
                                                             {error, term()}.
endpoint_from_json(ServiceName, #{<<"ip">> := IPString,
                                  <<"port">> := Port,
                                  <<"tags">> := Tags}) ->
    case inet:parse_address(binary_to_list(IPString)) of
        {ok, IP} ->
            {ok, #{service_name => ServiceName,
                   ip => IP,
                   port => Port,
                   tags => Tags}};
        {error, einval}=Error ->
            Error
    end;
endpoint_from_json(_, _) ->
    {error, bad_endpoint_json}.
```

Missing `port_name` in return results in map that does not match the \`service_discovery:endpoint()\` spec:

```erlang
apps/service_discovery_http/src/sdh_handler.erl
  42: The pattern 'ok' can never match the type {'error','bad_endpoint_json' | 'einval'}
 130: The pattern {'ok', Endpoint} can never match the type {'error','bad_endpoint_json' | 'einval'}
```


## XRef {#xref}


## Continuous Integration {#continuous-integration}


### Docker Compose {#docker-compose}


### CircleCI {#circleci}

CircleCI has a very flexible CI offering that integrates well with services like Github. One of this author's favorite features of CircleCI when working with Erlang is the built in support for serving up test artifacts after a test run. This is especially useful because Common Test will output details about test runs as HTML. Plus CircleCI supports displaying information about a test run based on JUnit structured XML which CT can output using the `surefire` hook.

The `surefire` hook can be enabled in `rebar.config` with the following `ct_opts`:

```erlang
%% generate junit xml report from test results
{ct_opts, [{ct_hooks, [cth_surefire]}]}.
```

CircleCI has no official support for Rebar3 projects, but they do host Erlang docker images which contain Rebar3 and tools necessary for interacting with CircleCI's cache and artifact store. But thanks to [CircleCI Orbs](https://circleci.com/orbs/) it is easy to get started with Rebar3 on CircleCI and take advantage of its unique features. CircleCI Orbs are reusable commands, executors and jobs that make writing workflows simpler. The [Rebar3 Orb](https://circleci.com/orbs/registry/orb/tsloughter/rebar3) takes care of caching built dependencies and moving artifacts, like CT's HTML output, with CircleCI's artifacts commands.

CircleCI runs Docker images on machines with many more CPU's than are allocated for your job. Because Erlang can not limit the number of schedulers it starts to the number of CPU's allocated to the container, Erlang will start as many schedulers as CPU's it sees on the host. This results in wasted resources because the VM is busy scheduling something like 32 schedulers when it only has access to 1 or 2 processors. To configure the number of schedulers that Erlang will start when running `rebar3` use the environment variable `ERL_FLAGS` which takes the arguments that need to be passed to `erl` in this case `"+S 2"`.

```yaml
version: 2.1

orbs:
  rebar3: tsloughter/rebar3@0.7.0

workflows:
  version: 2.1
  build_and_test:
    jobs:
    - rebar3/compile

    - rebar3/xref:
        requires:
        - rebar3/compile
    - rebar3/dialyzer:
        requires:
        - rebar3/compile
    - rebar3/ct:
        requires:
        - rebar3/compile
```

The following screenshot show's CircleCI's workflow page for a project that runs Common Test, Dialyzer and xref in parallel followed by cover which is reported to [codecov.io](https://codecov.io):

{{< figure src="/img/circleci_ct_success_workflow.png" >}}

Clicking on the `rebar3/ct` job in the workflow graph goes to the job page which has the output from the steps in the job, a test summary based on the JUnit XML and a tab with artifacts kept after the job completed:

{{< figure src="/img/circleci_ct_junit_success.png" >}}

Going to the artifacts tab we see the contents from `_build/test/logs`:

{{< figure src="/img/circleci_ct_artifacts.png" >}}

Following the link for `index.html` will open a new browser tab for a url like <https://498-59958463-gh.circle-artifacts.com/0/common_test/index.html> where you can browse all Common Test test result pages:

{{< figure src="/img/circleci_ct_html.png" >}}

Lastly, this image shows an example of the test summary, from the `cth_surefire` output, for a failed test case in a suite:

{{< figure src="/img/circleci_ct_junit_failure.png" >}}


### Microsoft's visual studio output {#microsoft-s-visual-studio-output}

<https://github.com/ferd/trx>


### CirrusCI {#cirrusci}


### Google Cloud Build {#google-cloud-build}

```yaml
steps:
- name: 'docker/compose:1.24.0'
  args: ['-f', 'docker-compose.yml', 'up', '-d']

- name: 'gcr.io/kaniko-project/executor:latest'
  args:
  - --target=releaser
  - --dockerfile=./Dockerfile.cb
  - --build-arg=BASE_IMAGE=$_BASE_IMAGE
  - --build-arg=RUNNER_IMAGE=$_RUNNER_IMAGE
  - --destination=gcr.io/$PROJECT_ID/service_discovery:tester-$BUILD_ID
  - --cache=true
  - --cache-ttl=48h

- name: 'gcr.io/cloud-builders/docker'
  args: ['run', '--network', 'workspace_sd_net', '-e', 'BUILD_ID=$BUILD_ID', '--entrypoint', 'rebar3', 'gcr.io/$PROJECT_ID/service_discovery:tester-$BUILD_ID', 'ct']
```
