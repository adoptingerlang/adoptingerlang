+++
title = "Appendix 4: Systemd"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:10+00:00
draft = true
[menu]
  [menu.main]
    identifier = "appendix-4-systemd"
    weight = 6001
+++

## Systemd Unit {#systemd-unit}

```shell
[Unit]
Description=Service Discovery Runner
After=network.target

[Service]
WorkingDirectory=/opt/service_discovery
EnvironmentFile=/etc/default/service_discovery.env
ExecStart=/opt/service_discovery/bin/service_discovery foreground
Restart=on-failure
Environment=RELX_OUT_FILE_PATH=/tmp/
Environment=COOKIE=service_discovery_cookie

[Install]
WantedBy=multi-user.target
```

As of OTP 19.3 a `SIGTERM` signal causes the OTP VM to gracefully shutdown.

```shell
journalctl service_discovery
```
