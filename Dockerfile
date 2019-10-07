FROM circleci/node:buster
LABEL com.circleci.preserve-entrypoint=true

USER root

RUN mkdir -p /opt && \
    curl -L https://github.com/gohugoio/hugo/releases/download/v0.58.3/hugo_extended_0.58.3_Linux-64bit.tar.gz -o /opt/hugo.tar.gz && \
    tar -zxvf /opt/hugo.tar.gz -C /opt && \
    npm install netlify-cli -g --unsafe-perm

ENTRYPOINT ["/bin/sh"]
