FROM circleci/node
LABEL com.circleci.preserve-entrypoint=true

USER root

RUN apt-get install -f python-pygments && \
    mkdir -p /opt && \
    curl -L https://github.com/gohugoio/hugo/releases/download/v0.56.3/hugo_extended_0.56.3_Linux-64bit.tar.gz -o /opt/hugo.tar.gz && \
    tar -xvf /opt/hugo.tar.gz -C /opt && \
    npm install netlify-cli -g --unsafe-perm

ENTRYPOINT ["/bin/sh"]
