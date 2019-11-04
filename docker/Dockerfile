FROM fpco/pid1:18.04

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install -y letsencrypt git && \
    apt-get clean && \
    unset DEBIAN_FRONTEND

COPY artifacts/bin /usr/local/bin
COPY artifacts/app/ /app
