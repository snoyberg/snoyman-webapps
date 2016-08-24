#!/usr/bin/env bash

set -ex

cd $(dirname $0)
stack image container
NAME=snoyberg/snoyman-webapps:sha-$(git rev-parse HEAD)
docker tag snoyberg/snoyman-webapps:latest $NAME
docker push $NAME
kubectl set image deployments/snoyman-webapps webapps=$NAME
