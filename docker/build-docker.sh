#!/usr/bin/env bash

set -eux

cd "$(dirname "${BASH_SOURCE[0]}")/.."

rm -rf docker/artifacts
mkdir -p docker/artifacts/app/yesodweb.com docker/artifacts/app/webapps docker/artifacts/bin
stack install --local-bin-path docker/artifacts/bin
cp -r sites/yesodweb.com/config sites/yesodweb.com/static docker/artifacts/app/yesodweb.com
cp -r webapps/config docker/artifacts/app/webapps
docker build --tag snoyberg/snoyman-webapps docker
