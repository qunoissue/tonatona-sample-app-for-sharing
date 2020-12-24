# /bin/bash

set -Cue

docker build -t sample-build $(pwd)/deploy/docker/build
mkdir -p dist
docker run \
    --rm \
    --volume $(pwd):/hosts \
    --volume ~/.docker/uzuz/.stack:/root/.stack \
    --volume $(pwd)/.docker/.stack-work:/opt/app/.stack-work \
    --name sample-app \
    sample-build
