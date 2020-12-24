# /bin/bash

set -Cue

docker build -t sample-exec $(pwd)/deploy/docker/exec
docker run \
    --rm \
    --volume $(pwd):/hosts \
    sample-exec
