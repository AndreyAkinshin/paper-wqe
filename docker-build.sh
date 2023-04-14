#!/bin/bash

set -eux

IMAGE=wqe

docker build -t $IMAGE .

docker run --rm -v $(pwd):/work -w /work $IMAGE ./build.sh
