#!/bin/bash

cd "$(dirname "$0")"
docker build -f Dockerfile.simple --tag simple .
