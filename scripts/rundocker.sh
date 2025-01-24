#!/bin/bash

docker run -it --rm \
	--gpus all \
	-v /nix:/nix \
	-v $HOME:$HOME \
	-v $HOME/.cache/nix:/home/dockeruser/.cache/nix \
	--user 1000:1000 \
	--workdir $(pwd) \
	-e DISPLAY=$DISPLAY \
	-v /tmp/.X11-unix:/tmp/.X11-unix \
	-e PATH="/home/alan/.nix-profile/bin" \
	simple /bin/bash
