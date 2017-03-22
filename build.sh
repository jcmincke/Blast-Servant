#!/bin/bash

#    - "~/.stack"
#    - "~/kubit/.stack-work"
#    - "~/docker"

stack build examples


mkdir -p out
stack install --local-bin-path out
docker build --rm=false -t jcmincke/blastit:latest .
docker save jcmincke/blastit:latest > ~/docker/image.tar

echo $dockerpwd
docker login --username=jcmincke --email=jeanchristophe.mincke@gmail.com --password=$dockerpwd
echo "logged into docker"
echo "Pushing into docker"
docker push jcmincke/blastit:latest
