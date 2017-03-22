FROM ubuntu:latest
# :latest is necessary, see
# https://circleci.com/docs/docker/#caching-docker-layers

# heroku doesn't work with images based on fpco/stack-run, because of pid1 being
# used at entry point

RUN apt-get update && apt-get upgrade -y
RUN apt-get install libpq-dev libgmp10 gawk -y
RUN apt-get install curl -y

# mkdir -p out && stack install --local-bin-path out to build
ADD out /app
# ADD ci/run-in-heroku.sh /app

WORKDIR /app

CMD ["./bs"]

# ENTRYPOINT ["/bin/echo"]
# CMD ["hello"]