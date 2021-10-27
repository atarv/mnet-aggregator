# Code reused from: https://medium.com/permutive/optimized-docker-builds-for-haskell-76a9808eb10b
# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
FROM fpco/stack-build:lts-18.14 as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

# GHC dynamically links its compilation targets to lib gmp
# RUN apt-get update
RUN apt-get install -y \
  ca-certificates \
  libgmp-dev

# Docker build should not use cached layer if any of these is modified
COPY mnet-aggregator.cabal stack.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only

# -------------------------------------------------------------------------------------------
FROM fpco/stack-build:lts-18.14 as build

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# -------------------------------------------------------------------------------------------
# Base image for stack build so compiled artifact from previous
# stage should run
FROM ubuntu:20.04 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

# Install lib gmp
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev

COPY --from=build /opt/build/bin .

# Ubuntu is not utf-8 by default so this has to be run to make dhall work
# Set the locale https://stackoverflow.com/a/28406007
# Also ca-certificates to make HTTPS work
RUN apt-get -y install locales locales-all ca-certificates libtinfo5
RUN ln -s /usr/lib/libtinfo.so.6 /usr/lib/libtinfo.so.5
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8     

# Copy configurations for this app
COPY config.dhall ./config.dhall

EXPOSE 8080
CMD ["/opt/app/mnet-aggregator-exe"]
