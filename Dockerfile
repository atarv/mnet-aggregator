# TODO: Files don't persist between runs so a DB would be needed or something else
# https://medium.com/permutive/optimized-docker-builds-for-haskell-76a9808eb10b
# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
FROM fpco/stack-build:lts-16.2 as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

# GHC dynamically links its compilation targets to lib gmp
RUN apt-get update \
  && apt-get download libgmp10
RUN mv libgmp*.deb libgmp.deb

# Docker build should not use cached layer if any of these is modified
COPY mnet-paivystaja.cabal stack.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only

# -------------------------------------------------------------------------------------------
FROM fpco/stack-build:lts-16.2 as build

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# -------------------------------------------------------------------------------------------
# Base image for stack build so compiled artifact from previous
# stage should run
FROM ubuntu:18.04 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

# Install lib gmp
COPY --from=dependencies /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

COPY --from=build /opt/build/bin .

# Ubuntu is not utf-8 by default so this has to be run
# Set the locale https://stackoverflow.com/a/28406007
# Also ca-certificates to make HTTPS work
RUN apt-get update && apt-get -y install locales locales-all ca-certificates
# RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8     

# Configs for my app
COPY config.dhall ./config.dhall

EXPOSE 8080
CMD ["/opt/app/mnet-paivystaja-exe"]