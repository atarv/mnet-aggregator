# https://www.haskelltutorials.com/haskell-aws-lambda/compiling-haskell-runtime-in-docker.html#docker-solution
# NOTE: This Docker image is required ONLY to compile the lambda-haskell
# component that is deployed to AWS Lambda. This is the reason why it is not
# being built on an ubuntu container but on an amazonlinux container. The core
# OS libraries, like glibc, etc. are sufficiently different between the two
# systems, that a binary compiled on Ubunut 18.04 cannot work when deployed to
# AWS Lambda.
FROM amazonlinux:2.0.20230504.1

SHELL ["/bin/bash", "--rcfile", "~/.profile", "-c", "-o", "pipefail"]
ARG BOOTSTRAP=mnet-aggregator:exe:bootstrap

################################
#### Switching to root user
################################

USER root

# Saving default system libraries before doing anything else
RUN du -a /lib64 /usr/lib64 | cut -f2 > /root/default-libraries

# Installing basic dependencies
RUN yum install -y \
    git-core \
    tar \
    sudo \
    xz \
    zlib \
    libffi \
    make \
    gmp-devel \
    postgresql-devel \
    libicu libicu-devel \
    libyaml libyaml-devel \
    libtinfo libtinfo-devel \
    ncurses-term ncurses-devel \
 && yum clean all

RUN yum groupinstall -y "Development Tools" "Development Libraries"

# Installing Haskell Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

ARG STACK_RESOLVER=nightly-2022-02-07
# Setting up GHC
RUN stack setup --resolver=${STACK_RESOLVER} && stack update

RUN mkdir /root/lambda-function

COPY stack.yaml stack.yaml.lock mnet-aggregator.cabal /root/lambda-function/
WORKDIR /root/lambda-function

# Make cached layer from dependencies so subsequent builds where only source
# files change are faster
RUN stack build --resolver=${STACK_RESOLVER} --dependencies-only ${BOOTSTRAP} 

WORKDIR /tmp 
RUN git clone https://github.com/saurabhnanda/aws-lambda-packager.git

WORKDIR /tmp/aws-lambda-packager
# ARG PACKAGER_COMMIT_SHA=d3312736a38f7b93f87c313a8fb9c0798938b403
ARG PACKAGER_COMMIT_SHA=0efa19d99794493a5c7a7234975377e148d04875
RUN git checkout ${PACKAGER_COMMIT_SHA} && \
    stack install --resolver=${STACK_RESOLVER}

# Copying the source code of the lambda function into the Docker container
COPY . /root/lambda-function/

# Building the lambda-function and copying it to the output directory
WORKDIR /root/lambda-function
RUN stack build ${BOOTSTRAP}

ARG OUTPUT_DIR=/root/output
RUN mkdir ${OUTPUT_DIR} && \
    mkdir ${OUTPUT_DIR}/lib

RUN cp "$(stack path --local-install-root)/bin/bootstrap" ${OUTPUT_DIR}/bootstrap

# Finally, copying over all custom/extra libraries with the help of aws-lambda-packager
RUN /root/.local/bin/aws-lambda-packager copy-custom-libraries \
    -l /root/default-libraries \
    -f /root/output/bootstrap \
    -o /root/output/lib