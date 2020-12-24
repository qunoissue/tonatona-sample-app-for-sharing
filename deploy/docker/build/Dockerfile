FROM ubuntu:20.04

ENV LANG C.UTF-8
ENV DEBIAN_FRONTEND=noninteractive

# Install required packages.
RUN apt-get update
RUN apt-get upgrade -y --assume-yes
# Install packages for stack and ghc.
RUN apt-get install -y --assume-yes curl git make xz-utils gcc libgmp-dev zlib1g-dev
# Install convenience utilities, like tree, ping, and vim.
RUN apt-get install -y --assume-yes tree iputils-ping vim-nox

# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*

# Install stack to /opt/stack/bin.
RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'
ENV PATH "$PATH:/opt/stack/bin"

# # Create ~/.ssh/
# RUN mkdir -p ~/.ssh/
# COPY .ssh /root/.ssh/

# Install packages needed for libraries used by our app.
# https://github.com/bos/double-conversion/issues/11
RUN ln -s /usr/lib/x86_64-linux-gnu/libstdc++.so.6 /usr/lib/libstdc++.so
RUN apt-get update
RUN apt-get install -y --assume-yes g++
# Install packages for postgresql
RUN apt-get install -y --assume-yes libpq-dev
# Install packages for HsOpenSSL
RUN apt-get install -y --assume-yes libssl-dev libcrypto++-dev

# Set the working directory as shared dir.
RUN mkdir -p /opt/app
WORKDIR /opt/app

ENTRYPOINT ["/bin/sh", "-c" , "cp /hosts/deploy/docker/build.sh . && bash -x ./build.sh"]
