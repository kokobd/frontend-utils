#!/usr/bin/env bash

set -e
set -v

# apt-utils
apt-get update
apt-get install -y apt-utils

# curl
apt-get install -y curl

# libgmp-dev
apt-get install -y libgmp-dev

# node js 11
curl -sL https://deb.nodesource.com/setup_11.x | bash -
apt-get install -y nodejs
npm i -g sass

# stack
mkdir -p ~/.local/bin
curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

# add-apt-repository
apt-get install -y software-properties-common

# ghc
add-apt-repository -y ppa:hvr/ghc
apt-get update
apt-get install -y ghc-8.4.4 cabal-install-2.4

# ghcjs
add-apt-repository -y ppa:hvr/ghcjs
apt-get update
apt-get install -y ghcjs-8.4

# zlib
apt-get install -y zlib1g-dev

# alex, happy
stack install alex happy