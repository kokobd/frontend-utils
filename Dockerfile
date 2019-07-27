FROM ubuntu:18.04
WORKDIR /frontend-utils
COPY . /frontend-utils
ENV PATH=${PATH}:/opt/ghcjs/8.4/bin:/opt/ghc/bin:/root/.local/bin
RUN bin/install_tools.sh
RUN stack build --only-dependencies
RUN cabal v2-update && cabal v2-build --only-dependencies --project-file=cabal-prod.project all