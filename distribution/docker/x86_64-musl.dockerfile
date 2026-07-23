FROM alpine:3.15 AS build

RUN apk add --no-cache \
        alpine-sdk \
        autoconf \
        file \
        gcc \
        gmp \
        gmp-dev \
        libffi \
        libffi-dev \
        llvm10 \
        make \
        musl-dev \
        ncurses-dev \
        ncurses-static \
        tree \
        wget \
        zlib-dev \
        zlib-static \
        curl

RUN curl https://downloads.haskell.org/~ghcup/0.1.19.5/x86_64-linux-ghcup-0.1.19.5 -o /usr/local/bin/ghcup && chmod a+x /usr/local/bin/ghcup

# Setup GHC
RUN ghcup install ghc 9.2.8 --set
RUN ghcup install stack 2.11.1 --set

ENV PATH="${PATH}:/root/.ghcup/bin"

# FIX https://bugs.launchpad.net/ubuntu/+source/gcc-4.4/+bug/640734
# Use the next line to debug the right file source if this area starts failing in future
# RUN tree /usr/lib/gcc/x86_64-alpine-linux-musl
# @TODO is there a sure-fire way of getting this path?
WORKDIR /usr/lib/gcc/x86_64-alpine-linux-musl/10.3.1/
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o
RUN cp crtend.o crtend.o.orig
RUN cp crtendS.o crtend.o

# Install packages
WORKDIR /elm-dev
COPY elm-dev.cabal ./
COPY stack.yaml ./
COPY stack.yaml.lock ./
COPY vendor/elm-format vendor/elm-format

ENV STACKOPTS="--system-ghc --no-install-ghc --allow-different-user"
ENV GHCOPTS="-j4 +RTS -A256m -RTS -static -split-sections -optc-Os -optl=-pthread"
RUN stack $STACKOPTS build --only-dependencies --ghc-options="$GHCOPTS"

# Import source code
COPY builder builder
COPY compiler compiler
COPY reactor reactor
COPY terminal terminal
COPY LICENSE ./
COPY installers-elm-dev/npm/package.json installers-elm-dev/npm/package.json

COPY ext-common ext-common
COPY ext-debug ext-debug
COPY ext-dev ext-dev
COPY ext-optimization ext-optimization
COPY ext-sentry ext-sentry
COPY ext-trace ext-trace
COPY ext-generate ext-generate
COPY ext-watchtower ext-watchtower
COPY .git .git

# Inexplicably the first build fails, but the second succeeds
RUN (stack $STACKOPTS install --local-bin-path /elm-dev/output --ghc-options="$GHCOPTS" || true) && \
    stack $STACKOPTS install --local-bin-path /elm-dev/output --ghc-options="$GHCOPTS"

RUN cp /elm-dev/output/elm-dev ./elm-dev
RUN ./elm-dev --version-full
RUN strip elm-dev
RUN file elm-dev | grep -q "statically linked"
