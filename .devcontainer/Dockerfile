FROM debian:stable-slim as build

# Install dependencies *You don't need all of them
RUN apt-get update -y \
    && apt-get upgrade -y \
    && apt-get install -y git jq bc make automake libnuma-dev \
    && apt-get install -y rsync htop curl build-essential \
    && apt-get install -y pkg-config libffi-dev libgmp-dev \
    && apt-get install -y libssl-dev libtinfo-dev libsystemd-dev \
    && apt-get install -y zlib1g-dev make g++ wget libncursesw5 libtool autoconf \
    && apt-get clean

# Install ghcup
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"

# Add ghcup to PATH
ENV PATH=${PATH}:/root/.local/bin
ENV PATH=${PATH}:/root/.ghcup/bin

# Install cabal
RUN bash -c "ghcup upgrade"
RUN bash -c "ghcup install cabal ${CABAL_VERSION}"
RUN bash -c "ghcup set cabal ${CABAL_VERSION}"

# Install GHC
RUN bash -c "ghcup install ghc ${GHC_VERSION}"
RUN bash -c "ghcup set ghc ${GHC_VERSION}"

# Update Path to include Cabal and GHC exports
RUN bash -c "echo PATH="$HOME/.local/bin:$PATH" >> $HOME/.bashrc"
RUN bash -c "echo export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" >> $HOME/.bashrc"
RUN bash -c "source $HOME/.bashrc"

# Update cabal
RUN bash -c "cabal update"

RUN bash -c "ghcup install hls"