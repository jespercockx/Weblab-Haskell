FROM ubuntu:jammy

# Switch to bash
SHELL ["/bin/bash", "-c"]

# Install packages
RUN apt-get update && apt-get install -y \
      build-essential \
      curl \
      ca-certificates \
      libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 \
      libtinfo5 \
    --no-install-recommends \
 && rm -r /var/lib/apt/lists/*

# Create student user
ARG GNAME=student
ARG GID=1000
ARG UNAME=student
ARG UID=1000
ARG UHOME=/user_code
RUN set -o errexit -o nounset
RUN addgroup --gid ${GID} "${GNAME}"
RUN adduser --home "${UHOME}" --disabled-password --uid ${UID} --ingroup "${GNAME}" "${UNAME}" --gecos ""

# Switch to student user
WORKDIR ${UHOME}
ENV HOME ${UHOME}
USER ${UNAME}

# Install GHC, Cabal and HLS via GHCUP
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_MINIMAL=1
ENV BOOTSTRAP_HASKELL_NO_UPGRADE=1
# download and installation of ghcup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
# Add ghcup directory to the path
ENV PATH="${UHOME}/.ghcup/bin:${PATH}"
# install ghc and hls
RUN ghcup install ghc 9.0.2 --set \
 && rm -rf ~/.ghcup/ghc/*/share/doc \
 && find ~/.ghcup/ghc -type f -name '*.a' -exec rm {} + \
 && find ~/.ghcup/ghc -type f -name '*.hi' -exec rm {} + \
 && find ~/.ghcup/ghc -type f -name '*-prof' -exec rm {} + \
 && ghcup gc
RUN ghcup install hls 1.8.0 \
# && rm -rf ~/.ghcup/hls/*/lib \
 && find ~/.ghcup/hls/*/bin -type f ! -name 'haskell-language-server-9.0.2' -exec rm {} + \
 && ghcup gc

# Copy user code
COPY --chown=${UNAME} user_code/ ${UHOME}

# Run
CMD haskell-language-server-9.0.2 lsp
