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
# any nonzero value for noninteractive installation
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
# any nonzero value to only install ghcup itself
ENV BOOTSTRAP_HASKELL_MINIMAL=1
# download and installation of ghcup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
# Add ghcup directory to the path
ENV PATH="${UHOME}/.ghcup/bin:${PATH}"
# install ghc, cabal, and hls
RUN ghcup install ghc 9.0.2 --set
RUN ghcup install cabal 3.6.2.0

# Install libraries and test runner
COPY --chown=${UNAME} testrunner/ ${UHOME}
RUN cabal update \
 && cabal v2-install --enable-executable-dynamic --lib QuickCheck xml \
 && cabal v2-install --enable-executable-dynamic \
 && rm ${UHOME}/.cabal/packages/hackage.haskell.org/*-index.* \
 && rm -r ${UHOME}/.ghcup/ghc/*/lib/ghc-*/Cabal-* \
 && find ${UHOME}/.cabal -type f -name '*.a' -exec rm {} + \
 && find ${UHOME}/.ghcup/ghc -type f -name '*.a' -exec rm {} + \
 && find ${UHOME}/.ghcup/ghc -type f -name '*.hi' -exec rm {} + \
 && find ${UHOME}/.ghcup/ghc -name '*-prof' -exec rm -r {} +

# Copy user code
COPY --chown=${UNAME} user_code/ ${UHOME}

# Remove write permission on truncate_output.sh
RUN chmod u-w truncate_output.sh

# Run
RUN mkdir ${UHOME}/output
CMD ( ~/.cabal/bin/TestRunner | tee output/stdout.txt ) 3>&1 1>&2 2>&3 | tee output/stderr.txt \
 && ./truncate_output.sh
 