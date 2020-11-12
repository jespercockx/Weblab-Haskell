FROM haskell:8.10

WORKDIR /home/

# Install packages
RUN apt-get update && apt-get install -y \
      bash \
      libtinfo5 \
    --no-install-recommends \
 && rm -r /var/lib/apt/lists/*

# Switch to bash
SHELL ["/bin/bash", "-c"]

# Create student user and switch to it
ARG GNAME=student
ARG GID=1000
ARG UNAME=student
ARG UID=1000
ARG UHOME=/user_code
RUN set -o errexit -o nounset \
 && addgroup --gid ${GID} "${GNAME}" \
 && adduser --home "${UHOME}" --disabled-password --uid ${UID} --ingroup "${GNAME}" "${UNAME}"
USER ${UNAME}
WORKDIR ${UHOME}

# Install libraries and test runner
# Do this in one go to avoid creation of large intermediate images
# that contain the whole Cabal index
COPY --chown=${UNAME} testrunner/ ${UHOME}
RUN cabal new-update \
 && cabal v2-install --lib QuickCheck xml \
 && cabal v2-install \
 && rm .cabal/packages/hackage.haskell.org/*-index.*

# Copy user code
COPY --chown=${UNAME} user_code/ ${UHOME}
# Remove write permission on truncate_output.sh
RUN chmod u-w truncate_output.sh

# Run
ENV HOME ${UHOME}
RUN mkdir ${UHOME}/output
CMD ( ~/.cabal/bin/TestRunner | tee output/stdout.txt ) 3>&1 1>&2 2>&3 | tee output/stderr.txt \
 && ./truncate_output.sh
 