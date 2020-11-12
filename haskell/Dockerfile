FROM haskell:8.10

WORKDIR /home/

# Install packages
RUN apt-get update && apt-get install -y \
      software-properties-common \
      bash \
      make \
      sudo \
      libtinfo5 \
    --no-install-recommends \
 && rm -r /var/lib/apt/lists/*

# Switch to bash
SHELL ["/bin/bash", "-c"]

# Create student user
ARG GNAME=student
ARG GID=1000
ARG UNAME=student
ARG UID=1000
ARG UHOME=/user_code
RUN set -o errexit -o nounset \
 && addgroup --gid ${GID} "${GNAME}" \
 && adduser --home "${UHOME}" --disabled-password --uid ${UID} --ingroup "${GNAME}" "${UNAME}"

# Copy user code
USER ${UNAME}
COPY --chown=${UNAME} user_code/ ${UHOME}
WORKDIR ${UHOME}

# Update Cabal package database
RUN cabal new-update

# Install QuickCheck and xml libraries
RUN cabal v2-install --lib QuickCheck xml

# Install test runner
RUN cd testrunner && cabal v2-install

# Remove write permission on truncate_output.sh
RUN chmod u-w truncate_output.sh

# Run
ENV HOME ${UHOME}
RUN mkdir ${UHOME}/output
COPY --chown=${UNAME} user_code/testrunner/FormatResult.hs ${UHOME}
CMD ( ~/.cabal/bin/TestRunner | tee output/stdout.txt ) 3>&1 1>&2 2>&3 | tee output/stderr.txt \
 && ./truncate_output.sh
 