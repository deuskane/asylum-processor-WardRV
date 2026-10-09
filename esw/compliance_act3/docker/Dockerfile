# Environment to regenerate the WardRV ACT3 compliance tests
#
# All the tools (toolchain, Sail, riscv-arch-test) are installed in the image
# with the pinned versions of ../Makefile : the generation (make full) works
# without network. Keep the image with docker/Makefile (make save / make load).
#
# Build context : the compliance directory (parent of this one)

# Ubuntu 24.04 (pinned for reproducibility)
FROM docker.io/library/ubuntu:24.04@sha256:f610ab94648195aa356059f5b41d6085c9d4d903c072430cdd1af7bdb646106b

# Extra apt-get options (set by docker/Makefile for rootless podman)
ARG APT_OPT=""

RUN apt-get $APT_OPT update && \
    apt-get $APT_OPT install -y --no-install-recommends \
      bsdextrautils \
      ca-certificates \
      curl \
      git \
      make && \
    apt-get $APT_OPT clean && rm -rf /var/lib/apt/lists/*

# Tools used by ../Makefile (TOOLS_DIR)
ENV TOOLS_DIR=/opt/compliance_act3

COPY Makefile $TOOLS_DIR/Makefile

# Writable by all : the container runs with the uid of the user
RUN make -C $TOOLS_DIR toolchain sail fetch && \
    chmod -R a+rwX $TOOLS_DIR

WORKDIR /workspace
