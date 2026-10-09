# Container engine : docker or podman
ENGINE            ?= docker
# localhost/ prefix : same image name for docker and podman (make save / make load)
IMAGE_NAME        := localhost/asylum-compliance-act3
CONTAINER_NAME    := asylum-compliance-act3-container
TAG               := latest
DOCKERFILE        := Dockerfile
# Build context : the compliance directory (Makefile and config/)
BUILD_CONTEXT     := ..
# Mount the compliance directory (parent of this one)
DOCKER_MOUNT_PATH ?= $(abspath $(CURDIR)/..)
# Parallel jobs for make full
JOBS              ?= $(shell nproc)
# Archive of the image (make save / make load)
IMAGE_ARCHIVE     ?= $(notdir $(IMAGE_NAME))-$(TAG).tar.gz

DOCKER_BUILD_OPT  :=
DOCKER_RUN_OPT    := --rm --name $(CONTAINER_NAME) -e HOME=/tmp
DOCKER_RUN_OPT    += -v "$(DOCKER_MOUNT_PATH):/workspace" -w /workspace
# The tools are in the image : the generation does not need the network
GENERATE_OPT      ?= --network none

ifeq ($(ENGINE),docker)
# Run with the current user to keep the generated files owned by the user
DOCKER_RUN_OPT    += --user $(shell id -u):$(shell id -g)
else ifeq ($(ENGINE),podman)
# Rootless podman : the root of the container is the current user, so the
# generated files are already owned by the user.
# The default storage does not work on NFS and without subuid/subgid :
# use a local storage (vfs) and ignore the chown errors (only one uid mapped).
PODMAN_STORAGE    ?= /tmp/podman-$(shell id -u)
PODMAN_OPT        ?= --root $(PODMAN_STORAGE)/root --runroot $(PODMAN_STORAGE)/run \
                     --storage-driver vfs --storage-opt vfs.ignore_chown_errors=true
# apt-get cannot drop its privileges to the _apt user with only one uid mapped
DOCKER_BUILD_OPT  += --build-arg APT_OPT="-o APT::Sandbox::User=root"
else
$(error ENGINE must be docker or podman (ENGINE=$(ENGINE)))
endif

ENGINE_CMD        := $(ENGINE) $(PODMAN_OPT)

.PHONY: help build generate shell stop save load clean

help:
	@echo "Asylum WardRV ACT3 compliance Docker helpers"
	@echo ""
	@echo "Variables:"
	@echo "  ENGINE         Container engine : docker or podman ($(ENGINE))."
	@echo "  JOBS           Parallel jobs for make full ($(JOBS))."
	@echo "  IMAGE_ARCHIVE  Archive of the image ($(IMAGE_ARCHIVE))."
	@echo ""
	@echo "Targets:"
	@echo "  build     Build the image ($(IMAGE_NAME):$(TAG)) with all the tools (network needed)."
	@echo "  generate  Regenerate all the tests in the container (make full, without network)."
	@echo "  shell     Open a shell inside the container."
	@echo "  stop      Stop the container if it is running."
	@echo "  save      Save the image in IMAGE_ARCHIVE."
	@echo "  load      Load the image from IMAGE_ARCHIVE."
	@echo "  clean     Remove the local image."
	@echo ""
	@echo "Example:"
	@echo "  make build save"
	@echo "  make generate"
	@echo "  make ENGINE=podman load generate"

build:
	$(ENGINE_CMD) build $(DOCKER_BUILD_OPT) -t $(IMAGE_NAME):$(TAG) -f $(DOCKERFILE) $(BUILD_CONTEXT)

generate:
	$(ENGINE_CMD) rm -f $(CONTAINER_NAME) >/dev/null 2>&1 || true
	$(ENGINE_CMD) run $(DOCKER_RUN_OPT) $(GENERATE_OPT) $(IMAGE_NAME):$(TAG) make -j$(JOBS) full

shell:
	$(ENGINE_CMD) rm -f $(CONTAINER_NAME) >/dev/null 2>&1 || true
	$(ENGINE_CMD) run -it $(DOCKER_RUN_OPT) $(IMAGE_NAME):$(TAG) /bin/bash

stop:
	$(ENGINE_CMD) stop $(CONTAINER_NAME) || true

save:
	$(ENGINE_CMD) save $(IMAGE_NAME):$(TAG) | gzip > $(IMAGE_ARCHIVE)

load:
	gunzip -c $(IMAGE_ARCHIVE) | $(ENGINE_CMD) load

clean:
	$(ENGINE_CMD) rmi $(IMAGE_NAME):$(TAG) || true
