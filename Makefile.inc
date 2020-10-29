# File included in the Docker Makefiles.
SELF_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
include $(SELF_DIR)Config.inc

NAME              ?= $(subst /,-,tmp-$(IMAGE_NAME))
# The path to the directory whose files are able to be included in the build
BUILD_PATH        ?= .
# The path to the Dockerfile to execute, relative to the build path
DOCKERFILE_PATH   ?= Dockerfile
# Run arguments
RUN_ARGS          ?= --rm
# Execution arguments
EXEC_ARGS         ?=
FILENAME          ?= $(notdir $(IMAGE_NAME)).tar
CURRENT_DIR       ?= $(strip $(dir $(realpath $(MAKEFILE))))
TEST_DIRS         ?= $(sort $(patsubst tests/%/,%,$(dir $(wildcard tests/*/))))

# Asserts that a variable is defined:
check_defined = \
    $(strip $(foreach 1,$1, \
        $(call __check_defined,$1,$(strip $(value 2)))))
__check_defined = \
    $(if $(value $1),, \
        $(error Undefined $1$(if $2, ($2))$(if $(value @), \
                required by target `$@')))
lineeq = $(shell printf '%*s\n' "$${COLUMNS:-$$(tput cols)}" '' | tr ' ' =)
linedash = $(shell printf '%*s\n' "$${COLUMNS:-$$(tput cols)}" '' | tr ' ' -)

.PHONY: all generate build clean run start attach exec stop rm put get login deploy save load test

.SILENT:

# Generates the Docker image files
# Usage:
#     make generate                 Generates the docker image
generate::

# Builds the Docker image in the current Docker machine
# Usage:
#     make                          Builds the docker image
#     make IMAGE_TAG="3.0"          Builds the docker image and assigns the specified tag
build::
	@:$(call check_defined, IMAGE_NAME, image name)
	@:$(call check_defined, IMAGE_TAG, image tag)
	@:$(call check_defined, BUILD_PATH, build path)
	@:$(call check_defined, DOCKERFILE_PATH, dockerfile path)
	echo "Building image..."
	docker image build \
	  --build-arg BUILD_DATE=$(date -u +'%Y-%m-%dT%H:%M:%SZ') \
	  --build-arg VCS_REF=$(git rev-parse --short HEAD) \
	  -f $(DOCKERFILE_PATH) \
	  -t $(IMAGE_NAME):$(IMAGE_TAG) $(BUILD_PATH)
	docker image tag $(IMAGE_NAME):$(IMAGE_TAG) $(IMAGE_NAME):latest


# Deletes the Docker image from the current Docker machine
# Usage:
#     make clean
clean::
	@:$(call check_defined, IMAGE_NAME, image name)
	@:$(call check_defined, IMAGE_TAG, image tag)
	-docker image rm $(IMAGE_NAME):$(IMAGE_TAG) 2>/dev/null
	-docker image rm $(IMAGE_NAME):latest 2>/dev/null
	-docker image rm $(REGISTRY_URL)$(IMAGE_NAME):$(IMAGE_TAG) 2>/dev/null
	-docker image rm $(REGISTRY_URL)$(IMAGE_NAME):latest 2>/dev/null


# Starts the Docker container from the image and executes the specified command
# Usage:
#     make run
#     make run CMD=/bin/sh
run::
	@:$(call check_defined, IMAGE_NAME, image name)
	@:$(call check_defined, IMAGE_TAG, image tag)
	@:$(call check_defined, NAME, container name)
	@:$(call check_defined, CMD, command)
	docker container run -it $(RUN_ARGS)                                       \
	  --name $(NAME)                                                           \
	  $(IMAGE_NAME):$(IMAGE_TAG)                                               \
	  $(CMD)


# Starts the detached Docker container from the image
# Usage:
#     make start                    Starts the container with sh
#     make start NAME=mycontainer   Assign a name to the container
#     make start CMD=/bin/sh        Starts the container with a custom command
#     make start CMD=               Starts the container with the CMD in the Dockerfile
start::
	@:$(call check_defined, IMAGE_NAME, image name)
	@:$(call check_defined, IMAGE_TAG, image tag)
	@:$(call check_defined, NAME, container name)
	docker container run -itd $(RUN_ARGS)                                      \
	  --name $(NAME)                                                           \
	  $(IMAGE_NAME):$(IMAGE_TAG)                                               \
	  $(CMD)


# Attach to a running container
# Usage:
#     make attach
#     make attach NAME=mycontainer
attach::
	@:$(call check_defined, NAME, container name)
	docker container attach $(NAME)


# Executes a command in a running container
# Usage:
#     make exec CMD="ls -la"        Executes a custom command in the container
#     make exec NAME=mycontainer    Executes a command in the container with the specified name
exec::
	@:$(call check_defined, IMAGE_NAME, image name)
	@:$(call check_defined, IMAGE_TAG, image tag)
	@:$(call check_defined, CMD, command)
	docker container exec -it $(EXEC_ARGS)                                     \
	  $(NAME)                                                                  \
	  $(CMD)


# Stops a running container
# Usage:
#     make stop                     Stops a running container
#     make stop NAME=mycontainer    Stops the container with the specified name
stop::
	@:$(call check_defined, NAME, container name)
	docker container stop $(NAME)


# Removes a container
# Usage:
#     make rm                      Removes a container
#     make rm NAME=mycontainer     Removes the container with the specified name
rm::
	@:$(call check_defined, NAME, container name)
	docker container rm $(NAME)


# Copies a file into a running container
# Usage:
#     make put SRC=myfile.txt DEST=/myfolder/myfile.txt     Copies a local file or folder to the specified path in the container
put::
	@:$(call check_defined, NAME, container name)
	@:$(call check_defined, SRC, source)
	@:$(call check_defined, DEST, destination)
	docker container cp $(SRC) $(NAME):$(DEST)


# Copies a file out of a running container
# Usage:
#     make get SRC=/myfolder/myfile.txt DEST=myfile.txt     Copies a file or folder from the container to the specified path
get::
	@:$(call check_defined, NAME, container name)
	@:$(call check_defined, SRC, source)
	@:$(call check_defined, DEST, destination)
	docker container cp $(NAME):$(SRC) $(DEST)


# Logs into the Docker image registry
# Usage:
#     make login                   Logs into the default registry
#     make login REGISTRY="localhost:4242"   Logs into the specified registry
login::
	docker login $(REGISTRY) \
	$(if $(REGISTRY_USERNAME), --username $(REGISTRY_USERNAME)) \
	$(if $(REGISTRY_PASSWORD), --password $(REGISTRY_PASSWORD))


# Pushes the image to the remote registry
# Usage:
#     make deploy                   Pushes the docker image to the default registry
#     make deploy REGISTRY="localhost:4242"   Pushes the docker image to the specified registry
deploy::
	@:$(call check_defined, IMAGE_NAME, image name)
	@:$(call check_defined, IMAGE_TAG, image tag)
	docker image tag $(IMAGE_NAME):$(IMAGE_TAG) $(REGISTRY_URL)$(IMAGE_NAME):$(IMAGE_TAG)
	docker image tag $(IMAGE_NAME):$(IMAGE_TAG) $(REGISTRY_URL)$(IMAGE_NAME):latest
	docker image push $(REGISTRY_URL)$(IMAGE_NAME):$(IMAGE_TAG)
	docker image push $(REGISTRY_URL)$(IMAGE_NAME):latest
	-docker image rm $(REGISTRY_URL)$(IMAGE_NAME):$(IMAGE_TAG) 2>/dev/null
	-docker image rm $(REGISTRY_URL)$(IMAGE_NAME):latest 2>/dev/null


# Save the Docker image to a TAR file
# Usage:
#     make save                          Saves the docker image to a TAR file
#     make save FILENAME="output.tar"    Saves the docker image to the specified filename
save::
	@:$(call check_defined, IMAGE_NAME, image name)
	@:$(call check_defined, IMAGE_TAG, image tag)
	@:$(call check_defined, FILENAME, output filename)
	docker save -o $(FILENAME) $(IMAGE_NAME):$(IMAGE_TAG)


# Loads the Docker image from a TAR file
# Usage:
#     make load                          Loads the docker image from a TAR file
#     make load FILENAME="output.tar"    Loads the docker image from the specified filename
load::
	@:$(call check_defined, FILENAME, input filename)
	docker load -i $(FILENAME)

# Tests the Docker image.
# Usage:
#     make test                          Runs the tests from the tests/ folder on the Docker image
test:: $(addsuffix .test,$(TEST_DIRS))
	echo "Ran $(words $(TEST_DIRS)) test cases"

# Tests the Docker image with a specific test.
# Usage:
#     make mytest.test
%.test: %.test-prepare %.test-run %.test-cleanup
	echo "x" > /dev/null

# Prepares the test container and enters it before the test is run
# Usage:
#     make mytest.test-enter
%.test-enter: %.test-prepare
	docker commit $(NAME)-test user/$(NAME)-test > /dev/null
	$(if $(NO_RM),echo "Container '$(NAME)-test' not removed",docker container rm $(NAME)-test > /dev/null)
	docker container run -it $(RUN_ARGS)                                       \
	  --name $(NAME)                                                           \
	  user/$(NAME)-test                                                        \
	  $(CMD)
	$(if $(NO_RM),echo "Image 'user/$(NAME)-test' not removed",docker image rm user/$(NAME)-test > /dev/null)

# Runs the test container and enters it after the test is run
# Usage:
#     make mytest.test-inspect
%.test-inspect: %.test-prepare %.test-run
	@:$(call check_defined, NAME, container name)
	@:$(call check_defined, CMD, command)
	docker commit $(NAME)-test user/$(NAME)-test > /dev/null
	$(if $(NO_RM),echo "Container '$(NAME)-test' not removed",docker container rm $(NAME)-test > /dev/null)
	docker container run -it $(RUN_ARGS)                                       \
	  --name $(NAME)                                                           \
	  user/$(NAME)-test                                                        \
	  $(CMD)
	$(if $(NO_RM),echo "Image 'user/$(NAME)-test' not removed",docker image rm user/$(NAME)-test > /dev/null)


# Prepares the test.
%.test-prepare::
	@:$(call check_defined, IMAGE_NAME, image name)
	@:$(call check_defined, IMAGE_TAG, image tag)
	@:$(call check_defined, NAME, container name)
	@:$(call check_defined, DOCKER_USER_HOME, Docker user home dir)
	echo "TEST:" $*
	mkdir -p testresults/$*/
	docker container create --network none --name $(NAME)-test $(IMAGE_NAME):$(IMAGE_TAG) > /dev/null
	docker container cp tests/$*/. $(NAME)-test:$(DOCKER_USER_HOME)/

# Runs the test
%.test-run::
	@:$(call check_defined, NAME, container name)
	@:$(call check_defined, DOCKER_USER_OUTPUT, Docker user output dir)
	docker container start -a $(NAME)-test; ret=$$?; \
	docker container cp $(NAME)-test:$(DOCKER_USER_OUTPUT)/. testresults/$*/.; \
	cat testresults/$*/stdout.txt; \
	if [ -s "testresults/$*/stderr.txt" ]; then \
	echo "$(linedash)"; \
	cat testresults/$*/stderr.txt; \
	fi
	if grep -Fq -m 1 "<failed" "testresults/$*/results.xml"; then \
	echo "$(linedash)"; \
	cat testresults/$*/results.xml; \
	fi
	echo "$(lineeq)"
	exit $$ret

# Cleans up the test
%.test-cleanup::
	docker container rm $(NAME)-test > /dev/null

