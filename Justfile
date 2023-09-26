# Justfile
#
# The command 'just' will give usage information.
# See https://github.com/casey/just for more.

default:
  @just --list --justfile {{justfile()}}

# Set the environment variables defined in the '.env' file.
set dotenv-load

build: build-web build-server build-images

clean: clean-web clean-server

clean-web:
  cd web && pnpm clean

clean-server:
  cd server && rm -fr ./server

install-web:
  cd web && pnpm install

build-web-clean: clean-web install-web build-web
  cd web && pnpm build

build-web: lint-web
  cd web && pnpm build
  cd web && pnpm test

build-server: lint-server
  cd server && cargo build
  cd server && cargo make

test: test-web

test-web: launch-server build-web

test-server: launch-server
  sleep 5  # Wait for server to launch.
  curl http://127.0.0.1:8080/api/info | grep 'chain_tip'
  grep DEBUG container-logs/server.err
  curl http://127.0.0.1:8080/api/proposals \
    | grep 'jw8dXuUqXVgd6NvmpryGmFLnRv1176oozHAro8gMFwj8yuvhBeS'
  grep "status.*200.*/api/proposals" container-logs/server.err
  curl http://127.0.0.1:8080/api/proposal/4/results | grep 'MIP4'
  grep "status.*200.*/api/proposal/4/result" container-logs/server.err

lint: lint-web lint-server

lint-web: install-web
  cd web && pnpm ts-lint

lint-server:
  cd server && cargo audit
  cd server && cargo make clippy

# Build all container images
build-images: image-build-web image-build-server

# Build the container image for 'web'
image-build-web: lint-web
  podman build -t mina-ocv-web ./web

# Build the container image for 'server'
image-build-server: lint-server
  podman build -t mina-ocv-server ./server

# Stop and destroy the named container.
destroy name:
  -podman stop {{name}}
  -podman container rm {{name}}

# Stop and destroy all known containers.
destroy-all: (destroy "db") (destroy "server")

# Run the database container with migrations applied.
launch-db: (destroy "db")
  podman run \
    --name db \
    -e POSTGRES_DB=db \
    -e POSTGRES_USER=granola \
    -e POSTGRES_PASSWORD=systems \
    --expose 5432 \
    --network host \
    postgres:15.2 \
    > container-logs/db.out \
    2> container-logs/db.err &
  cd server && sleep 2 && diesel migration run

  # Running 'diesel migration run' actually makes changes to the source files!
  # WTF! This undoes that change.
  git restore -- server/src/schema.rs

launch-server: (destroy "server") (image-build-server) launch-db
  podman run \
    --name server \
    --env-file .env \
    --expose 8080 \
    --network host \
    localhost/mina-ocv-server:latest \
    > container-logs/server.out \
    2> container-logs/server.err &
