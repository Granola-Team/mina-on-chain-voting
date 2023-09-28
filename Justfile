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

install-server:
  cargo install cargo-make
  cargo install diesel_cli --no-default-features --features postgres
  cargo install cargo-audit

build-web-clean: clean-web install-web build-web
  cd web && pnpm build

build-web: lint-web
  cd web && pnpm build
  cd web && pnpm test

build-server: lint-server install-server
  cd server && cargo build
  cd server && cargo make

test: test-web

test-web: launch-server launch-web

test-server: launch-server
  mkdir -p container-logs
  sleep 10  # Wait for server to launch.
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
[macos]
image-build-web: lint-web
  docker build -t mina-ocv-web ./web

# Build the container image for 'web'
[linux]
image-build-web: lint-web
  docker build -t mina-ocv-web ./web

# Build the container image for 'server'
[macos]
image-build-server: lint-server
  docker build -t mina-ocv-server ./server

# Build the container image for 'server'
[linux]
image-build-server: lint-server
  podman build -t mina-ocv-server ./server

[macos]
destroy-db:
  docker-compose --profile=db down

[linux]
destroy-db:
  -podman stop db
  -podman container rm db

[macos]
destroy-server:
  docker-compose --profile=server-db down

[linux]
destroy-server:
  -podman stop server
  -podman container rm server

[macos]
destroy-web:
  docker-compose --profile=all down

[linux]
destroy-web:
  -podman stop web
  -podman container rm web

# Stop and destroy all known containers.
destroy-all: destroy-db destroy-server destroy-web

# Run the database container with migrations applied.
[linux]
launch-db: destroy-db
  mkdir -p container-logs
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

[macos]
launch-server: destroy-server
  mkdir -p container-logs
  docker-compose --profile=server-db up \
    > container-logs/server.out \
    2> container-logs/server.err &

[linux]
launch-server: destroy-server image-build-server launch-db
  mkdir -p container-logs
  podman run \
    --name server \
    --env-file .env \
    --expose 8080 \
    --network host \
    localhost/mina-ocv-server:latest \
    > container-logs/server.out \
    2> container-logs/server.err &

[macos]
launch-web: destroy-all
  mkdir -p container-logs
  docker-compose --profile=all up \
    > container-logs/server.out \
    2> container-logs/server.err &

[linux]
launch-web: destroy-all image-build-web launch-server launch-db
  mkdir -p container-logs
  podman run \
    --name web \
    --env-file .env \
    --expose 3000 \
    --network host \
    localhost/mina-ocv-web:latest \
    > container-logs/web.out \
    2> container-logs/web.err &
