# Justfile
#
# The command 'just' will give usage information.
# See https://github.com/casey/just for more.

# Default build target. Shows menu of targets which user runs 'just'.
#
default:
  @just --list --justfile {{justfile()}}

# Variables
#
DB_HOST := env_var_or_default('DB_HOST', "127.0.0.1")
DB_PORT := env_var_or_default('DB_PORT', "5432")
DB_NAME := env_var_or_default('DB_NAME', "db")
DB_USER := env_var_or_default('DB_USER', "granola")
DB_PASS := env_var_or_default('DB_PASS', "systems")
DATABASE_URL := env_var_or_default(
  'DATABASE_URL',
  "postgresql://" + DB_USER + ":" + DB_PASS + "@" + DB_HOST + ":" + DB_PORT + "/" + DB_NAME)
container_log_dir := `mktemp -d "${TMPDIR:-/tmp}"/container-logs-XXX`

#
# Targets
#

build: build-web build-server

clean: clean-web clean-server

clean-web:
  cd web && pnpm clean && rm -fr coverage && rm -f tsconfig.tsbuildinfo

clean-server:
  cd server && rm -fr ./target

# Install prerequisite packages for 'web' development.
install-web:
  cd web && pnpm install

# Install prerequisite packages for 'server' development.
install-server:
  cargo install diesel_cli --no-default-features --features postgres
  cargo install cargo-audit

# Install prerequisite packages.
install: install-web install-server

build-web-clean: clean-web install-web build-web
  cd web && pnpm build

# Build, lint, and unit-test the web component.
build-web: lint-web
  cd web && \
    NEXT_PUBLIC_API_BASE_URL=http://127.0.0.1:8080 \
    NEXT_PUBLIC_RELEASE_STAGE=development \
    pnpm build
  cd web && \
    NEXT_PUBLIC_API_BASE_URL=http://127.0.0.1:8080 \
    NEXT_PUBLIC_RELEASE_STAGE=development \
    pnpm test

# Build, lint, and unit-test the server component.
build-server: lint-server
  cd server && cargo build
  cd server && cargo test

test: test-web

test-web: test-server launch-web

test-server: launch-server
  sleep 10  # Wait for server to launch.
  curl http://127.0.0.1:8080/api/info | grep 'chain_tip'
  grep DEBUG {{ container_log_dir }}/server.err  # Ensure DEBUG info being logged.
  curl http://127.0.0.1:8080/api/proposals \
    | grep 'jw8dXuUqXVgd6NvmpryGmFLnRv1176oozHAro8gMFwj8yuvhBeS'
  grep "status.*200.*/api/proposals" {{ container_log_dir }}/server.err
  curl http://127.0.0.1:8080/api/proposal/4/results | grep 'MIP4'
  grep "status.*200.*/api/proposal/4/result" {{ container_log_dir }}/server.err

lint: lint-web lint-server

lint-web: install-web
  cd web && pnpm ts-lint
  cd web && \
    NEXT_PUBLIC_API_BASE_URL=http://127.0.0.1:8080 \
    NEXT_PUBLIC_RELEASE_STAGE=development \
    pnpm lint

lint-server: install-server
  cd server && cargo clippy -- -D warnings -D clippy::pedantic -D clippy::unwrap_used
  cd server && cargo audit


# Build all container images
image-build: image-build-web image-build-server

# Build the container image for 'web'
[macos]
image-build-web: clean-web
  docker build -t mina-ocv-web ./web

# Build the container image for 'web'
[linux]
image-build-web: clean-web
  podman build -t mina-ocv-web ./web

# Build the container image for 'server'
[macos]
image-build-server: clean-server
  docker build -t mina-ocv-server ./server

# Build the container image for 'server'
[linux]
image-build-server: clean-server
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
  podman run \
    --name db \
    -e POSTGRES_DB={{ DB_NAME }} \
    -e POSTGRES_USER={{ DB_USER }} \
    -e POSTGRES_PASSWORD={{ DB_PASS }} \
    -e DATABASE_URL={{ DATABASE_URL }} \
    --expose {{ DB_PORT }} \
    --network host \
    postgres:15.2 \
    > {{ container_log_dir }}/db.out \
    2> {{ container_log_dir }}/db.err &
  sleep 2
  cd server && \
    DATABASE_URL={{ DATABASE_URL }} diesel migration run

  # Running 'diesel migration run' actually makes changes to the source files!
  # WTF! This undoes that change.
  git restore -- server/src/schema.rs

[macos]
launch-server: destroy-server image-build-server
  docker-compose --profile=server-db up \
    > {{ container_log_dir }}/server.out \
    2> {{ container_log_dir }}/server.err &

[linux]
launch-server: destroy-server image-build-server launch-db
  podman run \
    --name server \
    --env-file .env \
    --expose 8080 \
    --network host \
    localhost/mina-ocv-server:latest \
    > {{ container_log_dir }}/server.out \
    2> {{ container_log_dir }}/server.err &

[macos]
launch-web: destroy-all image-build-server image-build-web
  docker-compose --profile=all up \
    > {{ container_log_dir }}/web.out \
    2> {{ container_log_dir }}/web.err &

[linux]
launch-web: destroy-all image-build-web launch-server
  podman run \
    --name web \
    --env-file .env \
    --expose 3000 \
    --network host \
    localhost/mina-ocv-web:latest \
    > {{ container_log_dir }}/web.out \
    2> {{ container_log_dir }}/web.err &
