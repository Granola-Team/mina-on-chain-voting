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

test-web: launch-web

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
image-build-web: clean-web
  docker build -t mina-ocv-web ./web

# Build the container image for 'server'
image-build-server: clean-server
  docker build -t mina-ocv-server ./server

destroy-db:
  docker-compose down db

destroy-server:
  docker-compose down server

destroy-web:
  docker-compose down web

# Stop and destroy all known containers.
destroy-all:
  docker-compose down

launch-db:
  docker-compose up -d db

test-db: launch-db && destroy-db
  docker-compose logs db \
    | grep "database system is ready to accept connections"

launch-server:
  docker-compose up -d server

test-server: launch-server && destroy-server
  sleep 10  # Wait for server to launch.
  curl http://127.0.0.1:8080/api/info | grep 'chain_tip'
  docker-compose logs server 2>&1 \
    | grep DEBUG  # Ensure DEBUG info being logged.
  curl http://127.0.0.1:8080/api/proposals \
    | grep 'jw8dXuUqXVgd6NvmpryGmFLnRv1176oozHAro8gMFwj8yuvhBeS'
  docker-compose logs server 2>&1 \
    | grep "status.*200.*/api/proposals"
  curl http://127.0.0.1:8080/api/proposal/4/results | grep 'MIP4'
  docker-compose logs server 2>&1 \
    | grep "status.*200.*/api/proposal/4/result"

launch-web:
  docker-compose up -d web
