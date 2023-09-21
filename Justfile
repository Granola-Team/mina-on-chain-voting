# Justfile

default:
  @just --list --justfile {{justfile()}}

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

build-web: install-web
  cd web && pnpm build

build-server:
  cd server && cargo build

test: test-web test-server

test-web:
  cd web && pnpm test

test-server:
  cd server && cargo make

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
  podman build ./web

# Build the container image for 'server'
image-build-server: lint-server
  podman build ./server

run-pg:
  podman run \
    -e POSTGRES_DB=db \
    -e POSTGRES_USER=granola \
    -e POSTGRES_PASSWORD=systems \
    --expose 5432 \
    --network host \
    postgres:15.2
