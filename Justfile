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
  podman build -t mina-ocv-web ./web

# Build the container image for 'server'
image-build-server: lint-server
  podman build -t mina-ocv-server ./server

destroy name:
  podman stop {{name}} || true
  podman container rm {{name}} || true

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

run-migrations:
  cd server && sleep 2 && diesel migration run
  # Undo the change that 'diesel migration run' creates!
  git restore -- server/src/schema.rs
  @echo "Migrations succeeded."

launch-server: (destroy "server") (image-build-server) launch-db run-migrations
  podman run \
    --name server \
    --env-file .env \
    --expose 8080 \
    --network host \
    localhost/mina-ocv-server:latest \
    > container-logs/server.out \
    2> container-logs/server.err &

destroy-all: (destroy "db") (destroy "server")
