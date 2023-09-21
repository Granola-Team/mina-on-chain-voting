# Justfile

default:
  @just --list --justfile {{justfile()}}


build-all: build-web build-server

build-web:
  podman build ./web

build-server:
  podman build ./server

run-pg:
  podman run \
    -e POSTGRES_DB=db \
    -e POSTGRES_USER=granola \
    -e POSTGRES_PASSWORD=systems \
    --expose 5432 \
    --network host \
    postgres:15.2
