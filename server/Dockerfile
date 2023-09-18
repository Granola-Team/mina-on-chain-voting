### track dependencies
FROM rust:bullseye as cacher

WORKDIR /app
RUN cargo install cargo-chef
COPY . .
RUN cargo chef prepare --recipe-path recipe.json
RUN cargo chef cook --release --recipe-path recipe.json

### build server
FROM rust:bullseye as builder

ENV USER=mina
ENV UID=1001

RUN adduser \
    --disabled-password \
    --gecos "" \
    --home "/nonexistent" \
    --shell "/sbin/nologin" \
    --no-create-home \
    --uid "${UID}" \
    "${USER}"

WORKDIR /app
COPY . .
COPY --from=cacher /app/server/target target
COPY --from=cacher /usr/local/cargo /usr/local/cargo

RUN cargo build --release

### extract debian dependencies
FROM debian:bullseye AS deb_extractor

RUN cd /tmp && \
    apt-get update && \
    apt-get download \
    libpq5 \
    libffi7 \
    libgmp10 \
    libkrb5-3 \
    libidn2-0 \
    libsasl2-2 \
    libtasn1-6 \
    libnettle8 \
    libcom-err2 \
    libgnutls30 \
    libp11-kit0 \
    libhogweed6 \
    libk5crypto3 \
    libkeyutils1 \
    libldap-2.4-2 \
    libunistring2 \
    libkrb5support0 \
    libgssapi-krb5-2 \
    && \
    mkdir -p /dpkg/var/lib/dpkg/status.d/ && \
    for deb in *.deb; do \
    package_name=$(dpkg-deb -I ${deb} | awk '/^ Package: .*$/ {print $2}'); \ 
    echo "Process: ${package_name}"; \
    dpkg --ctrl-tarfile $deb | tar -Oxvf - ./control > /dpkg/var/lib/dpkg/status.d/${package_name}; \
    dpkg --extract $deb /dpkg || exit 10; \
    done 

### copy binary and deps to runtime
FROM gcr.io/distroless/cc-debian11 as runtime

COPY --from=deb_extractor /dpkg /

COPY --from=builder /etc/passwd /etc/passwd
COPY --from=builder /etc/group /etc/group

COPY --from=builder /app/target/release/mina-ocv-server /app/mina-ocv-server

WORKDIR /app

USER mina:mina

# start the server
CMD ["./mina-ocv-server"]
