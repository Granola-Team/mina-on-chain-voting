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
RUN apt-get update && \
    apt-get install -y \
    curl \
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
    postgresql-client \
    ca-certificates
USER mina:mina
WORKDIR /app
RUN cargo install diesel_cli --no-default-features --features postgres
COPY --chown=mina:mina . .
RUN cargo build --release

# start the server
CMD ["./start"]
