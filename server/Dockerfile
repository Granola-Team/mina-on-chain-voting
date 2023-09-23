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
RUN cargo build --release


FROM debian:bullseye
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
    ca-certificates
COPY --from=builder /etc/passwd /etc/passwd
COPY --from=builder /etc/group /etc/group
COPY --from=builder /app/server/target/release/mina-ocv-server /app/mina-ocv-server
WORKDIR /app
USER mina:mina
ENV RUST_LOG "mina_governance_server=debug,tower_http=trace"
# start the server
CMD ["./mina-ocv-server"]
