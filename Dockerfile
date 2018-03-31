FROM alpine:3.7

RUN apk add --no-cache --verbose \
        ca-certificates \
    && update-ca-certificates
COPY build-cache/target/x86_64-unknown-linux-musl/release/gumby_app /

ENTRYPOINT ["/gumby_app"]
