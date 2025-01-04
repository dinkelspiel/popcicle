FROM ghcr.io/gleam-lang/gleam:v1.6.3-scratch as gleam
FROM erlang:27-alpine as builder
RUN apk add rebar3 build-base wget tar gzip bsd-compat-headers curl watchexec inotify-tools nodejs npm
COPY --from=gleam /bin/gleam /bin/gleam
COPY ./ /app
WORKDIR /app
# RUN gleam clean
CMD gleam run -m lustre/dev build
FROM nginx as runner
COPY --from=builder /app/nginx.conf /etc/nginx/
COPY --from=builder /app/index.html /usr/share/nginx/html/
COPY --from=builder /app/priv /usr/share/nginx/html/priv
CMD ["nginx", "-g", "daemon off;"]
