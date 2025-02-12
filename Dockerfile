# Build stage 0
FROM erlang:27-alpine AS build

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy your Erlang project
COPY . /buildroot

RUN rebar3 as prod release

# Build stage 1
FROM alpine:latest

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

# Copy the release from the build stage
COPY --from=build /buildroot/_build/prod/rel/web_server_erl /web_server_erl
COPY ./http /web_server_erl/http

# Expose the relevant ports
EXPOSE 8091

# Run the Erlang application
CMD ["/web_server_erl/bin/web_server_erl", "foreground"]
