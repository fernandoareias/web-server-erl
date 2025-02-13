FROM erlang:27-alpine AS build

RUN mkdir /buildroot
WORKDIR /buildroot

COPY . /buildroot

RUN rebar3 as prod release

FROM alpine:latest

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

COPY --from=build /buildroot/_build/prod/rel/web_server_erl /web_server_erl
COPY ./http /web_server_erl/http

EXPOSE 8091

CMD ["/web_server_erl/bin/web_server_erl", "foreground"]
