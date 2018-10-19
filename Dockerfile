FROM node:10 AS builder

USER root

COPY . /elm

RUN yarn global add elm

WORKDIR /elm

RUN elm make src/Main.elm --output=public/main.js

FROM nginx:1.15-alpine

COPY --from=builder /elm/public/ /usr/share/nginx/html/

EXPOSE 80