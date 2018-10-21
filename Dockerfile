FROM node:10 AS builder

USER root

COPY . /elm

RUN yarn global add elm uglify-js

WORKDIR /elm

RUN elm make src/Main.elm --optimize --output=public/main.js
RUN uglifyjs public/main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle


FROM nginx:1.15-alpine

COPY --from=builder /elm/public/ /usr/share/nginx/html/

EXPOSE 80