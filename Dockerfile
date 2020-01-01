FROM node:latest AS builder
WORKDIR /app
COPY . /app
RUN npm install && npm test && npm run build


FROM node:alpine
RUN apk add --update python py-pip jq
RUN pip install awscli

WORKDIR /app
COPY docker-entrypoint.sh /app
RUN chmod +x docker-entrypoint.sh
COPY --from=builder /app/build /app/build
COPY scripts /app/scripts

ENTRYPOINT ["sh", "docker-entrypoint.sh"]
