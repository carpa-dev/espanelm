FROM node:latest AS builder
WORKDIR /app
COPY . /app
RUN npm install && npm test && npm run build

FROM node:alpine
RUN apk add --update python py-pip jq bash
RUN pip install awscli

WORKDIR /app
COPY --from=builder /app/build /app/build
COPY scripts /app/scripts
RUN chmod +x /app/scripts/deploy.sh

ENTRYPOINT ["bash", "-c", "/app/scripts/deploy.sh"]
