FROM node:22

COPY . /app
WORKDIR /app

RUN corepack enable && yarn install --frozen-lockfile

RUN yarn build && mv /app/public /public
