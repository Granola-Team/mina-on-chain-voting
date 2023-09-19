FROM node:18-alpine
# RUN apk add --no-cache libc6-compat
WORKDIR /app
ENV SKIP_ENV_VALIDATION=1
ENV NEXT_ENV_DOCKER=1
RUN yarn global add pnpm
COPY . .
RUN pnpm i
RUN pnpm build
# start the app
CMD ["NODE_ENV=production pnpm start"]
