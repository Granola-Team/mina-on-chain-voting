FROM node:18-alpine
# RUN apk add --no-cache libc6-compat
WORKDIR /app
ENV SKIP_ENV_VALIDATION=1
ENV NEXT_ENV_DOCKER=1
ENV NODE_ENV=production
RUN yarn global add pnpm
COPY . .
RUN pnpm i
RUN pnpm build
# start the app
CMD ["pnpm start"]
