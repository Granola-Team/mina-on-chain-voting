FROM node:18-alpine
RUN apk add --no-cache libc6-compat

WORKDIR /src
ENV SKIP_ENV_VALIDATION=1
ENV NEXT_ENV_DOCKER=1
RUN yarn global add pnpm
COPY . .
RUN pnpm i
RUN pnpm build

WORKDIR /app
RUN cp -a /src/.next/standalone/. .
RUN cp -a /src/.next/static .next/
ENV NODE_ENV=production
ENV PORT 3000
ENV HOSTNAME "0.0.0.0"
EXPOSE 3000

CMD ["node", "server.js"]
