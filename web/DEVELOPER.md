# Development of the 'web' Component

## Make sure to have the necessary installations and dependencies

- If not installed, install [`pnpm`](https://pnpm.io/)

  ```bash
  brew install pnpm

  # or ...

  curl -fsSL https://get.pnpm.io/install.sh | sh -
  ```

- Set the NodeJS version to be used:

  ```bash
  pnpm env use --global 18
  ```


## Start developing

- In the 'web' directory, install dependencies

  ```bash
  pnpm clean && pnpm install
  ```

- Make sure your .env file is set-up correctly

  Please see the [`.env.example`](./.env.example) file in the root of the
  project for more details.


### Development

Lint:

```bash
pnpm ts-lint
```

Test the front end (web):

```bash
pnpm test
```

Building the front end (web):

```bash
pnpm build
```


## Running in the console

You can run the web-app in console and mount only both the database and the
server in Docker when developing the web app locally.

> **IMPORTANT:** When running this way, the database URL in the `.env` file has
> to point to `localhost`.</br>

See [`.env.example`](./.env.example) for more information on the `DATABASE_URL`
env var.

- Mount the database and server in Docker. The db and backend should be up and
  running now.

  ```sh
  docker-compose --profile server-db up
  ```

- Run migrations from the `server` directory.

  ```sh
  diesel migration run
  ```

- Run the app (frontend) in development mode.

  ```sh
  pnpm web dev
  ```

## Resources

- [Next.js Documentation](https://nextjs.org/docs/getting-started)
- [Typescript](https://www.typescriptlang.org/docs/)
