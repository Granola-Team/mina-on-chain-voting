# PoC Dashboard Design

> Minimal react with HMR for rapid development.

## Usage

**Development**

`yarn run start-dev`

- Build app continuously (HMR enabled)
- App served @ `http://localhost:8080`

**Production**

`yarn run start-prod`

- Build app once (HMR disabled) to `/build/`
- App served @ `http://localhost:3000`

---

**All commands**

| Command               | Description                                                                    |
| --------------------- | ------------------------------------------------------------------------------ |
| `yarn run start-dev`  | Build app continuously (HMR enabled) and serve @ `http://localhost:8080`       |
| `yarn run start-prod` | Build app once (HMR disabled) to `/build/` and serve @ `http://localhost:3000` |
| `yarn run build`      | Build app to `/build/`                                                         |
| `yarn run test`       | Run tests                                                                      |
| `yarn run lint`       | Run linter                                                                     |
| `yarn run lint --fix` | Run linter and fix issues                                                      |
| `yarn run start`      | (alias of `yarn run start-dev`)                                                |
