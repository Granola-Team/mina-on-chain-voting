# Contributing to the On Chain Signalling Client (ocs_client)

The On Chain Signalling Client is a React SPA built on the TanStack
that provides a dashboard for visualizing governance signals on the
Mina Blockchain

## Development Environment

- `nix develop .#client`

Run from the monorepo directory, 
and ensure Nix Flakes are enabled 
https://nixos.wiki/wiki/Flakes#Installing_nix_flakes

# Directory layout

> Use short lowercase names at least for the top-level files and folders.

    .
    ├── build                   # Compiled files (alternatively `dist`)
    ├── src                     # Source files (alternatively `lib` or `app`)
    ├── test                    # Automated tests (alternatively `spec` or `tests`)
    ├── LICENSE
    └── README.md

## Source layout

     .
    ├── ...
    ├── src
    │   ├── assets            # Images, CSS & Fonts
    │   ├── components        # UI components
    │   ├── hooks             # Custom state hooks
    │   ├── pages             # Pages & top-level routes
    │   ├── queries           # Global react query logic
    │   ├── store             # Global state store (Zustand)
    │   ├── types             # Typings & modules
    │   └── utils             # Unit tests
    └── ...

## Dependencies

- `Zustand` - Minimal dep. store centric state manager
- `React Query` - Asynchronous state manager
- `TailwindCSS` - Utility CSS Framework
- `Vite` - HMR enabled JS/TS bundler
- `Vitest` - Vite based Testing Framework

## Usage

**Development**

`yarn run dev`

- Serves a development build with HMR enabled
- App served @ `http://localhost:8080`

**Production**

`yarn run start`

- Bundles a production build to `/build/`
- App served @ `http://localhost:8080`

---

**All commands**

| Command          | Description                                                              |
| ---------------- | ------------------------------------------------------------------------ |
| `yarn run dev`   | Builds a development build and serves @ `http://localhost:8080`          |
| `yarn run start` | Bundles a production build to `/build/` serves @ `http://localhost:8080` |
| `yarn run build` | Bundles a production build to `/build/`                                  |
| `yarn run test`  | Runs tests with Vitest                                                   |
| `yarn run lint`  | Runs linter with ESLint                                                  |
| `yarn run clean` | Cleans folder & deletes `node_modules` & `build`                         |
