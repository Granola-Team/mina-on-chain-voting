/* eslint-disable spaced-comment */
/// <reference types="vite/client" />

interface ImportMetaEnv {
  readonly VITE_ENV_EXAMPLE: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}
