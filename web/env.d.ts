import { env } from './env.js';

type EnvType = typeof env;

export {};

declare global {
  namespace NodeJS {
    interface ProcessEnv extends EnvType, NodeJS.ProcessEnv {}
  }
}
