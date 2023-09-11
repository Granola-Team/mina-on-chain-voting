import nextJest from 'next/jest';

import type { Config } from 'jest';
import { pathsToModuleNameMapper } from 'ts-jest';

import { compilerOptions } from './tsconfig.json';

const createJestConfig = nextJest({
  dir: './',
});

export default createJestConfig({
  clearMocks: true,
  collectCoverageFrom: [
    '<rootDir>/**/*.{ts,tsx}',
    '!<rootDir>/**/{models,common,pages,app}/**',
    '!<rootDir>/**/{storybook,.storybook}/**',
    '!<rootDir>/components/{themes,provider,core}/**',
    '!<rootDir>/**/*.{generated,stories,d}.{ts,tsx}',
    '!<rootDir>/**/{index.ts,middleware.ts,*.config.{js,ts},jest.*.{js,ts}}',
  ],
  coverageDirectory: 'coverage',
  moduleDirectories: ['node_modules'],
  modulePaths: [compilerOptions.baseUrl],
  moduleNameMapper: {
    ...pathsToModuleNameMapper(compilerOptions.paths),
    uuid: require.resolve('uuid'),
  },
  preset: 'ts-jest',
  setupFilesAfterEnv: ['<rootDir>/jest.setup.ts'],
  testEnvironment: 'jsdom',
  testPathIgnorePatterns: ['/node_modules/'],
  resolver: '',
  globals: {
    'ts-jest': {
      tsconfig: 'tsconfig.jest.json',
      isolatedModules: true,
    },
  },
} satisfies Config);
