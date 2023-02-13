import nextJest from 'next/jest';

import type { Config } from 'jest';
import { pathsToModuleNameMapper } from 'ts-jest';

import { compilerOptions } from './tsconfig.json';

const createJestConfig = nextJest({
  dir: './',
});

export default createJestConfig({
  clearMocks: true,
  collectCoverage: true,
  collectCoverageFrom: [
    '<rootDir>/**/*.{ts,tsx}',
    '!<rootDir>/**/{common,pages}/**',
    '!<rootDir>/**/{storybook,.storybook}/**',
    '!<rootDir>/components/{themes,provider}/**',
    '!<rootDir>/**/*.{generated,stories,d}.{ts,tsx}',
    '!<rootDir>/**/{index.ts,middleware.ts,jest.*.{js,ts}}',
  ],
  coverageDirectory: 'coverage',
  coverageProvider: 'v8',
  moduleDirectories: ['node_modules', 'shared'],
  modulePaths: [compilerOptions.baseUrl],
  moduleNameMapper: pathsToModuleNameMapper(compilerOptions.paths),
  resolver: '',
  preset: 'ts-jest',
  setupFilesAfterEnv: ['<rootDir>/jest.setup.ts'],
  testEnvironment: 'jsdom',
  testPathIgnorePatterns: ['/node_modules/'],
  globals: {
    'ts-jest': {
      tsconfig: 'tsconfig.jest.json',
      isolatedModules: true,
    },
  },
} satisfies Config);
