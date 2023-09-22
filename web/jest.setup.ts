import '@testing-library/jest-dom';
import '@testing-library/jest-dom/extend-expect';
import 'mock-match-media/jest-setup';

jest.mock('next/router', () => require('next-router-mock'));

jest.mock(
  'next/dist/shared/lib/router-context',
  () => jest.requireActual('next/dist/shared/lib/router-context.shared-runtime'),
  { virtual: true }
);

global.ResizeObserver = jest.fn().mockImplementation(() => ({
  observe: jest.fn(),
  unobserve: jest.fn(),
  disconnect: jest.fn(),
}));
