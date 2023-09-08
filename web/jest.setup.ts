import '@testing-library/jest-dom';
import '@testing-library/jest-dom/extend-expect';
import 'mock-match-media/jest-setup';

jest.mock('next/router', () => require('next-router-mock'));

global.ResizeObserver = jest.fn().mockImplementation(() => ({
  observe: jest.fn(),
  unobserve: jest.fn(),
  disconnect: jest.fn(),
}));
