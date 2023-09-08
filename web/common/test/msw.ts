import { setupServer } from 'msw/node';

import '@testing-library/jest-dom';

export const mockServer = setupServer();

beforeEach(() => mockServer.listen());
afterEach(() => mockServer.resetHandlers());
afterAll(() => mockServer.close());
