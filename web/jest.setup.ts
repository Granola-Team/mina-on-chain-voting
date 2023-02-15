import { LocalStorageMock } from 'common/test/local-storage';

import '@testing-library/jest-dom';
import '@testing-library/jest-dom/extend-expect';

jest.mock('next/router', () => require('next-router-mock'));

Object.defineProperty(global.self, 'localStorage', { value: new LocalStorageMock() });
