import '@testing-library/jest-dom';
import '@testing-library/jest-dom/extend-expect';

jest.mock('next/router', () => require('next-router-mock'));
