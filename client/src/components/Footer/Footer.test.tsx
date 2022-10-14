import React from "react";
import { expect } from "vitest";
import { fireEvent, render } from "@testing-library/react";
import '@testing-library/jest-dom';

test('should navigate to ... when link is clicked', () => {
  const { getByText } = render(<a href="https://github.com/Granola-Team/onchain-signalling">GitHub</a>);

  const link = getByText('GitHub'); 

  fireEvent.click(link);

  expect(getByText('GitHub')).toHaveAttribute(
    'href', 
    'https://github.com/Granola-Team/onchain-signalling'
    );
});

test('should navigate to ... when link is clicked', () => {
  const { getByText } = render(<a href="https://granola.team">GitHub</a>);

  const link = getByText('GitHub');

  fireEvent.click(link);

  expect(getByText('GitHub')).toHaveAttribute(
    'href',
    'https://granola.team'
    );
});