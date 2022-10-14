import React from "react";
import { expect } from "vitest";
import { fireEvent, render } from "@testing-library/react";
import '@testing-library/jest-dom';
import { Footer } from "./Footer"


test('should navigate to ... when link is clicked', async () => {
    render(<Footer />);

    expect(screen.getByText('GitHub')).toHaveAttribute(
        'href', 
        'https://github.com/Granola-Team/onchain-signalling'
        );
});

test('should navigate to ... when link is clicked', async () => {
    render(<Footer />);

    expect(screen.getByText('Made with ❤️ by Granola')).toHaveAttribute(
        'href',
        'https://granola.team'
        );
});

