import React from "react";
import { expect } from "vitest";
import { screen, fireEvent, getByText, toHaveAttribute, render } from "@testing-library/react";
import '@testing-library/jest-dom';
import { Footer } from "./Footer";

test('should navigate to ... when link is clicked', async () => {
    const rendered = render(<Footer />);

    const link = rendered.getByText('GitHub'); 

    fireEvent.click(link);
    
    expect(screen.getByText('GitHub').closest('a'))
        .toHaveAttribute('href', 'https://github.com/Granola-Team/onchain-signalling');
});

test('should navigate to ... when link is clicked', async () => {
    const rendered = render(<Footer />);

    const link = rendered.getByText('Made with ❤️ by Granola'); 

    fireEvent.click(link);
    
    expect(screen.getByText('Made with ❤️ by Granola').closest('a'))
        .toHaveAttribute('href', 'https://granola.team');
});
