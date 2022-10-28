import { expect } from "vitest";
import { screen, fireEvent, render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Instructions } from "./Instructions";

test("should navigate to faucet when link is clicked", async () => {
    const rendered = render(<Instructions />);
    const link = rendered.getByText("Faucet");
    fireEvent.click(link);
    expect(screen.getByText("Faucet").closest("a"))
        .toHaveAttribute("href", "https://faucet.minaprotocol.com/");
});

test("Instructions are rendered and visible", async () => {
    render(<Instructions />);
    expect(screen.getByText("Instructions")).toBeInTheDocument();
});

test("Snapshot test passes", async () => {
    const instructions = render(<Instructions />);
    expect(instructions).toMatchSnapshot();
});
