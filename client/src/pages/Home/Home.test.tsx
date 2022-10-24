import { expect } from "vitest";
import { screen, fireEvent, render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Home } from "./Home";

test("should navigate from landing page with demo mode to home page with signal results bar when link is clicked", async () => {
    const rendered = render(<Home />);
    const link = rendered.getByText("Demo Mode");
    fireEvent.click(link);
    expect(screen.getByText("Signal Results"));
});
