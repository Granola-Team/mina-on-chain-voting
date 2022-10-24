import { expect } from "vitest";
import { screen, fireEvent, render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Home } from "./Home";
import { BrowserRouter as Router } from "react-router-dom";

test("should navigate from landing page with demo mode to home page with signal results bar when link is clicked", async () => {
    const rendered = render(<Router><Home /></Router>);
    const button = rendered.getByText("Demo Mode");
    fireEvent.click(button);
    expect(screen.getByText("Signal Results"));
});
