import { expect } from "vitest";
import { screen, fireEvent, render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Search } from "./Search";
import { BrowserRouter as Router } from "react-router-dom";
import { Layout } from "../Layout/Layout";

test("Clicking search icon should open modal, which implies state change for both SearchControl and Search", async () => {
    const { container } = render(
        <Router>
            <Layout>
                <Search />
            </Layout>
        </Router>);
    const button = container.querySelector("[xmlns='http://www.w3.org/2000/svg']");
    expect(button).toBeInTheDocument();
    fireEvent.click(button!);
    expect(screen.getByText("What are you looking for?")).toBeInTheDocument();
});
