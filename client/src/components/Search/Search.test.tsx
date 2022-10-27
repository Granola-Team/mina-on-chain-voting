import { expect } from "vitest";
import "@testing-library/jest-dom";
import userEvent from "@testing-library/user-event";
import { Search } from "./Search";
import { BrowserRouter as Router } from "react-router-dom";
import { Layout } from "../Layout/Layout";
import { SearchControl } from "./SearchControl";
import { cleanup, screen, render, waitFor } from "@testing-library/react";
afterEach(cleanup);
/*
test("Search icon visible", async () => {
    const rendered = render(
            <Router>
                <SearchControl />
            </Router>
            );
    const button = rendered.getByTestId('SC-element');
    expect(button).toBeInTheDocument();
});
*/
test("Click Search icon-button for modal pop-up, click search button to change url slug", async () => {
    const rendered = render(
        <Router>
            <Layout>
                <Search />
            </Layout>
        </Router>);
    const button = rendered.getByRole('button', { pressed: true })
    expect(button).toBeInTheDocument();
    userEvent.click(button!);
    await waitFor(() =>
        expect(screen.getByText("What are you looking for?")).toBeInTheDocument());

    userEvent.type(screen.getByRole("textbox"), "test");
    userEvent.click(screen.getByText("Search"));
    expect(window.location.href).toContain("/test?network=Mainnet");
});
