import { expect } from "vitest";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Header } from "../Header/Header";
import { Search } from "../Search/Search";
import { Settings } from "../Settings/Settings";
import { BrowserRouter as Router } from "react-router-dom";

test("Search is rendered", async () => {
    const search = render(
        <Router>
            <Search />
        </Router>,
    );
    expect(search).toMatchSnapshot();
});

test("Settings is rendered", async () => {
    const settings = render(
        <Router>
            <Settings />
        </Router>,
    );
    expect(settings).toMatchSnapshot();
});

test("Header renders title", async () => {
    const header = render(
        <Router>
            <Header />
        </Router>,
    );
    expect(header).toMatchSnapshot();
    const title = header.getByRole("heading", { level: 1 })
        .innerHTML;
    expect(title).toContain("Mina On-Chain Signals");
});
