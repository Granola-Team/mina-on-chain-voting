import { expect } from "vitest";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
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