import { expect } from "vitest";
import { cleanup, fireEvent, render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Header } from "../Header/Header";
import { Search } from "../Search/Search";
import { Settings } from "../Settings/Settings";
import { BrowserRouter as Router } from "react-router-dom";
import App from "../../App";
import mediaQuery from "css-mediaquery";

function createMatchMedia(width) {
    Object.defineProperty(window, 'matchMedia', {
    writable: true,
    value: vi.fn().mockImplementation(query => ({
        matches: mediaQuery.match(query, { width }),
        media: query,
        onchange: null,
        addListener: vi.fn(),
        removeListener: vi.fn(),
        addEventListener: vi.fn(),
        removeEventListener: vi.fn(),
        dispatchEvent: vi.fn(),
    })),
    });
};

beforeEach(() =>
    {
    window.scrollTo = vi.fn();
    window.HTMLDivElement.prototype.scrollIntoView = vi.fn();
    });

describe("isMobile function", () =>
    {
    afterEach(cleanup);

    test("Activates isMobile", async () =>
    {
        createMatchMedia("500px");
        const appOne = render(<App />)
        createMatchMedia("1000px");
        const appTwo = render(<App />)
        expect(appOne).not.toBe(appTwo);
    });
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

test("Should navigate to home page when link is clicked", async () => {
    const rendered = render(
        <Router>
            <Header />
        </Router>,
    );
    const link = rendered.getByText("Mina On-Chain Signals");
    fireEvent.click(link);
    expect(rendered.getByText("Mina On-Chain Signals").closest("a"))
        .toHaveAttribute("href", "");
});

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
