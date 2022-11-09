import { expect } from "vitest";
import { act, cleanup, fireEvent, render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Header } from "../Header/Header";
import { Search } from "../Search/Search";
import { Settings } from "../Settings/Settings";
import { BrowserRouter as Router } from "react-router-dom";
import mediaQuery from "css-mediaquery";

/*function resizeTo(num: number) {
    window.innerWidth = num;
    window.dispatchEvent(new Event("resize"));
}
*/
const resize = (x: number, y: number) => {
    window.innerWidth = x;
    window.innerHeight = y;
    act(() => {
        window.dispatchEvent(new Event('resize'));
    });
};
describe("isMobile function", () => {
    afterEach(cleanup);
    test("Activates isMobile", () => {
        resize(2800, 1080);
        console.log(window.innerWidth);
        const appOne = render(
        <Router>
            <Header />
        </Router>
        );
        expect(appOne.getByTestId("header-container"))
            .toHaveAttribute("class", "flex items-center justify-between px-5 py-4 lg:px-10");
        resize(300, 500);
        console.log(window.innerWidth);
        const appTwo = render(
        <Router>
            <Header />
        </Router>
        );
        expect(appTwo.getByTestId("header-container"))
            .toHaveAttribute("class", "flex items-center justify-between py-4 px-3.5");
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
