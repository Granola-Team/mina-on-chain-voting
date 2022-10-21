import { expect } from "vitest";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Header } from "../Header/Header";
import { Search } from "../Search/Search";
import { Settings } from "../Settings/Settings";
import { SearchControl } from "../Search/SearchControl";
import { SettingsControl } from "../Settings/SettingsControl";
import { BrowserRouter as Router } from "react-router-dom";
import { useMediaQuery } from "@react-hook/media-query";

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
    expect(header.getByRole("heading", { level: 1 })
        .innerHTML.toBe("Mina On-Chain Signals");
    // I know the last line needs fixing
});

// I know this doesn't work yet
// should test the state of the header based on screen size
test("State based on screen size", async () => {
    const isMobileMock = vi.fn();
    const isMobileMock: useMediaQuery("only screen and (max-width: 768px)");
    vi.spyOn(React, "useMediaQuery").mockImplementation(isMobileMock);
    if (isMobileMock) {
        return
        <div>
        <div>
            <div>
            <SearchControl />
            <SettingsControl />
          </div>
        </div>
        <Search />
        <Settings />
        </div>
    }
    return (
        <div>
        <div>
                <SearchControl />
            <div>
            <SettingsControl />
          </div>
        </div>
        <Search />
        <Settings />
        </div>
    )
});
