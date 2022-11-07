import "@testing-library/jest-dom";
import { fireEvent, render } from "@testing-library/react";
import { vi } from "vitest";
import { Settings } from "./Settings";
import { SettingsControl } from "./SettingsControl";
import { BrowserRouter as Router } from "react-router-dom";

describe("Settings and Theme Tests", () => {
    beforeEach(() => {
        const mock = vi.fn();

        mock.mockReturnValue({
            observe: () => null,
            unobserve: () => null,
            disconnect: () => null,
    });

        window.IntersectionObserver = mock;
    });

    test("should render", () => {
        expect(
            render(
                <Router>
                    <Settings />
                </Router>,
            ),
        ).toMatchSnapshot();
    });

    test("Search icon visible", async () => {
        const rendered = render(
          <Router>
            <SettingsControl />
          </Router>,
        );
        const button = rendered.getByTestId("settings-control-btn");
        expect(button).toBeInTheDocument();
    });

    test("Click Settings icon-button for modal pop-up", async () => {
        const rendered = render(
          <Router>
            <SettingsControl />
            <Settings />
          </Router>,
        );
        const button = rendered.getByTestId("settings-control-btn");
        expect(button).toBeInTheDocument();
        fireEvent.click(button);
        expect(rendered.getByText("OSC Controls")).toBeInTheDocument();
    });
});
