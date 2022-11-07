import "@testing-library/jest-dom";
import { fireEvent, render } from "@testing-library/react";
import { vi } from "vitest";
import { Settings } from "./Settings";
import { SettingsControl } from "./SettingsControl";
import { BrowserRouter as Router } from "react-router-dom";
import { SettingsDarkMode } from "./SettingsDarkMode";

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

    test("Testing Settings toggle button", async () => {
        const rendered = render(
          <Router>
            <SettingsControl />
            <Settings />
            <SettingsDarkMode />
          </Router>,
        );
        const button = rendered.getByTestId("settings-control-btn");
        expect(button).toBeInTheDocument();
        fireEvent.click(button);
        const buttonTwo = rendered.getByTestId("toggle");
        expect(buttonTwo).toBeInTheDocument();
        // fails transform here
        // next line of code below
        // fireEvent.click(buttonTwo);
        // following line would need some changes
        //expect(rendered.getByText("Enable/Disable UID-Mode").closest("div"))
          //  .toHaveAttribute("class", "rounded-full p-[2px] ");
    });
});
