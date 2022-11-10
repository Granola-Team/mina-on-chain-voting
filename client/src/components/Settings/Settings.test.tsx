import "@testing-library/jest-dom";
import { vi } from "vitest";
import { Settings } from "./Settings";
import { SettingsControl } from "./SettingsControl";
import { BrowserRouter as Router } from "react-router-dom";
import { SettingsDarkMode } from "./SettingsDarkMode";
import { SettingsButton } from "./SettingsButton";
import { SettingsNetwork } from "./SettingsNetwork";
import { cleanup, fireEvent, render } from "@testing-library/react";

afterEach(cleanup);

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

    test("Testing Settings toggle button changes theme", async () => {
        const rendered = render(
          <Router>
            <SettingsControl />
            <Settings />
            <SettingsDarkMode />
          </Router>,
        );
        const button = rendered.getByTestId("settings-control-btn");
        fireEvent.click(button);
        const buttonTwo = rendered.getByTestId("toggle");
        expect(buttonTwo).toBeInTheDocument();
        fireEvent.click(buttonTwo);
        expect(rendered.getByTestId("theme-change").closest("div"))
            .toHaveAttribute("class", "rounded-full p-[2px] bg-gradient animate-gradient");
    });
});

test("SettingsNetwork is rendered", async () => {
  const search = render(
    <Router>
      <SettingsNetwork />
    </Router>,
  );
  expect(search).toMatchSnapshot();
});

test("SettingsButton is rendered", async () => {
  const search = render(
    <Router>
      <SettingsButton title={"Devnet"} />
    </Router>,
  );
  expect(search).toMatchSnapshot();
});
