import { expect, vi } from "vitest";
import { screen, fireEvent, render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Settings } from "./Settings";
import { SettingsControl } from "./SettingsControl";
import userEvent from "@testing-library/user-event";
import { BrowserRouter as Router } from "react-router-dom";

// testings the SettingsButton & SettingsDarkMode will have the same logic as the SettingsControl below
// on the static IP site, SettingsNetwork works when changing the slug but not from the site itself since
// there does not appear an area in the modal to change networks but there the toggle to switch to dark mode
// is there

// the last two test below are the same style as search, so same errors here
// --------->>>>>>>>>>>>>>> first for SettingsControl then for Settings
test("State should change for Search", async () => {
    const setStateMock = vi.fn();
    const useStateMock: any = (useState: any) => [useState, setStateMock];
    vi.spyOn(React, "useState").mockImplementation(useStateMock);
    render(<Router><SettingsControl /></Router>);
    const BtnElement = screen.getByRole("button");
    //, {name: "Button", hidden: true
    expect(BtnElement).toBeInTheDocument();
    fireEvent.click(BtnElement);
    // console.log(BtnElement)
    expect(setStateMock).toHaveBeenCalled();
});

test("Clicking the Search button triggers the onSubmit function and change in url slug", async () => {
    const setStateMock = vi.fn();
    const useStateMock: any = (useState: any) => [useState, setStateMock];
    vi.spyOn(React, "useState").mockImplementation(useStateMock);
    render(<Router><Settings /></Router>);
    userEvent.type(screen.getByRole("textbox"), ""); // enter test in placeholder
    userEvent.click(screen.getByText("Search")); // press Search button
    expect(setStateMock).toHaveBeenCalled();
    // I need to mock window object
});