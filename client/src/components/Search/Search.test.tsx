import { expect, vi } from "vitest";
import { screen, fireEvent, render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Search } from "./Search";
import { SearchControl } from "./SearchControl";
import userEvent from "@testing-library/user-event";
import { BrowserRouter as Router } from "react-router-dom";
import { changeTheme } from "@/utils/theme";
import { Modal } from "../Modal";


test("State should change for Search", async () => {
    const setStateMock = vi.fn();
    const useStateMock: any = (useState: any) => [useState, setStateMock];
    vi.spyOn(React, "useState").mockImplementation(useStateMock);
    render(<Router><SearchControl /></Router>);
    const BtnElement = screen.getByRole("button");
    //, {name: "Button", hidden: true
    expect(BtnElement).toBeInTheDocument();
    fireEvent.click(BtnElement);
    // console.log(BtnElement)
    expect(setStateMock).toHaveBeenCalled();
});

// on the static IP site, Search button works but I know we disabled Advanced Search button which appears still
test("Clicking the Search button triggers the onSubmit function and change in url slug", async () => {
    const setStateMock = vi.fn();
    const useStateMock: any = (useState: any) => [useState, setStateMock];
    vi.spyOn(React, "useState").mockImplementation(useStateMock);
    render(<Router><Search /></Router>);
    userEvent.type(screen.getByRole("textbox"), ""); // enter test in placeholder
    userEvent.click(screen.getByText("Search")); // press Search button
    expect(setStateMock).toHaveBeenCalled();
    // I need to mock window object
});

/*
test("should call stateChange callback", () => {
    const setStateMock = vi.fn();
    const useStateMock: any = (useState: any) => [useState, setStateMock];
    const { getByTestId } = render(
        <Router><SearchControl setSearchState={setStateMock} /></Router>
    );
    fireEvent.click(getByTestId("button"));
    expect(setStateMock).toHaveBeenCalled();
  });
*/
