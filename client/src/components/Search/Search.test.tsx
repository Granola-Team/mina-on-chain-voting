import { expect } from "vitest";
import { screen, fireEvent, render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Search } from "./Search";
import { SearchControl } from "./SearchControl";
import React from "react";
import userEvent from "@testing-library/user-event";
import { BrowserRouter as Router } from "react-router-dom";

test("State should change for SearchControl", async () => {
    const setStateMock = jest.fn();
    const useStateMock: any = (useState: any) => [useState, setStateMock];
    jest.spyOn(React, "useState").mockImplementation(useStateMock);
    render(<SearchControl />);
    const btnElement = screen.getByRole("button");
    expect(btnElement).toBeInTheDocument();
    fireEvent.click(btnElement);
    expect(setStateMock).toHaveBeenCalledWith("!searchActive");
});

test("Clicking the Search button triggers the onSubmit function and change in url slug", async () => {
    render(<Router><SearchControl /></Router>);
    const Btn = screen.getByRole("button");
    fireEvent.click(Btn);
    expect(render(<Router><Search /></Router>));
    userEvent.type(screen.getByRole("dialog"), "test");
    // console.log(screen);
    userEvent.click(screen.getByText("Search"));
    // above should grab the input tag and write test
    expect(Search.find("Modal").prop("setState")).toBe(false);
    expect(global.window.location.pathname).toContain("/test?network=Mainnet");
});
