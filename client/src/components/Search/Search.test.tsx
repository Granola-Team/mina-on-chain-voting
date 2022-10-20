import { expect, vi } from "vitest";
import { screen, fireEvent, render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Search } from "./Search";
import React from "react";
import userEvent from "@testing-library/user-event";
import { BrowserRouter as Router } from "react-router-dom";

test("State should change for Search", async () => {
    const setStateMock = vi.fn();
    const useStateMock: any = (useState: any) => [useState, setStateMock];
    vi.spyOn(React, "useState").mockImplementation(useStateMock);
    render(<Router><Search /></Router>);
    const btnElement = screen.getByRole("Modal");
    expect(btnElement).toBeInTheDocument();
    fireEvent.click(btnElement);
    expect(setStateMock).toBe("!searchActive");
});

test("Clicking the Search button triggers the onSubmit function and change in url slug", async () => {
    render(<Router><Search /></Router>);
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
