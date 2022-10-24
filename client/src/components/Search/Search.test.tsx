import { expect, vi } from "vitest";
import { screen, fireEvent, render, getByAltText } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Search } from "./Search";
import { SearchControl } from "./SearchControl";
import React, { useState } from "react";
import userEvent from "@testing-library/user-event";
import { BrowserRouter as Router } from "react-router-dom";
import { Modal } from "../Modal";
import { Layout } from "../Layout/Layout";
import { Header } from "../Header/Header";
import { Footer } from "../Footer/Footer";

test("Clicking search icon should open modal, which implies state change for both SearchControl and Search", async () => {
    const { container } = render(
        <Router>
            <Layout>
                <Header />
                <Footer />
            </Layout>
        </Router>);
    const button = container.querySelector("[xmlns='http://www.w3.org/2000/svg']");
    expect(button).toBeInTheDocument();
    fireEvent.click(button!); // the ! transforms type Element | null back to Element
    expect(screen.getByText("What are you looking for?")).toBeInTheDocument();
});

/*
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

// older tests

test("State should change for Search", async () => {
    const setStateMock = vi.fn();
    const useStateMock: any = (useState: any) => [useState, setStateMock];
    vi.spyOn(React, "useState").mockImplementation(useStateMock);
    render(<Router><SearchControl /></Router>);
    const BtnElement = screen.getByRole("button");
    expect(BtnElement).toBeInTheDocument();
    fireEvent.click(BtnElement);
    // console.log(BtnElement)
    expect(setStateMock).toHaveBeenCalled();
});

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
*/
