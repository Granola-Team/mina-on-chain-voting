import { expect } from "vitest";
import { screen, fireEvent, render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Search } from "./Search";
import { SearchControl } from "./SearchControl";
import React from "react";

test("Search should change state", async () => {
    const setStateMock = jest.fn();
    const useStateMock: any = (useState: any) => [useState, setStateMock];
    jest.spyOn(React, "useState").mockImplementation(useStateMock);
    render(<SearchControl />);
    const btnElement = screen.getByRole("button");
    expect(btnElement).toBeInTheDocument();
    fireEvent.click(btnElement);
    expect(setStateMock).toHaveBeenCalledWith("!searchActive");
});
