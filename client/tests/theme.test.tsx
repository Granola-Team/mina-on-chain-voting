import "@testing-library/jest-dom";
import { fireEvent, render } from "@testing-library/react";
import { vi } from "vitest";
import App from "../src/App";

// this is the template and I'm tweaking to get it to work
test("renders with light mode default", () => {
    const view = render(<App />);
    expect(view.getByRole("class", { theme-light })).toBeInTheDocument();
    expect(getByTestId("header")).toHaveStyle("background-color: white");
    const toggleBtn = getByTestId("toggle-theme-btn");
    fireEvent.click(toggleBtn);
    expect(getByTestId("header")).toHaveStyle("background-color: black");
    fireEvent.click(toggleBtn);
    expect(getByTestId("header")).toHaveStyle("background-color: white");
  });
