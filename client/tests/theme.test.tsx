import "@testing-library/jest-dom";
import { vi } from "vitest";
import App from "../src/App";
import { cleanup, fireEvent, render } from "@testing-library/react";

afterEach(cleanup);

test("Renders with light mode default", () => {
  render(<App />);
  expect(document.body.getAttribute("class")).toBe("theme-light");
});

describe("Switches to dark mode, confirms it, then switches back to light mode", () => {
  beforeEach(() => {
  const mock = vi.fn();
  mock.mockReturnValue({
      observe: () => null,
      unobserve: () => null,
      disconnect: () => null,
  });
  window.IntersectionObserver = mock;
  });

  test("Switches to dark mode, confirms it, then switches back to light mode", () => {
    const rendered = render(<App />);
    expect(document.body.getAttribute("class")).toBe("theme-light");
    const button = rendered.getByTestId("settings-control-btn");
    fireEvent.click(button);
    const buttonTwo = rendered.getByTestId("toggle");
    fireEvent.click(buttonTwo);
    expect(document.body.getAttribute("class")).toBe("theme-dark");
    expect(document.documentElement.classList.contains("dark")).toBeTruthy();
    fireEvent.click(buttonTwo);
    expect(document.body.getAttribute("class")).toBe("theme-light");
    expect(document.documentElement.classList.contains("dark")).not.toBeTruthy();
  });
});
