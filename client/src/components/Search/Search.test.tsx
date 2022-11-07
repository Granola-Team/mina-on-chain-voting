import "@testing-library/jest-dom";
import { fireEvent, render } from "@testing-library/react";
import { BrowserRouter as Router } from "react-router-dom";
import { Search } from "./Search";
import { SearchControl } from "./SearchControl";
import { vi } from "vitest";

describe("Search Tests", () => {
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
          <Search />
        </Router>,
      ),
    ).toMatchSnapshot();
  });

  test("Search icon visible", async () => {
    const rendered = render(
      <Router>
        <SearchControl />
      </Router>,
    );
    const button = rendered.getByTestId("search-control-btn");
    expect(button).toBeInTheDocument();
  });

  test("Click Search icon-button for modal pop-up", async () => {
    const rendered = render(
      <Router>
        <SearchControl />
        <Search />
      </Router>,
    );
    const button = rendered.getByTestId("search-control-btn");
    expect(button).toBeInTheDocument();
    fireEvent.click(button);
    expect(rendered.getByText("What are you looking for?")).toBeInTheDocument();
  });
});
