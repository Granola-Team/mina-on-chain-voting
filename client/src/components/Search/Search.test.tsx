import "@testing-library/jest-dom";
import { render } from "@testing-library/react";
import { BrowserRouter as Router } from "react-router-dom";
import { Search } from "./Search";

describe("Search Tests", () => {
  test("should render", () => {
    expect(
      render(
        <Router>
          <Search />
        </Router>,
      ),
    ).toMatchSnapshot();
  });
});

/*
test("Search icon visible", async () => {
  const rendered = render(
          <Router>
              <SearchControl />
          </Router>
          );
  const button = rendered.getByTestId('SC-element');

    expect(button).toBeInTheDocument();
});


test("Click Search icon-button for modal pop-up, click search button to change url slug", async () => {
    const rendered = render(
        <Router>
            <SearchControl />
            <Search />
        </Router>
    );
    const button = rendered.getByTestId("search-control-button");
    expect(button).toBeInTheDocument();
    userEvent.click(button);
    expect(screen.getByText("What are you looking for?")).toBeInTheDocument();

    userEvent.type(screen.getByRole("textbox"), "test");
    userEvent.click(screen.getByText("Search"));
    expect(window.location.href).toContain("/test?network=Mainnet");
});
*/