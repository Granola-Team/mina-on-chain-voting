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
