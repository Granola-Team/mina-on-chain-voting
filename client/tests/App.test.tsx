import React from "react";
import { render } from "@testing-library/react";
import App from "../src/App";

describe("App", () => {
  test("should render", () => {
    render(<App />);
    expect(<App />).toMatchSnapshot();
  });
});
