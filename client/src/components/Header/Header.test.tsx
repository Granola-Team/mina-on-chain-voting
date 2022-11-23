import "@testing-library/jest-dom";
import { act, cleanup, fireEvent, render } from "@testing-library/react";
import { expect } from "vitest";
import { BrowserRouter as Router } from "react-router-dom";
import { Settings } from "../Settings/Settings";
import { Header } from "../Header/Header";
import { Search } from "../Search/Search";

const resize = (x: number, y: number) => {
  window.innerWidth = x;
  window.innerHeight = y;
  act(() => {
    window.dispatchEvent(new Event("resize"));
  });
};

describe("isMobile function", () => {
  afterEach(cleanup);
  test("Activates isMobile", () => {
    resize(2800, 1080);
    const appOne = render(
      <Router>
        <Header />
      </Router>,
    );
    expect(appOne.getByTestId("header-container")).toHaveAttribute(
      "class",
      "flex items-center justify-between px-5 py-4 lg:px-10",
    );
  });
});

test("Header renders title", async () => {
  const header = render(
    <Router>
      <Header />
    </Router>,
  );
  expect(header).toMatchSnapshot();
  const title = header.getByRole("heading", { level: 1 }).innerHTML;
  expect(title).toContain("Mina On-Chain Votes");
});

test("Should navigate to home page when link is clicked", async () => {
  const rendered = render(
    <Router>
      <Header />
    </Router>,
  );
  const link = rendered.getByText("Mina On-Chain Votes");
  fireEvent.click(link);
  expect(
    rendered.getByText("Mina On-Chain Votes").closest("a"),
  ).toHaveAttribute("href", "");
});

test("Search is rendered", async () => {
  const search = render(
    <Router>
      <Search />
    </Router>,
  );
  expect(search).toMatchSnapshot();
});

test("Settings is rendered", async () => {
  const settings = render(
    <Router>
      <Settings />
    </Router>,
  );
  expect(settings).toMatchSnapshot();
});
