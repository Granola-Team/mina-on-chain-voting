import { expect } from "vitest";
import { render } from "@testing-library/react";
import { BrowserRouter as Router } from "react-router-dom";
import { Layout } from "./Layout";
import { Header } from "../Header";
import { Footer } from "../Footer";

test("Header is rendered", async () => {
  const header = render(
    <Router>
      <Header />
    </Router>,
  );
  expect(header).toMatchSnapshot();
});

test("Footer is rendered", async () => {
  const footer = render(
    <Router>
      <Footer />
    </Router>,
  );
  expect(footer).toMatchSnapshot();
});

test("Layout is rendered", async () => {
  const layout = render(
    <Router>
      <Layout>
        <Header />
        <Footer />
      </Layout>
    </Router>,
  );
  expect(layout).toMatchSnapshot();
});
