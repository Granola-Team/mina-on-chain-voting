import "@testing-library/jest-dom";
import { render } from "@testing-library/react";
import { BrowserRouter as Router } from "react-router-dom";
import { Keyword } from "./Keyword";
import { ReactQueryClient, QueryClientProvider } from "@/query";

describe("<Keyword />", () => {
  test("should render", () => {
    render(
      <QueryClientProvider client={ReactQueryClient}>
        <Router>
          <Keyword />
        </Router>
      </QueryClientProvider>,
    );
  });
});
