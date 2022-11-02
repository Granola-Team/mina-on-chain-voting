import "@testing-library/jest-dom";
import { fireEvent, render } from "@testing-library/react";
import { BrowserRouter as Router } from "react-router-dom";
import { Search } from "./Search";
import { SearchControl } from "./SearchControl";
import { useAppStore } from "@/App.store";
import shallow from "zustand/shallow";

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

  test("handles onClick", async () => {
    const { searchActive, setSearchState } = useAppStore(
      (state) => ({
        searchActive: state.searchActive,
        setSearchState: state.setSearchState,
      }),
      shallow,
    );
    const rendered = render(
      <Router>
        <SearchControl />
      </Router>
    );
    const btnElement = rendered.getByRole("button");
    fireEvent.click(btnElement);
    expect(btnElement).toBe(!searchActive);
    });
/*
    test("testing state change", async () => {
    const setStateMock = vi.fn();
    const useStateMock: any = (useState: any) => [useState, setStateMock];
    vi.spyOn(React, "useState").mockImplementation(useStateMock);
    const rendered = render(
      <Router>
        <Layout>
          <TableNavigation />
        </Layout>
      </Router>,
    );
    const SettledCheck = rendered.getByRole("button", { name: "Settled" });
    expect(SettledCheck).toBeInTheDocument();
    fireEvent.click(SettledCheck);
    expect(setStateMock).toHaveBeenCalled();
  });
  */
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
