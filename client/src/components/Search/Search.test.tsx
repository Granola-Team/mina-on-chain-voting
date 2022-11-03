import "@testing-library/jest-dom";
import { fireEvent, render } from "@testing-library/react";
import { BrowserRouter as Router } from "react-router-dom";
import { Search } from "./Search";
import { SearchControl } from "./SearchControl";
import { useAppStore } from "@/App.store";
import shallow from "zustand/shallow";
import { immer } from 'zustand/middleware/immer'
import { useStore } from "zustand";
import create from "zustand";

describe("unit tests", () => {
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

describe("integration tests", () => {
  const originalState = useAppStore.getState();
  beforeEach(() => {
    useAppStore.setState(originalState);
  });

  vi.mock("@/App.store");
  const mockState = {
    darkMode: false,
    settingsActive: false,
    searchActive: false,
    isLoading: false,
    setDarkMode: (v: boolean) => {
      () => ({ darkMode: v });
    },
    setSettingsState: (v: boolean) => {
      () => ({ settingsActive: v });
    },
    setSearchState: (v: boolean) => {
      () => ({ searchActive: v });
    },
  };
  useAppStore.getState = () => mockState;
  test("state changes to true", () => {
    expect(SearchControl()).toBeTruthy();
  });
});

/*
});
    test("handles onClick", () => {
      const rendered = render(
        <Router>
          <SearchControl />
        </Router>
      );
          const btnElement = rendered.getByRole("button");
    fireEvent.click(btnElement);
    expect(btnElement).toBe(!searchActive);

      expect(rendered.getByTestId("search-control-button"))
        .toHaveAttribute("style", `margin-left: ${percentage}%;`);
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
