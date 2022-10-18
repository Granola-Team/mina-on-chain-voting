import { expect } from "vitest";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Header } from "./Header";
import { SearchControl } from "../Search/SearchControl";
import { SettingsControl } from "../Settings/SettingsControl";
import { Search } from "../Search/Search";
import { Settings } from "../Settings/Settings";
import { shallow, configure } from "enzyme";
import Adapter from "enzyme-adapter-react-16";

configure({adapter: new Adapter()});
test("SearchControl is rendered", async () => {
    const header = shallow(<Header><div><SearchControl /></div> </Header>);
    expect(header.getElements()).toMatchSnapshot();
});

test("renders Child components", async () => {
    const wrapper = shallow(<Header store={store} />);
    expect(wrapper.containsMatchingElement(<SearchControl />)).toEqual(true);
});
