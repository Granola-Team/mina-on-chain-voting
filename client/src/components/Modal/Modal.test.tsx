import { expect } from "vitest";
import "@testing-library/jest-dom";
import { Modal } from "./Modal";
// import type { ModalProps } from "@/types";
import { cleanup, render } from "@testing-library/react";
afterEach(cleanup);

const createTestProps = (props?: object): any => ({
    ...props,
});

// fixing Error: A <Transition /> is used but it is missing a `show={true | false}` prop.
const renderTest = () => {
    // const [state, setState] = useState(false)
    const props = createTestProps();
    const { getByTestId } = render(
        <Modal {...props}>
            <div data-testid="child" />
        </Modal>
    );
    const container = getByTestId("modal-container");
    return {
        getByTestId,
        container
    };
};

describe("ModalContainer", () => {

    describe("rendering", () => {
    test("a container", () => {
        const { container } = renderTest();
        expect(container).toHaveStyle(`
            boxSizing: 'border-box';
            display: flex;
            flexWrap: 'wrap';
            width: '100%';
        `);
    });

    test("children are passed through", () => {
        const { container, getByTestId } = renderTest();
        expect(container.children.length).toBe(1);
        expect(getByTestId("child")).toBeDefined();
    });

    });
});

/*
test("Clicking search icon should open modal, which implies state change for both SearchControl and Search", async () => {
    const { container } = render(
        <Router>
            <Layout>
                <Header />
                <Footer />
            </Layout>
        </Router>);
    const button = container.querySelector("[xmlns='http://www.w3.org/2000/svg']");
    expect(button).toBeInTheDocument();
    fireEvent.click(button!); // the ! transforms type Element | null back to Element
    expect(screen.getByText("What are you looking for?")).toBeInTheDocument();
});

test("children are passed through the modal component", async () => {
    const rendered = render(<Modal />);

    const link = rendered.getByText("GitHub");

    fireEvent.click(link);

    expect(screen.getByText("GitHub").closest("a"))
        .toHaveAttribute("href", "https://github.com/Granola-Team/onchain-signalling");
});
*/
