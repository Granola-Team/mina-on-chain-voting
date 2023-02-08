export interface ExampleAtomProps {
  default?: boolean;
}

export const ExampleAtom = ({ default: _default }: ExampleAtomProps) => {
  return <div>ExampleAtom!</div>;
};
