<h1 align="center">Mina Governance Web</h1>

<p align="center">
  <b>A NextJS based frontend for the Mina Governance Dashboard.</b>
</p>

### Prerequisites

- Node.js >=16
- pnpm (recommended for package management)

### Getting started

Build for production:

```bash
pnpm run build
```

Production mode:

```bash
pnpm run start
```

Development mode:

```bash
pnpm run dev
```

## Storybook

Storybook is a tool for developing and testing UI components in isolation.

### Usage

To start the Storybook server, run the following command:

```bash
pnpm run storybook
```

Storybook will be available at http://localhost:6006.

## Component Generator

This script allows you to generate a component with the following template files:

- `${COMPONENT_NAME}.tsx`: contains the base component
- `${COMPONENT_NAME}.stories.tsx`: contains the base Storybook stories
- `${COMPONENT_NAME}.spec.tsx`: contains the base unit tests

### Usage

You can run the generator with:

```bash
pnpm generate-component [COMPONENT_NAME]
```

### Notes

- The component name must be in PascalCase (e.g. MyComponent, Button).
- The script will not overwrite existing directories with the same name as the component.
- The ${COMPONENT_NAME} placeholder in the template files will be replaced with the actual component name.
- The component will be created in the respective component type folder. (Atoms, Molecules & Organisms).

### Example

```bash
pnpm generate-component MyButton
```

This will generate the following files and directory structure:

    MyButton/
    ├── MyButton.spec.tsx
    ├── MyButton.stories.tsx
    └── MyButton.tsx
