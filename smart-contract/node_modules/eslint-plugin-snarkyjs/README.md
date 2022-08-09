# eslint-plugin-snarkyjs

SnarkyJS rules for ESLint to detect bugs and invalid patterns in your Smart Contract code.

# Rules

- The maximum allowed state variables in a Smart Contract is 8
- Throw statements should not be used in a Circuit method
- If statements should not be used in a Circuit method
- Ternary statements should not be used in a Circuit method
- JavaScript JSON functions should not be used in a Circuit method
- JavaScript random functions should not be used in a Circuit method
- Overriding the constructor in a SmartContract is disallowed

# Installation

```
# npm
npm install eslint-plugin-snarkyjs --save-dev

# yarn
yarn add eslint-plugin-snarkyjs --dev
```

Add `eslint-plugin-snarkyjs` to the `plugins` option in your eslint config:

```
{
  "plugins": ["snarkyjs"]
}
```

- Then extend the recommended eslint config:

```
{
  extends: ['plugin:snarkyjs/recommended'],
}
```

- You can enable specific rules manually:

```
{
  "rules": {
    "snarkyjs/no-greater-storage-limit-in-circuit": "error",
    "snarkyjs/no-throw-in-circuit": "error"
    ...
  }
}
```

- An example ESLint configuration looks like:

```
module.exports = {
  parser: '@typescript-eslint/parser',
  plugins: ['snarkyjs'],
  extends: ['plugin:snarkyjs/recommended'],
};

```

# Valid and Invalid Examples

Please refer to the project tests to learn more about the usage of the rules.
