module.exports = {
  extends: ['../.eslintrc.js'],
  parserOptions: {
    root: true,
    tsconfigRootDir: __dirname,
    project: ['./tsconfig.json'],
  },
  rules: {
    'simple-import-sort/imports': [
      'warn',
      {
        groups: [
          ['^react'],
          ['^next'],
          ['^@mui|@emotion|@storybook'],
          ['^common'],
          ['^components'],
          ['reflect-metadata$'],
          ['^\\w'],
          ['^@\\w'],
          ['^\\..\\/'],
          ['^\\.\\/'],
        ],
      },
    ],
  },
};
