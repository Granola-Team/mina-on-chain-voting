import { cleanup, render, screen, TestVariant } from 'common/test';

import { Providers } from 'components/providers';

describe(TestVariant.Component, () => {
  describe(Providers, () => {
    it('renders children inside component', () => {
      const child = 'Test content';

      render(
        <Providers>
          <div>{child}</div>
        </Providers>
      );

      expect(screen.getByText(child)).toBeInTheDocument();
      cleanup();
    });
  });
});
