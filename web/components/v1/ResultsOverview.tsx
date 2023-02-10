import { Link, Stack, Typography } from '@mui/material';

import { SectionLayout } from './SectionLayout';

export const ResultsOverview = () => {
  return (
    <SectionLayout>
      <Typography fontSize={22} fontWeight={600}>
        Results Overview
      </Typography>
      <Stack>
        <Typography variant="subtitle2">
          Please read the{' '}
          <Link
            href="https://forums.minaprotocol.com/t/on-chain-voting-frequently-asked-questions-faq/5959"
            target="_blank"
            rel="noreferrer"
          >
            FAQ
          </Link>{' '}
          to understand how the results were calculated.
        </Typography>
        <Typography variant="subtitle2">
          If an account delegates and then also votes (having delegated), thehn this direct vote has 0 weight since the
          account has already delegated its stake.
        </Typography>
        <Typography variant="subtitle2">
          This 0 weighted vote will not be counted in the results. &quot;Stake Delegated&quot; will display in the
          Weighted Stake and Weighted Stake % column in this scenario.
        </Typography>
      </Stack>

      <Stack>
        <Typography variant="subtitle2">
          The results can be verified on chain. Please read these{' '}
          <Link
            href="https://github.com/Granola-Team/blog/blob/main/voting-results-instructions.md"
            target="_blank"
            rel="noreferrer"
          >
            instructions
          </Link>{' '}
          for doing so.
        </Typography>
        <Typography variant="subtitle2">
          If you find an issue, a bug or simply have a question, please feel free to write us some{' '}
          <Link
            href="https://github.com/Granola-Team/blog/blob/main/voting-results-instructions.md"
            target="_blank"
            rel="noreferrer"
          >
            feedback
          </Link>
          . We would love to hear your thoughts!
        </Typography>
      </Stack>
    </SectionLayout>
  );
};
