import { Link, Stack, Typography } from '@mui/material';

import { SectionLayout } from './SectionLayout';

export const IntroOverview = () => {
  return (
    <SectionLayout>
      <Typography fontSize={22} fontWeight={600}>
        Mina Improvement Proposals (MIPs)
      </Typography>
      <Stack>
        <Typography variant="subtitle2">
          Each dashboard provides instructions about how to vote on a specific MIP, as well as displaying how you have
          voted.
        </Typography>
        <Typography variant="subtitle2">
          You can vote on multiple MIPs during the same Voting Period; simply select the relevant dashboard for each
          MIP.
        </Typography>
        <Typography variant="subtitle2">
          Please also see this{' '}
          <Link
            href="https://youtu.be/sJxUy6cjXqg"
            target="_blank"
            rel="noreferrer"
            color="#FF6739"
          >
            video
          </Link>{' '}
          or read the section How do I vote? in the{' '}
          <Link
            href="https://forums.minaprotocol.com/t/on-chain-voting-frequently-asked-questions-faq/5959"
            target="_blank"
            rel="noreferrer"
            color="#FF6739"
          >
            FAQ
          </Link>
          .
        </Typography>
      </Stack>
    </SectionLayout>
  );
};
