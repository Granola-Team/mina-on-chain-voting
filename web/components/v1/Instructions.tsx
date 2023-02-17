import { Link, Stack, Typography } from '@mui/material';

import { SectionLayout } from './SectionLayout';

export type InstructionsProps = {
  keyword: string;
};

export const Instructions = ({ keyword }: InstructionsProps) => {
  console.log('Hello');
  return (
    <SectionLayout>
      <Typography fontSize={22} fontWeight={600}>
        Instructions
      </Typography>
      <Stack>
        <Typography variant="subtitle2">Send yourself a transaction with the keyword(s) in the memo field:</Typography>
        <Typography variant="subtitle2" ml={1}>
          · To vote in favor of the proposal, enter in the memo field &apos;{keyword}&apos;
        </Typography>
        <Typography variant="subtitle2" ml={1}>
          · To vote against of the proposal, enter in the memo field &apos;no {keyword}&apos;
        </Typography>
      </Stack>

      <Stack>
        <Typography variant="subtitle2">
          Refresh the dashboard, your vote should appear in the dashboard within 10-15 minutes
        </Typography>
        <Typography variant="subtitle2">
          A vote will first appear in a pending state. After ten blocks have been confirmed the voting status will be
          marked as Canonical where appropriate.
        </Typography>
      </Stack>

      <Stack>
        <Typography variant="subtitle2">
          See Frequently Asked Questions{' '}
          <Link
            href="https://forums.minaprotocol.com/t/on-chain-voting-frequently-asked-questions-faq/5959"
            target="_blank"
            rel="noreferrer"
            color="#FF6739"
          >
            (FAQs)
          </Link>{' '}
          for more information, including how to send yourself a transaction
        </Typography>
        <Typography variant="subtitle2">
          If you find an issue, a bug or simply have a question, please feel free to write us some{' '}
          <Link
            href="https://github.com/Granola-Team/blog/blob/main/voting-results-instructions.md"
            target="_blank"
            rel="noreferrer"
            color="#FF6739"
          >
            feedback
          </Link>
          . We would love to hear your thoughts!
        </Typography>
      </Stack>
    </SectionLayout>
  );
};
