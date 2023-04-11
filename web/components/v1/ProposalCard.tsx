import { Chip, Stack, Typography } from '@mui/material';

import { proposalIdAtom } from 'common/store';

import { useTheme } from 'components/provider';

import { useAtom } from 'jotai';
import type { ProposalListParserOutcome } from 'models';
import moment from 'moment';

import { Link } from './Link';
import { SectionLayout } from './SectionLayout';

export type ProposalCardProps = {
  proposal: ProposalListParserOutcome[0];
};

export const ProposalCard = ({ proposal }: ProposalCardProps) => {
  const { theme } = useTheme();
  const [_, setId] = useAtom(proposalIdAtom);

  const now = moment(new Date()).utc();
  const startDate = moment(new Date(proposal.start_time)).utc();
  const endDate = moment(new Date(proposal.end_time)).utc();

  const isDone = now.isAfter(endDate);
  const hasNotStarted = now.isBefore(startDate);
  const inProgress = !isDone && !hasNotStarted;

  const isTBD = proposal.start_time === 2000000000000 && proposal.end_time === 2000000000000;

  return (
    <Link
      href={isDone ? `/proposal/${proposal.id}/results` : `/proposal/${proposal.id}`}
      onClick={() => {
        setId(proposal.id);
      }}
    >
      <SectionLayout
        justifyContent="space-between"
        sx={{
          minHeight: 130,
          ':hover': {
            backgroundColor: theme.key === 'dark' ? '#1C1C1C' : '#e8e8e8',
            transition: 'ease-in-out background 0.075s',
          },
        }}
      >
        <Stack direction="row" justifyContent="space-between" alignItems="center">
          <Typography fontSize={15} fontWeight={600}>
            {proposal.title ? `${proposal.key}: ${proposal.title}` : proposal.key}
          </Typography>

          {isDone && <Chip label="Completed" variant="outlined" color="success" size="small" />}
          {hasNotStarted && <Chip label="Not Started" variant="outlined" color="info" size="small" />}
          {inProgress && <Chip label="In Progress" variant="outlined" color="warning" size="small" />}
        </Stack>
        <Stack direction="row" justifyContent="space-between" alignItems="center">
          <Typography fontSize={12} fontWeight={500}>
            {proposal.description ? proposal.description : 'No description provided.'}
          </Typography>
        </Stack>

        <Stack direction="row" justifyContent="space-between" alignItems="center">
          <Stack>
            <Typography variant="body2" fontSize={13} fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="left">
              Start Date
            </Typography>
            <Typography variant="body2" fontSize={12} fontWeight={600}>
              {isTBD ? 'TBD' : `${startDate.format('YYYY-MM-DD - hh:mm').toString()} UTC`}
            </Typography>
          </Stack>
          <Stack>
            <Typography variant="body2" fontSize={13} fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="right">
              End Date
            </Typography>
            <Typography variant="body2" fontSize={12} fontWeight={600}>
              {isTBD ? 'TBD' : `${endDate.format('YYYY-MM-DD - hh:mm').toString()} UTC`}
            </Typography>
          </Stack>
        </Stack>
      </SectionLayout>
    </Link>
  );
};
