import { AccountEntry, VoteEntry, VoteCheckResult } from '../../../types';
import SignalTableAccountEntry from './SignalTableAccountEntry';
import React from 'react';
import {Collapse} from 'react-collapse';

interface VotingDetailsProps {
  accountDetails: AccountEntry[];
  votesDiscriminator: (entry: AccountEntry) => VoteEntry | null;
  isValidVote: (vote: VoteEntry) => VoteCheckResult;
}

const SignalTable: React.FC<VotingDetailsProps> = ({
  accountDetails,
  votesDiscriminator,
  isValidVote,
}) => {
  const votes = accountDetails
    .map((accountEntry) => [accountEntry, votesDiscriminator(accountEntry)])
    .filter((entry): entry is [AccountEntry, VoteEntry] => entry[1] !== null);

  return (
    <table style={{ borderSpacing: '1rem 0.5rem' }}>
      <thead>
        <tr>
          <th>Account</th>
          <th>Memo</th>
          <th>Signal</th>
        </tr>
      </thead>
      <tbody>
        {votes.map(([accountEntry, vote], index) => (
          <SignalTableAccountEntry
            key={index}
            account={accountEntry.account}
            memo={vote.memo}
            validity={isValidVote(vote)}
          />
        ))}
      </tbody>
    </table>
  );
};

export default SignalTable;
