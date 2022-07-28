import { AccountEntry, VoteEntry, VoteCheckResult } from '../../../types';
import SignalTableAccountEntry from './SignalTableAccountEntry';
import React from 'react';
import {Collapse} from 'react-collapse';

interface VotingDetailsProps {
  votes: VoteEntry[];
  isValidVote: (vote: VoteEntry) => VoteCheckResult;
}

const SignalTable: React.FC<VotingDetailsProps> = ({
  votes,
  isValidVote,
}) => {
  // const votes = accountDetails
  //   .map((accountEntry) => [accountEntry, votesDiscriminator(accountEntry)])
  //   .filter((entry): entry is [AccountEntry, VoteEntry] => entry[1] !== null);

  return (
    <table style={{ borderSpacing: '1rem 0.5rem' }}>
      <thead>
        <tr>
          <th style={{ textAlign: 'center' }}>Account</th>
          <th style={{ textAlign: 'left' }}> &nbsp; Memo</th>
          <th style={{ textAlign: 'left' }}>Signal</th>
        </tr>
      </thead>
      <tbody>
        {votes.map((vote, index) => (
          <SignalTableAccountEntry
            key={index}
            account={vote.account}
            memo={vote.memo.substring(0,10)}
            validity={isValidVote(vote)}
          />
        ))}
      </tbody>
    </table>
  );
};

export default SignalTable;
