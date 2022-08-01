import { AccountEntry, VoteEntry, VoteCheckResult } from '../../../types';
import SignalTableAccountEntry from './SignalTableAccountEntry';
import React from 'react';
import {Collapse} from 'react-collapse';

interface VotingDetailsProps {
  votes: VoteEntry[]; 
  isValidVote: (vote: VoteEntry) => VoteCheckResult;
  showsSignal: boolean
}

const SignalTable: React.FC<VotingDetailsProps> = ({
  votes,
  isValidVote,
  showsSignal
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
          { showsSignal && <th style={{ textAlign: 'left' }}>Signal</th> }
        </tr>
      </thead>
      <tbody>
        {votes.map((vote, index) => (
          <SignalTableAccountEntry
            key={index}
            account={vote.account}
            memo={vote.memo.substring(0,12)}
            validity={isValidVote(vote)} 
          />
        ))}
      </tbody>
    </table>
  );
};
// could've made validity a ternary operator by adding //  == 'invalid' ? '' : isValidVote(vote)      
export default SignalTable;
