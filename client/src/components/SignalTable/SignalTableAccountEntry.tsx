import { VoteCheckResult } from '../../../types';
import React from 'react';

interface SignalTableAccountEntryProps {
  account: string;
  memo: string;
  validity: VoteCheckResult;
}

const SignalTableAccountEntry: React.FC<SignalTableAccountEntryProps> = ({
  account,
  memo,
  validity,
}) => {
  return (
    <tr key={account}>
      <td style={{}}>{account}</td>
      <td
        style={{
          textAlign: 'left',
          paddingRight: '0.5rem',
          paddingLeft: '0.5rem',
        }}
      >
        {memo}
      </td>
      <td style={{ textAlign: 'left' }}>
        {validity === 'for' && 'For'}
        {validity === 'against' && 'Against'} 
      </td>
    </tr>
  );
};
// deleted   {validity === 'invalid' && 'Invalid'}
export default SignalTableAccountEntry;
