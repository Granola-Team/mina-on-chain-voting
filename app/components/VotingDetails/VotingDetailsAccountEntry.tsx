interface VotingDetailsAccountEntryProps {
    account: string,
    memo: string,
    validity: VoteCheckResult
}

import React from 'react'

const VotingDetailsAccountEntry: React.FC<VotingDetailsAccountEntryProps> = ({
    account, memo, validity
}) => {
    return (
        <tr key={account}>
            <td style={{padding: '0em 1.5em 0.5em 0em'}}>{account}</td>
            <td style={{padding: '0em 1.5em 0em 0em'}}>{memo}</td>
            <td>
                {validity == "for" && "For"}
                {validity == "against" && "Against"}
                {validity == "invalid" && "Invalid"}
            </td>
        </tr>
    )
}

export default VotingDetailsAccountEntry