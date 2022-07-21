interface VotingDetailsAccountEntryProps {
    account: string,
    memo: string,
    validity: VoteCheckResult
}

const VotingDetailsAccountEntry: React.FC<VotingDetailsAccountEntryProps> = ({
    account, memo, validity
}) => {
    return (
        <tr key={account}>
            <td>{account}</td>
            <td>{memo}</td>
            <td>
                {validity == "for" && "For"}
                {validity == "against" && "Against"}
                {validity == "invalid" && "Invalid"}
            </td>
        </tr>
    )
}

export default VotingDetailsAccountEntry