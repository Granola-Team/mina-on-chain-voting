import VotingDetailsAccountEntry from "./VotingDetailsAccountEntry"

interface VotingDetailsProps {
    accountDetails: AccountEntry[]
    votesDiscriminator: (entry: AccountEntry) => VoteEntry | null
    isValidVote: (vote: VoteEntry) => VoteCheckResult
}

const VotingDetails: React.FC<VotingDetailsProps> = ({ accountDetails, votesDiscriminator, isValidVote }) => {

    const votes = accountDetails
        .map((accountEntry) => [accountEntry, votesDiscriminator(accountEntry)])
        .filter((entry): entry is [AccountEntry, VoteEntry] => entry[1] !== null)

 return (
    <table>
        <thead>
            <tr>
                <th>Account Number</th>
                <th>Vote Memo</th>
                <th>Validity</th>
            </tr>
        </thead>
        <tbody>
            {votes.map(([accountEntry, vote]) => 
                <VotingDetailsAccountEntry 
                    account={accountEntry.account}
                    memo={vote.memo}
                    validity={isValidVote(vote)}
                />
            )}
        </tbody>
    </table>
 )
}

export default VotingDetails

