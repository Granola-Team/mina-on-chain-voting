type Status = "Pending" | "Canonical";
type VoteEntry = {memo: string, height: number, status: Status}
type AccountEntry = {account: string, votes: VoteEntry[]}
type VoteCheckResult = "for" | "against" | "invalid"