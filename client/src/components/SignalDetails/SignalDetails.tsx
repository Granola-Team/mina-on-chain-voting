import { AccountEntry, VoteCheckResult, VoteEntry } from "../../../types"
import SignalTable from "../SignalTable"
import {Collapse} from 'react-collapse';
import { useState } from "react";

interface DetailsProps {
    discriminators: [string, ((entry: AccountEntry) => VoteEntry), boolean][]
    verifier: (vote: VoteEntry) => VoteCheckResult
    data: AccountEntry[]
}

const Details: React.FC<DetailsProps> = ({
    discriminators, verifier, data
}) => {
    const [open, setOpen] = useState(false);
    return (
        <div>
            <h1 style={{ color: '#EEF5DB', textAlign: 'center' }}>
                Signalling Details
            </h1>

            <div style={{ display: 'flex', flexDirection: 'column' }}>
                { discriminators.map(([name, discriminator, collapse], index) =>
                    <div
                        style={{
                        backgroundColor: '#EEF5DB',
                        marginBottom: '2em',
                        padding: '1em',
                        borderRadius: '1em',
                        }}
                        key={index}
                    >
                        <div style={{display: 'flex', flexDirection: 'row', justifyContent: 'space-between'}}>
                            {collapse && <p>{name}</p>}
                            {collapse || <h2>{name}</h2>}
                            {collapse && <button onClick={() => setOpen(!open)}>Collapse</button>}
                        </div>
                        {collapse && 
                            <Collapse isOpened={open}>
                                {data && (
                                    <SignalTable
                                        accountDetails={data}
                                        votesDiscriminator={discriminator}
                                        isValidVote={verifier}
                                    />
                                )}
                            </Collapse>
                        }
                        {collapse || data &&
                            <SignalTable
                                accountDetails={data}
                                votesDiscriminator={discriminator}
                                isValidVote={verifier}
                            />
                        }
                    </div>
                )}
            </div>
        </div>
    )
}

export default Details