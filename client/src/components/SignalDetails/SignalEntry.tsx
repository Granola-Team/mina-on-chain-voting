import { VoteCheckResult, VoteEntry } from "../../../types"
import SignalTable from "../SignalTable"
import { Collapse } from 'react-collapse'
import { useState } from "react"

interface SignalEntryProps {
    index: number,
    collapse: boolean,
    name: string,
    entries: VoteEntry[],
    verifier: (vote: VoteEntry) => VoteCheckResult
    showsSignal: boolean
}

export const SignalEntry: React.FC<SignalEntryProps> = ({
    index,
    collapse,
    name,
    entries,
    verifier,
    showsSignal
}) => {
    const [open, setOpen] = useState(false)
    const collapseText = (open: boolean): string => {
        if (open) return "collapse";
        return "expand";
      }

    return (
        <div
            style={{
              backgroundColor: '#EEF5DB',
              marginBottom: '2em',
              padding: '1em',
              borderRadius: '1em',
            }}
            key={index}
          >
            <div
              style={{
                display: 'flex',
                flexDirection: 'row',
                justifyContent: 'space-between',
              }}
            >
              {collapse && <p>{name}</p>}
              {collapse || <h2>{name}</h2>}
              {collapse && (
                <button onClick={() => setOpen(!open)}>{collapseText(open)}</button>
              )}
            </div>
            {collapse && (
              <Collapse isOpened={open}>
                <SignalTable
                  votes={entries}
                  isValidVote={verifier}
                  showsSignal={showsSignal}
                />
              </Collapse>
            )}
            {collapse ||
              <SignalTable
                votes={entries}
                isValidVote={verifier}
                showsSignal={showsSignal}
              />
            }
          </div>
    )
}