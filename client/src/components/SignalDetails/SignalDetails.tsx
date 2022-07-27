import { AccountEntry, VoteCheckResult, VoteEntry } from '../../../types';
import SignalTable from '../SignalTable';
import { Collapse } from 'react-collapse';
import { useState } from 'react';

interface DetailsProps {
  categories: [string, VoteEntry[], (vote: VoteEntry) => VoteCheckResult, boolean][]
}

const Details: React.FC<DetailsProps> = ({
  categories
}) => {
  const [open, setOpen] = useState(false);

  return (
    <div>
      <h1 style={{ color: '#EEF5DB', textAlign: 'center' }}>
        Signalling Details
      </h1>

      <div style={{ display: 'flex', flexDirection: 'column' }}>
        {categories.map(([name, entries, verifier, collapse], index) => (
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
                <button onClick={() => setOpen(!open)}>Collapse</button>
              )}
            </div>
            {collapse && (
              <Collapse isOpened={open}>
                <SignalTable
                  votes={entries}
                  isValidVote={verifier}
                />
              </Collapse>
            )}
            {collapse ||
              <SignalTable
                votes={entries}
                isValidVote={verifier}
              />
            }
          </div>
        ))}
      </div>
    </div>
  );
};

export default Details;
