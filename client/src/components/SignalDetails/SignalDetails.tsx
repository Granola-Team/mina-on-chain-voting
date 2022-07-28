import { AccountEntry, VoteCheckResult, VoteEntry } from '../../../types';
import SignalTable from '../SignalTable';
import { Collapse } from 'react-collapse';
import { useState } from 'react';
import { SignalEntry } from './SignalEntry';

interface DetailsProps {
  categories: [string, VoteEntry[], (vote: VoteEntry) => VoteCheckResult, boolean][]
}


const Details: React.FC<DetailsProps> = ({
  categories
}) => {
  const [open, setOpen] = useState(false);
  const [opens, setOpens] = useState<boolean[]>(
    Array.apply(null, Array(categories.length)).map((_) => false)
  )

  return (
    <div>
      <h1 style={{ color: '#EEF5DB', textAlign: 'center' }}>
        Signalling Details
      </h1>

      <div style={{ display: 'flex', flexDirection: 'column' }}>
        {categories.map(([name, entries, verifier, collapse], index) => (
          <SignalEntry 
            index={index}
            collapse={collapse}
            name={name}
            entries={entries}
            verifier={verifier}
          />
        ))}
      </div>
    </div>
  );
};

export default Details;
