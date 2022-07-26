import { dummyData } from '../dummy';
import React, { useState, useEffect } from 'react';
import VotingDetails from '../components/VotingDetails';
import { useParams } from 'react-router-dom';
import type {
  AccountEntry,
  VoteEntry,
  VoteCheckResult,
  Status,
} from '../../types';

const memoChecked = dummyData.map((dData2) => ({
  ...dData2,
  votes: dData2.votes.map((votes) => ({
    ...votes,
    memo: votes.memo.toLowerCase(),
  })),
}));

const verifyVote = (vote: VoteEntry): VoteCheckResult => {
  if (vote.memo.toLowerCase() === 'magenta') return 'for';
  if (vote.memo.toLowerCase() === 'no magenta') return 'against';
  return 'invalid';
};

const votesTotal = (votes: VoteEntry[]) => {
  return votes
    .map((vote) => verifyVote(vote))
    .map((result) => {
      if (result === 'against') return [0, 1];
      if (result === 'for') return [1, 0];
      return [0, 0];
    })
    .reduce(
      (v1, v2) => {
        return [v1[0] + v2[0], v1[1] + v2[1]];
      },
      [0, 0]
    );
};

const Home = () => {
  const [data, setData] = useState<AccountEntry[] | null>(dummyData);
  const { key } = useParams();

  useEffect(() => {
    fetch('http://35.203.38.140:8080/api/votes', {
      method: 'GET',
      mode: 'same-origin',
    })
      .then((response) => {
        console.log(response);
        return response.json();
      })
      .then((json) => setData(json))
      .catch((error) => {
        console.log(error);
      });
  }, [data]);

  const selectHighestVoteWith =
    (status: Status) =>
    (entry: AccountEntry): VoteEntry | null => {
      let vote: VoteEntry | null = null;
      entry.votes.forEach((voteEntry) => {
        if (voteEntry.status !== status) return;
        if (vote == null || vote.height < voteEntry.height) vote = voteEntry;
      });

      return vote;
    };

  const canonicalVotes =
    data &&
    data
      .map((accountEntry) => [
        accountEntry,
        selectHighestVoteWith('Settled')(accountEntry),
      ])
      .filter((entry): entry is [AccountEntry, VoteEntry] => entry[1] !== null);

  const pendingVotes =
    data &&
    data
      .map((accountEntry) => [
        accountEntry,
        selectHighestVoteWith('Undecided')(accountEntry),
      ])
      .filter((entry): entry is [AccountEntry, VoteEntry] => entry[1] !== null);

  const [forCan, agCan] = votesTotal(canonicalVotes.map(([_, vote]) => vote));
  const [forPen, agPen] = votesTotal(pendingVotes.map(([_, vote]) => vote));

  return (
    <main
      style={{
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
      }}
    >
      <div>
        <h1 style={{ color: '#EEF5DB' }}>OnChainSignalling Totals</h1>
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
            justifyContent: 'center',
            padding: '1em',
            backgroundColor: '#EEF5DB',
            borderRadius: '1em',
          }}
        >
          <div style={{ margin: '1em' }}>
            <h2>
              <b>Settled</b>
            </h2>
            For {key}: <b> {forCan} </b>
            <br></br>
            Against {key}: <b> {agCan} </b>
          </div>
          <div style={{ margin: '1em' }}>
            <h2>
              <b>Undecided</b>
            </h2>
            For {key}: <b> {forPen} </b>
            <br></br>
            Against {key}: <b> {agPen} </b>
          </div>
        </div>
      </div>

      <div>
        <h1 style={{ color: '#EEF5DB', textAlign: 'center' }}>
          Signalling Details
        </h1>
        <div style={{ display: 'flex', flexDirection: 'column' }}>
          <div
            style={{
              backgroundColor: '#EEF5DB',
              marginBottom: '-2em',
              padding: '1em',
              borderRadius: '1em',
            }}
          >
            <h2>Settled</h2>
            {data && (
              <VotingDetails
                accountDetails={data}
                votesDiscriminator={selectHighestVoteWith('Settled')}
                isValidVote={verifyVote}
              />
            )}
          </div>

        <div style={{ display: 'flex', flexDirection: 'column' }}>
          <div
            style={{
              backgroundColor: '#EEFB',
              margin: '5em',
              marginBottom: '1em',
              padding: '1em',
              borderRadius: '1em',
            }}
          >
            <h2>Undecided</h2>
            {data && (
              <VotingDetails
                accountDetails={data}
                votesDiscriminator={selectHighestVoteWith('Undecided')}
                isValidVote={verifyVote}
              />
            )}
          </div>
        </div>
      </div>
      </div>

      <div style={{ color: '#EEF5DB', maxWidth: '65%' }}>
        <em>
          Canonical messages are incorporated in the Mina Blockchain. Pending
          messages are not yet incorporated into the Mina Blockchain. To signal
          support, send a transaction to yourself and enter '{key}' in the memo
          field. To opppose, send a transaction to yourself and enter 'no {key}'
          in the memo field.
        </em>
      </div>
    </main>
  );
};

export default Home;
