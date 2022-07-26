import { dummyData } from '../dummy';
import React, { useState, useEffect } from 'react';
import SignalTable from '../components/SignalTable';
import { useParams } from 'react-router-dom';
import type {
  AccountEntry,
  VoteEntry,
  VoteCheckResult,
  Status,
} from '../../types';
import Totals from '../components/Totals';
import Details from '../components/SignalDetails';
import Footer from '../components/Footer';

const memoChecked = dummyData.map((dData2) => ({
  ...dData2,
  votes: dData2.votes.map((votes) => ({
    ...votes,
    memo: votes.memo.toLowerCase(),
  })),
}));

const verifyVote = (vote: VoteEntry): VoteCheckResult => {
  if (vote.memo === 'magenta') return 'for';
  if (vote.memo === 'no magenta') return 'against';
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

const Home = ({ testing }) => {
  const [data, setData] = useState<AccountEntry[] | null>(dummyData);
  const { key } = useParams();

  useEffect(() => {
    if (testing) {
      setData(dummyData)
    } else {
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
        setData(dummyData)
      });
    }
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

  const settled = votesTotal(canonicalVotes.map(([_, vote]) => vote)) as [number, number];
  const unsettled = votesTotal(pendingVotes.map(([_, vote]) => vote)) as [number, number];

  return (
    <main
      style={{
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
      }}
    >
      <Totals 
        signallingKey={key} 
        settledSignals={settled} 
        unsettledSignals={unsettled} 
      />

      <Details 
        discriminators={[
          ["Settled", selectHighestVoteWith("Settled"), false],
          ["Unsettled", selectHighestVoteWith("Undecided"), true]
        ]}
        verifier={verifyVote}
        data={data}
      />

      <Footer key={key} />
    </main>
  );
};

export default Home;
