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

// const memoChecked = dummyData.map((dData2) => ({
//   ...dData2,
//   votes: dData2.votes.map((votes) => ({
//     ...votes,
//     memo: votes.memo.toLowerCase(),
//   })),
// }));

const verifyVote = (vote: VoteEntry): VoteCheckResult => {
  const voteMemo = vote.memo.toLowerCase();
  if (voteMemo === 'magenta') return 'for';
  if (voteMemo === 'no magenta') return 'against';
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

  const getKey = (): string => {
    if (key) return key;
    return "Magenta"
  }

  useEffect(() => {
    if (testing) {
      setData(dummyData);
    } else {
      fetch('http://localhost/api/v1/magenta', {
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
          setData(dummyData);
        });
    }
  }, [data]);

  return (
    <main
      style={{
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
      }}
    >
      <Totals
        totalsTitle={'Mina On-Chain Signals for ' + getKey()}
        signallingKey={key}
        signals={votesTotal(data[0].results) as [number, number]}
      />

      <Details
        categories = {[
          ["Settled", data[0].results, verifyVote, false, true],
          ["Unsettled", data[1].results, verifyVote, true, true],
          ["Invalid", data[2].results, verifyVote, true, false]
        ]}
      />

      <Footer keyword={getKey()} />
    </main>
  );
};

export default Home;
