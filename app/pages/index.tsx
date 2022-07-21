import type { NextPage } from 'next'
import styles from '../styles/Home.module.css'

import React, { useState, useEffect } from 'react';
import {Datatable} from '../components/datatable'; 
// import {SecondDatatable} from '../components/datatable/SecondDatatable'; 
import dummyData from '../lib/dummy'
import VotingDetails from '../components/VotingDetails';
// import axios from 'axios';

const Home: NextPage = () => {

  const [data, setData] = useState<AccountEntry[] | null>([])  

  useEffect(() => {
    fetch("http://35.203.38.140:8081/votes",   
        {
      method: 'GET', 
      mode: 'same-origin',
        }) 
    .then(response => {console.log(response); return response.json()})
    .then(json => setData(json))
    .catch(error => {console.log(error); setData(dummyData)});
  }, [data]) 

  const selectHighestCanonicalVote = (entry: AccountEntry): VoteEntry | null => {
    let vote: VoteEntry | null = null
    entry.votes.forEach((voteEntry) => {
      if (vote?.status === "Pending") return;

      if (vote == null || vote.height < voteEntry.height) vote = voteEntry;
    })
    return vote
  }

  const selectHighestPendingVote = (entry: AccountEntry): VoteEntry | null => {
    let vote: VoteEntry | null = null
    entry.votes.forEach((voteEntry) => {
      if (vote?.status == "Pending" && (vote == null || vote.height < voteEntry.height)) vote = voteEntry;
    })
    return vote
  }

  const verifyVote = (vote: VoteEntry): VoteCheckResult => {
    if (vote.memo === "magenta") return "for"
    if (vote.memo === "no magenta") return "against"
    return "invalid"
  }

  const canonicalVotes = data && data
    .map((accountEntry) => [accountEntry, selectHighestCanonicalVote(accountEntry)])
    .filter((entry): entry is [AccountEntry, VoteEntry] => entry[1] !== null)

  const votesTotal = (votes: VoteEntry[]) => {
    return votes.map((vote) => verifyVote(vote))
    .map((result) => {
      if (result === "against") return [0, 1]
      if (result === "for") return [1, 0]
      return [0, 0]
    }).reduce((v1, v2) => {
      return [v1[0] + v2[0], v1[1] + v2[1]]
    }, [0, 0])
  }
    

  const pendingVotes = data && data
    .map((accountEntry) => [accountEntry, selectHighestPendingVote(accountEntry)])
    .filter((entry): entry is [AccountEntry, VoteEntry] => entry[1] !== null)

  return (
    <div>
      <main className={styles.main}> 
        <h1 className={styles.title}>
          Voting Totals
        </h1>
        <div style={{display: "flex", flexDirection: "row", padding: "1em"}}>
          <div style={{marginRight: "4em"}}>
            <h2><b>Canonical Votes</b></h2>
            {canonicalVotes && votesTotal(canonicalVotes.map(([_, vote]) => vote))}
          </div>
          <div>
            <h2><b>Pending Votes</b></h2>
            {pendingVotes && votesTotal(pendingVotes.map(([_, vote]) => vote))}
          </div>
        </div>

        <h2 className={styles.title}>
          Voting Detail
        </h2>
        {data &&
          <VotingDetails
            accountDetails={data}
            votesDiscriminator={selectHighestCanonicalVote}
            isValidVote={verifyVote}
          />
        }

        <h4 className={styles.card}> 
          <em>
          Canonical votes are blocks incorporated in the Mina Blockchain. Pending votes are blocks that have not yet been confirmed or "orphaned".
          To vote yes, send a transaction to yourself and enter "magenta" in the memo field. To vote no, enter "no magenta" in the memo field.
          </em>
        </h4>
      </main>
    </div>
  )
}

export default Home
