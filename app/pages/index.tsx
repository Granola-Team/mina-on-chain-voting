import type { NextPage } from 'next'
import styles from '../styles/Home.module.css'

import React, { useState, useEffect } from 'react';
import {Datatable} from '../components/datatable'; 
// import {SecondDatatable} from '../components/datatable/SecondDatatable'; 
import dummyData from '../dummy'
// import axios from 'axios';

const Home: NextPage = () => {

  const [data, setData] = useState<{}[]>([])  
  const [q, setQ] = useState("") 

  const requestHeaders: HeadersInit = new Headers();
  requestHeaders.set('Content-Type', 'application/json');

  useEffect(() => {
    fetch("http://35.203.38.140:8080/votes",   
        {
      method: 'POST', 
      headers: requestHeaders,
      mode: 'no-cors',
        }) 
    .then(response => response.json())
    .then(json => setData(json))
    .catch(error => setData(dummyData));
  }) 

  /*
  const sliced_array = (data: {}[]) => {
    return data.length > 5 ? `${data.splice(0,3)}...` : data;
  } */  // it keeps saying type string | {}[] even when forced so did so in table instead



/*  
let counted_no_magenta = data.reduce(function (all_no_magenta, data) {
  if (data in all_no_magenta) {
    all_no_magenta[data]++
  }
  else {
    all_no_magenta[data] = 1
  }
  return all_no_magenta
}, {})

/*
 const num_of_no_magenta = (rows) => {
    return data.reduce((num_of_no_magenta, [_, Object.values(data)]) => 
        Object.values(data) == "no magenta" ? 
        num_of_no_magenta + 1 : num_of_no_magenta, 0)
  } // add variations: | "no_magenta" | "no-magenta" | "nomagenta"


const number_of_magenta = (rows: []) => {
  return rows.reduce(
    (num_magenta, [_, memo]) => memo == "magenta" ? num_magenta + 1 : num_magenta,
    0
  )
} 
*/
  return (
    <div>
        <main className={styles.main}> 
        <h1 className={styles.title}>
            Voting Totals
          </h1>
          <h2>
            <b>
              Canonical Votes
            </b>
              <p> Yes = </p>
              <p> No = </p>
          </h2>
            <br />
          <h2>
            <b>
              Pending Votes
            </b>
              <p> Yes = </p>
              <p> No = </p>
          </h2>

          <h4 className={styles.card}> 
            Canonical votes are blocks incorporated in the Mina Blockchain. Pending votes are blocks that have not yet been confirmed or "orphaned".
            To vote yes, send a transaction to yourself and enter "magenta" in the memo field. To vote no, enter "no magenta" in the memo field.
          </h4>

          <h2 className={styles.title}>
            Voting Details
          </h2>
          <div>
            <Datatable data={data.slice(0,3)} /> 
          </div>
          <h2 className={styles.description}>
            ...
          </h2>
        </main>
    </div>
  )
}

export default Home
