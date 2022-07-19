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

  const num_of_no_magenta = (rows: [string, string][]) => {
    return rows.reduce((num_of_no_magenta, [_, Memo]) => 
        Memo == "no magenta" ? 
        num_of_no_magenta + 1 : num_of_no_magenta, 0)
  } // add variations: | "no_magenta" | "no-magenta" | "nomagenta"

  return (
    <div>
        <main className={styles.main}> 
        <h1 className={styles.title}>
            Voting Totals
          </h1>
          <div>
            <Datatable data={data} /> 
          </div>
          
          <h4 className={styles.card}> 
            Canonical votes are blocks incorporated in the Mina Blockchain. Pending votes are blocks that have not yet been confirmed or "orphaned".
            To vote yes, send a transaction to yourself and enter "magenta" in the memo field. To vote no, enter "no magenta" in the memo field.
          </h4>

          <h2 className={styles.title}>
            Voting Detail
          </h2>
          <div>
            <Datatable data={data.slice(0,5)} /> 
          </div>
          <h2 className={styles.description}>
            ...
          </h2>
        </main>
    </div>
  )
}

export default Home
