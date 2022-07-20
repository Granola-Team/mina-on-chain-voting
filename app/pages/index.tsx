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

  // I tried using .toString() after Object.values(obj)[1] to make it string from unknown, 
  // then the .toLowerCase to remove the redundancy of if statements, but it isn't letting me 
  
  // the below code does work with the dummyData we are feeding into it
  
  let total = [0, 0, 0, 0];
  data.forEach(function(obj) {
    if (Object.values(obj)[1] == "magenta" && Object.values(obj)[2] == "canonical" 
    || Object.values(obj)[1] == "Magenta" && Object.values(obj)[2] == "canonical"
    || Object.values(obj)[1] == "Magenta Magenta" && Object.values(obj)[2] == "canonical"
    || Object.values(obj)[1] == "Magenta_magenta" && Object.values(obj)[2] == "canonical"
    || Object.values(obj)[1] == "maegenta" && Object.values(obj)[2] == "canonical") {
      total[0]++;
    }
    if (Object.values(obj)[1] == "no magenta" && Object.values(obj)[2] == "canonical"
        || Object.values(obj)[1] == "no-magenta" && Object.values(obj)[2] == "canonical"
        || Object.values(obj)[1] == "no_magenta" && Object.values(obj)[2] == "canonical"
        || Object.values(obj)[1] == "No magenta" && Object.values(obj)[2] == "canonical"
        || Object.values(obj)[1] == "noMagenta" && Object.values(obj)[2] == "canonical") {
      total[1]++;
    }
    if (    Object.values(obj)[1] == "magenta" && Object.values(obj)[2] == "pending" 
        || Object.values(obj)[1] == "Magenta" && Object.values(obj)[2] == "pending"
        || Object.values(obj)[1] == "Magenta Magenta" && Object.values(obj)[2] == "pending"
        || Object.values(obj)[1] == "Magenta_magenta" && Object.values(obj)[2] == "pending"
        || Object.values(obj)[1] == "maegenta" && Object.values(obj)[2] == "pending") {
      total[2]++;
    }
    if (Object.values(obj)[1] == "no magenta" && Object.values(obj)[2] == "pending"
    || Object.values(obj)[1] == "no-magenta" && Object.values(obj)[2] == "pending"
    || Object.values(obj)[1] == "no_magenta" && Object.values(obj)[2] == "pending"
    || Object.values(obj)[1] == "No magenta" && Object.values(obj)[2] == "pending"
    || Object.values(obj)[1] == "noMagenta" && Object.values(obj)[2] == "pending") {
      total[3]++;
    }
  })

  /* for pending yes votes, the below is filtering for pending (count of 7) but 
  not filtering further for magenta bringing the count to 4 but instead staying at 7
    Object.values(obj)[2] == "pending" && (Object.values(obj)[1] == 
          "magenta" || "Magenta" || "Magenta Magenta" || "Magenta_magenta" || "maegenta")        
  */

/* the following functions and reduce were not working, hence the bulky code above

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
              <p> Yes = {total[0]} </p>
              <p> No = {total[1]} </p>
          </h2> 
            <br />
          <h2>
            <b>
              Pending Votes
            </b>
              <p> Yes = {total[2]} </p>
              <p> No = {total[3]} </p>
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

  /* an attempt to make the data.slice(0,3) above in a function below
  const sliced_array = (data: {}[]) => {
    return data.length > 5 ? `${data.splice(0,3)}...` : data;
  } */  // it keeps saying type string | {}[] even when forced so did so in table instead
