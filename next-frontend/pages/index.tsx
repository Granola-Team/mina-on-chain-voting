import type { NextPage } from 'next'
import Head from 'next/head'
import Image from 'next/image'
import styles from '../styles/Home.module.css'

import React, { useState, useEffect, memo } from 'react';
import {Datatable} from '../components/datatable'; 
import dummyData from '../dummy'

const Home: NextPage = () => {

  const [data, setData] = useState<string[][]>([])  
  const [q, setQ] = useState("") 

  useEffect(() => {
    fetch("0.0.0.0:8081") // enter URL from backend 
    .then(response => response.json())
    .then(json => setData(json))
    .catch(error => setData(dummyData));
  }, []) // [] is an array of dependencies and when it changes, 
  // the function fires
  // but since it is empty it is only done at initilization

  type APIdata = {account : string, memo : string}[]
  function transform(rows : string[][]) : APIdata {
      const data : APIdata = rows.map(row => {return({account: row[0], memo: row[1]})})
      return data;
  }

  function search(rows : any) {
    return rows.filter((row : any) => row.memo.toLowerCase().indexOf(q) > -1)
  } //this will search for "magenta", "no magenta", or whatever the user wants
    //since they are in count, they should all add up

  return (
    <div>
      <main className={styles.main}> 
        <h1 className={styles.title}>
          HF Voting Table Results
        </h1>
      
      <div>
        <input type="text" value={q} onChange={(e) => setQ(e.target.value)}/>
      </div>
      <div>
          <Datatable data={transform(data)} filter={search} /> 
      </div>
      </main>
    </div>
  )
}

export default Home
