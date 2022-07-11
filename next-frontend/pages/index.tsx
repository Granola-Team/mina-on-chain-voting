import type { NextPage } from 'next'
import Head from 'next/head'
import Image from 'next/image'
import styles from '../styles/Home.module.css'

import React, { useState, useEffect } from 'react';
import Datatable from '../components/datatable'; 

require("es6-promise").polyfill();
require("isomorphic-fetch");

const Home: NextPage = () => {

  const [data, setData] = useState([])  
  const [q, setQ] = useState("") 

  useEffect(() => {
    fetch() // enter URL from backend 
    .then(response => response.json())
    .then(json => setData(json));
  }, []) // [] is an array of dependencies and when it changes, 
  // the function fires
  // but since it is empty it is only done at initilization

  function search(rows) {
    return rows.filter(row => row.memo.toLowerCase().indexOf(q) > -1)
  } //this will search for "magenta", "no magenta", or whatever the user wants
    //since they are in count, they should all add up

  return (
    <div>
      <main className={styles.main}>
        <h1 className={styles.title}>
          HF Voting Table Results
        </h1>
      </main>
      
      <div>
        <input type="text" value={q} onChange={(e) => setQ(e.target.value)}/>
      </div>
      <div>
          <Datatable data={search(data)} />
      </div>
    </div>
  )
}

export default Home
