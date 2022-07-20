import type { NextPage } from 'next';
import styles from '../styles/Home.module.css';

import React, { useState, useEffect } from 'react';
import { Datatable } from '../components/datatable';
// import {SecondDatatable} from '../components/datatable/SecondDatatable';
import dummyData from '../dummy';
// import axios from 'axios';

const Home: NextPage = () => {
  const [data, setData] = useState<Data[]>([]);

  useEffect(() => {
    fetch('http://35.203.38.140:8081/votes', {
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
  }, [data]);

  type Data = {
    Account: string;
    Memo: string;
    Status: string;
  };

  // type filtered_data = { Memo: string; Status: string }
  const sums_status = data.filter((value) => {
    if (value.Status == 'canonical') return data.splice;
    if (value.Status == 'pending') return data.splice;
  });
  const sums_memo = sums_status
    .map((value) => {
      if (value.Memo == 'magenta') return [1, 0];
      if (value.Memo == 'no magenta') return [0, 1];
      return [0, 0];
    })
    .reduce(
      ([for1, against1], [for2, against2]) => [
        for1 + for2,
        against1 + against2,
      ],
      [0, 0]
    );

  /*
  const sliced_array = (data: {}[]) => {
    return data.length > 5 ? `${data.splice(0,3)}...` : data;
  } */ // it keeps saying type string | {}[] even when forced so did so in table instead

  const num_of_no_magenta = (rows: [string, string][]) => {
    return rows.reduce(
      (num_of_no_magenta, [_, Memo]) =>
        Memo == 'no magenta' ? num_of_no_magenta + 1 : num_of_no_magenta,
      0
    );
  }; // add variations: | "no_magenta" | "no-magenta" | "nomagenta"

  return (
    <div>
      <main className={styles.main}>
        <h1 className={styles.title}>Voting Totals</h1>
        <div>
          <h2>
            <b>Canonical Votes</b>
          </h2>
          <p>For magenta: {sums_memo[0]}</p>
          <p>Against magenta: {sums_memo[1]}</p>
          {sums_memo[0] > sums_memo[1] ? 'Magenta Wins!' : 'No Magenta :('}
        </div>
        <br />
        <h2>
          <b>Pending Votes</b>
        </h2>
        <p>For magenta: {sums_memo[0]}</p>
        <p>Against magenta: {sums_memo[1]}</p>
        {sums_memo[0] > sums_memo[1] ? 'Magenta Wins!' : 'No Magenta :('}

        <h4 className={styles.card}>
          Canonical votes are blocks incorporated in the Mina Blockchain.
          Pending votes are blocks that have not yet been confirmed or
          &quot;orphaned&quot;. To vote yes, send a transaction to yourself and
          enter &quot;magenta&quot; in the memo field. To vote no, enter
          &quot;no magenta&quot; in the memo field.
        </h4>

        <h2 className={styles.title}>Voting Detail</h2>
        <div>
          <Datatable data={data.slice(0, 3)} />
        </div>
        <h2 className={styles.description}>...</h2>
      </main>
    </div>
  );
};

export default Home;
