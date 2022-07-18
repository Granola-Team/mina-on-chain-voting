import React from "react";

export function Datatable({ data }: any) {
  const columns = data[0] && Object.keys(data[0]);
  return (
  <table cellPadding={0}> 
    <thead>
      <tr>{data[0] && columns.map((heading : any) => <th key={data}>{heading}</th>)}</tr>
    </thead>
    <tbody>
      {data.map((row : any) => ( 
      <tr key={data}>
        {columns.map((column : any) => (
          <td key={data}>{row[column]}</td>
      ))}
      </tr>
    ))} 
    </tbody>
  </table>
  );
}

