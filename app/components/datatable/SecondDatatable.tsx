import React from "react";

export function SecondDatatable({ cloneData }: any): JSX.Element {
  const columns = cloneData[0] && Object.keys(cloneData[0]);
  return (
  <table cellPadding={0}> 
    <thead>
      <tr>{cloneData[0] && columns.map((heading : any) => <th key={cloneData}>{heading}</th>)}</tr>
    </thead>
    <tbody>
      {cloneData.map((row : any) => ( 
      <tr key={cloneData}>
        {columns.map((column : any) => (
          <td key={cloneData}>{row[column]}</td>
      ))}
      </tr>
    ))} 
    </tbody>
  </table>
  );
}

