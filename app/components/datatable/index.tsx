import React from "react";



export function Datatable({ data }: any) {
  const columns = data[0] && Object.keys(data[0]);

  

  return (
    <table cellPadding={0}> 
      <tbody>
        {data.map((row : any, index: number) => ( 
        <tr key={index}>
          {columns.map((column : any, index: number) => (
            <td key={index}>{row[column]}</td>
        ))}
        </tr>
      ))} 
      </tbody>
    </table>
  );
}

