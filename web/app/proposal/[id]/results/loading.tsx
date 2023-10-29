'use client';
import { useTheme } from 'next-themes';

import '../../../styles.css'

export default function PageLoading() {
    const { theme } = useTheme();
    return (
        <div className="container relative">
            <div className="max-w-[980px] flex-col items-start gap-2 px-4 pt-6 md:pt-10 hidden md:block pb-6">
                <div className={`skeleton-loader-${theme} h-10 bg-gray-300 rounded my-1`}></div>
                <div className={`skeleton-loader-${theme} h-8 bg-gray-300 rounded my-1`}></div>
                <div className={`skeleton-loader-${theme} h-7 bg-gray-300 rounded my-1`}></div>
            </div>
            <div className="flex flex-col gap-2">
                <div className="grid gap-1.5 grid-cols-2 grid-rows-4 md:grid-cols-4 md:grid-rows-3 xl:grid-cols-5 xl:grid-rows-2">
                    <div className="rounded-lg border bg-card text-card-foreground shadow row-span-2 col-span-2 md:col-span-4 xl:col-span-3">
                        <div className="flex flex-col space-y-1.5 p-6 pb-0">
                            <h3 className="font-semibold leading-none tracking-tight">Voting Distribution</h3>
                            <p className="text-sm text-muted-foreground">Track the distribution of <span className="inline font-semibold">FOR / AGAINST</span> votes.</p>
                        </div>
                        <div className='skeleton-loader-orange h-32 bg-orange-400 rounded m-1'></div>
                    </div>
                    <div className="rounded-lg border bg-card text-card-foreground shadow row-start-4 md:row-start-3 xl:row-start-1 xl:col-start-4">
                        <div className="flex flex-col space-y-1.5 p-6 pb-0.5">
                            <h3 className="tracking-tight text-sm font-normal">Total Votes</h3>
                        </div>
                        <div className={`skeleton-loader-${theme} h-10 bg-gray-300 rounded m-1`}></div>
                    </div>
                    <div className="rounded-lg border bg-card text-card-foreground shadow row-start-4 md:row-start-3 xl:row-start-1 xl:col-start-5">
                        <div className="p-6 flex flex-row items-center justify-between space-y-0 pb-0.5">
                            <h3 className="tracking-tight text-sm font-normal">
                                Total Stake Participated
                            </h3>
                        </div>
                        <div className={`skeleton-loader-${theme} h-10 bg-gray-300 rounded m-1`}></div>
                    </div>
                    <div className="rounded-lg border bg-card text-card-foreground shadow col-span-2 md:row-start-3 xl:row-start-2 xl:col-start-4">
                        <div className="p-6 flex flex-row items-center justify-between space-y-0 pb-0.5">
                            <h3 className="font-semibold leading-none tracking-tight">
                                Voting Results
                            </h3>
                        </div>
                        <div className='skeleton-loader-orange h-10 bg-gray-300 rounded m-1'></div>
                    </div>
                </div>
                <div data-orientation="horizontal" role="none" className="shrink-0 bg-border h-[1px] w-full"></div>
                    <div className="flex flex-1 items-center space-x-2">
                    <span className="flex rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm transition-colors  h-8 w-full">
                        <div className='skeleton-loader-dark h-7 bg-gray-300 rounded m-1'></div>
                    </span>
                </div>
                <div className="rounded-md border">
                    <table className="w-full caption-bottom text-sm loading">
                        <thead className="[&amp;_tr]:border-b">
                            <tr className="border-b transition-colors hover:bg-muted/50 data-[state=selected]:bg-muted">
                                <th className="h-10 px-2 text-left align-middle font-medium text-muted-foreground [&amp;:has([role=checkbox])]:pr-0 [&amp;>[role=checkbox]]:translate-y-[2px]">
                                    <div className="text-xs pl-2.5">
                                        Height
                                    </div>
                                </th>
                                <th className="h-10 px-2 text-left align-middle font-medium text-muted-foreground [&amp;:has([role=checkbox])]:pr-0 [&amp;>[role=checkbox]]:translate-y-[2px]">
                                    <div className="text-xs">
                                        Timestamp
                                    </div>
                                </th>
                                <th className="h-10 px-2 text-left align-middle font-medium text-muted-foreground [&amp;:has([role=checkbox])]:pr-0 [&amp;>[role=checkbox]]:translate-y-[2px]">
                                    <div className="text-xs pl-[1px]">
                                        Account
                                    </div>
                                </th>
                                <th className="h-10 px-2 text-left align-middle font-medium text-muted-foreground [&amp;:has([role=checkbox])]:pr-0 [&amp;>[role=checkbox]]:translate-y-[2px]">
                                    <div className="text-xs">
                                        Hash
                                    </div>
                                </th>
                                <th className="h-10 px-2 text-left align-middle font-medium text-muted-foreground [&amp;:has([role=checkbox])]:pr-0 [&amp;>[role=checkbox]]:translate-y-[2px]">
                                    <div className="text-xs">
                                        Vote
                                    </div>
                                </th>
                                <th className="h-10 px-2 text-left align-middle font-medium text-muted-foreground [&amp;:has([role=checkbox])]:pr-0 [&amp;>[role=checkbox]]:translate-y-[2px]">
                                    <div className="text-xs">
                                        Status
                                    </div>
                                </th>
                            </tr>
                        </thead>
                        <tbody className="[&_tr:last-child]:border-0">
                            {Array.from({ length: 10 }).map((_, index) => (
                                <TableLoadingElement key={index} />
                            ))}
                        </tbody>
                    </table>
                </div>
            </div>
        </div>
    )
}

function TableLoadingElement() {
    const { theme } = useTheme();
    return (
        <tr className="border-b transition-colors">
            <td colSpan={6}>
                <div className={`skeleton-loader-${theme} h-7 bg-gray-300 rounded`}></div>
            </td>
        </tr>
    );
}
  
