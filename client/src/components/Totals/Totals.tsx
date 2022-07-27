interface TotalsProps {
    totalsTitle: string,
    signallingKey: string
    signals: [number, number]
}

const Totals: React.FC<TotalsProps> = ({
    totalsTitle, signallingKey, signals
}) => {
 return (
    <div>
        <h1 style={{ color: '#EEF5DB' }}>OnChainSignalling Totals</h1>
        <div
            style={{
            display: 'flex',
            flexDirection: 'row',
            justifyContent: 'center',
            padding: '1em',
            backgroundColor: '#EEF5DB',
            borderRadius: '1em',
            }}
        >
            <div style={{ margin: '1em' }}>
            <h2>
                <b>{ totalsTitle }</b>
            </h2>
                For {signallingKey}: <b> {signals[0]} </b>
            <br></br>
            Against {signallingKey}: <b> { signals[1]} </b>
            </div>
        </div>
        </div>
 )
}

export default Totals