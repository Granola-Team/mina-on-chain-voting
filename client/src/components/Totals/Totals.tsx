interface TotalsProps {
    signallingKey: string
    settledSignals: [number, number]
    unsettledSignals: [number, number]
}

const Totals: React.FC<TotalsProps> = ({
    signallingKey, settledSignals, unsettledSignals
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
                <b>Settled</b>
            </h2>
                For {signallingKey}: <b> {settledSignals[0]} </b>
            <br></br>
            Against {signallingKey}: <b> {settledSignals[1]} </b>
            </div>
            <div style={{ margin: '1em' }}>
            <h2>
                <b>Unsettled</b>
            </h2>
            For {signallingKey}: <b> {unsettledSignals[0]} </b>
            <br></br>
            Against {signallingKey}: <b> {unsettledSignals[1]} </b>
            </div>
        </div>
        </div>
 )
}

export default Totals