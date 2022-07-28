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
        <h1 style={{ color: '#EEF5DB' }}>{totalsTitle}</h1>
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
            <div style={{ margin: '1em', display: 'flex', flexDirection: 'column' }}>
                <div>
                    For {signallingKey}: <b> {signals[0]} </b>
                </div>
                <div>
                    Against {signallingKey}: <b> { signals[1]} </b>
                </div>
            </div>
        </div>
        </div>
 )
}

export default Totals