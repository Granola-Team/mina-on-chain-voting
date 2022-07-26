interface FooterProps {
    key: string
}

const Footer: React.FC<FooterProps> = ({key}) => {
    return (
        <div style={{ color: '#EEF5DB', maxWidth: '65%' }}>
            <em>
            Settled messages have a chain of at least 20 blocks behind them, and can be taken to be the most accurate representation of the on-chain signal. Unsettled messages have less than 20 blocks and are less certain, however, they update much faster. Invalid means a transaction's memo has failed to resolve to a signal For or Against. To signal
            support, send a transaction to yourself and enter '{key}' in the memo
            field. To opppose, send a transaction to yourself and enter 'no {key}'
            in the memo field.
            </em>
        </div>
    )
}

export default Footer