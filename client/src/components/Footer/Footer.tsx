interface FooterProps {
    key: string
}

const Footer: React.FC<FooterProps> = ({key}) => {
    return (
        <div style={{ color: '#EEF5DB', maxWidth: '65%' }}>
            <em>
            Canonical messages are incorporated in the Mina Blockchain. Pending
            messages are not yet incorporated into the Mina Blockchain. To signal
            support, send a transaction to yourself and enter '{key}' in the memo
            field. To opppose, send a transaction to yourself and enter 'no {key}'
            in the memo field.
            </em>
        </div>
    )
}

export default Footer