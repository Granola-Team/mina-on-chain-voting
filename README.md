# Mina On-Chain Voting

On-Chain Voting is a protocol developed to help with governing the Mina L1
blockchain.


## Protocol Specifications

The On-Chain Voting Protocol is designed to provide community members with a
transparent and secure method of participating in the decision-making process
for the Mina blockchain. The aim for this protocol is to provide stake holders
with the ability to vote on MIPs (Mina Improvement Proposals) in a clear and
concise way.

Individual MIPs should be created by following the process described on the
[GitHub repository](https://github.com/MinaProtocol/MIPs).


### Voting on a MIP

To cast a vote on a particular MIP, users must send a transaction to the
**themselves** with a specific content memo field. The memo field must adhere
to the following convention.

**For example:**

```
To vote in favor of 'MIP1', the memo field would be populated with: 'MIP1'
Similarly - if your intent is to vote against 'MIP1', the memo field would
contain: 'no MIP1'
```

**The transaction amount must be 0, with the user only paying for the
transaction fee.**

For more details, see:

- The [article by Granola](https://granola.team/blog/mina-on-chain-voting-results-instructions/)
- The [FAQ](https://forums.minaprotocol.com/t/on-chain-voting-frequently-asked-questions-faq/5959)


## Software Development

See the [DEVELOPER.md](./docs/DEVELOPER.md) document.


## License

This project is licensed under the Mozilla Public License 2.0. See the
[LICENSE](./LICENSE) file for the full license text.
