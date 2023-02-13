<h1 align="center">Mina Governance</h1>

<p align="center">
  <b>Mina Governance provides users a web outlet to govern the Mina Blockchain.</b>
</p>

## Protocol Specifications (WIP)

The Mina Governance Protocol is designed to provide community members with a transparent and secure method of participating in the decision-making process for the Mina blockchain. The aim for this protocol is to provide stake holders with the ability to vote on MIPs (Mina Improvement Proposals) in a clear & concise way.

Individual MIPs can be created on Github. ([https://github.com/MinaProtocol/MIPs](https://github.com/MinaProtocol/MIPs))

### Voting on a MIP

To cast a vote on a particular MIP, users must send a transaction to the **themselves** with a specific memo.<br>
The memo field must adhere to the following convention:<br>

**For example:**

```
To vote in favor of 'MIP1', the memo field would be populated with: 'MIP1'
Similarly - if your intent is to vote against 'MIP1', the memo field would contain: 'no MIP1'
```

**The transaction amount must be 0, with the user only paying for the transaction fee.**

### Protocol Flow

This flow chart illustrates the process of voting for a specific MIP on Mina blockchain.<br>
**Documentation will be updated.**

## Development

- If not installed, install [`nvm`](https://github.com/nvm-sh/nvm)

  ```bash
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash

  # or ...

  wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash
  ```

  ```bash
  nvm install 16

  # and ...

  nvm use default v16
  ```

- If not installed, install [`pnpm`](https://pnpm.io/)

  ```bash
  brew install pnpm

  # or ...

  curl -fsSL https://get.pnpm.io/install.sh | sh -
  ```

- If not installed, install [Rust](https://www.rust-lang.org/) - [Cargo-Make](https://github.com/sagiegurari/cargo-make) - [Typeshare-CLI](https://github.com/1Password/typeshare) - [Diesel-CLI](https://crates.io/crates/diesel_cli/2.0.1)

  ```bash
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh # install rust
  cargo install --force cargo-make # install cargo-make
  cargo install diesel_cli --no-default-features --features postgres # install diesel-cli
  cargo install typeshare-cli # install typeshare-cli
  
  ```

- Checkout this repository via `git` or the [Github CLI](https://cli.github.com/)

  ```bash
  git clone git@github.com:Granola-Team/mina-governance.git

  # or ...

  gh repo clone Granola-Team/mina-governance
  ```

- In the new directory, install dependencies

  ```bash
  pnpm clean && pnpm install
  ```

## Resources

- [Next.js Documentation](https://nextjs.org/docs/getting-started)
- [Rust Programming Language](https://doc.rust-lang.org/book/)
- [Typescript](https://www.typescriptlang.org/docs/)
