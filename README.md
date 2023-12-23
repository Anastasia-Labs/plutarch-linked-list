<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
# Table of Contents

- [Plutarch Linked List](#plutarch-linked-list)
  - [Introduction](#introduction)
  - [Documentation](#documentation)
    - [Linked List](#linked-list)
    - [Plutarch Linked List](#plutarch-linked-list-implementation)
  - [Getting Started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Building and Developing](#building-and-developing)
  - [Sample Validator](#sample-validator)
  - [Case study](#case-study)
  - [Acknowledgments](#acknowledgments)

<!-- markdown-toc end -->

# Plutarch Linked List

## Introduction

Linked list structures leverage the EUTXO model to enhancing scalability and throughput significantly. By linking multiple UTXOs together through a series of minting policies and validators, it can improve the user experience interacting with smart contract concurrently.

This project is funded by the Cardano Treasury in [Catalyst Fund 10](https://projectcatalyst.io/funds/10/f10-osde-open-source-dev-ecosystem/anastasia-labs-the-trifecta-of-data-structures-merkle-trees-tries-and-linked-lists-for-cutting-edge-contracts) and is aimed at enhancing the capabilities of Cardano smart contracts in handling complex data structures.

## Documentation

### Linked List

The Plutarch Linked List is an on-chain, sorted linked list solution designed for blockchain environments, specifically utilizing NFTs (Non-Fungible Tokens) and datums. It provides a structured and efficient way to store and manipulate a list of key/value pairs on-chain.

![linked-list](https://static.slab.com/prod/uploads/pigzq8jp/posts/images/CrHALgKG4UBGKJxXzwB1zWdP.png)

#### Entry Structure

Each entry in the list comprises:

- **NFT**: A unique identifier for each entry.
- **EntryDatum**: A data structure containing the key/value pair, a reference to the entry's NFT, and a pointer to the next NFT in the list.

#### EntryDatum Definition

```haskell
data EntryDatum = EntryDatum {
  key :: BuiltinByteString,
  value :: Maybe SomeValue,
  nft :: NFT,
  next :: Maybe NFT
}
```

- **key**: A unique identifier for the entry.
- **value**: The value associated with the key. It can be Nothing for the head entry.
- **nft**: The NFT representing the entry.
- **next**: The NFT of the next entry in the list, or Nothing for the last entry.

#### Operations

##### Inserting an Entry

![insert entry](https://static.slab.com/prod/uploads/pigzq8jp/posts/images/5JvbsTZjWTi0YsTAhveRZfWB.png)

Insertion involves:

- **Inputs**: Two adjacent list entries.
- **Outputs**:
  - The first input entry, modified to point to the new entry.
  - The newly inserted entry, pointing to the second input entry.
  - The second input entry, unchanged.

Validation Rules

- Keys must maintain the order: a < b < c, where a is the lowest, b is the new key, and c is the highest.
- The pointers must be correctly updated to maintain list integrity.

##### Removing an Entry

![remove entry](https://static.slab.com/prod/uploads/pigzq8jp/posts/images/cPWvnR5BVwdZjpUGHTXXQtx8.png)

To remove an entry:

- **Inputs**: The entry to remove and its preceding entry.
- **Output**: The preceding entry is modified to point to what the removed entry was pointing to.

#### Utilizing NFTs as Pointers

NFTs serve as robust and unique pointers within the list. Their uniqueness is ensured by a specific minting policy related to the list's head NFT.

#### Key Considerations

- **Efficiency**: As on-chain lookups are inefficient, off-chain structures are recommended for this purpose.
- **Datum Hashes**: Not suitable for pointers due to the complexity of updates and security concerns.
- **Security**: The integrity of the list is maintained through careful minting policies and entry validation.

#### Advanced Features

- **[Forwarding Minting Policy](https://github.com/Plutonomicon/plutonomicon/blob/main/forwarding1.md)**: A feature of Plutus to control NFT minting dynamically.
- **List Head**: Utilizes an empty head entry for validating insertions at the start of the list.
- **End-of-List Insertions**: Handled by ensuring the last entry points to Nothing.

### Plutarch Linked List implementation

The Plutarch Linked List implementation provides several functions to create and manipulate Linked List. Below is a brief overview of each function:

- `pInit`: Constructs the Linked List head
- `pDeinit`: Destructs the Linked List
- `pInsert`: Inserts a node into the linked list
- `pRemove`: Removes a node from the linked list

## Getting Started

### Prerequisites

Before you begin, ensure you have [Nix](https://nixos.org/download.html) installed on your system. Nix is used for package management and to provide a consistent development environment.
To install run the following command:

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

and follow the instructions.

```sh
$ nix --version
nix (Nix) 2.18.1
```

Make sure to enable [Nix Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes) by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` on
your machine and add the following configuration entries:

```yaml
experimental-features = nix-command flakes ca-derivations
allow-import-from-derivation = true
```

Optionally, to improve build speed, it is possible to set up binary caches
maintained by IOHK and Plutonomicon by setting additional configuration entries:

```yaml
substituters = https://cache.nixos.org https://iohk.cachix.org https://cache.iog.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=
```

To facilitate seamlessly moving between directories and associated Nix development shells we use [direnv](https://direnv.net) and [nix-direnv](https://github.com/nix-community/nix-direnv):

Your shell and editors should pick up on the `.envrc` files in different directories and prepare the environment accordingly. Use `direnv allow` to enable the direnv environment and `direnv reload` to reload it when necessary. Otherwise, the `.envrc` file contains a proper Nix target you can use with the `nix develop` command.

To install both using `nixpkgs`:

```sh
nix profile install nixpkgs#direnv
nix profile install nixpkgs#nix-direnv
```

### Building and developing

Once Nix is installed, you should be able to seamlessly use the repository to
develop, build and run packages.

Download the Git repository:

```sh
git clone https://github.com/Anastasia-Labs/plutarch-linked-list.git
```

Navigate to the repository directory:

```sh
cd plutarch-linked-list
direnv allow
```

Activate the development environment with Nix:

```sh
nix develop .
```

Additionally, when you run `nix run .#help` you'll get a list of scripts you can run, the Github CI (nix flake check) is setup in a way where it checks the project builds successfully, haskell format is done correctly, and commit message follows conventional commits. Before pushing you should run `cabal run` , `nix run .#haskellFormat` (automatically formats all haskell files, including cabal), if you want to commit a correct format message you can run `cz commit`

Build:

```sh
cabal build all
```

Execute the test suite:

```sh
cabal test
```

![plutarch-linked-list.gif](/assets/images/plutarch-linked-list.gif)

# Sample validator

For a complete example, including tests and further explanations, reger to the provided sample validator: [LinkedListSpec.hs](/test/Spec/LinkedListSpec.hs).

# Case study

Linked list can be leveraged in smart contract applications where the order of inputs is not crucial, and multiple users can interact with the contracts simultaneously. For example, consider a decentralized voting system where users can cast their votes concurrently. A linked list data structure can be employed to store and manage the votes efficiently. Each user's vote can be represented as a node in the linked list, containing relevant information such as the user's address and their chosen candidate.

# Acknowledgments

This documentation and the implementation of the Plutarch Linked List draw inspiration from an original idea presented in the Plutonomicon. We acknowledge the innovative concepts and approaches outlined in their work, specifically regarding associative data structures on blockchain platforms.

For further details on the foundational ideas and their context, please refer to the Plutonomicon's overview of associative data structures: [Plutonomicon's Associative Data Structures Overview](https://github.com/Plutonomicon/plutonomicon/blob/main/assoc.md#overview).

We express our gratitude to the creators and contributors of the Plutonomicon for their pioneering work in this area, which has significantly influenced the development of our Plutarch Linked List.
