# Transaction Signing

This article describes how we sign transactions in Cardano SL.

## Input and Witness

Transaction in Cardano SL includes 3 parts:

1. list of inputs,
2. list of outputs,
3. attributes.

But since currently attributes are always empty, we can think of transaction as
a pair of non-empty lists: list of inputs and list of outputs.

There is important concept: **witness**. A witness is a proof that a transaction
is allowed to spend the funds it spends. A separate witness contains a signature
and is provided for each input of transaction.

As a result we have a pair `(Tx, [TxInputWitness])`, where:

* `Tx` is a new transaction,
* `TxInputWitness` is a witness for particular input of `Tx`.

## Witness Structure

Witness is a pair `(PublicKey, Signature (Hash Tx))`, where:

* `Tx` is a new transaction,
* `Hash Tx` is a BLAKE2b-256 hash of `Tx`,
* `PublicKey` is a public key of the creator of `Tx`,
* `Signature` is a signature of the hash of `Tx` (Ed25519-based signature system is used).
  Secret key corresponding to `PublicKey` is using for signing.

TO BE CONTINUED.
