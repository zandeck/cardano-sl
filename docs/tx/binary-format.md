# Transaction Binary Format

## Notes

1. We use [CBOR](http://cbor.io/) data format for transaction binary representation. See below
   for more details.
2. All lengths are specified in bytes. The term "byte" is used as a synonym for "octet",
   8-bit unsigned value. All multi-byte values are encoded in network byte order (most
   significant byte first, also known as "Big-Endian").
3. Length of some multi-byte values is **platform-dependent**, 64-bit platform is assumed
   by default.

## What is CBOR?

The Concise Binary Object Representation (CBOR) is a data format whose design goals include
the possibility of extremely small code size, fairly small message size, and extensibility
without the need for version negotiation.

The stable reference (RFC 7049) is available [here](https://tools.ietf.org/html/rfc7049).

### Diagnostic Notation

You can test CBOR in action [online](http://cbor.me/). Just paste diagnostic notation in the
left field and click "Convert to bytes" button - you will see real binary representation in
the right field. Diagnostic notation is a human-readable data representation based on JSON,
extending it where needed.

For example, this diagnostic notation:

```
[1, 2, [3, "45"], '6']
```

will be represented as:

```
bytes         comments

84            # array(4)
   01         # unsigned(1)
   02         # unsigned(2)
   82         # array(2)
      03      # unsigned(3)
      62      # text(2)
         3435 # "45"
   41         # bytes(1)
      36      # "6"
```

Hereafter diagnostic notation will be used, for more convenience.

## Basic Format of Transaction

Each transaction is represented as a 3-parts list:

```
[
    [ ... ],    # Inputs
    [ ... ],    # Outputs 
    { ... }     # Attributes
]
```

where:

* `Inputs` - list of transaction inputs (at least one input is required),
* `Outputs` - list of transaction outputs (at least one output is required),
* `Attributes` - transaction attributes (can be empty).

## Transaction Inputs

Transaction inputs is a non-empty list of inputs:

```
[
    [ Input 0 ],
    [ Input 1 ],
    ...
    [ Input N ]
]
```

Each input corresponds to the output of some previous transaction, so input contains:

```
[
    0,                                                      # InType
    [
        h'BB57EF4DDC170EFAE1B1...FD42A498664BB6E7F1B5',     # TxId
        28491                                               # TxOutputIndex
    ]
]
```

where:

* `InType` - type of this input,
* `TxId` - identifier of previous transaction `T` (prefix `h` means raw bytestring in hex-format),
* `TxOutputIndex` - index of the output in `T` that corresponds to this input.

This input has type `0`, it means that it is common input corresponding to
`utxo` (unspent transaction output). Other type (with value `1`) corresponds to
unknown type of input (for some new types in the future).

Identifier of previous transaction `T` is a hash of `T`. It is BLACK2b-256 hash,
so its length is 32 bytes.

Index of corresponding output in `T` is 4-byte integer.

## Transaction Outputs

Transaction outputs is a non-empty list of outputs:

```
[
    [ Output 0 ],
    [ Output 1 ],
    ...
    [ Output M ]
]
```

Each output includes transaction value (amount of money) and recipient's address. Is is
represented as a list, for example:

```
[
    [ ... ],                # Address
    43633881177268914       # Value
]
```

where `Address` is an address of recipient, and `Value` is a value we sent, in [Lovelaces](https://cardanodocs.com/glossary/#lovelace).
`Value` is 8-byte integer, because Lovelace is the smallest unit of money in Cardano SL.

### Address

Recipient's address is a list:

```
[
    [
        h'C61F822357B7F4A48CB9...A7C602C4A7856A6',    # Root
        {                                             # Attributes
            0: h'8200581CFC310...F678BD553206AC4',    # StakeDistribution
            1: h'49EB93EC6B3A6205311D'                # PkDerivationPath
        },
        0                                             # AddrType
    ],
    3948132476                                        # Checksum
]
```

where:

* `Root` - address root,
* `Attributes` - map with address attributes,
* `AddrType` - type of an address,
* `Checksum` - CRC32 checksum of an address.

#### Address Root

Address root is a root of imaginary pseudo Merkle tree stored in this address.
Actually it is a hash used to identify the address. Technically it is BLACK2b-224 hash,
so its length is 28 byte.

#### Address Attributes

Technically attributes is a map, where `key` is 1-byte integer, and `value` is a value
of arbitrary type.

Address attributes have two keys:

* `0` for stake distribution,
* `1` for derivation path.

Default values for it are:

* `BootstrapEraDistr` for `0` (default stake distribution for [Bootstrap era](https://cardanodocs.com/timeline/bootstrap/)),
* `Nothing` for `1` (there is no derivation path).

There are 3 options for stake distribution:

* Bootstrap era distribution,
* single key distribution`,
* multiple key distribution`.

Please see the full example below for single key distribution.

#### Address Type

Supported types of an address are:

* `0` (like in example above) - `PublicKey`-address,
* `1` - `Script`-address,
* `2` - `Redeem`-address,
* `3` - unknown address (for new types in future releases).

Address type is 1-byte value.

## Transaction Attributes

As mentioned before, attributes is a map, where `key` is 1-byte integer, and `value` is a
value of arbitrary type.

Please note that currently transactions always have **empty** attributes. Probably in future
releases transactions will have real attributes.

## Full Example

This is an example of the real transaction with 2 inputs, 2 outputs and without attributes.
For more convenience this transaction is presented in different formats.

### Raw Bytestring (HEX)

```
839f8200d81858268258204806bbdfa6bbbfea0443ab6c301f6d7d04442f0a146877f654c08da092af3dd8193c508200d818582682582060fc8fbdd6ff6c3b455d8a5b9f86d33f4137c45ece43abb86e04671254e12c08197a8bff9f8282d818585583581ce6e37d78f4326709af13851862e075bce800d06401ad5c370d4d48e8a20058208200581c23f1de5619369c763e19835e0cb62c255c3fca80aa13057a1760e804014f4e4ced4aa010522e84b8e70a121894001ae41ef3231b0075fae341e487158282d818585f83581cfd9104b3efb4c7425d697eeb3efc723ef4ff469e7f37f41a5aff78a9a20058208200581c53345e24a7a30ec701611c7e9d0593c41d6ea335b2eb195c9a0d2238015818578b485adc9d142b1e692de1fd5929acfc5a31332938f192011ad0fcdc751b0003d8257c6b4db7ffa0
```

### CBOR Diagnostic Notation

```
[
    [
        [
            0,
            [
                h'4806BBDFA6BBBFEA0443AB6C301F6D7D04442F0A146877F654C08DA092AF3DD8',
                15440
            ]
        ],
        [
            0,
            [
                h'60FC8FBDD6FF6C3B455D8A5B9F86D33F4137C45ECE43ABB86E04671254E12C08',
                31371
            ]
        ]
    ],
    [
        [
            [
                [
                    h'E6E37D78F4326709AF13851862E075BCE800D06401AD5C370D4D48E8',
                    {
                        0: h'8200581C23F1DE5619369C763E19835E0CB62C255C3FCA80AA13057A1760E804',
                        1: h'4E4CED4AA010522E84B8E70A121894'
                    },
                    0
                ],
                3827233571
            ],
            33208426245162773
        ],
        [
            [
                [
                    h'FD9104B3EFB4C7425D697EEB3EFC723EF4FF469E7F37F41A5AFF78A9',
                    {
                        0: h'8200581C53345E24A7A30EC701611C7E9D0593C41D6EA335B2EB195C9A0D2238',
                        1: h'578B485ADC9D142B1E692DE1FD5929ACFC5A31332938F192'
                    },
                    1
                ],
                3506232437
            ],
            1082080442928567
        ]
    ],
    {}
]
```

### Haskell-based Form:

```
UnsafeTx {
    _txInputs =
        TxInUtxo {
            txInHash = AbstractHash 4806bbdfa6bbbfea0443ab6c301f6d7d04442f0a146877f654c08da092af3dd8,
            txInIndex = 15440
        } :| [
        TxInUtxo {
            txInHash = AbstractHash 60fc8fbdd6ff6c3b455d8a5b9f86d33f4137c45ece43abb86e04671254e12c08,
            txInIndex = 31371
        }],
    _txOutputs =
        TxOut {
            txOutAddress = Address {
                addrRoot = AbstractHash e6e37d78f4326709af13851862e075bce800d06401ad5c370d4d48e8,
                addrAttributes = Attributes {
                    data: AddrAttributes {
                        aaPkDerivationPath = Just (HDAddressPayload {
                            getHDAddressPayload = "L\237J\160\DLER.\132\184\231\n\DC2\CAN\148"
                        }),
                        aaStakeDistribution = SingleKeyDistr (AbstractHash 23f1de5619369c763e19835e0cb62c255c3fca80aa13057a1760e804)
                    }
                },
                addrType = ATPubKey
            },
            txOutValue = Coin {getCoin = 33208426245162773}
        } :| [
        TxOut {
            txOutAddress = Address {
                addrRoot = AbstractHash fd9104b3efb4c7425d697eeb3efc723ef4ff469e7f37f41a5aff78a9,
                addrAttributes = Attributes {
                    data: AddrAttributes {
                        aaPkDerivationPath = Just (HDAddressPayload {
                            getHDAddressPayload = "\139HZ\220\157\DC4+\RSi-\225\253Y)\172\252Z13)8\241\146"
                        }),
                        aaStakeDistribution = SingleKeyDistr (AbstractHash 53345e24a7a30ec701611c7e9d0593c41d6ea335b2eb195c9a0d2238)
                    }
                },
                addrType = ATScript
            },
            txOutValue = Coin {getCoin = 1082080442928567}
        }],
        _txAttributes = Attributes { data: () }
}
```

### Human-Readable Short Form

```
Tx 48a404c7 with
    inputs
    [
        TxInUtxo 4806bbdf #15440,
        TxInUtxo 60fc8fbd #31371
    ],
    outputs:
    [
        TxOut 33208426245162772 coin(s) -> BDHJBaaLRuJoaXR7USuHDyFgoBWnaaaA88EMu8aH9wcR9BB4XmgAtmVfZNvNThsnXdgNwr1MaLG1AaBu9riCZaTMaBnoGzshgs7CHi4P3YKqBFioMne92Ym4LgLG9wDuiA,
        TxOut 1082080442928567 coin(s) -> 3XsWSbV7z5bxQVv3YScPKuv6AQbNswgu4phHXmqcnQDnt9QC1WkrnvHsLkRxQVcPE78iXVUymMhYx72EL9jDFfvjhrerXQqc2Y31ab5pLhhfWcfbKwQNXzmdcZZuFR6cJecqSvjeVSU3pG4L
    ]
```
