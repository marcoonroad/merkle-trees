# merkle-trees

Optimized Merkle Tree algorithm for traversal/verification and path computation. [Work In Progress]

[![Build Status](https://dev.azure.com/marcoonroad/marcoonroad/_apis/build/status/marcoonroad.merkle-trees?branchName=stable)](https://dev.azure.com/marcoonroad/marcoonroad/_build/latest?definitionId=3&branchName=stable)

## API

This library is generic over the underlying used hash algorithm. Once the hash is provided, the main functor will return a merkle tree algorithm. The interface for such returned merkle tree module follows below:

```ocaml
type tree
```
The abstract data type of Merkle Tree instance.

```ocaml
val tree : string list -> tree
```
Computes a Merkle Tree instance given a list of leaves. This list can be of any size, but sizes such as 2.0 ** 20.0 break the runtime with stack overflow exceptions. This operation, so, is unfeasible on huge values, even if we rewrite the implementation to avoid stack overflows.

```ocaml
val root : tree -> string
```
Retrieves the Root Hash of Merkle Tree for verification. Runs in O(1) complexity.

```ocaml
val path : tree:tree -> leaf:string -> string list
```
Generates a proof-of-inclusion through a list of hashes known as the Authentication Path.

```ocaml
val verify : root:string -> leaf:string -> path:string list -> bool
```
Verifies the proof-of-inclusion of the leaf for given Merkle Tree Root Hash.
