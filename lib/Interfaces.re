module type IHex = {
  let encode: string => string;
  let decode: string => string;
};

/** The interface for Merkle Tree operations. */
module type IMerkleTree = {
  /** The abstract data type of Merkle Tree instance. */
  type tree;

  /**
     Computes a Merkle Tree instance given a list of leaves. This list can be of any size,
    but sizes such as [2.0 ** 20.0] break the runtime with stack overflow exceptions. This
    operation, so, is unfeasible on huge values, even if we rewrite the implementation to
    avoid stack overflows.
  */
  let tree: list(string) => tree;

  /** Retrieves the Root Hash of Merkle Tree for verification. Runs in O(1) complexity. */
  let root: tree => string;

  /**
     Generates a proof-of-inclusion through a list of hashes known as the
    Authentication Path.
  */
  let path: (~tree: tree, ~leaf: string) => list(string);

  /** Verifies the proof-of-inclusion of the leaf for given Merkle Tree Root Hash. */
  let verify: (~root: string, ~leaf: string, ~path: list(string)) => bool;
};

/** The contract for Merkle Tree's underlying parametric hash algorithm. */
module type IHash = {
  /**
     The hash function. It's recommended for the output be under binary/ASCII
    mode instead of hexadecimals, otherwise, bogus behavior could happen 'cause
    this library uses the binary output assumption in many places.
  */
  let digest: string => string;
};

/** The interface for abstract Merkle Tree algorithm. */
module type ITreeBuilder = (Hash: IHash) => IMerkleTree;
