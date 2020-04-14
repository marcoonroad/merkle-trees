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

/**
 This functor provides only an abstract Merkle Tree
 algorithm ready to be instanced with given hash
 function (recommended digest sizes are at least
 256-bits).
*/
module Make: ITreeBuilder =
  (Hash: IHash) => {
    type tree = {
      leaves: list(string),
      root: string,
    };

    let digest = Hash.digest;

    let _HASH_LENGTH = String.length @@ digest("Hello, World!");
    let _NULL_HASH = String.make(_HASH_LENGTH) @@ Char.chr(0);

    let __hash_leaf = leaf => {
      let image1 = digest(leaf);
      let image2 = digest(image1);
      let image3 = /* Crypto.xor_string(image1, image2) */ image1 ++ image2;
      let image4 = digest(image3);
      image4;
    };

    let __hash_node = (left, right) => {
      let fst = min(left, right);
      let snd = max(left, right);
      digest(fst ++ snd);
    };

    let __get_nth = (list, index) =>
      try(List.nth(list, index)) {
      | _ => _NULL_HASH
      };

    let __boot = (list, idx) => {
      let fst = __get_nth(list, idx * 2);
      let snd = __get_nth(list, idx * 2 + 1);
      __hash_node(fst, snd);
    };

    let rec __merkle = list => {
      let len = List.length(list);

      if (len <= 1) {
        list;
      } else if (len mod 2 == 0) {
        __merkle @@ Helpers.List.init(~f=__boot(list), ~len=len / 2);
      } else {
        __merkle @@ Helpers.List.init(~f=__boot(list), ~len=len / 2 + 1);
      };
    };

    let __compute_root =
      fun
      | [] => digest("")
      | [root] => digest(root)
      | nodes => digest @@ List.nth(__merkle(nodes), 0);

    let tree = values => {
      let leaves = List.map(__hash_leaf, values);
      let root = __compute_root(leaves);
      {leaves, root};
    };

    let root = ({root, _}) => root;

    let rec __find_position = (value, counter) =>
      fun
      | [] => failwith("Not found!")
      | [value', ...list'] =>
        if (value == value') {
          counter;
        } else {
          __find_position(value, counter + 1, list');
        };

    let rec __compute_path = (path, index) =>
      fun
      | [] => path
      | [_] => List.rev(path)
      | list => {
          let length = List.length(list);
          let index' =
            if (index mod 2 == 0) {
              index + 1;
            } else {
              index - 1;
            };
          let length' =
            if (length mod 2 == 0) {
              length / 2;
            } else {
              length / 2 + 1;
            };
          let node = __get_nth(list, index');
          let path' = [node, ...path];
          let list' = Helpers.List.init(~f=__boot(list), ~len=length');
          __compute_path(path', index / 2, list');
        };

    let path = (~tree as {leaves, _}, ~leaf) => {
      let image = __hash_leaf(leaf);
      let index = __find_position(image, 0, leaves);
      __compute_path([], index, leaves);
    };

    let verify = (~root, ~leaf, ~path) => {
      let image = __hash_leaf(leaf);
      let traversal = List.fold_left(__hash_node, image, path);
      root == Hash.digest(traversal);
    };
  };

/**
  WARNING: Helper function used only for internal purposes. Don't rely on
  it, it's prone to have a broken API and even to be removed entirely. You
  have been warned.
*/
module Helpers = Helpers;

