open Core_bench.Bench;
module Command = Core.Command;
module PRNG = Support.PRNG;

let heights = [3, 5, 10, 12, 15];

let power_of_two = number => {
  int_of_float(2.0 ** float_of_int(number));
};

module MerkleBench = (Hash: Support.IHash) => {
  module Merkle = MerkleTrees.Make(Hash);

  let generate = height => {
    let length = power_of_two(height);
    let leaves = PRNG.list_of_random_256_bits(length);
    let case = () => {
      ignore(Merkle.tree(leaves));
    };
    let name =
      "merkle-trees tree generation ("
      ++ Hash.name
      ++ " / h = "
      ++ string_of_int(height)
      ++ ")";
    Test.create(case, ~name);
  };

  let compute = height => {
    let length = power_of_two(height);
    let leaves = PRNG.list_of_random_256_bits(length);
    let tree = Merkle.tree(leaves);
    let index = PRNG.random_int(length);
    let leaf = List.nth(leaves, index);
    let case = () => {
      ignore(Merkle.path(~tree, ~leaf));
    };
    let name =
      "merkle-trees path computation ("
      ++ Hash.name
      ++ " / h = "
      ++ string_of_int(height)
      ++ ")";
    Test.create(case, ~name);
  };

  let verify = height => {
    let length = power_of_two(height);
    let leaves = PRNG.list_of_random_256_bits(length);
    let tree = Merkle.tree(leaves);
    let index = PRNG.random_int(length);
    let leaf = List.nth(leaves, index);
    let path = Merkle.path(~tree, ~leaf);
    let root = Merkle.root(tree);
    let case = () => {
      assert(Merkle.verify(~root, ~leaf, ~path));
    };
    let name =
      "merkle-trees path verification ("
      ++ Hash.name
      ++ " / h = "
      ++ string_of_int(height)
      ++ ")";
    Test.create(case, ~name);
  };

  let generations = List.map(generate, heights);
  let computations = List.map(compute, heights);
  let verifications = List.map(verify, heights);
};

module SHA256Bench = MerkleBench(Support.SHA256);
module Blake2BBench = MerkleBench(Support.Blake2B);

let suite =
  SHA256Bench.generations
  @ Blake2BBench.generations
  @ SHA256Bench.computations
  @ Blake2BBench.computations
  @ SHA256Bench.verifications
  @ Blake2BBench.verifications;

Command.run(make_command(suite));
