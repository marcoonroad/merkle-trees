open Core_bench.Bench;
module Command = Core.Command;
module PRNG = Support.PRNG;
module Merkle = Support.MerkleTree;

let heights = [3, 5, 10, 12, 15];

let power_of_two = number => {
  int_of_float(2.0 ** float_of_int(number));
};

let generate = height => {
  let length = power_of_two(height);
  let leaves = PRNG.list_of_random_256_bits(length);
  let case = () => {
    ignore(Merkle.tree(leaves));
  };
  let name = "merkle-trees tree generation (sha256 / h = " ++ string_of_int(height) ++ ")";
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
  let name = "merkle-trees path computation (sha256 / h = " ++ string_of_int(height) ++ ")";
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
  let name = "merkle-trees path verification (sha256 / h = " ++ string_of_int(height) ++ ")";
  Test.create(case, ~name);
};

let generations = List.map(generate, heights);
let computations = List.map(compute, heights);
let verifications = List.map(verify, heights);

let suite = generations @ computations @ verifications;

Command.run(make_command(suite));
