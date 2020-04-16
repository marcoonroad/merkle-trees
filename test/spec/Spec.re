/* the test pre-computes two sets of random strings */

open Support;

let _LENGTH = PRNG.random_int(64) + 4;
let _FST_SET = PRNG.list_of_random_256_bits(_LENGTH);
let _SND_SET = PRNG.list_of_random_256_bits(_LENGTH);

let __deterministic_generation = () => {
  let merkle_tree_a = MerkleTree.tree(_FST_SET);
  let merkle_tree_b = MerkleTree.tree(_FST_SET);
  let root_a = MerkleTree.root(merkle_tree_a);
  let root_b = MerkleTree.root(merkle_tree_b);
  Alcotest.(check(string))(
    "should compute the same root given same leaves",
    root_a,
    root_b,
  );
};

let __deterministic_path_computation = () => {
  let tree = MerkleTree.tree(_SND_SET);
  let index = PRNG.random_int(_LENGTH);
  let value = List.nth(_SND_SET, index);

  let path_a = MerkleTree.path(~tree, ~leaf=value);
  let path_b = MerkleTree.path(~tree, ~leaf=value);

  Alcotest.(check(list(string)))(
    "should compute the same path given same leaf",
    path_a,
    path_b,
  );
};

let __deterministic_verification = () => {
  let tree = MerkleTree.tree(_FST_SET);
  let root = MerkleTree.root(tree);
  let index = PRNG.random_int(_LENGTH);
  let leaf = List.nth(_FST_SET, index);
  let path = MerkleTree.path(~tree, ~leaf);
  let noise_leaf = PRNG.random_different_256_bits(leaf);
  let noise_path = PRNG.list_of_random_images(PRNG.random_int(6));
  let other_index = (index + 1) mod _LENGTH;
  let other_leaf = List.nth(_FST_SET, other_index);
  let other_path = MerkleTree.path(~tree, ~leaf=other_leaf);

  let result1 = MerkleTree.verify(~root, ~leaf, ~path);
  let result2 = MerkleTree.verify(~root, ~leaf=noise_leaf, ~path);
  let result3 = MerkleTree.verify(~root, ~leaf, ~path);
  let result4 = MerkleTree.verify(~root, ~leaf=other_leaf, ~path);
  let result5 = MerkleTree.verify(~root, ~leaf, ~path);
  let result6 = MerkleTree.verify(~root, ~leaf, ~path=noise_path);
  let result7 = MerkleTree.verify(~root, ~leaf, ~path);
  let result8 = MerkleTree.verify(~root, ~leaf, ~path=other_path);

  let success = result1 && result3 && result5 && result7;
  let failure = result2 || result4 || result6 || result8;

  Alcotest.(check(bool))(
    "should pass verification given proper leaf and path",
    true,
    success,
  );
  Alcotest.(check(bool))(
    "should fail verification given wrong leaf or path",
    false,
    failure,
  );
};

let suite = [
  (
    "deterministic merkle tree generation",
    `Quick,
    __deterministic_generation,
  ),
  (
    "deterministic authentication path computation",
    `Quick,
    __deterministic_path_computation,
  ),
  (
    "deterministic merkle tree path verification",
    `Quick,
    __deterministic_verification,
  ),
];

Alcotest.run(
  "merkle trees specification",
  [("merkle trees test suite", suite)],
);
