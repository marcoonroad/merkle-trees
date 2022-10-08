/* helper library for test suite */

module type IHash = {
  let name: string;
  let digest: string => string;
};

module SHA256: IHash = {
  let name = "SHA256";
  let digest = message => {
    message
    |> Cstruct.of_string
    |> Nocrypto.Hash.SHA256.digest
    |> Cstruct.to_string;
  };
};

module Blake2B: IHash = {
  let name = "Blake2B";
  let digest = message => {
    message |> Digestif.BLAKE2B.digest_string |> Digestif.BLAKE2B.to_raw_string;
  };
};

module type IPRNG = {
  let random_256_bits: unit => string;
  let random_int: int => int;
  let list_of_random_256_bits: int => list(string);
  let list_of_random_images: int => list(string);
  let random_different_256_bits: string => string;
};

module PRNG: IPRNG = {
  let __min_random = Z.of_string_base(2, "1" ++ String.make(255, '0'));
  let __max_random = Z.of_string_base(2, String.make(256, '1'));

  let random_256_bits = () => {
    let number = Nocrypto.Rng.Z.gen_r(__min_random, __max_random);
    Cstruct.to_string(Nocrypto.Numeric.Z.to_cstruct_be(number));
  };

  let rec random_different_256_bits = random => {
    let fresh_random = random_256_bits();
    if (fresh_random == random) {
      random_different_256_bits(random);
    } else {
      fresh_random;
    };
  };

  let random_int = limit => {
    Nocrypto.Rng.Int.gen(limit);
  };

  let __random = _ => {
    random_256_bits();
  };

  let list_of_random_256_bits = length => {
    MerkleTrees.Helpers.List.init(~len=length, ~f=__random);
  };

  let list_of_random_images = length => {
    let random_list = list_of_random_256_bits(length);
    List.map(MerkleTrees.Helpers.Hex.encode, random_list);
  };
};

let __random_direction_prefix = node =>
  if (PRNG.random_int(1) == 1) {
    "R" ++ node;
  } else {
    "L" ++ node;
  };

let list_of_random_path_nodes = length => {
  let images = PRNG.list_of_random_images(length);
  List.map(__random_direction_prefix, images);
};

module MerkleTree = MerkleTrees.Make(SHA256);

Nocrypto_entropy_unix.initialize();
