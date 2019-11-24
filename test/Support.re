/* helper library for test suite */

module Hash = {
  let digest = message => {
    message
    |> Cstruct.of_string
    |> Nocrypto.Hash.SHA256.digest
    |> Cstruct.to_string;
  };
};

module type IPRNG = {
  let random_256_bits: unit => string;
  let random_int: int => int;
  let list_of_random_256_bits: int => list(string);
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
    List.init(length, __random);
  };
};

module MerkleTree = MerkleTrees.Make(Hash);

Nocrypto_entropy_unix.initialize();
