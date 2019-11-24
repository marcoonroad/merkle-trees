open Core_bench.Bench;
module Command = Core.Command;
module PRNG = Support.PRNG;
module Merkle = Support.MerkleTree;

let __power_of_two = number => {
  int_of_float(2.0 ** float_of_int(number));
};

let _SET_1 = PRNG.list_of_random_256_bits(__power_of_two(3));
let _SET_2 = PRNG.list_of_random_256_bits(__power_of_two(5));
let _SET_3 = PRNG.list_of_random_256_bits(__power_of_two(10));
let _SET_4 = PRNG.list_of_random_256_bits(__power_of_two(12));
let _SET_5 = PRNG.list_of_random_256_bits(__power_of_two(15));

let __generate = leaves => {
  () => {
    ignore(Merkle.tree(leaves));
  };
};

let __test_1 = Test.create(__generate(_SET_1), ~name="generation:sha256 | height = 3");
let __test_2 = Test.create(__generate(_SET_2), ~name="generation:sha256 | height = 5");
let __test_3 = Test.create(__generate(_SET_3), ~name="generation:sha256 | height = 10");
let __test_4 = Test.create(__generate(_SET_4), ~name="generation:sha256 | height = 12");
let __test_5 = Test.create(__generate(_SET_5), ~name="generation:sha256 | height = 15");

let suite = [
  __test_1,
  __test_2,
  __test_3,
  __test_4,
  __test_5
];

Command.run (make_command (suite));

