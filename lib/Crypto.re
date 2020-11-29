module String = Helpers.String;

let xor_char = (left, right) => {
  let left_code = Char.code(left);
  let right_code = Char.code(right);
  Char.chr(left_code lxor right_code);
};

let xor_string = (left, right) => {
  let left_list = String.to_list(left);
  let right_list = String.to_list(right);
  let xor_list = List.map2(xor_char, left_list, right_list);
  String.of_list(xor_list);
};

module Mac = (Hash: Interfaces.IHash) => {
  let digest = Hash.digest;

  let _HASH_LENGTH = String.length @@ digest("Hello, World!");
  let _NULL_HASH = String.make(_HASH_LENGTH) @@ Char.chr(0);
  let _OPAD = String.make(_HASH_LENGTH) @@ Char.chr(0x5c);
  let _IPAD = String.make(_HASH_LENGTH) @@ Char.chr(0x36);

  let rec mac = (~key, ~data) => {
    let secret = digest(key);
    let ikey = xor_string(secret, _IPAD);
    let okey = xor_string(secret, _OPAD);
    let inner = digest(ikey ++ data);
    digest(okey ++ inner);
  };
};
