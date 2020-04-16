module List = {
  include List;

  let reduce = (~f, list) => {
    let head = List.hd(list);
    let tail = List.tl(list);
    List.fold_left(f, head, tail);
  };

  let init = (~f, ~len) => {
    let rec loop = (index, buffer) =>
      if (index <= 0) {
        buffer;
      } else {
        let node_index = index - 1;
        let new_buffer = [f(node_index), ...buffer];
        loop(node_index, new_buffer);
      };
    loop(len, []);
  };
};

module String = {
  include String;

  let of_list = list => {
    let f = String.make(1);
    let pieces = List.map(f, list);
    List.reduce(~f=(++), pieces);
  };

  let to_list = string => {
    let len = String.length(string);
    let f = index => {
      string.[index];
    };
    List.init(~f, ~len);
  };
};

module type IHex = {
  let encode: string => string;
  let decode: string => string;
};

module Hex: IHex = {
  let table = [
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
  ];

  let map = index => {
    List.nth(table, index);
  };

  exception InvalidHexChar(char);
  exception InvalidHexLength(int);

  let unmap = code => {
    switch (code) {
    | '0' => 0
    | '1' => 1
    | '2' => 2
    | '3' => 3
    | '4' => 4
    | '5' => 5
    | '6' => 6
    | '7' => 7
    | '8' => 8
    | '9' => 9
    | 'a' => 10
    | 'b' => 11
    | 'c' => 12
    | 'd' => 13
    | 'e' => 14
    | 'f' => 15
    | x => raise(InvalidHexChar(x))
    };
  };

  let encode = text => {
    let len = String.length(text);
    let f = index => {
      let char = text.[index];
      let byte = Char.code(char);
      let first = map(byte / 16);
      let second = map(byte mod 16);
      String.make(1, first) ++ String.make(1, second);
    };
    let hex = List.init(~f, ~len);
    List.reduce(~f=(++), hex);
  };

  let decode = hex => {
    let len = String.length(hex);
    if (len mod 2 != 0) {
      raise(InvalidHexLength(len));
    } else {
      let f = index => {
        let offset = index * 2;
        let first = unmap(hex.[offset]);
        let second = unmap(hex.[offset + 1]);
        let char = Char.chr(first * 16 + second);
        String.make(1, char);
      };
      let bytes = List.init(~f, ~len=len / 2);
      List.reduce(~f=(++), bytes);
    };
  };
};

module Crypto = {
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
};
