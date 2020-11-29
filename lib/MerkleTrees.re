module Hex = Helpers.Hex;
module List = Helpers.List;
module String = Helpers.String;

/**
  This functor provides only an abstract Merkle Tree
 algorithm ready to be instanced with given hash
 function (recommended digest sizes are at least
 256-bits).
*/
module Make: Interfaces.ITreeBuilder =
  (Hash: Interfaces.IHash) => {
    type tree = {
      leaves: list(string),
      root: string,
    };

    type direction =
      | L
      | R;

    module Hmac = Crypto.Mac(Hash);
    open Hmac;

    let __hash_leaf = leaf => {
      mac(~key="LEAF", ~data=leaf);
    };

    let __hash_node = (left, right) => {
      digest(left ++ right);
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
        __merkle @@ List.init(~f=__boot(list), ~len=len / 2);
      } else {
        __merkle @@ List.init(~f=__boot(list), ~len=len / 2 + 1);
      };
    };

    let __compute_root =
      fun
      | [] => Hash.digest("")
      | [root] => Hash.digest(root)
      | nodes => Hash.digest @@ List.nth(__merkle(nodes), 0);

    let tree = values => {
      let leaves = List.map(__hash_leaf, values);
      let root = Hex.encode(__compute_root(leaves));
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
          let prefix' =
            if (index' mod 2 == 0) {
              "L";
            } else {
              "R";
            };
          let node = __get_nth(list, index'); /* get sibling node of current hash node*/
          let path' = [prefix' ++ Hex.encode(node), ...path];
          let list' = List.init(~f=__boot(list), ~len=length');
          __compute_path(path', index / 2, list');
        };

    let path = (~tree as {leaves, _}, ~leaf) => {
      let image = __hash_leaf(leaf);
      let index = __find_position(image, 0, leaves);
      let path = __compute_path([], index, leaves);
      if (List.length(path) > 0) {
        path;
      } else {
        failwith("Could not compute an existing authentication path!");
      };
    };

    let __decode_node = node => {
      let chars = String.to_list(node);
      let direction =
        switch (List.hd(chars)) {
        | 'L' => L
        | 'R' => R
        | chr =>
          failwith(
            "Invalid path node, could not decode direction ("
            ++ Char.escaped(chr)
            ++ ")!",
          )
        };
      let hash = Hex.decode @@ String.of_list @@ List.tl(chars);
      (direction, hash);
    };

    let __hash_path_node = (current, node) => {
      let (direction, hash) = __decode_node(node);
      switch (direction) {
      | L => digest(hash ++ current)
      | R => digest(current ++ hash)
      };
    };

    let rec __fold_ordered = (current, path) => {
      switch (path) {
      | [] => current
      | [node, ...rest] =>
        __fold_ordered(__hash_path_node(current, node), rest)
      };
    };

    let rec __ordered_traversal = (root, image, path) => {
      root == Hex.encode(Hash.digest(__fold_ordered(image, path)));
    };

    let verify = (~root, ~leaf, ~path) => {
      let image = __hash_leaf(leaf);
      __ordered_traversal(root, image, path);
    };
  };

/**
   WARNING: Helper function used only for internal purposes. Don't rely on
  it, it's prone to have a broken API and even to be removed entirely. You
  have been warned.
*/
module Helpers = Helpers;
