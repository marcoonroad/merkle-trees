module List = {
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
