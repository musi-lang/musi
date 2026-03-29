let quoted_value := quote (#value);
let quoted_block := quote {
  #value;
  #(f(x));
  #[xs, ys];
};
