export const is_newline := proc (c: Int) -> Bool {
  c = 10
};

export const is_carriage_return := proc (c: Int) -> Bool {
  c = 13
};

export const is_space := proc (c: Int) -> Bool {
  c = 32
};

export const is_tab := proc (c: Int) -> Bool {
  c = 9
};

export const is_digit := proc (c: Int) -> Bool {
  c >= 48 and then c <= 57
};

export const is_upper := proc (c: Int) -> Bool {
  c >= 65 and then c <= 90
};

export const is_lower := proc (c: Int) -> Bool {
  c >= 97 and then c <= 122
};

export const is_alpha := proc (c: Int) -> Bool {
  is_upper(c) or else is_lower(c)
};

export const is_alnum := proc (c: Int) -> Bool {
  is_alpha(c) or else is_digit(c)
};

export const to_upper := proc (c: Int) -> Int {
  if is_lower(c) then {
    c - 32
  } else {
    c
  }
};

export const to_lower := proc (c: Int) -> Int {
  if is_upper(c) then {
    c + 32
  } else {
    c
  }
};
