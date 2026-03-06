export record Pair['A, 'B] { first: 'A, second: 'B };

export fn pair_new['A, 'B](a: 'A, b: 'B): Pair['A, 'B] => Pair.{ first := a, second := b };
export fn pair_first['A, 'B](p: Pair['A, 'B]): 'A => p.first;
export fn pair_second['A, 'B](p: Pair['A, 'B]): 'B => p.second;
export fn pair_swap['A, 'B](p: Pair['A, 'B]): Pair['B, 'A] => Pair.{ first := p.second, second := p.first };
