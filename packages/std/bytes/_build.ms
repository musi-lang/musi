export let empty () : []Int := [];

export let single (value : Int) : []Int := [value];

export let copy (src : []Int) : []Int := [...src];

export let concat (left : []Int, right : []Int) : []Int := [...left, ...right];

export let append (bytes : []Int, value : Int) : []Int := [...bytes, value];

export let prepend (value : Int, bytes : []Int) : []Int := [value, ...bytes];
