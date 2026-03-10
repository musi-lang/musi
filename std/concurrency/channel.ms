export let send : ('T) ~> () under { Async };
export let recv : () ~> 'T under { Async };
