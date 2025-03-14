let n := -5
if n > 10 then
    writeln("n > 10")
else if n = 10 then 
    writeln("n = 10")
else
    writeln("n < 10")

unless n > 0 then writeln("negative")

let abs_n := n * -1 unless n > 0
writeln("zero") unless n