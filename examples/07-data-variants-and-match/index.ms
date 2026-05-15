let io := import "@std/io";

export hidden let Color := data {
  | Rgb(r : Int, g : Int, b : Int)
  | Gray(v : Int)
};

export let red := .Rgb(r := 255, g := 0, b := 0);

export let midGray := .Gray(v := 128);

export let luminance (color : Color) :=
  match color (
  | .Rgb(r := r, g := g, b := b) => (r + g + b) / 3
  | .Gray(v := v) => v
  );

export let demo () := (luminance(red), luminance(midGray));

demo();
io.writeLn("match: RGB and grayscale variants were destructured");
