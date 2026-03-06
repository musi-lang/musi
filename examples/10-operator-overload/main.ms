record Vec2 { x: Int, y: Int };

given Add[Vec2] {
    fn add(a: Vec2, b: Vec2): Vec2 => Vec2.{ x := a.x + b.x, y := a.y + b.y };
};

const v1 := Vec2.{ x := 1, y := 2 };
const v2 := Vec2.{ x := 3, y := 4 };
const v3 := v1.add(v2);
writeln(int_to_string(v3.x));
writeln(int_to_string(v3.y));
