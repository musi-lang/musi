data Value :=
  case IntVal(value : Int32)
| case StrVal(value : String)
| case BoolVal(value : Int32)
| case None;

data Env {
  vars : List;
};

data List := case Cons(head : (String, Value), tail : ^List) | case Nil;

def Env.lookup(env : ^Env, name : String) -> Value {
  match [env.vars] {
    case Cons((k, v), tail) => if (k = name) { v } else { Env.lookup(@Env{ .vars := tail }, name) },
    case Nil => None
  }
};

def Env.set(env : ^Env, name : String, value : Value) -> ^Env {
  val new_vars : List := Cons((name, value), env.vars);
  return @Env{ .vars := new_vars };
};

data Expr :=
  case LitInt(value : Int32)
| case LitStr(value : String)
| case LitBool(value : Int32)
| case Var(name : String)
| case Add(lhs : ^Expr, rhs : ^Expr)
| case Print(expr : ^Expr)
| case Assign(name : String, expr : ^Expr)
| case Seq(first : ^Expr, second : ^Expr);

def eval(expr : ^Expr, env : ^Env) -> (Value, ^Env) {
  match [expr] {
  case LitInt(v) => (IntVal(v), env),
  case LitStr(s) => (StrVal(s), env),
  case LitBool(b) => (BoolVal(b), env),
  case Var(name) => (Env.lookup(env, name), env),
  case Add(lhs, rhs) => {
    val (lv, env1) := eval(lhs, env);
    val (rv, env2) := eval(rhs, env1);
    match (lv, rv) {
    case (IntVal(a), IntVal(b)) => (IntVal(a + b), env2),
    case _ => (None, env2)
    }
  },
  case Print(e) => {
    val (v, env1) := eval(e, env);
    match v {
    case IntVal(i) => writeln($"PRINT: {i}"),
    case StrVal(s) => writeln($"PRINT: {s}"),
    case BoolVal(b) => writeln($"PRINT: {b}"),
    case None => writeln("PRINT: <none>")
    };
    (v, env1)
  },
  case Assign(name, e) => {
    val (v, env1) := eval(e, env);
    val env2 := Env.set(env1, name, v);
    (v, env2)
  },
  case Seq(fst, snd) => {
    val (_, env1) := eval(fst, env);
    eval(snd, env1)
  }
  }
};

// let x = 10; let y = 20; print(x + y); x = x + 5; print(x)
val prog : Expr := Seq(
  @Assign("x", @LitInt(10)),
  @Seq(
    @Assign("y", @LitInt(20)),
    @Seq(
      @Print(@Add(@Var("x"), @Var("y"))),
      @Seq(
        @Assign("x", @Add(@Var("x"), @LitInt(5))),
        @Print(@Var("x"))
      )
    )
  )
);
val env : Env := { .vars := Nil };
val (_, _) := eval(@prog, @env);
