let io := import "@std/io";

export hidden let User := data {
  | User(id : Int, name : String, admin : Bit)
};

export let ada := .User(id := 1, name := "Ada", admin := 1 = 1);

export let grace := .User(id := 2, name := "Grace", admin := 1 = 0);

export let canEdit (user : User) :=
  match user (
  | .User(id := id, name := name, admin := admin) => (id, name, admin)
  );

export let demo () := (canEdit(ada), canEdit(grace));

demo();
io.writeLn("data model: typed user rows evaluated");
