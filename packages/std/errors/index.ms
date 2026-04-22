--- Error value with a diagnostic message.
export opaque let Error := data {
  | Error(message : String)
};

--- Build an error value.
export let error (message : String) : Error := .Error(message := message);

--- Return error message.
export let(self : Error).message () : String :=
  match self (
  | .Error(message := value) => value
  );

--- Return printable error text.
export let(self : Error).toString () : String := self.message();
