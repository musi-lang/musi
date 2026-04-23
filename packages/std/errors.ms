--- Error value with a diagnostic message.
export opaque let Error := data {
  | ErrorMessage(message : String)
};

--- Build an error value.
export let error (message : String) : Error :=
  .ErrorMessage(message := message);

--- Return error message.
export let (self : Error).message () : String :=
  match self (
  | .ErrorMessage(message := value) => value
  );

--- Return printable error text.
export let (self : Error).toString () : String := self.message();
