--! Error value helpers.

--- Error value with a diagnostic message.
export hidden let Error := data {
  | ErrorMessage(message : String)
};

--- Build an error value.
export let error (message : String) := .ErrorMessage(message := message);

--- Return error message.
export let (self : Error).message () :=
  match self (
  | .ErrorMessage(message := value) => value
  );

--- Return printable error text.
export let (self : Error).toString () := self.message();
