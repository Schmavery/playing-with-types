open Util;

let contramap = (fatob, btostring, a) => {
  btostring(fatob(a));
};

let ignore = _ => "_";
let int = string_of_int;
let bool = string_of_bool;
let string = s => s;
let pair = (fa, fb, (a, b)) => "(" ++ fa(a) ++ ", " ++ fb(b) ++ ")";
let list = (f, l) => "[" ++ String.concat(", ", List.map(f, l)) ++ "]";
let array = f => contramap(Array.to_list, list(f));
let option = (f, o) =>
  switch (o) {
  | Some(a) => "Some(" ++ f(a) ++ ")"
  | None => "None"
  };
let result = (fa, fb, o) =>
  switch (o) {
  | Ok(a) => "Ok(" ++ fa(a) ++ ")"
  | Error(b) => "Error(" ++ fb(b) ++ ")"
  };
