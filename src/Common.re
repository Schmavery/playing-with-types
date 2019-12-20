type token =
  | TrueL
  | FalseL
  | LParenL
  | RParenL
  | LBracketL
  | RBracketL
  | CommaL
  | ColonL
  | FunL
  | ArrowL
  | IntL(int)
  | IdentL(string)
  | StringL(string);

type typ =
  | BoolT
  | IntT
  | UnitT
  | ListT(typ)
  | FnT(list(typ), typ);

type syntax =
  | Unit
  | Bool(bool)
  | Int(int)
  | String(string)
  | Var(string)
  | EmptyList
  | Cons(syntax, syntax)
  | If(syntax, syntax, syntax)
  | App(syntax, list(syntax))
  | Fn(list((string, option(typ))), syntax)
  | Annot(syntax, typ)
  | LetIn(string, syntax, syntax);

module Stringify = {
  let sp = Printf.sprintf;
  let rec typ = t =>
    switch (t) {
    | BoolT => "bool"
    | IntT => "int"
    | UnitT => "unit"
    | ListT(t) => sp("list(%s)", typ(t))
    | FnT(t1s, t2) =>
      sp("(%s -> %s)", String.concat(", ", List.map(typ, t1s)), typ(t2))
    };

  let rec syntax = (~tab=0, s) => {
    let tabStr = String.make(tab * 2, ' ');
    switch (s) {
    | Unit => "unit"
    | EmptyList => "[]"
    | Bool(b) => b ? "true" : "false"
    | Int(i) => string_of_int(i)
    | Var(s) => s
    | String(s) => "\"" ++ s ++ "\""
    | Cons(hd, tl) => sp("(%s :: %s)", syntax(hd), syntax(tl))
    | If(cond, fst, snd) =>
      sp(
        "if (%s) {\n%s\n%s} else {\n%s\n%s}",
        syntax(cond),
        tabStr ++ syntax(~tab=tab + 1, fst),
        tabStr,
        tabStr ++ syntax(~tab=tab + 1, snd),
        tabStr,
      )
    | App(Var(v), argList) =>
      sp("%s(%s)", v, String.concat(", ", List.map(syntax, argList)))
    | App(fn, argList) =>
      sp(
        "(%s)(%s)",
        syntax(fn),
        String.concat(", ", List.map(syntax, argList)),
      )
    | Fn(argList, body) =>
      let argStr =
        List.map(
          ((argName, argT)) =>
            switch (argT) {
            | Some(t) => argName ++ ":" ++ typ(t)
            | None => argName
            },
          argList,
        )
        |> String.concat(", ");
      sp("(%s) -> { %s}", argStr, syntax(body));
    | Annot(s, t) => sp("(%s: %s)", syntax(s), typ(t))
    | LetIn(n, v, b) => sp("(let %s = %s;\n%s)", n, syntax(v), syntax(b))
    };
  };
};
