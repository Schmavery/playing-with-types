open Util;

Lex.Test.run();

type token =
  | LParen
  | RParen
  | Comma
  | Num(int)
  | Ident(string)
  | String(string);

let lexer = [
  ("\\(", _ => Some(LParen)),
  ("\\)", _ => Some(RParen)),
  (",", _ => Some(Comma)),
  ("[0-9]+", arr => Some(Num(int_of_string(arr[0])))),
  ("[a-z][a-zA-Z0-9]*", arr => Some(Ident(arr[0]))),
  ("\"([^\"]*)\"", arr => Some(String(arr[1]))),
  (" +", _ => None),
];

let (tokenToString, resultToString) = {
  open ToString;
  let token = t =>
    switch (t) {
    | LParen => "LParen"
    | RParen => "RParen"
    | Comma => "Comma"
    | Num(n) => "Num(" ++ int(n) ++ ")"
    | Ident(s) => "Ident(" ++ s ++ ")"
    | String(s) => "String(" ++ s ++ ")"
    };
  let toks = list(token);
  let res = result(toks, pair(string, toks));
  (token, res);
};

let example = {|foo(bar, 1, baz(12), "abc123")|};
/* let example = {|foo()|}; */

print(resultToString, Lex.go(lexer, example));

type ast =
  | Num(int)
  | Apply(ast, list(ast))
  | Ident(string)
  | String(string);

let astToString = {
  open ToString;
  let parenList = (f, l) =>
    "(" ++ String.concat(", ", List.map(f, l)) ++ ")";
  let rec ast = t =>
    switch (t) {
    | Num(n) => int(n)
    | Ident(s) => s
    | String(s) => "\"" ++ s ++ "\""
    | Apply(f, l) => ast(f) ++ parenList(ast, l)
    };
  ast;
};

let error = (s, t) => {
  Error("Expected " ++ s ++ " but got " ++ tokenToString(t) ++ " instead.");
};

let parser = {
  open Parse;

  let str =
    one(
      fun
      | (String(s): token) => Ok(String(s): ast)
      | t => error("String", t),
    );

  let lparen =
    one(
      fun
      | LParen => Ok()
      | t => error("LParen", t),
    );

  let comma =
    one(
      fun
      | Comma => Ok()
      | t => error("Comma", t),
    );

  let rparen =
    one(
      fun
      | RParen => Ok()
      | t => error("RParen", t),
    );

  let ident =
    one(
      fun
      | (Ident(s): token) => Ok(Ident(s): ast)
      | t => error("Ident", t),
    );

  let rec commaExprList = () => list(~sep=comma, expr())
  and fn = () =>
    ident
    |>> (
      i =>
        lparen
        *>> join(commaExprList(), rparen)
        ==> (((l, _)) => Apply(i, l))
    )
  and expr = () => first([fn(), str, ident]);

  expr();
};

let example = {|foo("bar",    foo, foo(""))|};

let res =
  switch (Lex.go(lexer, example)) {
  | Ok(stream) =>
    switch (parser(stream)) {
    | Ok((ast, _)) => astToString(ast)
    | Error(e) => e
    }
  | Error(_) as e => resultToString(e)
  };

print_endline(res);
