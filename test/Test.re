open Util;
open Common;

Lex.Test.run();

let lexer = [
  ("\\(", _ => Some(LParenL)),
  ("\\)", _ => Some(RParenL)),
  ("\\[", _ => Some(LBracketL)),
  ("\\]", _ => Some(RBracketL)),
  ("true", _ => Some(TrueL)),
  ("false", _ => Some(FalseL)),
  ("fun", _ => Some(FunL)),
  (",", _ => Some(CommaL)),
  (":", _ => Some(ColonL)),
  ("->", _ => Some(ArrowL)),
  ("[0-9]+", arr => Some(IntL(int_of_string(arr[0])))),
  ("[a-z][a-zA-Z0-9]*", arr => Some(IdentL(arr[0]))),
  ("\"([^\"]*)\"", arr => Some(StringL(arr[1]))),
  (" +", _ => None),
];

let (tokenToString, resultToString) = {
  open ToString;
  let token = t =>
    switch (t) {
    | TrueL => "True"
    | FalseL => "False"
    | LParenL => "LParen"
    | RParenL => "RParen"
    | LBracketL => "LBracket"
    | RBracketL => "RBracket"
    | CommaL => "Comma"
    | ColonL => "Colon"
    | FunL => "Fun"
    | ArrowL => "Arrow"
    | IntL(n) => "Int(" ++ int(n) ++ ")"
    | IdentL(s) => "Ident(" ++ s ++ ")"
    | StringL(s) => "String(" ++ s ++ ")"
    };
  let toks = list(token);
  let res = result(toks, pair(string, toks));
  (token, res);
};

let example = {|foo(bar, true, baz(12, false), "abc123")|};
print(resultToString, Lex.go(lexer, example));

/* type ast = */
/*   | Unit */
/*   | Bool(bool) */
/*   | Int(int) */
/*   | Var(string) */
/*   | EmptyList */
/*   | App(ast, list(ast)) */
/*   | String(string); */

/* let astToString = { */
/*   open ToString; */
/*   let parenList = (f, l) => */
/*     "(" ++ String.concat(", ", List.map(f, l)) ++ ")"; */
/*   let rec ast = t => */
/*     switch (t) { */
/*     | Int(n) => int(n) */
/*     | Bool(b) => bool(b) */
/*     | EmptyList => "[]" */
/*     | Unit => "()" */
/*     | Var(s) => s */
/*     | Annot(s) => s */
/*     | String(s) => "\"" ++ s ++ "\"" */
/*     | App(f, l) => ast(f) ++ parenList(ast, l) */
/*     }; */
/*   ast; */
/* }; */
let astToString = Common.Stringify.syntax;

let error = (s, t) => {
  Error("Expected " ++ s ++ " but got " ++ tokenToString(t) ++ " instead.");
};

let parser = {
  open Parse;

  let simple = toMatch =>
    one(nextToken =>
      nextToken == toMatch ? Ok() : error(tokenToString(toMatch), nextToken)
    );

  let lparen = simple(LParenL);
  let rparen = simple(RParenL);
  let lbracket = simple(LBracketL);
  let rbracket = simple(RBracketL);
  let comma = simple(CommaL);
  let colon = simple(ColonL);
  let fun_ = simple(FunL);
  let arrow = simple(ArrowL);
  let true_ = simple(TrueL) ==> (_ => Bool(true));
  let false_ = simple(FalseL) ==> (_ => Bool(false));

  let parenWrap = parser =>
    lparen *>> join(parser, rparen) ==> (((a, _)) => a);

  let str =
    one(
      fun
      | StringL(s) => Ok(String(s))
      | t => error("String", t),
    );
  let ident =
    one(
      fun
      | IdentL(s) => Ok(Var(s))
      | t => error("Ident", t),
    );

  let emptyList = join(lbracket, rbracket) ==> (_ => EmptyList);
  let unit = join(lparen, rparen) ==> (_ => Unit);
  let bool = first([true_, false_]);

  /* let typIdent = */
  /*   one( */
  /*     fun */
  /*     | IdentL("bool") => Ok(BoolT) */
  /*     | IdentL("int") => Ok(IntT) */
  /*     | IdentL("unit") => Ok(UnitT) */
  /*     | t => error("Ident", t), */
  /*   ); */
  /* let rec typ = () => first([typIdent, typList, arrowTyp()]) */
  /* and typList = */
  /*   simple(IdentL("list")) *>> parenWrap(typIdent) ==> (t => ListT(t)) */
  /* and commaTypList = () => list(~sep=comma, typ()) */
  /* and arrowTyp = () => */
  /*   join(commaTypList(), typ()) ==> (((fst, snd)) => FnT(fst, snd)); */

  let rec commaExprList = () => list(~sep=comma, expr())
  /* and annot = () => */
  /*   parenWrap(join(expr(), colon *>> typ())) */
  /*   ==> (((e, t)) => Annot(e, t)) */
  and fn = () => {
    let args = fun_ *>> parenWrap(list(~sep=comma, ident));
    let body = arrow *>> expr();
    let mapArgs = List.map((Var(s)) => (s, None));
    join(args, body) ==> (((a, b)) => Fn(mapArgs(a), b));
  }
  and apply = () =>
    ident |>> (i => parenWrap(commaExprList()) ==> (l => App(i, l)))
  and expr = () => first([apply(), fn(), str, ident, emptyList, bool, unit]);

  expr();
};

let example = {|foo("bar", "", foo("", []))|};

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
