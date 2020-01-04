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
  ("let", _ => Some(LetL)),
  ("=", _ => Some(EqL)),
  (",", _ => Some(CommaL)),
  (":", _ => Some(ColonL)),
  (";", _ => Some(SemicolonL)),
  ("=>", _ => Some(ArrowL)),
  ("[0-9]+", arr => Some(IntL(int_of_string(arr[0])))),
  ("[a-z_][a-zA-Z0-9_]*", arr => Some(IdentL(arr[0]))),
  ("\"([^\"]*)\"", arr => Some(StringL(arr[1]))),
  ("\\s", _ => None),
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
    | SemicolonL => "Semicolon"
    | FunL => "Fun"
    | LetL => "Let"
    | EqL => "Eq"
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

let astToString = Common.Stringify.syntax;

let error = (s, t) => {
  Error("Expected " ++ s ++ " but got " ++ tokenToString(t) ++ " instead.");
};

let parser = {
  open! Parse;

  let simple = toMatch =>
    one(nextToken =>
      nextToken == toMatch ? Ok() : error(tokenToString(toMatch), nextToken)
    );
  let name = (a, b, c) => trace(tokenToString, a, b, c);

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

  let bracketWrap = parser =>
    lbracket *>> join(parser, rbracket) ==> (((a, _)) => a);

  let str =
    one(
      fun
      | StringL(s) => Ok(String(s))
      | t => error("String", t),
    );
  let int =
    one(
      fun
      | IntL(s) => Ok(Int(s))
      | t => error("Int", t),
    );
  let ident =
    one(
      fun
      | IdentL(s) => Ok(Var(s))
      | t => error("Ident", t),
    );

  let identStr =
    one(
      fun
      | IdentL(s) => Ok(s)
      | t => error("Ident", t),
    );

  let unit = join(lparen, rparen) ==> (_ => Unit);
  let bool = name("bool", first([true_, false_]));

  /*   let varToString = */
  /*     fun */
  /*     | Var(v) => v */
  /*     | _ => failwith("expected var"); */

  let typIdent =
    one(
      fun
      | IdentL("bool") => Ok(BoolT)
      | IdentL("int") => Ok(IntT)
      | IdentL("string") => Ok(StringT)
      | IdentL("unit") => Ok(UnitT)
      | t => error("Ident(*)", t),
    );
  let rec typ = s => name("type", first([typIdent, typList, arrowTyp]), s)
  and typList =
    simple(IdentL("list")) *>> parenWrap(typIdent) ==> (t => ListT(t))
  and typArgs = s => parenWrap(list(~sep=comma, typ), s)
  and arrowTyp = s => {
    (
      join(typArgs, simple(ArrowL) *>> typ)
      ==> (((fst, snd)) => FnT(fst, snd))
    )(
      s,
    );
  };

  let rec commaExprList = s => list(~sep=comma, expr, s)
  and annot = s =>
    (join(expr_no_annot, colon *>> typ) ==> (((e, t)) => Annot(e, t)))(s)
  and listLit = s =>
    bracketWrap(
      list(~sep=comma, expr)
      ==> (l => List.fold_right((v, a) => Cons(v, a), l, EmptyList)),
      s,
    )
  and fn = s => {
    let args =
      fun_
      *>> first([
            parenWrap(list(~sep=comma, identStr)),
            identStr ==> (s => [s]),
          ]);
    let body = arrow *>> expr;
    let mapArgs = List.map(var => (var, None));
    let p = args <*> body ==> (((a, b)) => Fn(mapArgs(a), b));
    p(s);
  }
  and apply = s =>
    (ident |>> (i => parenWrap(commaExprList) ==> (l => App(i, l))))(s)
  and letin = s => {
    (
      fst(join(simple(LetL) *>> identStr, simple(EqL)))
      |>> (
        var => {
          let v = fst(join(expr, simple(SemicolonL)));
          join(v, expr) ==> (((v, rest)) => LetIn(var, v, rest));
        }
      )
    )(
      s,
    );
  }
  and expr_no_annot = s => {
    first(
      [
        letin,
        apply,
        fn,
        str,
        ident,
        listLit,
        bool,
        int,
        unit,
        parenWrap(expr),
      ],
      s,
    );
  }
  and expr = s => name("expr", first([annot, expr_no_annot]), s);

  expr;
};

/* let example = {|foo("bar", fun (a) -> 1, "aas" : string, foo2("", []))|}; */
/* let example = {|(fun (a) -> 1)(1)|}; */
let example = {|
   let empty = ((fun a => a("")) : ((string) => string) => string);
   let a = empty(fun _ => "");
   print_int(1)
   |};

let stdlib =
  StringMap.empty |> StringMap.add("print_int", FnT([IntT], UnitT));

let res =
  switch (Lex.go(lexer, example)) {
  | Ok(stream) =>
    switch (parser(stream)) {
    | Ok((ast, _)) =>
      let t = Typecheck.synth(stdlib, ast);
      let typ = ToString.(result(Stringify.typ, string));
      astToString(ast) ++ "\ntype: " ++ typ(t);
    | Error(e) => e
    }
  | Error(_) as e => resultToString(e)
  };

print_endline(res);
