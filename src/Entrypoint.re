open Common;

type evalResult =
  | URes // unit
  | LRes(list(evalResult)) // list
  | IRes(int) // int
  | FRes((StringMap.t(evalResult), list(evalResult)) => evalResult) // func
  | BRes(bool); // bool

let rec evalToString = r =>
  switch (r) {
  | URes => "()"
  | IRes(i) => string_of_int(i)
  | BRes(b) => string_of_bool(b)
  | FRes(_) => "<function>"
  | LRes(l) => "[" ++ String.concat(", ", List.map(evalToString, l)) ++ "]"
  };

// Dumb evaluator
let rec eval = (ctx, s) =>
  switch (s) {
  | Bool(b) => BRes(b)
  | Int(i) => IRes(i)
  | Annot(s, _) => eval(ctx, s)
  | Unit => URes
  | Var(v) =>
    switch (StringMap.find(v, ctx)) {
    | v => v
    | exception Not_found => failwith("Undefined var " ++ v)
    }
  | If(c, fst, snd) =>
    switch (eval(ctx, c)) {
    | BRes(b) =>
      if (b) {
        eval(ctx, fst);
      } else {
        eval(ctx, snd);
      }
    | _ => failwith("non bool in if")
    }
  | Fn(argList, body) =>
    FRes(
      (ctx, argVList) =>
        List.fold_left2(
          (acc, (argName, _), argV) => StringMap.add(argName, argV, acc),
          ctx,
          argList,
          argVList,
        )
        ->eval(body),
    )
  | App(fn, argList) =>
    switch (eval(ctx, fn), List.map(eval(ctx), argList)) {
    | (FRes(f), a) => f(ctx, a)
    | _ => failwith("calling nonfunc")
    }
  | LetIn(name, value, body) =>
    eval(StringMap.add(name, eval(ctx, value), ctx), body)
  | EmptyList => LRes([])
  | Cons(hd, tl) =>
    switch (eval(ctx, hd), eval(ctx, tl)) {
    | (h, LRes(t)) => LRes([h, ...t])
    | _ => failwith("appending to nonlist")
    }
  };

let inferAndPrint = (name, s) => {
  print_endline(name ++ ":");
  print_endline(Stringify.syntax(s));
  switch (Typecheck.synth(StringMap.empty, s)) {
  | Ok(t) =>
    print_endline("\027[32mType:\027[0m " ++ Stringify.typ(t));
    let evalRes =
      switch (evalToString(eval(StringMap.empty, s))) {
      | v => v
      | exception (Failure(e)) => "Failed to eval -- " ++ e
      };
    print_endline("Value: " ++ evalRes);
  | Error(e) => print_endline("\027[31mError:\027[0m " ++ e)
  };
  print_newline();
};

let true_ = Bool(true);
let false_ = Bool(false);
let not =
  Annot(
    Fn([("x", None)], If(Var("x"), false_, true_)),
    FnT([BoolT], BoolT),
  );
let and_ =
  Annot(
    Fn(
      [("x", None)],
      Fn(
        [("y", None)],
        If(Var("x"), If(Var("y"), true_, false_), false_),
      ),
    ),
    FnT([BoolT], FnT([BoolT], BoolT)),
  );
let print_bool_ = Annot(Fn([("v", None)], Unit), FnT([BoolT], UnitT));
let print_int_ = Annot(Fn([("v", None)], Unit), FnT([IntT], UnitT));

let withStdlib = s =>
  LetIn(
    "print_int",
    print_int_,
    LetIn("print_bool", print_bool_, LetIn("not", not, s)),
  );

inferAndPrint("False", If(true_, false_, true_));
inferAndPrint("Cons", Cons(Int(1), EmptyList));
inferAndPrint("Annot Cons", Annot(Cons(Int(1), EmptyList), ListT(IntT)));
inferAndPrint("List of list", Cons(Cons(Int(1), EmptyList), EmptyList));
inferAndPrint(
  "Annot List of list",
  Annot(Cons(EmptyList, EmptyList), ListT(ListT(IntT))),
);
inferAndPrint("Unmatching", If(true_, false_, Int(1)));
inferAndPrint("Unknown variable", If(Var("y"), false_, true_));
inferAndPrint(
  "Return Int",
  Annot(
    Fn([("x", None)], If(Var("x"), Int(2), Int(1))),
    FnT([BoolT], IntT),
  ),
);
inferAndPrint("Annotated Not", not);
inferAndPrint(
  "Inline-annotated Not",
  Fn([("x", Some(BoolT))], If(Var("x"), false_, true_)),
);
inferAndPrint("Applied Not", App(not, [true_]));
inferAndPrint("And", and_);
inferAndPrint("Applied And", App(App(and_, [true_]), [false_]));
inferAndPrint("stdlib test", withStdlib(Var("print_bool")));
inferAndPrint(
  "stdlib",
  withStdlib(App(Var("print_bool"), [App(Var("not"), [true_])])),
);
inferAndPrint(
  "Better And",
  Annot(
    Fn(
      [("x", None), ("y", None)],
      If(Var("x"), If(Var("y"), true_, false_), false_),
    ),
    FnT([BoolT, BoolT], BoolT),
  ),
);
inferAndPrint(
  "Better Applied And",
  App(
    Fn(
      [("x", Some(BoolT)), ("y", Some(BoolT))],
      If(Var("x"), If(Var("y"), true_, false_), false_),
    ),
    [true_, false_],
  ),
);
inferAndPrint(
  "Better Applied And",
  LetIn(
    "and",
    Fn(
      [("x", Some(BoolT)), ("y", Some(BoolT))],
      If(Var("x"), If(Var("y"), true_, false_), false_),
    ),
    App(Var("and"), [true_, true_]),
  ),
);
