open! Util;

/* https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf */

type syntaxInfer = [
  | `Ann(syntaxCheck, typ)
  | `BoundVar(int)
  | `FreeVar(string)
  | `App(syntaxInfer, syntaxCheck)
]
and syntaxCheck = [ | `Inf(syntaxInfer) | `Abs(syntaxCheck)]
and name =
  | Local(int)
  | Global(string)
and typ =
  | TFree(string)
  | TFun(typ, typ)
and value =
  | VNeutral(neutral)
  | VLam(value => value)
and neutral =
  | NFree(string)
  | NApp(neutral, value);

type syntaxAll = [
  | `Ann(syntaxCheck, typ)
  | `BoundVar(int)
  | `FreeVar(string)
  | `App(syntaxInfer, syntaxCheck)
  | `Inf(syntaxInfer)
  | `Abs(syntaxCheck)
];

let subtype = (t1, t2) => {
  t1 == t2;
};

module ToString = {
  include ToString;
  let sp = Printf.sprintf;
  let rec typ =
    fun
    | TFree(s) => sp("TFree(%s)", s)
    | TFun(t1, t2) => sp("TFun(%s -> %s)", typ(t1), typ(t2));

  let rec neutral =
    fun
    | NFree(s) => sp("Free(%s)", s)
    | NApp(n, v) => sp("%s %s", neutral(n), value(v))
  and value =
    fun
    | VNeutral(n) => neutral(n)
    | VLam(fn) => "_ -> " ++ value(fn(VNeutral(NFree("_"))));

  let rec syn = (e: [< syntaxAll]) =>
    switch (e) {
    | `Ann((s: syntaxCheck), (t: typ)) =>
      sp("(%s: %s)", syn((s :> syntaxAll)), typ(t))
    | `BoundVar(i) => sp("Bound(%i)", i)
    | `FreeVar(s) => sp("Free(%s)", s)
    | `App(fn, arg) =>
      sp("%s %s", syn((fn :> syntaxAll)), syn((arg :> syntaxAll)))
    | `Inf((s: syntaxInfer)) => sp("%s", syn((s :> syntaxAll)))
    | `Abs((s: syntaxCheck)) => sp("lam.%s", syn((s :> syntaxAll)))
    };
};

let rec infer = (e: syntaxInfer, ctx) =>
  switch (e) {
  | `Ann(e, t) => check(e, t, ctx) |> Result.map(() => t)
  | `BoundVar(i) =>
    switch (List.nth(ctx, i)) {
    | v => Ok(v)
    | exception Not_found => Error("Couldn't find var")
    }
  | `FreeVar(s) => Ok(TFree(s))
  | `App(fn, arg) =>
    Result.bind(
      infer(fn, ctx),
      fun
      | TFun(argT, bodyT) => check(arg, argT, ctx) |> Result.map(() => bodyT)
      | _ => Error("oh no"),
    )
  }
and check = (e: syntaxCheck, t, ctx) =>
  switch (e, t) {
  | (`Inf(e), tCheck) =>
    Result.bind(infer(e, ctx), tInferred =>
      if (subtype(tInferred, tCheck)) {
        Ok();
      } else {
        Error(
          ToString.(sp("Got %s, expected %s", typ(tInferred), typ(tCheck))),
        );
      }
    )
  | (`Abs(e), TFun(tArg, tBody)) => check(e, tBody, [tArg, ...ctx])
  | (`Abs(_), t) => Error("Got function, expected " ++ ToString.typ(t))
  };

let rec evalInfer = (e: syntaxInfer, ctx) =>
  switch (e) {
  | `FreeVar(s) => VNeutral(NFree(s))
  | `BoundVar(i) => List.nth(ctx, i)
  | `Ann(e, _) => evalCheck(e, ctx)
  | `App(fn, arg) =>
    let a = evalCheck(arg, ctx);
    switch (evalInfer(fn, ctx)) {
    | VNeutral(n) => VNeutral(NApp(n, a))
    | VLam(f) =>
      // TODO: don't do this weird fn thing
      f(a)
    };
  }
and evalCheck = (e: syntaxCheck, ctx) =>
  switch (e) {
  | `Inf(e) => evalInfer(e, ctx)
  | `Abs(body) => VLam(v => evalCheck(body, [v, ...ctx]))
  }
and eval = (e: syntaxAll, c) =>
  switch (e) {
  | #syntaxCheck as e => evalCheck(e, c)
  | #syntaxInfer as e => evalInfer(e, c)
  };

let rec subst = (e: [< syntaxAll], i, term) =>
  switch (e) {
  | `Abs(body) => subst(body, i + 1, term)
  | `BoundVar(j) when i == j => term
  | `FreeVar(_)
  | `BoundVar(_)
  | `Ann(_, _)
  | `Inf(_)
  | `App(_, _) => e
  };

module Test = {
  let id = `Abs(`Inf(`BoundVar(0)));
  let idT = TFun(TFree("a"), TFree("a"));
  let term1 = `Ann((id, idT));

  let run = (s: syntaxInfer) => {
    open ToString;
    print(syn, (s :> syntaxAll));
    print(result(typ, string), infer(s, []));
    print(value, evalInfer(s, []));
  };
  run(term1);
};
