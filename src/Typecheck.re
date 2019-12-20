open Common;
open! Util;

let rec synth = (ctx, s) =>
  switch (s) {
  | Unit => Ok(UnitT)
  | Bool(_) => Ok(BoolT)
  | Int(_) => Ok(IntT)
  | Var(s) =>
    switch (StringMap.find(s, ctx)) {
    | t => Ok(t)
    | exception Not_found => Error("Undefined variable `" ++ s ++ "`.")
    }
  | If(cond, fst, snd) =>
    let condT = synth(ctx, cond);
    let fstT = synth(ctx, fst);
    let sndT = synth(ctx, snd);
    switch (condT, fstT, sndT) {
    | (Ok(BoolT), Ok(t1), Ok(t2)) when t1 == t2 => Ok(t1)
    | (Ok(BoolT), Ok(t1), Ok(t2)) =>
      Error(
        Printf.sprintf(
          "Unmatched types in if statement branches. First: `%s`, second: `%s`",
          Stringify.typ(t1),
          Stringify.typ(t2),
        ),
      )
    | (Error(_) as e, _, _)
    | (_, Error(_) as e, _)
    | (_, _, Error(_) as e) => e
    | (Ok(t), _, _) =>
      Error(
        "Expected bool as the condition of the if statement. Got `"
        ++ Stringify.typ(t)
        ++ "` instead.",
      )
    };
  | Annot(s, t) =>
    switch (checkType(ctx, s, t)) {
    | Ok () => Ok(t)
    | Error(e) => Error(e)
    }
  | App(fn, argList) =>
    switch (synth(ctx, fn)) {
    | Ok(FnT(fnArgTList, t2)) =>
      List.fold_left2(
        (acc, arg, fnArgT) =>
          switch (acc) {
          | Ok () => checkType(ctx, arg, fnArgT)
          | Error(_) as e => e
          },
        Ok(),
        argList,
        fnArgTList,
      )
      ->Result.bind(() => Ok(t2))
    | Ok(t) =>
      Error(
        "Tried to call `" ++ Stringify.typ(t) ++ "`, which isn't a function.",
      )
    | err => err
    }
  | LetIn(name, value, body) =>
    switch (synth(ctx, value)) {
    | Ok(val_t) => synth(StringMap.add(name, val_t, ctx), body)
    | Error(e) => Error(e)
    }
  | Fn(args, body) =>
    let res =
      List.fold_right(
        (acc, v) =>
          switch (v, acc) {
          | (Ok((ctx, l)), (argName, Some(argType))) =>
            Ok((StringMap.add(argName, argType, ctx), [argType, ...l]))
          | (Ok(_), (argName, _)) =>
            Error("Couldn't infer type of argument " ++ argName)
          | (Error(e), _) => Error(e)
          },
        args,
        Ok((ctx, [])),
      );
    switch (res) {
    | Ok((ctx, typeList)) =>
      synth(ctx, body)->Result.bind(t => Ok(FnT(typeList, t)))
    | Error(_) as e => e
    };
  | Cons(hd, tl) =>
    switch (synth(ctx, hd)) {
    | Ok(a) =>
      switch (checkType(ctx, tl, ListT(a))) {
      | Ok () => Ok(ListT(a))
      | Error(e) => Error(e)
      }
    | e => e
    }
  | EmptyList => Error("Unable to infer type of " ++ Stringify.syntax(s))
  }
and checkType = (ctx, s, t) =>
  switch (s) {
  | Unit
  | Bool(_)
  | Int(_)
  | App(_, _)
  | Var(_)
  | LetIn(_, _, _)
  | Annot(_, _) =>
    switch (synth(ctx, s)) {
    | Ok(inferred) when inferred == t => Ok()
    | Ok(inferred) =>
      Error(
        Printf.sprintf(
          "Inferred type `%s` did not match expected type of `%s`.",
          Stringify.typ(inferred),
          Stringify.typ(t),
        ),
      )
    | Error(e) => Error(e)
    }
  | If(cond, fst, snd) =>
    checkType(ctx, cond, BoolT)
    ->Result.bind(_ => checkType(ctx, fst, t))
    ->Result.bind(_ => checkType(ctx, snd, t))
  // TODO check that arg type is correct
  | Fn(argList, body) =>
    switch (t) {
    | FnT(argTList, returnT) =>
      List.fold_right2(
        (arg, argT, acc) =>
          switch (acc, arg) {
          | (Ok(ctx), (argName, None)) =>
            Ok(StringMap.add(argName, argT, ctx))
          | (Ok(ctx), (argName, Some(annotArgT))) =>
            annotArgT == argT
              ? Ok(StringMap.add(argName, argT, ctx))
              : Error(
                  "Annotated arg value of "
                  ++ Stringify.typ(annotArgT)
                  ++ "doesn't match inferred type of "
                  ++ Stringify.typ(argT)
                  ++ " for argument "
                  ++ argName,
                )
          | (e, _) => e
          },
        argList,
        argTList,
        Ok(ctx),
      )
      ->Result.bind(ctxWithArgs => checkType(ctxWithArgs, body, returnT))
    | actualT =>
      Error(
        "Fn did not match non-function type of `"
        ++ Stringify.typ(actualT)
        ++ "`.",
      )
    }
  | EmptyList =>
    switch (t) {
    | ListT(_) => Ok()
    | _ => Error("Expected [] to be a list, not a " ++ Stringify.typ(t))
    }
  | Cons(hd, tl) =>
    switch (t) {
    | ListT(listT) =>
      checkType(ctx, hd, listT)
      ->Result.bind(_ => checkType(ctx, tl, ListT(listT)))
    | _ =>
      Error(Stringify.syntax(s) ++ " is a list, not a " ++ Stringify.typ(t))
    }
  };
