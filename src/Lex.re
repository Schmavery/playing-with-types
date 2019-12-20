open! Util;

type assoc('a, 'b) = list(('a, 'b));
type handler('a) = array(string) => 'a;
type t('a) = assoc(Re.Core.t, handler('a));

let lex_one = (t, s, pos) => {
  open Re.Core;
  let options =
    t
    |> List.map(((re, fn)) => (compile(Re.Pcre.re(re)), fn))
    |> List.filter_map(((re, fn)) =>
         exec_opt(~pos, re, s) |> Option.map(re => (re, fn))
       )
    |> List.filter(((re, _)) => fst(Group.offset(re, 0)) == pos)
    |> List.stable_sort(((a, _), (b, _)) => {
         let adiff = Group.stop(a, 0) - Group.start(a, 0);
         let bdiff = Group.stop(b, 0) - Group.start(b, 0);
         /* negative to sort longest first (descending) */
         - compare(adiff, bdiff);
       });

  switch (options) {
  | [] =>
    failwith(
      "Could not find match in "
      ++ String.sub(s, pos, String.length(s) - pos),
    )
  | [(re, fn), ..._] => (re, fn(Group.all(re)))
  };
};

let go = (t, str) => {
  let pos = ref(0);
  let res = ref([]);
  try (
    {
      while (pos^ < String.length(str)) {
        let (grp, res_one) = lex_one(t, str, pos^);
        pos := snd(Re.Core.Group.offset(grp, 0));
        Option.iter(res_one => res := [res_one, ...res^], res_one);
      };
      Result.Ok(List.rev(res^));
    }
  ) {
  | Failure(e) => Error((e, List.rev(res^)))
  };
};

module Test = {
  let fail = (~err="", ()) => {
    print_endline("Test failed (Lexer)" ++ err);
  };
  let assertEq = (a, b, toString) => {
    let (a, b) = (toString(a), toString(b));
    if (String.compare(a, b) != 0) {
      fail(~err=": Expected `" ++ b ++ "` got `" ++ a ++ "`", ());
    };
  };
  type token =
    | Foo
    | Foo2
    | Bar
    | Any
    | Whitespace;

  let token_pp = token =>
    switch (token) {
    | Foo => "foo"
    | Foo2 => "foo2"
    | Bar => "bar"
    | Any => "any"
    | Whitespace => "ws"
    };

  let lexer = [
    ("fooo", _ => Some(Foo2)),
    ("foo", _ => Some(Foo)),
    ("bar", _ => Some(Bar)),
    (" +", _ => Some(Whitespace)),
    (".", _ => Some(Any)),
  ];

  let tests = [
    ("foo bar", Ok([Foo, Whitespace, Bar])),
    ("foo  bar", Ok([Foo, Whitespace, Bar])),
    ("foo  #bar", Ok([Foo, Whitespace, Any, Bar])),
  ];

  let resultToString = {
    open ToString;
    let toks = list(token_pp);
    result(toks, pair(string, toks));
  };

  let run = (~verbose=false, ()) => {
    tests
    |> List.iter(((testStr, expected)) => {
         let res = go(lexer, testStr);
         if (verbose) {
           print(resultToString, res);
         };
         assertEq(res, expected, resultToString);
       });
  };
};
