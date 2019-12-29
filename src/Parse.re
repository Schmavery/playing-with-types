open! Util;
let verbose = true;

module Stream = {
  type t('a) = list('a);
  /* type t = {text: string, pos: int}; */
  /* let get = l => */
  /*   switch (l) { */
  /*   | [] => Error("End of stream") */
  /*   | [tok, ...rest] => Ok((tok, rest)) */
  /*   }; */
};

type parseResult('a, 's) = result(('a, Stream.t('s)), string);
type parser('a, 's) = Stream.t('s) => parseResult('a, 's);

/* let pattern = (pattern) => { */
/*   let re = compile(Re.Pcre.re(pattern)); */
/*   (stream) => { */
/*     exec_opt() */
/*   }; */
/* }; */

let first =
    (parsers: list(parser('a, 's)), stream: Stream.t('s))
    : parseResult('a, 's) => {
  let rec inner = parsers =>
    switch (parsers) {
    | [] => Error("No matching parsers")
    | [p, ...rest] => Result.bind_error(p(stream), _ => inner(rest))
    };
  inner(parsers);
};

let join = (parserA, parserB, stream) => {
  switch (parserA(stream)) {
  | Ok((a, stream)) =>
    switch (parserB(stream)) {
    | Ok((b, stream)) => Ok(((a, b), stream))
    | Error(_) as e => e
    }
  | Error(_) as e => e
  };
};

let trace = (tokenPrinter, name, parser, stream) => {
  verbose ? print_endline("Starting to parse `" ++ name ++ "`") : ();
  let r = parser(stream);
  if (verbose) {
    switch (r) {
    | Ok((_, _)) => print_endline("Finished parsing `" ++ name ++ "`")
    | Error(e) =>
      let next =
        switch (stream) {
        | [t, ..._] => tokenPrinter(t)
        | [] => "EOF"
        };
      print_endline(
        "Error parsing `" ++ name ++ "` (next token `" ++ next ++ "`): " ++ e,
      );
    };
  };
  r;
};

let (<*>) = join;

let list = (~sep=?, parser, stream) => {
  let rec inner = (stream, acc) =>
    switch (parser(stream), sep) {
    | (Ok((v, stream)), Some(sep)) =>
      let acc = [v, ...acc];
      switch (sep(stream)) {
      | Ok((_, stream)) => inner(stream, acc)
      | Error(_) => Ok((List.rev(acc), stream))
      };
    | (Ok((v, stream)), _) => inner(stream, [v, ...acc])
    | (Error(_), _) => Ok((List.rev(acc), stream))
    };
  inner(stream, []);
};

let one = (fn, stream) =>
  switch (stream) {
  | [] => Error("End of stream")
  | [tok, ...rest] => fn(tok) |> Result.map(v => (v, rest))
  };

// bind?
let (|>>) = (parser, f, stream): parseResult(_, _) => {
  switch (parser(stream)) {
  | Ok((a, stream)) =>
    let parser2 = f(a);
    parser2(stream);
  | Error(_) as e => e
  };
};

// ignore
let ( *>> ) = (parser1, parser2, stream): parseResult(_, _) => {
  switch (parser1(stream)) {
  | Ok((_, stream)) => parser2(stream)
  | Error(_) as e => e
  };
};

let map = (parser, f, stream): parseResult(_, _) => {
  parser(stream) |> Result.map(((a, stream)) => (f(a), stream));
};

let (==>) = (a, b): parser(_, _) => map(a, b);

let fst = p => {
  p ==> (((a, _)) => a);
};

let snd = p => {
  p ==> (((_, b)) => b);
};
