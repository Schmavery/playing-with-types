type result('a, 'b) = Result.t('a, 'b) = | Ok('a) | Error('b);
module StringMap = Map.Make(String);

module List = {
  include List;

  let rec find_opt = p =>
    fun
    | [] => None
    | [x, ...l] =>
      if (p(x)) {
        Some(x);
      } else {
        find_opt(p, l);
      };

  let filter_map = f => {
    let rec aux = accu =>
      fun
      | [] => rev(accu)
      | [x, ...l] =>
        switch (f(x)) {
        | None => aux(accu, l)
        | Some(v) => aux([v, ...accu], l)
        };
    aux([]);
  };

  let peek = (f, t) => {
    List.iter(f, t);
    t;
  };
};

module Result = {
  include Result;
  let bind_error = (r, f) =>
    switch (r) {
    | Error(e) => f(e)
    | Ok(_) as o => o
    };
};

let print = (s, v) => print_endline(s(v));

let traceWrap = (s, v) => {
  print_endline("Start " ++ s);
  let a = v();
  print_endline("End " ++ s);
  a;
};
