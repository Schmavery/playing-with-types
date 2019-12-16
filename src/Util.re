module List = {
  include List;

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

type result('a, 'b) = Result.t('a, 'b) = | Ok('a) | Error('b);
