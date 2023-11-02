enum Term {
  Var(String)
  Abs(String, Term)
  App(Term, Term)
}

struct Index {
  name : String
  depth : Int
}

enum TermDBI {
  VarDBI(Int)
  AbsDBI(TermDBI)
  AppDBI(TermDBI, TermDBI)
}

/// apply function `f` to each element of List, collect the results into a new List.
fn map[X, Y](self: List[X], f: (X) -> Y) -> List[Y] {
  match self {
    Nil => Nil
    Cons(x, rest) => Cons(f(x), map(rest, f))
  }
}

fn cons[T](self : List[T], x : T) -> List[T] {
  Cons(x, self)
}


/// https://bor0.wordpress.com/2019/03/19/writing-a-lambda-calculus-evaluator-in-haskell/
/// https://magic.huohuo.moe/html/DBI.html
fn bruijn(self : Term) -> Result[TermDBI, String] {
  fn find(map : List[Index], varname : String) -> Result[Int, String] {
    match map {
      Nil => Err(varname) // abort("variable \'\(varname)\' not found")
      Cons(i, rest) => {
        if i.name == varname {
          Ok(i.depth)
        } else {
          find(rest, varname)
        }
      }
    }
  }
  fn go(m : List[Index], t : Term) -> Result[TermDBI, String] {
    match t {
      Var(name) => {
        let idx = find(m, name)
        match idx {
          Err(name) => Err(name)
          Ok(idx) => Ok(VarDBI(idx))
        }
      }
      Abs(varname, body) => {
        let m = m.map(fn (index){
          { name : index.name, depth : index.depth + 1 }
        }).cons({ name : varname, depth : 0 })
        let res = go(m, body)
        match res {
          Err(name) => Err(name)
          Ok(term) => Ok(AbsDBI(term))
        }
      }
      App(e1, e2) => {
        match (go(m, e1), go(m, e2)) {
          (Ok(e1), Ok(e2)) => Ok(AppDBI(e1, e2))
          (Err(name), _) => Err(name)
          (_, Err(name)) => Err(name)
        }
      }
    }
  }
  go(Nil, self)
}

fn init {
  println("")
}
