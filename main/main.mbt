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
  Var(String, Int) 
  Abs(String, TermDBI)
  App(TermDBI, TermDBI)
}


fn to_string(self : TermDBI) -> String {
  match self {
    Var(name, _) => name
    Abs(name, body) => "(\\\(name).\(body))"
    App(t1, t2) => "\(t1) \(t2)"
  }
}

/// https://bor0.wordpress.com/2019/03/19/writing-a-lambda-calculus-evaluator-in-haskell/
/// https://magic.huohuo.moe/html/DBI.html
fn bruijn(self : Term) -> Result[TermDBI, String] {
  // 查找环境中第一个varname对应的整数
  fn find(m : List[Index], varname : String) -> Result[Int, String] {
    match m {
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
        let idx = find(m, name)?
        Ok(Var(name, idx))
      }
      Abs(varname, body) => {
        let m = m.map(fn (index){
          { name : index.name, depth : index.depth + 1 }
        })
        let m = List::Cons({ name : varname, depth : 0 }, m)
        let term = go(m, body)?
        Ok(Abs(varname, term))
      }
      App(e1, e2) => {
        let e1 = go(m, e1)?
        let e2 = go(m, e2)?
        Ok(App(e1, e2))
      }
    }
  }
  go(Nil, self)
}

fn subst(t1 : TermDBI, t2 : TermDBI) -> TermDBI {
  fn go(t1 : TermDBI, t2 : TermDBI, depth : Int) -> TermDBI {
    match t1 {
      Var(_, d) => {
        if d == depth {
          t2
        } else {
          t1
        }
      }
      Abs(name, t) => {
        Abs(name, go(t, t2, depth + 1))
      }
      App(tl, tr) => {
        App(go(tl, t2, depth), go(tr, t2, depth))
      }
    }
  }
  go(t1, t2, 0)
}

fn eval(self : TermDBI) -> TermDBI {
  match self {
    App(t1, t2) => {
      match (eval(t1), eval(t2)) {
        (Abs(_, t1), t2) => eval(subst(t1, t2))
        (t1, t2) => App(t1, t2)
      }
    }
    Abs(_) => self
    otherwise => abort("eval(): \(otherwise) ") 
    // eval应该不会遇到自由变量才对
  }
}

fn test1() -> Unit {
  // (\x.x x) (\y.y)
  let exp : Term = App(Abs("x", App(Var("x"), Var("x"))), Abs("x", Var("x")))
  let exp = exp.bruijn()
  match exp {
    Err(name) => abort("err on \(name)\n")
    Ok(t) => {
      println("\(t)")
      let t = eval(t)
      println("\(t)")
    }
  }
}

fn main {
  test1()
}
