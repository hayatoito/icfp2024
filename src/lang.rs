// https://icfpcontest2024.github.io/icfp.html

use crate::prelude::*;

use std::{collections::BTreeMap, io::Read};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct LangS(Vec<u8>);

const CONVERT: &[u8; 94] = byte_strings::concat_bytes!(br##"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!"#$%&'()*+,-./:;<=>?@[\]^_`|~ "##, b"\n");

impl LangS {
    pub fn new(body: Vec<u8>) -> Self {
        Self(body)
    }

    pub fn as_raw_str(&self) -> &str {
        std::str::from_utf8(&self.0).unwrap()
    }

    pub fn to_int(&self) -> Number {
        parse_int(&self.0)
    }

    pub fn to_s_string(&self) -> String {
        format!("S{}", self.as_raw_str())
    }

    pub fn to_human_string(&self) -> String {
        let bs: Vec<u8> = self.0.iter().map(|b| CONVERT[(b - 33) as usize]).collect();
        String::from_utf8(bs).unwrap()
    }

    pub fn from_human_str(s: &str) -> Self {
        lazy_static! {
            static ref ENCODE: [u8; 128] = {
                let mut res = [0; 128];
                let mut index = ASCII_START;
                for b in CONVERT {
                    res[*b as usize] = index;
                    index += 1;
                }
                res
            };
        }
        Self::new(s.as_bytes().iter().map(|b| ENCODE[*b as usize]).collect())
    }
}

impl std::fmt::Display for LangS {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_human_string())
    }
}

pub fn send_file(path: impl AsRef<Path>) -> Result<String> {
    let s = std::fs::read_to_string(path.as_ref())?;
    Ok(send_string(&s))
}

pub fn send_string(s: &str) -> String {
    let token = std::env::var("API_TOKEN").unwrap();

    let url = "https://boundvariable.space/communicate";
    let client = reqwest::blocking::Client::new();
    let mut res = client
        .post(url)
        .header("Authorization", token)
        .body(s.trim().to_string())
        .send()
        .unwrap();
    let mut content = String::new();
    res.read_to_string(&mut content).unwrap();
    content
}

type Number = i64;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum Exp {
    // * Primitives
    // T or F
    Bool(bool),
    // I
    Int(Number),
    // S
    S(LangS),

    // * Unarray
    // U -
    Negate(Box<Exp>),
    // U !
    Not(Box<Exp>),
    // U #
    StringToInt(Box<Exp>),
    // U $
    IntToString(Box<Exp>),

    // * Binary
    // B +
    Add(Box<Exp>, Box<Exp>),
    // B -
    Sub(Box<Exp>, Box<Exp>),
    // B *
    Mul(Box<Exp>, Box<Exp>),
    // B /
    Div(Box<Exp>, Box<Exp>),
    // B %,
    Mod(Box<Exp>, Box<Exp>),
    // B <
    Lt(Box<Exp>, Box<Exp>),
    // B >
    Gt(Box<Exp>, Box<Exp>),
    // B =
    Eq(Box<Exp>, Box<Exp>),
    // B |
    Or(Box<Exp>, Box<Exp>),
    // B &
    And(Box<Exp>, Box<Exp>),
    // B .
    Concat(Box<Exp>, Box<Exp>),
    // B T
    Take(Box<Exp>, Box<Exp>),
    // B D
    Drop(Box<Exp>, Box<Exp>),
    // B $
    Apply(Box<Exp>, Box<Exp>),

    // Efficiency Extension
    // B! (Lazy)
    // TODO
    //
    // B ! (Strict)
    ApplyStrict(Box<Exp>, Box<Exp>),

    // * If
    // ?
    If(Box<Exp>, Box<Exp>, Box<Exp>),

    // * Lambdan
    // L
    Lambda(Env, Number, Box<Exp>),
    // Lambda(CachedEnv, Number, Box<Exp>),

    // v
    Var(Number),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct BoundExp {
    exp: Exp,
    env: Env,
}

impl BoundExp {
    fn eval(&self) -> Exp {
        self.exp.eval(self.env.clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Env(BTreeMap<Number, BoundExp>);

impl std::fmt::Display for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (n, e) in self.0.iter() {
            write!(f, "({}: {}), ", n, e.exp)?;
        }
        Ok(())
    }
}

impl Env {
    fn new() -> Self {
        Self(BTreeMap::new())
    }

    fn extend(&self, inner: Env) -> Self {
        let mut e = self.0.clone();
        e.extend(inner.0);
        Self(e)
    }

    fn bind(&self, n: Number, exp: Exp, env: Env) -> Env {
        let mut e = self.0.clone();
        e.insert(n, BoundExp { exp, env });
        Env(e)
    }

    fn lookup(&self, n: Number) -> Option<BoundExp> {
        if let Some(exp) = self.0.get(&n) {
            return Some(exp.clone());
        }
        None
    }
}

impl std::fmt::Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Exp::*;
        match self {
            Bool(bool) => {
                write!(f, "{}", bool)
            }
            Int(n) => {
                write!(f, "{}", n)
            }
            S(s) => {
                write!(f, r#""{}""#, s.to_human_string())
            }
            Negate(e) => {
                write!(f, "-{}", e)
            }
            Not(e) => {
                write!(f, "!{}", e)
            }
            StringToInt(e) => {
                write!(f, "(string-to-int {})", e)
            }
            IntToString(e) => {
                write!(f, "(int-to-string {})", e)
            }
            Add(e1, e2) => {
                write!(f, "(+ {} {})", e1, e2)
            }
            Sub(e1, e2) => {
                write!(f, "(- {} {})", e1, e2)
            }
            Mul(e1, e2) => {
                write!(f, "(* {} {})", e1, e2)
            }
            Div(e1, e2) => {
                write!(f, "(/ {} {})", e1, e2)
            }
            Mod(e1, e2) => {
                write!(f, "(% {} {})", e1, e2)
            }
            Lt(e1, e2) => {
                write!(f, "(< {} {})", e1, e2)
            }
            Gt(e1, e2) => {
                write!(f, "(> {} {})", e1, e2)
            }
            Eq(e1, e2) => {
                write!(f, "(== {} {})", e1, e2)
            }
            Or(e1, e2) => {
                write!(f, "(or {} {})", e1, e2)
            }
            And(e1, e2) => {
                write!(f, "(and {} {})", e1, e2)
            }
            Concat(e1, e2) => {
                write!(f, "(concat {} {})", e1, e2)
            }
            Take(e1, e2) => {
                write!(f, "(take {} {})", e1, e2)
            }
            Drop(e1, e2) => {
                write!(f, "(drop {} {})", e1, e2)
            }
            Apply(e1, e2) => {
                write!(f, "(apply {} {})", e1, e2)
            }
            ApplyStrict(e1, e2) => {
                write!(f, "(apply! {} {})", e1, e2)
            }
            If(e1, e2, e3) => {
                write!(f, "(if {} {} {})", e1, e2, e3)
            }
            Lambda(_env, n, e) => {
                write!(f, "(lambda (v{}) {})", n, e)
            }
            Var(n) => {
                write!(f, "v{}", n)
            }
        }
    }
}

const ASCII_START: u8 = 33;

fn parse_int(body: &[u8]) -> Number {
    body.iter().fold(0, |acc, b| acc * 94 + (b - 33) as Number)
}

pub fn parse_and_eval(src: &[u8]) -> String {
    log::debug!("parse_and_eval: {}", String::from_utf8_lossy(src));
    let (exp, src) = parse(src);
    assert!(src.is_empty());
    exp.eval(Env::new()).to_string()
}

pub fn parse_and_eval_file(path: impl AsRef<Path>) -> Result<String> {
    let s = std::fs::read_to_string(path.as_ref())?;
    Ok(parse_and_eval(s.trim().as_bytes()))
}

fn parse(src: &[u8]) -> (Exp, &[u8]) {
    assert!(!src.is_empty());
    let pos = src.iter().position(|b| *b == b' ');
    let (head, tail) = if let Some(pos) = pos {
        (&src[0..pos], &src[pos + 1..])
    } else {
        (src, &src[src.len()..])
    };

    let indicator = head[0];
    let body = &head[1..];
    match indicator {
        b'T' => (Exp::Bool(true), tail),
        b'F' => (Exp::Bool(false), tail),
        b'I' => (Exp::Int(parse_int(body)), tail),
        b'S' => (Exp::S(LangS(body.to_vec())), tail),
        // Unarray
        b'U' => {
            let (e1, tail) = parse(tail);
            let e1 = Box::new(e1);
            match body[0] {
                b'-' => (Exp::Negate(e1), tail),
                b'!' => (Exp::Not(e1), tail),
                b'#' => (Exp::StringToInt(e1), tail),
                b'$' => (Exp::IntToString(e1), tail),
                _ => unreachable!(),
            }
        }
        // Binary
        b'B' => {
            let (e1, tail) = parse(tail);
            let e1 = Box::new(e1);
            let (e2, tail) = parse(tail);
            let e2 = Box::new(e2);
            match body[0] {
                b'+' => (Exp::Add(e1, e2), tail),
                b'-' => (Exp::Sub(e1, e2), tail),
                b'*' => (Exp::Mul(e1, e2), tail),
                b'/' => (Exp::Div(e1, e2), tail),
                b'%' => (Exp::Mod(e1, e2), tail),
                b'<' => (Exp::Lt(e1, e2), tail),
                b'>' => (Exp::Gt(e1, e2), tail),
                b'=' => (Exp::Eq(e1, e2), tail),
                b'|' => (Exp::Or(e1, e2), tail),
                b'&' => (Exp::And(e1, e2), tail),
                b'.' => (Exp::Concat(e1, e2), tail),
                b'T' => (Exp::Take(e1, e2), tail),
                b'D' => (Exp::Drop(e1, e2), tail),
                b'$' => (Exp::Apply(e1, e2), tail),
                // Efficiency Extension
                b'!' => (Exp::ApplyStrict(e1, e2), tail),
                _ => unreachable!(),
            }
        }
        // If
        b'?' => {
            let (e1, tail) = parse(tail);
            let (e2, tail) = parse(tail);
            let (e3, tail) = parse(tail);
            (Exp::If(Box::new(e1), Box::new(e2), Box::new(e3)), tail)
        }
        // Lambda
        b'L' => {
            let num = parse_int(body);
            let (e1, tail) = parse(tail);
            (Exp::Lambda(Env::new(), num, Box::new(e1)), tail)
        }
        // Variable
        b'v' => (Exp::Var(parse_int(body)), tail),
        _ => unreachable!(),
    }
}

impl Exp {
    fn eval(&self, env: Env) -> Exp {
        log::debug!("eval: {}", self);
        use Exp::*;
        match self {
            Bool(bool) => Bool(*bool),
            Int(n) => Int(*n),
            S(s) => S(s.clone()),
            Negate(e) => {
                if let Int(n) = e.eval(env) {
                    Int(-n)
                } else {
                    unreachable!()
                }
            }
            Not(ref e) => {
                if let Bool(n) = e.eval(env) {
                    Bool(!n)
                } else {
                    unreachable!()
                }
            }
            StringToInt(e) => {
                if let S(s) = e.eval(env) {
                    Int(s.to_int())
                } else {
                    unreachable!()
                }
            }
            IntToString(e) => {
                if let Int(mut n) = e.eval(env) {
                    // [2024-07-01 Mon] echo service timeout if IntToString is called for a negative number.
                    assert!(n >= 0);
                    let mut res = Vec::new();
                    loop {
                        let m = n % 94;
                        n /= 94;
                        res.push(ASCII_START + m as u8);
                        if n == 0 {
                            break;
                        }
                    }
                    res.reverse();
                    Exp::S(LangS(res))
                } else {
                    unreachable!()
                }
            }
            Add(e1, e2) => match (e1.eval(env.clone()), e2.eval(env)) {
                (Int(n1), Int(n2)) => Int(n1 + n2),
                _ => unreachable!(),
            },
            Sub(e1, e2) => match (e1.eval(env.clone()), e2.eval(env)) {
                (Int(n1), Int(n2)) => Int(n1 - n2),
                _ => unreachable!(),
            },
            Mul(e1, e2) => match (e1.eval(env.clone()), e2.eval(env)) {
                (Int(n1), Int(n2)) => Int(n1 * n2),
                _ => unreachable!(),
            },
            Div(e1, e2) => match (e1.eval(env.clone()), e2.eval(env)) {
                (Int(n1), Int(n2)) => Int(n1 / n2),
                _ => unreachable!(),
            },
            Mod(e1, e2) => match (e1.eval(env.clone()), e2.eval(env)) {
                (Int(n1), Int(n2)) => Int(n1 % n2),
                _ => unreachable!(),
            },
            Lt(e1, e2) => match (e1.eval(env.clone()), e2.eval(env)) {
                (Int(n1), Int(n2)) => Bool(n1 < n2),
                _ => unreachable!(),
            },
            Gt(e1, e2) => match (e1.eval(env.clone()), e2.eval(env)) {
                (Int(n1), Int(n2)) => Bool(n1 > n2),
                _ => unreachable!(),
            },
            Eq(e1, e2) => match (e1.eval(env.clone()), e2.eval(env)) {
                (Int(n1), Int(n2)) => Bool(n1 == n2),
                (Bool(b1), Bool(b2)) => Bool(b1 == b2),
                (S(s1), S(s2)) => Bool(s1 == s2),
                _ => unreachable!(),
            },
            Or(e1, e2) => match (e1.eval(env.clone()), e2.eval(env)) {
                (Bool(b1), Bool(b2)) => Bool(b1 || b2),
                _ => unreachable!(),
            },
            And(e1, e2) => match (e1.eval(env.clone()), e2.eval(env)) {
                (Bool(b1), Bool(b2)) => Bool(b1 && b2),
                _ => unreachable!(),
            },
            Concat(e1, e2) => match (e1.eval(env.clone()), e2.eval(env)) {
                (S(s1), S(mut s2)) => {
                    let mut s = s1.0;
                    s.append(&mut s2.0);
                    S(LangS::new(s))
                }
                _ => unreachable!(),
            },
            Take(e1, e2) => match (e1.eval(env.clone()), e2.eval(env)) {
                (Int(n), S(s)) => {
                    assert!(n >= 0);
                    assert!(n as usize <= s.0.len());
                    S(LangS::new(s.0[0..n as usize].to_vec()))
                }
                _ => unreachable!(),
            },
            Drop(e1, e2) => match (e1.eval(env.clone()), e2.eval(env)) {
                (Int(mut n), S(s)) => {
                    if n < 0 {
                        log::warn!("Drop is called for negative n: {}", n);
                        n = -n;
                    }
                    S(LangS::new(s.0[n as usize..].to_vec()))
                }
                _ => unreachable!(),
            },
            Apply(e1, arg) => match (
                e1.eval(env.clone()),
                // don't eval argment at this point
                arg,
            ) {
                (Lambda(l_env, n, body), arg) => {
                    let env = l_env.bind(n, arg.as_ref().clone(), env.clone());
                    body.eval(env)
                }
                _ => {
                    unreachable!()
                }
            },
            ApplyStrict(e1, arg) => match (e1.eval(env.clone()), arg.eval(env.clone())) {
                (Lambda(l_env, n, body), arg) => {
                    let env = l_env.bind(n, arg, env.clone());
                    body.eval(env)
                }
                _ => {
                    unreachable!()
                }
            },
            If(e1, e2, e3) => match e1.eval(env.clone()) {
                Bool(true) => e2.eval(env),
                Bool(false) => e3.eval(env),
                _ => unreachable!(),
            },
            Lambda(l_env, n, e) => {
                env.extend(l_env.clone());
                Lambda(env, *n, e.clone())
            }
            Var(n) => env.lookup(*n).unwrap().eval(),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn parse_and_to_string(src: &[u8]) -> String {
        let (exp, tail) = parse(src);
        assert!(tail.is_empty());
        exp.to_string()
    }

    fn parse_and_eval(src: &[u8]) -> Exp {
        let (exp, tail) = parse(src);
        assert!(tail.is_empty());
        exp.eval(Env::new())
    }

    #[test]
    fn parse_test() {
        assert_eq!(parse_and_to_string(b"SB%,,/"), r#""Hello""#);
        assert_eq!(parse_and_to_string(b"S'%4}).$%8"), r#""get index""#);
        assert_eq!(parse_and_to_string(b"I#"), "2");
        assert_eq!(parse_and_to_string(b"I$"), "3");
    }

    #[test]
    fn parse_test_2() {
        assert_eq!(
            parse_and_to_string(b"B. SB%,,/ SB%,,/"),
            r#"(concat "Hello" "Hello")"#
        );
    }

    #[test]
    fn parse_int_test() {
        assert_eq!(parse_int(b"K"), 42);
        assert_eq!(parse_int(b"KK"), 94 * 42 + 42);
    }

    #[test]
    fn eval_test() {
        assert_eq!(parse_and_eval(b"I$"), Exp::Int(3));
        assert_eq!(parse_and_eval(b"I/6"), Exp::Int(1337));
        assert_eq!(parse_and_eval(b"U- I$"), Exp::Int(-3));
        assert_eq!(parse_and_eval(b"U! T"), Exp::Bool(false));

        // U# S4%34 -> 15818151
        assert_eq!(parse_and_eval(b"U# S4%34"), Exp::Int(15818151));

        // U$ I4%34 -> test
        assert_eq!(parse_and_eval(b"I4%34"), Exp::Int(15818151));

        assert_eq!(
            parse_and_eval(b"U$ I4%34"),
            Exp::S(LangS::from_human_str("test"))
        );

        assert_eq!(parse_and_eval(b"B+ I# I$"), Exp::Int(5));
        assert_eq!(parse_and_eval(b"B- I$ I#"), Exp::Int(1));
        assert_eq!(parse_and_eval(b"B* I$ I#"), Exp::Int(6));
        assert_eq!(parse_and_to_string(b"B/ U- I( I#"), "(/ -7 2)");
        assert_eq!(parse_and_eval(b"B/ U- I( I#"), Exp::Int(-3));
        assert_eq!(parse_and_eval(b"B% U- I( I#"), Exp::Int(-1));
        assert_eq!(parse_and_eval(b"B< I$ I#"), Exp::Bool(false));
        assert_eq!(parse_and_eval(b"B> I$ I#"), Exp::Bool(true));
        //
        assert_eq!(parse_and_eval(b"B= I$ I#"), Exp::Bool(false));
        assert_eq!(parse_and_eval(b"B= T T"), Exp::Bool(true));
        assert_eq!(parse_and_eval(b"B= SB%,,/ SB%,,/"), Exp::Bool(true));
        assert_eq!(parse_and_eval(b"B= SB%,,/ SB%,,,"), Exp::Bool(false));

        assert_eq!(parse_and_to_string(b"B| T F"), "(or true false)");
        assert_eq!(parse_and_eval(b"B| T F"), Exp::Bool(true));
        assert_eq!(parse_and_eval(b"B& T F"), Exp::Bool(false));

        assert_eq!(
            parse_and_eval(b"B. S4% S34"),
            Exp::S(LangS::from_human_str("test"))
        );

        assert_eq!(
            parse_and_eval(b"BT I$ S4%34"),
            Exp::S(LangS::from_human_str("tes"))
        );

        assert_eq!(
            parse_and_eval(b"BD I$ S4%34"),
            Exp::S(LangS::from_human_str("t"))
        );
        assert_eq!(
            parse_and_eval(b"BD I# S4%34"),
            Exp::S(LangS::from_human_str("st"))
        );

        assert_eq!(
            parse_and_eval(b"? B> I# I$ S9%3 S./"),
            Exp::S(LangS::from_human_str("no"))
        );

        assert_eq!(
            parse_and_eval(b"B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK"),
            Exp::S(LangS::from_human_str("Hello World!"))
        );

        assert_eq!(
            parse_and_to_string(br#"B$ L# B$ L" B+ v" v" B* I$ I# v8"#),
            "(apply (lambda (v2) (apply (lambda (v1) (+ v1 v1)) (* 3 2))) v23)"
        );

        // Evaluation: non-strict:
        // B$ L# B$ L" B+ v" v" B* I$ I# v8  -> I-
        assert_eq!(
            parse_and_eval(br#"B$ L# B$ L" B+ v" v" B* I$ I# v8"#),
            Exp::Int(12)
        );
        assert_eq!(parse_and_eval(b"I-"), Exp::Int(12));

        // Limits
        // B$ B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L" L# ? B= v# I! I" B$ L$ B+ B$ v" v$ B$ v" v$ B- v# I" I%

        assert_eq!(
            parse_and_to_string(br#"B$ B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L" L# ? B= v# I! I" B$ L$ B+ B$ v" v$ B$ v" v$ B- v# I" I%"#),
            "(apply (apply (lambda (v1) (apply (lambda (v2) (apply v1 (apply v2 v2))) (lambda (v2) (apply v1 (apply v2 v2))))) (lambda (v1) (lambda (v2) (if (== v2 0) 1 (apply (lambda (v3) (+ (apply v1 v3) (apply v1 v3))) (- v2 1)))))) 4)"
        );

        assert_eq!(
            parse_and_eval(br#"B$ B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L" L# ? B= v# I! I" B$ L$ B+ B$ v" v$ B$ v" v$ B- v# I" I%"#),
            Exp::Int(16)
        );
    }

    #[test]
    fn lambdaman_problem_6() {
        let src = br#"B. SF B$ B$ L" B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L# ? B= v# I" v" B. v" B$ v$ B- v# I" Sl I#,"#;
        assert_eq!(
            parse_and_to_string(src),
            "(concat \"L\" (apply (apply (lambda (v1) (apply (lambda (v1) (apply (lambda (v2) (apply v1 (apply v2 v2))) (lambda (v2) (apply v1 (apply v2 v2))))) (lambda (v3) (lambda (v2) (if (== v2 1) v1 (concat v1 (apply v3 (- v2 1)))))))) \".\") 199))"
        );
        // stack overflow
        // assert_eq!(parse_and_eval(src), Exp::S(LangS::from_human_str("L.........")));

        // Problem is:
        // "L...." (dot is 199 length)

        let src = br#"B. SF B$ B$ L" B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L# ? B= v# I" v" B. v" B$ v$ B- v# I" Sl I0"#;
        assert_eq!(
            parse_and_to_string(src),
            "(concat \"L\" (apply (apply (lambda (v1) (apply (lambda (v1) (apply (lambda (v2) (apply v1 (apply v2 v2))) (lambda (v2) (apply v1 (apply v2 v2))))) (lambda (v3) (lambda (v2) (if (== v2 1) v1 (concat v1 (apply v3 (- v2 1)))))))) \".\") 15))"
        );

        assert_eq!(
            parse_and_eval(src),
            Exp::S(LangS::from_human_str("L..............."))
        );
    }

    #[test]
    fn lambdaman_problem_9() {
        let src = br#"B$ L+ B. B. SF B$ B$ v+ Sl IR B$ B$ v+ B. S~ B$ B$ v+ Sl IS IR L" B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L# ? B= v# I" v" B. v" B$ v$ B- v# I""#;
        assert_eq!(
            parse_and_to_string(src),
            "(apply (lambda (v10) (concat (concat \"L\" (apply (apply v10 \".\") 49)) (apply (apply v10 (concat \"\n\" (apply (apply v10 \".\") 50))) 49))) (lambda (v1) (apply (lambda (v1) (apply (lambda (v2) (apply v1 (apply v2 v2))) (lambda (v2) (apply v1 (apply v2 v2))))) (lambda (v3) (lambda (v2) (if (== v2 1) v1 (concat v1 (apply v3 (- v2 1)))))))))"
        );
    }

    #[test]
    fn apply_test() {
        let src = br#"B$ L" B$ L# B$ v" v# I" L" B$ L# v" I#"#;
        assert_eq!(
            parse_and_to_string(src),
            "(apply (lambda (v1) (apply (lambda (v2) (apply v1 v2)) 1)) (lambda (v1) (apply (lambda (v2) v1) 2)))"
        );
        assert_eq!(parse_and_eval(src), Exp::Int(1));
    }

    #[test]
    fn encode_test() -> Result<()> {
        assert_eq!(LangS::from_human_str("Hello").as_raw_str(), "B%,,/");
        // entry point
        assert_eq!(LangS::from_human_str("get index").as_raw_str(), "'%4}).$%8");

        assert_eq!(
            LangS::from_human_str("get scoreboard").as_raw_str(),
            "'%4}3#/2%\"/!2$"
        );
        assert_eq!(
            LangS::from_human_str("get scoreboard").as_raw_str(),
            r##"'%4}3#/2%"/!2$"##
        );

        // S'%4}3#/2%"/!2$

        Ok(())
    }

    #[test]
    fn efficiency_1() {
        let src = br#"B$ L! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! I" L! B+ B+ v! v! B+ v! v!"#;
        assert_eq!(
            parse_and_to_string(src),
            "(apply (lambda (v0) (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 (apply v0 1))))))))))))))))))))))) (lambda (v0) (+ (+ v0 v0) (+ v0 v0))))"
        );

        // This doesn't end.
        // assert_eq!(parse_and_eval(src), Exp::Int((4 as Number).pow(42)));

        // answer = 4^22 => 17,592,186,044,416

        // 17592186044416

        let src = br#"B! L! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! I" L! B+ B+ v! v! B+ v! v!"#;
        assert_eq!(
            parse_and_to_string(src),
            "(apply! (lambda (v0) (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 (apply! v0 1))))))))))))))))))))))) (lambda (v0) (+ (+ v0 v0) (+ v0 v0))))"
        );

        assert_eq!(parse_and_eval(src), Exp::Int(17592186044416));
    }

    #[test]
    fn efficiency_2() {
        let src = br#"B+ I7c B* B$ B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L% ? B= v% I! I" B+ I" B$ v$ B- v% I" I":c1+0 I!"#;
        assert_eq!(
            parse_and_to_string(src),
            "(+ 2134 (* (apply (apply (lambda (v1) (apply (lambda (v2) (apply v1 (apply v2 v2))) (lambda (v2) (apply v1 (apply v2 v2))))) (lambda (v3) (lambda (v4) (if (== v4 0) 1 (+ 1 (apply v3 (- v4 1))))))) 9345873499) 0))"
        );

        // assert_eq!(parse_and_eval(src), Exp::Int(0));

        // let src = br#"B+ I7c B* B! B! L" B! L# B! v" B! v# v# L# B! v" B! v# v# L$ L% ? B= v% I! I" B+ I" B! v$ B- v% I" I":c1+0 I!"#;
        // assert_eq!(parse_and_eval(src), Exp::Int(0));
    }

    #[test]
    fn efficiency_3() {
        let src = br#"B+ I7c B* B$ B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L% ? B= v% I! I" B+ I" B$ v$ B- v% I" I":c1+0 I""#;
        assert_eq!(
            parse_and_to_string(src),
            "(+ 2134 (* (apply (apply (lambda (v1) (apply (lambda (v2) (apply v1 (apply v2 v2))) (lambda (v2) (apply v1 (apply v2 v2))))) (lambda (v3) (lambda (v4) (if (== v4 0) 1 (+ 1 (apply v3 (- v4 1))))))) 9345873499) 1))"
        );

        // assert_eq!(parse_and_eval(src), Exp::Int(0));

        // let src = br#"B+ I7c B* B! B! L" B! L# B! v" B! v# v# L# B! v" B! v# v# L$ L% ? B= v% I! I" B+ I" B! v$ B- v% I" I":c1+0 I!"#;
        // assert_eq!(parse_and_eval(src), Exp::Int(0));
    }

    #[test]
    fn efficiency_4() {
        let src = br#"B$ B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L% ? B< v% I# I" B+ B$ v$ B- v% I" B$ v$ B- v% I# II"#;
        assert_eq!(
            parse_and_to_string(src),
            "(apply (apply (lambda (v1) (apply (lambda (v2) (apply v1 (apply v2 v2))) (lambda (v2) (apply v1 (apply v2 v2))))) (lambda (v3) (lambda (v4) (if (< v4 2) 1 (+ (apply v3 (- v4 1)) (apply v3 (- v4 2))))))) 40)"
        );

        // let src = br#"B$ B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L% ? B< v% I# I" B+ B$ v$ B- v% I" B$ v$ B- v% I# I%"#;
        // assert_eq!(parse_and_eval(src), Exp::Int(0));
    }

    #[test]
    fn cache_test() {
        let src = br#"B+ B$ L# B+ v# v# I# B$ L# B+ v# v# I# "#;
        assert_eq!(
            parse_and_to_string(src),
            "(+ (apply (lambda (v2) (+ v2 v2)) 2) (apply (lambda (v2) (+ v2 v2)) 2))"
        );
        assert_eq!(parse_and_eval(src), Exp::Int(8));
    }

    #[ignore]
    #[test]
    fn efficiency_5() {
        let src = br#"B$ L' B$ L( B$ B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L% ? B& B> v% I"41= B& B$ v' v% B$ v( B+ v% I" v% B$ v$ B+ v% I" I# B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L% ? B= v% I" T ? B= B% v% I# I" F B$ v$ B/ v% I# L& B$ B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L% ? B= v% v& T ? B= B% v& v% I! F B$ v$ B+ v% I" I#"#;
        assert_eq!(
            parse_and_to_string(src),
            "(apply (lambda (v6) (apply (lambda (v7) (apply (apply (lambda (v1) (apply (lambda (v2) (apply v1 (apply v2 v2))) (lambda (v2) (apply v1 (apply v2 v2))))) (lambda (v3) (lambda (v4) (if (and (> v4 1000000) (and (apply v6 v4) (apply v7 (+ v4 1)))) v4 (apply v3 (+ v4 1)))))) 2)) (apply (lambda (v1) (apply (lambda (v2) (apply v1 (apply v2 v2))) (lambda (v2) (apply v1 (apply v2 v2))))) (lambda (v3) (lambda (v4) (if (== v4 1) true (if (== (% v4 2) 1) false (apply v3 (/ v4 2))))))))) (lambda (v5) (apply (apply (lambda (v1) (apply (lambda (v2) (apply v1 (apply v2 v2))) (lambda (v2) (apply v1 (apply v2 v2))))) (lambda (v3) (lambda (v4) (if (== v4 v5) true (if (== (% v5 v4) 0) false (apply v3 (+ v4 1))))))) 2)))"
        );

        assert_eq!(parse_and_eval(src), Exp::Int(0));

        // let src = br#"B+ I7c B* B! B! L" B! L# B! v" B! v# v# L# B! v" B! v# v# L$ L% ? B= v% I! I" B+ I" B! v$ B- v% I" I":c1+0 I!"#;
        // assert_eq!(parse_and_eval(src), Exp::Int(0));
    }

    #[test]
    fn efficiency_6() {
        let src = br#"B$ L' B$ L( B$ B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L% ? B& B> v% I? B$ v' B$ v( v% v% B$ v$ B+ v% I" I# B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L% ? B< v% I# I" B+ B$ v$ B- v% I" B$ v$ B- v% I# L& B$ B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L% ? B= v% v& T ? B= B% v& v% I! F B$ v$ B+ v% I" I#"#;
        assert_eq!(
            parse_and_to_string(src),
            "(apply (lambda (v6) (apply (lambda (v7) (apply (apply (lambda (v1) (apply (lambda (v2) (apply v1 (apply v2 v2))) (lambda (v2) (apply v1 (apply v2 v2))))) (lambda (v3) (lambda (v4) (if (and (> v4 30) (apply v6 (apply v7 v4))) v4 (apply v3 (+ v4 1)))))) 2)) (apply (lambda (v1) (apply (lambda (v2) (apply v1 (apply v2 v2))) (lambda (v2) (apply v1 (apply v2 v2))))) (lambda (v3) (lambda (v4) (if (< v4 2) 1 (+ (apply v3 (- v4 1)) (apply v3 (- v4 2))))))))) (lambda (v5) (apply (apply (lambda (v1) (apply (lambda (v2) (apply v1 (apply v2 v2))) (lambda (v2) (apply v1 (apply v2 v2))))) (lambda (v3) (lambda (v4) (if (== v4 v5) true (if (== (% v5 v4) 0) false (apply v3 (+ v4 1))))))) 2)))"
        );
    }
}
