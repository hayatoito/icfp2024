use crate::prelude::*;

type Num = i64;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum E {
    // // .
    // Empty,
    // nnn
    Int(Num),
    // <
    MoveLeft,
    // >
    MoveRight,
    // ^
    MoveUp,
    // v
    MoveDown,
    // +
    Add,
    // -
    Sub,
    // *
    Mul,
    // /
    Div,
    // %
    Mod,
    // =
    Eq,
    // #
    Ne,
    // Submit
    S,
    // @
    Time,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, Eq, PartialEq, Hash, derive_more::Display)]
#[display(fmt = "({}, {})", x, y)]
pub struct P {
    pub x: Num,
    pub y: Num,
}

impl P {
    pub fn new(x: Num, y: Num) -> Self {
        P { x, y }
    }

    fn left(&self) -> P {
        P::new(self.x.wrapping_sub(1), self.y)
    }

    fn right(&self) -> P {
        P::new(self.x.wrapping_add(1), self.y)
    }

    fn down(&self) -> P {
        P::new(self.x, self.y.wrapping_add(1))
    }

    fn up(&self) -> P {
        P::new(self.x, self.y.wrapping_sub(1))
    }
}

type Board = HashMap<P, E>;

#[derive(Default)]
struct Evaluator {
    board: Board,
    history: Vec<Board>,
    tick: usize,
    // TODO: should consider nagative x, y.
    xs: usize,
    ys: usize,

    // For simulator
    terminate: Option<E>,
    write_happen: HashSet<P>,
    next_board: Board,
}

impl std::fmt::Display for Evaluator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut max_width = vec![1_usize; self.xs];
        for (p, e) in &self.board {
            // TODO: bound check. p.x can be nagative.
            max_width[p.x as usize] = max_width[p.x as usize].max(e.to_string().len())
        }
        writeln!(f, "t={}", self.tick + 1)?;
        for y in 0..self.ys {
            for (x, width) in max_width.iter().enumerate().take(self.xs) {
                // write!(f, "{} ", width)?;
                if let Some(e) = self.get(P::new(x as Num, y as Num)) {
                    write!(f, "{:>width$} ", e.to_string(), width = width)?;
                } else {
                    write!(f, "{:>width$} ", ".", width = width)?;
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for E {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use E::*;
        if let Int(n) = self {
            write!(f, "{}", n)
        } else {
            write!(
                f,
                "{}",
                match self {
                    MoveLeft => "<",
                    MoveRight => ">",
                    MoveUp => "^",
                    MoveDown => "v",
                    Add => "+",
                    Sub => "-",
                    Mul => "*",
                    Div => "/",
                    Mod => "%",
                    Eq => "=",
                    Ne => "#",
                    S => "S",
                    Time => "@",
                    Int(_) => unreachable!(),
                }
            )
        }
    }
}

impl Evaluator {
    fn new(s: &str, a: Num, b: Num) -> Self {
        use E::*;
        let mut board = Board::new();
        let mut xs = 0;
        let mut ys = 0;
        for (y, row) in s.trim().split('\n').enumerate() {
            ys = ys.max(y);
            for (x, s) in row.split_whitespace().enumerate() {
                xs = xs.max(x);
                if s != "." {
                    board.insert(
                        P::new(x as Num, y as Num),
                        match s {
                            "." => unreachable!(),
                            "<" => MoveLeft,
                            ">" => MoveRight,
                            "^" => MoveUp,
                            "v" => MoveDown,
                            "+" => Add,
                            "-" => Sub,
                            "*" => Mul,
                            "/" => Div,
                            "%" => Mod,
                            "=" => Eq,
                            "#" => Ne,
                            "A" => Int(a),
                            "B" => Int(b),
                            "S" => S,
                            "@" => Time,
                            n => Int(n.parse::<Num>().unwrap()),
                        },
                    );
                }
            }
        }
        Self {
            board,
            xs: xs + 1,
            ys: ys + 1,
            ..Default::default()
        }
    }

    fn move_to(&mut self, from: P, to: P) {
        if let Some(e) = self.get(from) {
            self.next_board.remove(&from);
            self.write_next(to, e);
        }
    }

    fn write_next(&mut self, p: P, e: E) {
        // Disallows write at the same place.
        assert!(!self.write_happen.contains(&p));
        self.write_happen.insert(p);

        if let Some(E::S) = self.next_board.get(&p) {
            // Rule 7) Disallow more than one submit operators are overwritten.
            assert!(self.terminate.is_none());
            log::debug!("Terminate write: {} {:?}", p, e);
            self.terminate = Some(e);
        }
        self.next_board.insert(p, e);
    }

    fn op<F>(&mut self, p: P, f: F)
    where
        F: Fn(Num, Num) -> Num,
    {
        use E::*;
        if let (Some(Int(n1)), Some(Int(n2))) = (self.get(p.left()), self.get(p.up())) {
            let n = Int(f(n1, n2));
            // left -> down
            self.next_board.remove(&p.left());
            self.write_next(p.down(), n);
            // up -> right
            self.next_board.remove(&p.up());
            self.write_next(p.right(), n);
        }
    }

    fn comp<F>(&mut self, p: P, f: F)
    where
        F: Fn(Num, Num) -> bool,
    {
        use E::*;
        if let (Some(Int(n1)), Some(Int(n2))) = (self.get(p.left()), self.get(p.up())) {
            if f(n1, n2) {
                // left -> down
                self.next_board.remove(&p.left());
                self.write_next(p.down(), Int(n1));
                // up -> right
                self.next_board.remove(&p.up());
                self.write_next(p.right(), Int(n2));
            }
        }
    }

    fn get(&self, p: P) -> Option<E> {
        self.board.get(&p).cloned()
    }

    fn run_step(&mut self) -> StepResult {
        self.next_board.clone_from(&self.board);
        self.write_happen.clear();
        use E::*;

        let mut time_travel_board = None;
        let mut time_travel_dt = None;

        for (p, e) in self.board.clone() {
            match e {
                MoveLeft => self.move_to(p.right(), p.left()),
                MoveRight => self.move_to(p.left(), p.right()),
                MoveUp => self.move_to(p.down(), p.up()),
                MoveDown => self.move_to(p.up(), p.down()),
                Add => self.op(p, |n1, n2| n1 + n2),
                Sub => self.op(p, |n1, n2| n1 - n2),
                Mul => self.op(p, |n1, n2| n1 * n2),
                Div => self.op(p, |n1, n2| n1 / n2),
                Mod => self.op(p, |n1, n2| n1 % n2),
                Eq => self.comp(p, |n1, n2| n1 == n2),
                Ne => self.comp(p, |n1, n2| n1 != n2),
                Time => {
                    if let (Some(e), Some(Int(dx)), Some(Int(dy)), Some(Int(dt))) = (
                        self.get(p.up()),
                        self.get(p.left()),
                        self.get(p.right()),
                        self.get(p.down()),
                    ) {
                        assert!(dt >= 1);
                        if let Some(a) = &time_travel_dt {
                            assert_eq!(*a, dt);
                        } else {
                            time_travel_dt = Some(dt);
                        }
                        let tb = time_travel_board.get_or_insert({
                            TimeTravelBoard::new(
                                self.history[self.history.len() - dt as usize].clone(),
                            )
                        });
                        assert_eq!(time_travel_dt.unwrap(), dt);
                        let new_p = P::new(p.x - dx, p.y - dy);
                        tb.write_to(new_p, e);
                    }
                }
                S => {
                    // do nothing
                }
                Int(_) => {
                    // do nothing
                }
            }
        }

        // Check terminate at first.
        if let Some(e) = self.terminate {
            StepResult::Terminate(e)
        } else if let Some(tb) = time_travel_board {
            if let Some(e) = tb.terminate {
                StepResult::Terminate(e)
            } else {
                let dt = time_travel_dt.unwrap() as usize;
                self.tick -= dt;
                self.history.truncate(self.history.len() - dt);
                self.board = tb.board;
                // self.terminate = tb.terminate;
                StepResult::Continue
            }
        } else {
            self.tick += 1;
            self.history.push(self.board.clone());
            std::mem::swap(&mut self.board, &mut self.next_board);
            StepResult::Continue
        }
    }
}

enum StepResult {
    Terminate(E),
    Continue,
}

#[derive(Default)]
struct TimeTravelBoard {
    board: Board,
    write_happen: HashMap<P, E>,
    terminate: Option<E>,
}

impl TimeTravelBoard {
    fn new(board: Board) -> Self {
        Self {
            board,
            ..Default::default()
        }
    }

    fn write_to(&mut self, p: P, e: E) {
        log::debug!("time travel write: {} {:?}", p, e);
        if let Some(a) = self.write_happen.get(&p) {
            // Rule 5) Disallow different value write at the same place.
            assert_ne!(*a, e);
        }

        if let Some(E::S) = self.board.get(&p) {
            // Okay to write the same value to S.
            // assert!(self.terminate.is_none());
            self.terminate = Some(e);
        }

        self.write_happen.insert(p, e);
        self.board.insert(p, e);
    }
}

pub fn run(s: &str, a: Num, b: Num) -> Result<Num> {
    let mut evaluator = Evaluator::new(s, a, b);
    log::info!("{}", evaluator.to_string());

    for i in 0..100 {
        match evaluator.run_step() {
            StepResult::Terminate(E::Int(n)) => {
                log::info!("terminates: S={:?}", n);
                return Ok(n);
            }
            StepResult::Terminate(e) => {
                anyhow::bail!("terminates with non-number: S={:?}", e);
            }
            _ => {
                log::debug!("i={}", i);
                log::debug!("{}", evaluator);
            }
        }
    }
    anyhow::bail!("Didn't terminate");
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn solve_3d_test() -> Result<()> {
        assert_eq!(
            run(
                "
. . . . 0 . . . .
. B > . = . . . .
. v 1 . . > . . .
. . - . . . + S .
. . . . . ^ . . .
. . v . . 0 > . .
. . . . . . A + .
. 1 @ 6 . . < . .
. . 3 . 0 @ 3 . .
. . . . . 3 . . .",
                3,
                4,
            )?,
            12
        );
        Ok(())
    }

    #[test]
    fn solve_3d1_test() -> Result<()> {
        assert_eq!(
            run(
                "
. 0  .  . < .
. =  .  + S ^
^ .  .  . < 1
A >  .  * . .
v 1  .  . v .
. -  .  . . .
. .  v -1 @ 4
. .  .  . 3 .
. 2  @  5 . .
. .  3 . . .",
                5,
                0,
            )?,
            120
        );
        Ok(())
    }

    #[test]
    fn solve_3d2_test() -> Result<()> {
        assert_eq!(
            run(
                "
. >  .  > . > .  > .
^ .  .  0 . . v  . v
A >  .  # . . .  . .
v 1  .  . . 0 =  S v
. +  .  / . = .  . S
v .  .  . . . . -1 .
. >  .  > . + .  * S
. .  .  . . . .  . .
",
                0,
                0,
            )?,
            0
        );
        Ok(())
    }

    #[test]
    fn solve_3d3_test() -> Result<()> {
        assert_eq!(
            run(
                "
. 0  .  . . . .  .
A =  S  . . . .  .
v .  .  0 . . .  .
. >  .  + . . .  .
v 1  .  . . 0 . -1
. +  .  / . = .  +
. .  .  . . . .  S
. .  0  # . . .  .
. .  .  . . . .  .
. .  1  + S . .  .
",
                0,
                0,
            )?,
            0
        );
        Ok(())
    }

    #[test]
    fn solve_3d4_test() -> Result<()> {
        assert_eq!(
            run(
                "
. . B > .  >  . > . >  .
. A v .  .  . . . .  . v
. v . . 0  .  . . . .  .
. . - . =  S  . . . .  v
. v . v .  .  0 . . .  .
. . . . >  .  + . . .  v
. v . v 1  .  . . 0 .  .
. . . . +  .  / . = .  +
. v . . .  .  . . . .  S
. . . . .  0  # . . .  .
. v . . .  .  . . . .  .
. . > . >  .  + S . .  .
",
                -2,
                4,
            )?,
            4
        );
        Ok(())
    }
}
