use crate::prelude::*;

#[derive(Debug)]
struct Problem {
    points: Vec<(i64, i64)>,
}

pub fn solve(id: usize) -> Result<String> {
    let problem = Problem::from_id(id)?;
    Ok(problem.solve())
}

pub fn solve2(id: usize) -> Result<String> {
    let problem = Problem::from_id(id)?;
    Ok(problem.solve2())
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Accel {
    Dec,
    Neu,
    Inc,
}

impl Accel {
    fn negate(self) -> Self {
        match self {
            Accel::Dec => Accel::Inc,
            Accel::Neu => Accel::Neu,
            Accel::Inc => Accel::Dec,
        }
    }
}

fn solve_path(mut dx: i64, mut dist: i64) -> Vec<Accel> {
    const MAX_SPEED: usize = 1_000_000;
    lazy_static! {
        static ref sum_cache: Vec<u64> = {
            let mut res = vec![0u64; MAX_SPEED];
            for i in 1..MAX_SPEED {
                res[i] = res[i - 1] + i as u64;
            }
            res
        };
    }

    let mut res = Vec::new();
    while dist != 0 || dx != 0 {
        if dist < 0 {
            let mut res2 = solve_path(-dx, -dist)
                .into_iter()
                .map(|a| a.negate())
                .collect();
            res.append(&mut res2);
            return res;
        }
        let a = if dx < 0 || sum_cache[(dx + 1) as usize] <= dist as u64 {
            Accel::Inc
        } else if sum_cache[dx as usize] + (dx as u64) > dist as u64 {
            Accel::Dec
        } else {
            Accel::Neu
        };
        dx += match a {
            Accel::Inc => 1,
            Accel::Neu => 0,
            Accel::Dec => -1,
        };
        res.push(a);

        dist -= dx;
    }
    res
}

impl Problem {
    fn from_id(id: usize) -> Result<Self> {
        let s = read_from(format!("problem/spaceship/{}.txt", id))?;
        Ok(Self::new(s.trim()))
    }

    fn new(p: &str) -> Self {
        let points = p
            .split('\n')
            .map(|line| {
                let mut xy = line.split(' ');
                (
                    xy.next().unwrap().parse::<i64>().unwrap(),
                    xy.next().unwrap().parse::<i64>().unwrap(),
                )
            })
            .collect();
        Self { points }
    }

    fn solve(&self) -> String {
        let mut remain: HashSet<(i64, i64)> = self.points.iter().copied().collect();

        let mut res = String::new();
        let mut x = 0;
        let mut y = 0;
        let mut dx = 0;
        let mut dy = 0;

        let acc = [
            ("1", (-1, -1)),
            ("2", (0, -1)),
            ("3", (1, -1)),
            ("4", (-1, 0)),
            ("5", (0, 0)),
            ("6", (1, 0)),
            ("7", (-1, 1)),
            ("8", (0, 1)),
            ("9", (1, 1)),
        ];

        while !remain.is_empty() {
            let (k, (ax, ay)) = acc
                .iter()
                .min_by_key(|(_, (ax, ay))| {
                    let x = x + dx + ax;
                    let y = y + dy + ay;
                    // Find the shortest point.
                    let p = remain
                        .iter()
                        .min_by_key(|p| (x - p.0).abs() + (y - p.1).abs())
                        .unwrap();
                    (x - p.0).abs() + (y - p.1).abs()
                })
                .unwrap();

            dx += ax;
            dy += ay;
            x += dx;
            y += dy;
            res.push_str(k);
            remain.remove(&(x, y));
        }
        res
    }

    fn solve2(&self) -> String {
        let mut remain: HashSet<(i64, i64)> = self.points.iter().copied().collect();

        let mut res = String::new();
        let mut x = 0;
        let mut y = 0;
        let mut dx = 0;
        let mut dy = 0;

        while !remain.is_empty() {
            // Find the shortest point.
            let p = remain
                .iter()
                .min_by_key(|p| (x - p.0).abs().max((y - p.1).abs()))
                .unwrap();

            let mut xs = solve_path(dx, p.0 - x);
            let mut ys = solve_path(dy, p.1 - y);
            dx = 0;
            dy = 0;
            x = p.0;
            y = p.1;
            remain.remove(&(x, y));

            if xs.len() < ys.len() {
                xs.extend(vec![Accel::Neu; ys.len() - xs.len()]);
            } else {
                ys.extend(vec![Accel::Neu; xs.len() - ys.len()]);
            }

            res.extend(xs.iter().zip(ys.iter()).map(|(x, y)| {
                use Accel::*;
                match (x, y) {
                    (Dec, Dec) => "1",
                    (Neu, Dec) => "2",
                    (Inc, Dec) => "3",
                    (Dec, Neu) => "4",
                    (Neu, Neu) => "5",
                    (Inc, Neu) => "6",
                    (Dec, Inc) => "7",
                    (Neu, Inc) => "8",
                    (Inc, Inc) => "9",
                }
            }));
        }
        res
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn solve_test1() -> Result<()> {
        let problem = Problem::from_id(1)?;
        let solution = problem.solve();
        assert_eq!(solution, "31619");
        Ok(())
    }

    #[test]
    fn solve_path_test() {
        assert!(solve_path(4, 20).len() < 100);
        assert!(solve_path(4, 15).len() < 100);
        assert!(solve_path(4, 100).len() < 100);
        assert!(solve_path(80, 100).len() < 300);
    }

    #[test]
    fn solve2_test() -> Result<()> {
        let problem = Problem::from_id(1)?;
        let solution = problem.solve2();
        assert_eq!(solution, "3728283728258283728");

        let problem = Problem::from_id(2)?;
        let solution = problem.solve();
        assert_eq!(solution, "8499532374293251556543637625491511281142535957777777777779999999999999999331131111111111333333211132222222222577779999997779888888888888888888212322222222222222222222222597888888888888888888888");

        // let solution = problem.solve2();
        // assert_eq!(solution, "8273828294282942829191916491643764736464916491646546438464646437373737373737381943819437373573736718337764367183671943377643377643377367194337733773279183194828319188319482831828822585828135858281328881328882228882822288828312888312888");

        let problem = Problem::from_id(3)?;
        let solution = problem.solve();
        assert_eq!(solution, "4986852165");

        // let solution = problem.solve2();
        // assert_eq!(solution, "46828282918294282942829182829191");

        Ok(())
    }
}
