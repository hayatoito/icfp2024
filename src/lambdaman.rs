use crate::prelude::*;

#[derive(Debug)]
struct Problem {
    maze: Vec<Vec<P>>,
    start_x: usize,
    start_y: usize,
    pill_cnt: usize,
    x_len: usize,
    y_len: usize,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
enum P {
    Empty,
    Pill,
    Wall,
}

pub fn solve(id: usize) -> Result<String> {
    let mut problem = Problem::from_id(id)?;
    Ok(problem.solve())
}

impl Problem {
    fn from_id(id: usize) -> Result<Self> {
        let s = read_from(format!("problem/lambdaman/{}.txt", id))?;
        Ok(Self::new(s.trim()))
    }

    fn new(p: &str) -> Self {
        let rows: Vec<_> = p.split('\n').collect();
        let y_len = rows.len();
        let x_len = rows[0].len();
        let mut maze = vec![vec![P::Empty; x_len]; y_len];
        let mut start_x = 0;
        let mut start_y = 0;
        let mut pill_cnt = 0;
        for (y, row) in rows.iter().enumerate() {
            for (x, b) in row.as_bytes().iter().enumerate() {
                maze[y][x] = match b {
                    b'#' => P::Wall,
                    b'.' => {
                        pill_cnt += 1;
                        P::Pill
                    }
                    b'L' => {
                        (start_x, start_y) = (x, y);
                        P::Empty
                    }
                    _ => {
                        unreachable!()
                    }
                }
            }
        }
        Self {
            maze,
            start_x,
            start_y,
            pill_cnt,
            x_len,
            y_len,
        }
    }

    // [[file:~/src/leetcode/src/bin/0417-pacific-atlantic-water-flow.rs::fn bfs(&self, mut q: VecDeque<(usize, usize)>) -> HashSet<(usize, usize)> {][[0417-pacific-atlantic-water-flow.rs] fn bfs(&self, mut q: VecDeque<(usize, usize)>) -> HashSet<(usize, usize)> {]]
    fn solve(&mut self) -> String {
        // Naive hueristics
        // Move to direction where the longest pill is minimum.
        let mut lx = self.start_x;
        let mut ly = self.start_y;
        let mut solution = String::new();
        let mut virtual_wall = HashSet::new();
        while self.pill_cnt > 0 {
            log::debug!("{:?}, pill cnt: {}", (lx, ly), self.pill_cnt);
            let mut best_direc = None;
            let mut shortest_longest_pill = usize::MAX;
            for (dx, dy) in [(0, 1), (0, !0), (1, 0), (!0, 0)] {
                let (nx, ny) = (lx.wrapping_add(dx), ly.wrapping_add(dy));
                if nx >= self.x_len || ny >= self.y_len || self.maze[ny][nx] == P::Wall {
                    continue;
                }
                let mut visited = HashSet::new();
                visited.insert((lx, ly));

                let mut longest_pill = 0;
                let mut q = VecDeque::new();
                // println!("try {:?}", (nx, ny));
                q.push_back(((nx, ny), 1));

                while let Some(((x, y), dist)) = q.pop_front() {
                    if !visited.insert((x, y)) {
                        continue;
                    }
                    if virtual_wall.contains(&(x, y)) {
                        continue;
                    }
                    // println!("visit {:?}", (x, y));
                    if self.maze[y][x] == P::Pill {
                        longest_pill = dist;
                    }
                    for (dx, dy) in [(0, 1), (0, !0), (1, 0), (!0, 0)] {
                        let (nx, ny) = (x.wrapping_add(dx), y.wrapping_add(dy));
                        if nx >= self.x_len || ny >= self.y_len || self.maze[ny][nx] == P::Wall {
                            continue;
                        }
                        q.push_back(((nx, ny), dist + 1));
                    }
                }
                if longest_pill == 0 {
                    // Pill is not found
                    continue;
                }
                if longest_pill < shortest_longest_pill {
                    shortest_longest_pill = longest_pill;
                    best_direc = Some((dx, dy));
                }
            }
            let (dx, dy) = best_direc.unwrap();
            // Prevet loop.
            virtual_wall.insert((lx, ly));
            (lx, ly) = (lx.wrapping_add(dx), ly.wrapping_add(dy));
            if self.maze[ly][lx] == P::Pill {
                self.pill_cnt -= 1;
                self.maze[ly][lx] = P::Empty;
                virtual_wall.clear();
            }
            solution.push_str(match (dx, dy) {
                (0, 1) => "D",
                (0, _) => "U",
                (1, 0) => "R",
                (_, 0) => "L",
                _ => {
                    unreachable!()
                }
            });
        }
        solution
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn solve_test() {
        let mut problem = Problem::new(
            "###.#...
...L..##
.#######",
        );

        // println!("problem: {:?}", problem);
        let solution = problem.solve();
        assert_eq!(solution, "UDLLLDURRRRRURR");
    }
}
