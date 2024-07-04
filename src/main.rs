use clap::Parser;

use icfp2024::{lambdaman, lang, prelude::*, spaceship};
// use icfp2024::problem::*;
// use icfp2024::solution;
// use icfp2024::solver;
// use icfp2024::solver_sa;
// use icfp2024::render;

#[derive(Parser, Debug)]
#[clap(name = "icfp2024")]
enum Cli {
    Send { file: PathBuf },
    Decode { file: PathBuf },
    Encode { file: PathBuf },
    Eval { file: PathBuf },
    LambdamanSolve { id: usize },
    SpaceshipSolve { id: usize },
    SpaceshipSolve2 { id: usize },
}

fn main() -> Result<()> {
    env_logger::init();
    match Cli::parse() {
        Cli::Send { file } => {
            println!("{}", lang::send_file(file)?);
        }
        Cli::Decode { file } => {
            let s = std::fs::read_to_string(file)?;
            let bytes = s.trim().as_bytes();
            assert_eq!(bytes[0], b'S');
            println!(
                "{}",
                lang::LangS::new(bytes[1..].to_vec()).to_human_string()
            );
        }
        Cli::Encode { file } => {
            let s = std::fs::read_to_string(file)?;
            let s = s.trim_end_matches("\n");
            println!("S{}", lang::LangS::from_human_str(s).as_raw_str());
        }
        Cli::Eval { file } => {
            println!("{}", lang::parse_and_eval_file(file)?.trim_matches('"'));
        }
        Cli::LambdamanSolve { id } => {
            println!("{}", lambdaman::solve(id)?);
        }
        Cli::SpaceshipSolve { id } => {
            println!("{}", spaceship::solve(id)?);
        }
        Cli::SpaceshipSolve2 { id } => {
            println!("{}", spaceship::solve2(id)?);
        }
    }
    Ok(())
}
