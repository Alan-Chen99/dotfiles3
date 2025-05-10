use clap::Parser;
use color_eyre::eyre::Result;

#[derive(Parser)]
struct Args {}

fn main() -> Result<()> {
    let _args = Args::parse();

    color_eyre::install()?;

    Ok(())
}
