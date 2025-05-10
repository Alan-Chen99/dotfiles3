#![feature(never_type)]

use std::ffi::OsString;
use std::os::unix::process::CommandExt;
use std::process::Command;

use clap::Parser;
use color_eyre::eyre::Result;
use nix::libc::{gid_t, uid_t};
use nix::unistd::{setgid, setuid, Gid, Uid};

#[derive(Parser)]
struct Args {
    #[arg(short, long)]
    uid: Option<uid_t>,

    #[arg(short, long)]
    gid: Option<gid_t>,

    #[arg(trailing_var_arg = true)]
    rest: Vec<OsString>,
}

fn main() -> Result<!> {
    let mut args = Args::parse();

    color_eyre::install()?;

    if args.uid.is_none() && args.gid.is_none() {
        args.uid = Some(0);
    }

    if let Some(uid) = args.uid {
        setuid(Uid::from_raw(uid))?;
    }

    if let Some(gid) = args.gid {
        setgid(Gid::from_raw(gid))?;
    }

    if args.rest.is_empty() {
        args.rest = ["bash", "--noprofile", "--norc"]
            .iter()
            .map(OsString::from)
            .collect();
    }

    Err(Command::new(&args.rest[0])
        .args(&args.rest[1..])
        .exec()
        .into())
}
