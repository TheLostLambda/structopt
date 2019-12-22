// Copyright 2018 Guillaume Pinot (@TeXitoi) <texitoi@texitoi.eu>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

mod utils;

use structopt::StructOpt;
use utils::*;

#[test]
fn it_works() {
    #[derive(StructOpt, PartialEq, Debug)]
    enum Opt {
        /// Fetch stuff from GitHub
        Fetch {
            #[structopt(long)]
            all: bool,
            #[structopt(short, long)]
            /// Overwrite local branches.
            force: bool,
            repo: String,
        },

        Add {
            #[structopt(short, long)]
            interactive: bool,
            #[structopt(short, long)]
            verbose: bool,
        },
    }

    assert_eq!(
        Opt::Fetch {
            all: true,
            force: false,
            repo: "origin".to_string()
        },
        Opt::from_clap(&Opt::clap().get_matches_from(&["test", "fetch", "--all", "origin"]))
    );
    assert_eq!(
        Opt::Fetch {
            all: false,
            force: true,
            repo: "origin".to_string()
        },
        Opt::from_clap(&Opt::clap().get_matches_from(&["test", "fetch", "-f", "origin"]))
    );
    assert_eq!(
        Opt::Add {
            interactive: false,
            verbose: false
        },
        Opt::from_clap(&Opt::clap().get_matches_from(&["test", "add"]))
    );
    assert_eq!(
        Opt::Add {
            interactive: true,
            verbose: true
        },
        Opt::from_clap(&Opt::clap().get_matches_from(&["test", "add", "-i", "-v"]))
    );

    assert!(Opt::clap()
        .get_matches_from_safe(&["test", "badcmd", "-i", "-v"])
        .is_err());
    assert!(Opt::clap()
        .get_matches_from_safe(&["test", "add", "--badoption"])
        .is_err());
    assert!(Opt::clap().get_matches_from_safe(&["test"]).is_err());
}

#[test]
/// This test is specifically to make sure that hyphenated subcommands get
/// processed correctly.
fn test_hyphenated_subcommands() {
    #[derive(StructOpt, PartialEq, Debug)]
    enum Opt {
        DoSomething { arg: String },
    }

    assert_eq!(
        Opt::DoSomething {
            arg: "blah".to_string()
        },
        Opt::from_clap(&Opt::clap().get_matches_from(&["test", "do-something", "blah"]))
    );
}

#[test]
fn test_null_commands() {
    #[derive(StructOpt, PartialEq, Debug)]
    enum Opt {
        Add,
        Init,
        Fetch,
    }

    assert_eq!(
        Opt::Add,
        Opt::from_clap(&Opt::clap().get_matches_from(&["test", "add"]))
    );
    assert_eq!(
        Opt::Init,
        Opt::from_clap(&Opt::clap().get_matches_from(&["test", "init"]))
    );
    assert_eq!(
        Opt::Fetch,
        Opt::from_clap(&Opt::clap().get_matches_from(&["test", "fetch"]))
    );
}

#[test]
fn test_tuple_commands() {
    #[derive(StructOpt, PartialEq, Debug)]
    #[structopt(about = "Not shown")]
    struct Add {
        file: String,
    }

    /// Not shown
    #[derive(StructOpt, PartialEq, Debug)]
    struct Fetch {
        remote: String,
    }

    #[derive(StructOpt, PartialEq, Debug)]
    enum Opt {
        // Not shown
        /// Add a file
        Add(Add),
        Init,
        /// download history from remote
        Fetch(Fetch),
    }

    assert_eq!(
        Opt::Add(Add {
            file: "f".to_string()
        }),
        Opt::from_clap(&Opt::clap().get_matches_from(&["test", "add", "f"]))
    );
    assert_eq!(
        Opt::Init,
        Opt::from_clap(&Opt::clap().get_matches_from(&["test", "init"]))
    );
    assert_eq!(
        Opt::Fetch(Fetch {
            remote: "origin".to_string()
        }),
        Opt::from_clap(&Opt::clap().get_matches_from(&["test", "fetch", "origin"]))
    );

    let output = get_long_help::<Opt>();

    assert!(output.contains("download history from remote"));
    assert!(output.contains("Add a file"));
    assert!(!output.contains("Not shown"));
}

#[test]
fn enum_in_enum_subsubcommand() {
    #[derive(StructOpt, Debug, PartialEq)]
    pub enum Opt {
        Daemon(DaemonCommand),
    }

    #[derive(StructOpt, Debug, PartialEq)]
    pub enum DaemonCommand {
        Start,
        Stop,
    }

    let result = Opt::clap().get_matches_from_safe(&["test"]);
    assert!(result.is_err());

    let result = Opt::clap().get_matches_from_safe(&["test", "daemon"]);
    assert!(result.is_err());

    let result = Opt::from_iter(&["test", "daemon", "start"]);
    assert_eq!(Opt::Daemon(DaemonCommand::Start), result);
}

#[test]
fn flatten_enum() {
    #[derive(StructOpt, Debug, PartialEq)]
    struct Opt {
        #[structopt(flatten)]
        sub_cmd: SubCmd,
    }
    #[derive(StructOpt, Debug, PartialEq)]
    enum SubCmd {
        Foo,
        Bar,
    }

    assert!(Opt::from_iter_safe(&["test"]).is_err());
    assert_eq!(
        Opt::from_iter(&["test", "foo"]),
        Opt {
            sub_cmd: SubCmd::Foo
        }
    );
}

#[test]
fn external_subcommand_string() {
    #[derive(StructOpt, Debug, PartialEq)]
    struct Opt {
        #[structopt(flatten)]
        sub_cmd: SubCmd,
    }
    #[derive(StructOpt, Debug, PartialEq)]
    enum SubCmd {
        Foo {
            #[structopt(long)]
            flag: bool,
            #[structopt(long)]
            arg: u32,
        },

        #[structopt(external_subcommand)]
        Other(Vec<String>),
    }

    assert_eq!(
        Opt::from_iter(&["test", "foo", "--flag", "--arg=0"]),
        Opt {
            sub_cmd: SubCmd::Foo { flag: true, arg: 0 }
        }
    );

    assert_eq!(
        Opt::from_iter(&["test", "bar", "--flag", "--arg=0"]),
        Opt {
            sub_cmd: SubCmd::Other(vec!["bar".into(), "--flag".into(), "--arg=0".into()])
        }
    );
    assert_eq!(
        Opt::from_iter(&["test"]),
        Opt {
            sub_cmd: SubCmd::Other(vec!["".into()])
        }
    );
}

#[test]
fn external_subcommand_os_string() {
    use std::ffi::OsString;

    #[derive(StructOpt, Debug, PartialEq)]
    struct Opt {
        #[structopt(flatten)]
        sub_cmd: SubCmd,
    }
    #[derive(StructOpt, Debug, PartialEq)]
    enum SubCmd {
        Foo {
            #[structopt(long)]
            flag: bool,
            #[structopt(long)]
            arg: u32,
        },

        #[structopt(external_subcommand)]
        Other(Vec<OsString>),
    }

    assert_eq!(
        Opt::from_iter(&["test", "foo", "--flag", "--arg=0"]),
        Opt {
            sub_cmd: SubCmd::Foo { flag: true, arg: 0 }
        }
    );

    assert_eq!(
        Opt::from_iter(&["test", "bar", "--flag", "--arg=0"]),
        Opt {
            sub_cmd: SubCmd::Other(vec![
                OsString::from("bar".to_string()),
                OsString::from("--flag".to_string()),
                OsString::from("--arg=0".to_string())
            ])
        }
    );
    assert_eq!(
        Opt::from_iter(&["test"]),
        Opt {
            sub_cmd: SubCmd::Other(vec![OsString::from("".to_string())])
        }
    );

    assert!(Opt::from_iter_safe(&["test", "--foo"]).is_err());
}
