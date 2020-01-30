use std::path::{Path, PathBuf};
use structopt::StructOpt;

use std::str::FromStr;
use url::{ParseError, Url};
#[derive(Debug, Eq, PartialEq)]
pub enum AddKind {
    Local(PathBuf),
    Git(String),
}

impl FromStr for AddKind {
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let path = Path::new(s);

        //maybe local path
        if path.exists() {
            let path = path.canonicalize()?;
            if !path.is_dir() {
                return Err(failure::format_err!("only support dir now {:?}", path));
            }
            if !(path.join(".tpm").exists() && path.join(".tpm").is_file()) {
                return Err(failure::format_err!("must be a template now {:?}", path));
            }
            return Ok(AddKind::Local(path.to_path_buf()));
        }

        //maybe git url
        let url = Url::parse(s)?;
        return Ok(AddKind::Git(s.to_string()));
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name = "tpm")]
pub struct Opts {
    #[structopt(subcommand)]
    pub subcmd: SubCommand,
}

#[derive(StructOpt, Debug, Eq, PartialEq)]
#[structopt(version = "0.1", author = "fwdx")]
pub enum SubCommand {
    Add {
        path: AddKind,
    },
    New {
        id: String,
        #[structopt(default_value = "./")]
        expect_path: String,
    },
    Search {
        tags: Vec<String>,
    },
    ReIndex,
    List {
        #[structopt(short = "t")]
        tag: bool,
        #[structopt(short = "m")]
        template: bool,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parser_cli() {
        let opts: Opts = Opts::from_iter(vec!["./", "add", "https://github.com/woodgear/t.git"]);
        assert_eq!(
            opts.subcmd,
            SubCommand::Add {
                path: AddKind::Git("https://github.com/woodgear/t.git".to_string())
            }
        );
        let opts: Opts = Opts::from_iter(vec!["./", "new", "123", "./"]);
        assert_eq!(
            opts.subcmd,
            SubCommand::New {
                id: "123".to_string(),
                expect_path: "./".to_string(),
            }
        );

        let opts: Opts = Opts::from_iter(vec!["./", "new", "123"]);
        assert_eq!(
            opts.subcmd,
            SubCommand::New {
                id: "123".to_string(),
                expect_path: "./".to_string(),
            }
        );

        let opts: Opts = Opts::from_iter(vec!["./", "search", "123", "223"]);
        assert_eq!(
            opts.subcmd,
            SubCommand::Search {
                tags: vec!["123".to_string(), "223".to_string(),]
            }
        );
        println!("opts {:?}", opts);
    }
}
