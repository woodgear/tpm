use failure;
use std::path::{Path,PathBuf};
use serde::{Serialize, Deserialize};
use serde_json;
enum Add{
    Git(String),
    Local(String)
}

enum CliConfig {
    Add(Add),
    Search(String),
    New(String),
}

impl CliConfig {
    // pub fn new()->Self {
    //     // Self::Add("https://github.com/amrayn/easyloggingpp.git".to_string());
    // }
    pub fn get_home_dir(&self)-> PathBuf {
        return PathBuf::from("~/.tpm");
    }

    pub fn execute(&self) -> Result<(),failure::Error> {
        match self {
            CliConfig::Add(add)=> {
                self.do_add(add)
            }
            CliConfig::Search(taglist)=> {
                self.do_search(taglist)
            }
            CliConfig::New(id)=> {
                self.do_new(id)
            }
        }
    }

    pub fn do_add(&self,add_config:&Add)-> Result<(),failure::Error> {
        Ok(())
    }

    pub fn do_update(&self)-> Result<(),failure::Error> {
        Ok(())
    }
    
    pub fn do_search(&self,tag:&str)-> Result<(),failure::Error> {
        Ok(())
    }

    pub fn do_new(&self,id:&str)-> Result<(),failure::Error> {
        Ok(())
    }

    pub fn get_all_meta(&self)->Result<TempleteMetas,failure::Error> {
        let config_json_str = std::fs::read_to_string(self.get_home_dir().join("meta.json"))?;
        let config: TempleteMetas = serde_json::from_str(&config_json_str)?;
        Ok(config)
    }
}

#[derive(Serialize, Deserialize,Debug,PartialEq, Eq)]
struct TempleteMeta {
    url:Option<String>,
    id:String,
    tag:Vec<String>,
    path:String,
}

#[derive(Serialize, Deserialize,Debug,PartialEq, Eq)]
struct TempleteMetas {
    metas:Vec<TempleteMeta>
}

fn main() {
    // let cli CliConfig
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_custom() {
        // let config = CliConfig::Add("https://github.com/amrayn/easyloggingpp.git".to_string());
        // config.execute();
        // assert_eq!(config.get_home_dir().join("lib/easyloggingpp").exists(),true);

        // let config = CliConfig::Add(Add::Git("https://github.com/woodgear/my-templete".to_string()));
        // config.execute();
        // assert_eq!(config.get_home_dir().join("lib/easyloggingpp").exists(),true);
        // assert_eq!(config.get_all_meta(),TempleteMetas {
        //     metas:vec![
        //         TempleteMeta {
        //             url:Some("https://github.com/woodgear/my-templete"),
        //             id:"woodgear-rust-cli",
        //             tag:vec!["woodgear","rust-cli"],
        //             path:config.get_home_dir().join("lib/woodgear/my-templete/rust-cli"),
        //         },
        //         TempleteMeta {
        //             url:Some("https://github.com/woodgear/my-templete"),
        //             id:"woodgear-rust-cli",
        //             tag:vec!["woodgear","cpp","cli","cmake","cross-platform"],
        //             path:config.get_home_dir().join("lib/woodgear/my-templete/cpp-cli"),
        //         },
        //         ]
        // });


        let config = CliConfig::Add(Add::Local("C:\\Users\\developer\\work\\otx\\t".to_string()));
        config.execute();
        assert_eq!(config.get_home_dir().join("lib/local/t").exists(),true);
        assert_eq!(config.get_all_meta().unwrap(),TempleteMetas {
            metas:vec![
                TempleteMeta {
                    url:None,
                    id:"local-t-rust-cli".to_string(),
                    tag:vec!["local".to_string(),"rust-cli".to_string(),"rust".to_string(),"cli".to_string(),"failure".to_string()],
                    path:config.get_home_dir().join("lib/local/t/rust-cli").to_string_lossy().to_string(),
                },
                TempleteMeta {
                    url:None,
                    id:"local-t-mock-gateway".to_string(),
                    tag:vec!["local".to_string(),"mock-gateway".to_string(),"cpp".to_string(),"cmake".to_string(),"cross-platform".to_string(),"cli".to_string(),"log".to_string()],
                    path:config.get_home_dir().join("lib/local/t/mock-gateway").to_string_lossy().to_string(),
                },
                ]
        });


    }
}
