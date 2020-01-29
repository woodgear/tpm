use failure;
use serde::{Deserialize, Serialize};
use serde_json;
use std::cmp::{Ord, Ordering};
use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt::Debug;
use std::fs;
use std::marker::Sized;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::ptr::NonNull;
use std::time::Instant;

mod cli;
use cli::CliConfig;

fn main() {}

trait Tag {
    fn get_tags(&self) -> Vec<String>;
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
enum TemplateKind {
    Git(String),
    Local(String),
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
struct Meta {
    tag: Vec<String>,
    name: String,
    path: String,
}

impl Tag for Meta {
    fn get_tags(&self) -> Vec<String> {
        return self.tag.clone();
    }
}

impl Ord for Meta {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl PartialOrd for Meta {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

unsafe fn clone_from_nonnull<T>(refs: Vec<NonNull<T>>) -> Vec<T>
where
    T: Clone,
{
    return refs
        .into_iter()
        .map(|p| unsafe { p.as_ref() }.clone())
        .collect();
}

fn count_and_sort<T>(mut data_source: Vec<T>) -> Vec<T>
where
    T: Ord + Eq + Debug,
{
    data_source.sort();

    let mut data: VecDeque<(T, u32)> = VecDeque::new();
    for i in data_source.into_iter() {
        if let Some(front) = data.back_mut() {
            if i.eq(&front.0) {
                front.1 += 1;
                continue;
            }
        }
        let i: T = i;
        data.push_back((i, 1));
    }
    let mut data: Vec<(T, u32)> = data.into_iter().collect();

    data.sort_by(|b, a| {
        if a.1 == b.1 {
            return a.0.cmp(&b.0);
        }
        return a.1.cmp(&b.1);
    });
    let data = data.into_iter().map(|i| i.0).collect();
    return data;
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
struct TemplateConfigLock {
    metas: Vec<Meta>,
}

impl TemplateConfigLock {
    fn add(&mut self, meta: Meta) {
        self.metas.push(meta);
    }

    fn into_search(&self) -> Pin<Box<Searable<Meta>>> {
        return Searable::from(self.metas.clone());
    }
}

impl TemplateConfigLock {
    fn from_file(path: String) -> Result<Self, failure::Error> {
        let data = std::fs::read_to_string(path)?;
        return Self::from_str(&data);
    }

    fn from_str(json_str: &str) -> Result<Self, failure::Error> {
        let s: Self = serde_json::from_str(json_str)?;
        return Ok(s);
    }

    fn into_str(&self) -> Result<String, failure::Error> {
        let res = serde_json::to_string(self)?;
        Ok(res)
    }
    fn save_to_file(&self, path: &Path) -> Result<(), failure::Error> {
        let json_str = self.into_str()?;
        std::fs::write(path, json_str.as_bytes())?;
        Ok(())
    }
}

struct Searable<T>
where
    T: Tag + Clone,
{
    hash: HashMap<String, Vec<NonNull<T>>>,
    data: Vec<T>,
}

impl<T> Searable<T>
where
    T: Tag + Clone,
{
    fn get_refs(self: &Pin<Box<Self>>, tag: &str) -> Vec<NonNull<T>> {
        let lis = self.hash.get(tag).unwrap_or(&vec![]).to_vec();
        return lis.into_iter().collect();
    }

    fn get(self: &Pin<Box<Self>>, tag: &str) -> Vec<T> {
        let lis = self.hash.get(tag).unwrap_or(&vec![]).to_vec();
        return unsafe { clone_from_nonnull(lis) };
    }

    fn get_all(self: &Pin<Box<Self>>) -> Vec<T> {
        return self.data.clone();
    }

    fn search(self: &Pin<Box<Self>>, keywords: Vec<&str>) -> Vec<T>
    where
        T: Ord + Debug,
    {
        use itertools::concat;

        let refs: Vec<Vec<NonNull<T>>> = keywords.iter().map(|tag| self.get_refs(tag)).collect();
        let refs = concat(refs);

        let refs = count_and_sort(refs);
        return unsafe { clone_from_nonnull(refs) };
    }

    fn from(data: Vec<T>) -> Pin<Box<Self>> {
        let mut res = Box::pin(Self {
            hash: HashMap::new(),
            data,
        });

        let mut hash: HashMap<String, Vec<NonNull<T>>> = HashMap::new();
        for meta in res.data.iter() {
            let meta = NonNull::from(meta);
            for t in unsafe { meta.as_ref() }.get_tags() {
                hash.entry(t)
                    .and_modify(|v| v.push(meta))
                    .or_insert(vec![meta]);
            }
        }
        unsafe {
            let mut_ref: Pin<&mut Self> = Pin::as_mut(&mut res);
            Pin::get_unchecked_mut(mut_ref).hash = hash;
        }
        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get() {
        #[derive(Debug, Clone, Eq, PartialEq)]
        struct Meta {
            tag: Vec<String>,
            name: String,
        }

        impl Tag for Meta {
            fn get_tags(&self) -> Vec<String> {
                return self.tag.clone();
            }
        }
        let m1 = Meta {
            tag: vec!["t3".to_string()],
            name: "m1".to_string(),
        };
        let m2 = Meta {
            tag: vec!["t1".to_string(), "t2".to_string()],
            name: "m2".to_string(),
        };
        let m3 = Meta {
            tag: vec!["t1".to_string(), "t2".to_string()],
            name: "m3".to_string(),
        };

        let s = Searable::from(vec![m1.clone(), m2.clone(), m3.clone()]);

        let l = s.get("t3");
        assert_eq!(l, vec![m1.clone()]);
    }

    #[test]
    fn test_search() {
        #[derive(Debug, Clone, Eq, PartialEq)]
        struct Meta {
            tag: Vec<String>,
            name: String,
        }

        impl Tag for Meta {
            fn get_tags(&self) -> Vec<String> {
                return self.tag.clone();
            }
        }

        impl Ord for Meta {
            fn cmp(&self, other: &Self) -> Ordering {
                self.name.cmp(&other.name)
            }
        }

        impl PartialOrd for Meta {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        let m1 = Meta {
            tag: vec!["t1".to_string()],
            name: "m1".to_string(),
        };
        let m2 = Meta {
            tag: vec!["t1".to_string(), "t2".to_string()],
            name: "m2".to_string(),
        };
        let m3 = Meta {
            tag: vec!["t1".to_string(), "t3".to_string()],
            name: "m3".to_string(),
        };

        let s = Searable::from(vec![m1.clone(), m2.clone(), m3.clone()]);

        let l = s.search(vec!["t2", "t1"]);
        assert_eq!(l, vec![m2.clone(), m3.clone(), m1.clone(),]);

        let m1 = Meta {
            tag: vec!["t1".to_string()],
            name: "m1".to_string(),
        };
        let m2 = Meta {
            tag: vec!["t1".to_string(), "t2".to_string()],
            name: "m2".to_string(),
        };
        let m3 = Meta {
            tag: vec!["t2".to_string(), "t3".to_string()],
            name: "m3".to_string(),
        };

        let s = Searable::from(vec![m1.clone(), m2.clone(), m3.clone()]);

        let l = s.search(vec!["t2", "t3"]);
        assert_eq!(l, vec![m3.clone(), m2.clone()]);

        println!("{:?} {:?}", std::line!(), l);
    }
    fn do_mock_fs(root: &Path, mock_fs: Vec<(&str, &str, &str)>) {
        for (kind, path, content) in mock_fs {
            println!("k {} p {} c {}", kind, path, content);
            if kind == "f" {
                let full_path = root.join(path);
                let parent_dir = full_path.parent().unwrap();
                std::fs::create_dir_all(parent_dir).unwrap();
                std::fs::write(full_path, content);
            }
        }
    }

    fn assert_generate_locks(
        mock_fs: Vec<(&str, &str, &str)>,
        expect_metas: Vec<(&str, Vec<&str>, &str)>,
    ) {
        use filesystem::FakeFileSystem;
        use filesystem::TempDir;
        use filesystem::TempFileSystem;
        let fake_fs = FakeFileSystem::new().temp_dir("test_tpm").unwrap();
        let root_path = fake_fs.path();
        let tpm_path = root_path.join(".tpm");
        do_mock_fs(&root_path, mock_fs);
        let expect_metas: Vec<Meta> = expect_metas
            .into_iter()
            .map(|(name, tags, path)| Meta {
                name: name.to_string(),
                tag: tags.into_iter().map(|s| s.to_string()).collect(),
                path: root_path.join(path).to_string_lossy().to_string(),
            })
            .collect();
        let real_metas = generate_metas(&tpm_path).unwrap();
        println!("real_metas {:?}", real_metas);
        println!("expect_metas {:?}", expect_metas);
        assert_eq!(real_metas, expect_metas);
    }

    fn generate_metas(root_path: &Path) -> Result<Vec<Meta>, failure::Error> {
        let tpm_config_path = root_path.join(".tpm");
        if !(tpm_config_path.exists() && tpm_config_path.is_file()) {
            return Err(failure::format_err!(
                "tpm_config_path.exists {} tpm_config_path.is_file {}",
                tpm_config_path.exists(),
                tpm_config_path.is_file()
            ));
        }

        #[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
        struct TpmConfig {
            kind: TemplateKind,
            #[serde(default)]
            tag: Vec<String>,
        }

        #[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
        enum TemplateKind {
            mutli,
            single,
            root,
            file, //not now
        }

        let config: TpmConfig = serde_json::from_str(&std::fs::read_to_string(&tpm_config_path)?)?;

        if config.kind == TemplateKind::single {
            let name = root_path.file_name().unwrap().to_string_lossy().to_string();
            return Ok(vec![Meta {
                name,
                path: root_path.to_string_lossy().to_string(),
                tag: config.tag.clone(),
            }]);
        }

        if config.kind == TemplateKind::root || config.kind == TemplateKind::mutli {
            //iter dirs
            let mut metas: Vec<Meta> = vec![];
            for entry in root_path.read_dir()? {
                let entry = entry?;
                if entry.path().is_dir() {
                    let mut sub_metas = generate_metas(&entry.path())?;
                    if config.kind == TemplateKind::mutli {
                        let name = root_path.file_name().unwrap().to_string_lossy().to_string();
                        for m in sub_metas.iter_mut() {
                            m.name = format!("{}-{}", name, m.name);
                        }
                    }
                    metas.append(&mut sub_metas);
                }
            }
            return Ok(metas);
        }
        unreachable!()
    }
    #[test]
    fn test_generate_lock() {
        let mock_fs = vec![
            (
                "f",
                ".tpm/.tpm",
                r#"
            {
                "kind":"root"
            }
            "#,
            ),
            (
                "f",
                ".tpm/a/.tpm",
                r#"
            {
                "kind":"mutli"
            }
            "#,
            ),
            (
                "f",
                ".tpm/a/b/.tpm",
                r#"
        {
            "tag":["b"],
            "kind":"single"
        }
        "#,
            ),
            (
                "f",
                ".tpm/a/c/.tpm",
                r#"
        {
            "tag":["c"],
            "kind":"single"
        }
        "#,
            ),
        ];
        let expect_metas = vec![
            ("a-b", vec!["b"], ".tpm/a/b"),
            ("a-c", vec!["c"], ".tpm/a/c"),
        ];

        assert_generate_locks(mock_fs, expect_metas)
    }
}
