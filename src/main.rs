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
use structopt::StructOpt;
mod cli;
use cli::*;

fn app() -> Result<(), failure::Error> {
    let opts: Opts = Opts::from_args();
    let home_path = "~/.tpm";
    let mut app = TemplateConfigLock::new(Path::new(home_path))?;
    match opts.subcmd {
        SubCommand::Add { path } => {
            app.do_add(path)?;
        }
        SubCommand::New { id, expect_path } => {
            app.do_new(id, expect_path)?;
        }
        SubCommand::Search { tags } => {
            app.do_search(tags);
        }
        SubCommand::ReIndex => {
            app.reindex()?;
        }
        SubCommand::List { tag, template } => {
            if tag {
                app.do_list_tag();
            } else if template {
                app.do_list_template();
            }
        }
    };
    Ok(())
}

fn main() {
    if let Err(e) = app() {
        println!("err {:?}", e);
    }
}

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

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
struct TemplateConfigLock {
    root_path: PathBuf,
    metas: Vec<Meta>,
}

fn copy_dir(origin_path: &Path, targent_path: &Path) -> Result<(), failure::Error> {
    use fs_extra;
    let mut options = fs_extra::dir::CopyOptions::new();
    options.copy_inside = true;
    fs_extra::dir::copy(origin_path, targent_path, &options)?;
    Ok(())
}

impl TemplateConfigLock {
    fn new(path: &Path) -> Result<Self, failure::Error> {
        Self::init(path);
        let mut s = Self {
            root_path: path.to_path_buf(),
            metas: vec![],
        };
        s.init_from_file(&s.root_path.join(".tpm.lock"))?;
        return Ok(s);
    }

    fn reindex(&mut self) -> Result<(), failure::Error> {
        let metas = generate_metas(&self.root_path)?;
        self.metas = metas;
        self.save_to_file(&self.root_path.join(".tpm.lock"))?;
        Ok(())
    }

    fn init(path: &Path) -> Result<(), failure::Error> {
        if !path.exists() {
            std::fs::create_dir_all(path)?
        }
        std::fs::write(path.join(".tpm"), r#"{"kind":"root"}"#)?;
        Ok(())
    }

    fn do_add(&mut self, add: AddKind) -> Result<(), failure::Error> {
        if let AddKind::Local(path) = add {
            return self.do_add_local_file(&path);
        }
        return Ok(());
    }

    fn do_add_local_file(&mut self, path: &Path) -> Result<(), failure::Error> {
        if !path.exists() {
            return Err(failure::err_msg("could not find this local template"));
        }
        copy_dir(path, &self.root_path)?;
        self.reindex();
        Ok(())
    }

    fn do_new(&self, id: String, exptect_path: String) -> Result<(), failure::Error> {
        let template = self
            .metas
            .iter()
            .find(|t| t.name == id)
            .ok_or(failure::format_err!("could not find template {}", id))?;
        copy_dir(Path::new(&template.path), Path::new(&exptect_path))?;
        Ok(())
    }

    fn do_list_tag(&self) {
        use std::collections::HashSet;
        let mut set = HashSet::new();
        for m in self.metas.iter() {
            for t in m.tag.iter() {
                set.insert(t);
            }
        }
        for t in set {
            println!("tag: {}", t);
        }
    }

    fn do_list_template(&self) {
        for m in self.metas.iter() {
            println!("{:?}", m);
        }
    }

    fn do_search(&self, tag: Vec<String>) {
        let search = Searable::from(self.metas.clone());
        let tag: Vec<&str> = tag.iter().map(AsRef::as_ref).collect();
        let templates = search.search(tag);
        for t in templates {
            println!("{:?}", t);
        }
    }
}

impl TemplateConfigLock {
    fn into_search(&self) -> Pin<Box<Searable<Meta>>> {
        return Searable::from(self.metas.clone());
    }
}

impl TemplateConfigLock {
    fn init_from_file(&mut self, path: &Path) -> Result<(), failure::Error> {
        if path.exists() {
            let data = std::fs::read_to_string(path)?;
            return self.init_from_str(&data);
        }
        return Ok(());
    }

    fn init_from_str(&mut self, json_str: &str) -> Result<(), failure::Error> {
        let metas: Vec<Meta> = serde_json::from_str(json_str)?;
        self.metas = metas;
        Ok(())
    }

    fn into_str(&self) -> Result<String, failure::Error> {
        let res = serde_json::to_string(&self.metas)?;
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
    use filesystem::FakeFileSystem;
    use filesystem::TempDir;
    use filesystem::TempFileSystem;

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

    fn assert_meta_eq(
        root_path: &Path,
        real_metas: Vec<Meta>,
        expect_metas: Vec<(&str, Vec<&str>, &str)>,
    ) {
        let expect_metas: Vec<Meta> = expect_metas
            .into_iter()
            .map(|(name, tags, path)| Meta {
                name: name.to_string(),
                tag: tags.into_iter().map(|s| s.to_string()).collect(),
                path: root_path.join(path).to_string_lossy().to_string(),
            })
            .collect();
        assert_eq!(real_metas, expect_metas);
    }

    fn assert_generate_locks(
        mock_fs: Vec<(&str, &str, &str)>,
        expect_metas: Vec<(&str, Vec<&str>, &str)>,
    ) {
        let fake_fs = FakeFileSystem::new().temp_dir("test_tpm").unwrap();
        let root_path = fake_fs.path();
        let tpm_path = root_path.join(".tpm");
        do_mock_fs(&root_path, mock_fs);
        let real_metas = generate_metas(&tpm_path).unwrap();
        assert_meta_eq(root_path, real_metas, expect_metas);
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

    #[test]
    fn test_add_local_file() {
        let local_dir_guard = FakeFileSystem::new().temp_dir("test_add_local").unwrap();
        let local_path = local_dir_guard.path();
        let app_dir_guard = FakeFileSystem::new()
            .temp_dir("test_add_local_app")
            .unwrap();
        let app_path = app_dir_guard.path();

        do_mock_fs(
            local_path,
            vec![
                ("f", "a/.tpm", r#"{"kind":"mutli"}"#),
                ("f", "a/b/.tpm", r#"{"kind":"single","tag":["1"]}"#),
                ("f", "a/c/.tpm", r#"{"kind":"single","tag":["2"]}"#),
            ],
        );
        println!("{:?}", app_path);
        let mut app = TemplateConfigLock::new(&app_path.join(".tpm")).unwrap();
        app.do_add_local_file(&local_path.join("a"));
        let metas = app.metas;
        assert_meta_eq(
            app_path,
            metas,
            vec![
                ("a-b", vec!["1"], ".tpm/a/b"),
                ("a-c", vec!["2"], ".tpm/a/c"),
            ],
        );

        let local_dir_guard = FakeFileSystem::new().temp_dir("test_add_local").unwrap();
        let local_path = local_dir_guard.path();
        let app_dir_guard = FakeFileSystem::new()
            .temp_dir("test_add_local_app")
            .unwrap();
        let app_path = app_dir_guard.path();

        do_mock_fs(
            local_path,
            vec![("f", "a/.tpm", r#"{"kind":"single","tag":["123"]}"#)],
        );
        let mut app = TemplateConfigLock::new(&app_path.join(".tpm")).unwrap();
        app.do_add_local_file(&local_path.join("a"));
        let metas = app.metas;
        assert_meta_eq(app_path, metas, vec![("a", vec!["123"], ".tpm/a")]);
    }
}
