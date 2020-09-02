#![allow(clippy::needless_return)]
use failure;
use serde::{Deserialize, Serialize};
use serde_json;
use std::cmp::{Ord, Ordering};
use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt::Debug;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::ptr::NonNull;
use structopt::StructOpt;
mod cli;
use cli::*;
use context_attribute::context;
use failure::ResultExt;

#[context(fn)]
fn app() -> Result<(), failure::Error> {
    let opts: Opts = Opts::from_args();
    let home_dir = dirs::home_dir().ok_or_else(|| failure::err_msg("could not find home dir"))?;

    let home_path = home_dir.join(".tpm");
    let mut app = TemplateConfigLock::new(&home_path)?;
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
        SubCommand::Update => {
            app.do_update()?;
        }
        SubCommand::List { show_tag } => {
            if show_tag {
                app.do_list_tag();
            } else {
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

/// meta which in .tpm.lock
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
    return refs.into_iter().map(|p| p.as_ref().clone()).collect();
}

#[context(fn)]
fn git_clone_or_update_master(url: &str, path: &Path) -> Result<(), failure::Error> {
    use git_url_parse::GitUrl;
    let git_url = GitUrl::parse(url).map_err(|e| failure::format_err!("parser url err {:?}", e))?;
    let git_repo_path = path.join(&git_url.name);
    if git_repo_path.exists() {
        //what do you want more
        fs_extra::dir::remove(&git_repo_path)?;
    }
    use git2::build::{CloneLocal, RepoBuilder};
    RepoBuilder::new()
        .clone_local(CloneLocal::Auto)
        .clone(url, &git_repo_path)?;
    Ok(())
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

#[context(fn)]
fn generate_metas(root_path: &Path) -> Result<Vec<Meta>, failure::Error> {
    let tpm_config_path = root_path.join(".tpm");
    if !(tpm_config_path.exists() && tpm_config_path.is_file()) {
        return Err(failure::format_err!(
            "{:?} tpm_config_path.exists {} tpm_config_path.is_file {}",
            root_path,
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
    #[serde(rename_all = "lowercase")]
    enum TemplateKind {
        Mutli,
        Single,
        Root,
        File, //not now
    }

    let config: TpmConfig = serde_json::from_str(&std::fs::read_to_string(&tpm_config_path)?)?;

    if config.kind == TemplateKind::Single {
        let name = root_path.file_name().unwrap().to_string_lossy().to_string();
        let root_path = root_path.canonicalize().unwrap();
        return Ok(vec![Meta {
            name,
            path: root_path.to_string_lossy().to_string(),
            tag: config.tag,
        }]);
    }

    if config.kind == TemplateKind::Root || config.kind == TemplateKind::Mutli {
        //iter dirs
        let mut metas: Vec<Meta> = vec![];
        for entry in root_path.read_dir()? {
            let entry = entry?;
            let entry_path = entry.path();
            let entry_name = entry_path
                .file_name()
                .ok_or_else(|| failure::format_err!("could not get name of {:?}", entry_path))?;
            if !entry_path.is_dir() {
                continue;
            }
            if entry_name == ".git" {
                continue;
            }

            let mut sub_metas = generate_metas(&entry_path)?;
            if config.kind == TemplateKind::Mutli {
                let name = root_path.file_name().unwrap().to_string_lossy().to_string();
                for m in sub_metas.iter_mut() {
                    m.name = format!("{}-{}", name, m.name);
                }
            }
            metas.append(&mut sub_metas);
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

#[context(fn)]
/// origin_path /a/b/c => c is a directory
/// target_path /a/b/c1 => c1 doest not exist
/// after copy dir, c1 and c are exactly same except and name 'c' 'c1' it self
fn copy_dir(origin_path: &Path, targent_path: &Path) -> Result<(), failure::Error> {
    use fs_extra;
    let mut options = fs_extra::dir::CopyOptions::new();
    options.copy_inside = true;
    fs_extra::dir::copy(origin_path, targent_path, &options)?;
    Ok(())
}

mod render_template {
    use super::render_template_with_prefix;
    use crate::context;
    use failure::ResultExt;
    use serde::{Deserialize, Serialize};
    use std::path::Path;
    // .tpm struct
    #[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
    struct Meta {
        template: Option<Template>,
    }
    #[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
    struct Template {
        prefix: String,
    }
    #[context(fn)]
    pub fn render_template(path: &Path) -> Result<(), failure::Error> {
        use failure::ResultExt;

        let config_path = path.join(".tpm");
        if !config_path.exists() {
            return Err(failure::format_err!(
                "could not find cofnig {:?}",
                config_path
            ));
        }

        let meta_raw_json = &std::fs::read_to_string(&config_path)?;
        let meta: Meta = serde_json::from_str(meta_raw_json).context("read meta json fail")?;
        let prefix = {
            if let Some(template) = meta.template {
                template.prefix
            } else {
                return Ok(());
            }
        };
        render_template_with_prefix(path, &prefix)?;

        Ok(())
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        #[test]
        fn test_parser_meta() {
            let raw = r#"
           {
            "tag":["rust","cli","log"],
            "kind":"single",
            "template":{
                "prefix":"_t_"
            }
        }
           "#;
            let meta: Meta = serde_json::from_str(raw).unwrap();
            assert_eq!(
                meta,
                Meta {
                    template: Some(Template {
                        prefix: "_t_".to_string()
                    })
                }
            )
        }
    }
}
use render_template::render_template;
#[context(fn)]
fn pick_names(path: &Path, prefix: &str) -> Result<Vec<String>, failure::Error> {
    use failure::ResultExt;
    use indexmap::IndexSet;
    use simple_replace_templete_engine::get_variables;
    use walkdir::WalkDir;
    let mut names = IndexSet::new();
    for entry in WalkDir::new(path).into_iter().filter_map(|e| e.ok()) {
        let path = entry.path().display().to_string();
        for var in get_variables(&path, prefix).into_iter() {
            names.insert(var);
        }
        let path = Path::new(&path);
        if path.is_file() {
            let content =
                std::fs::read_to_string(&path).context(format!("read {:?} fail", path))?;

            for var in get_variables(&content, prefix).into_iter() {
                names.insert(var);
            }
        }
    }
    Ok(names.into_iter().collect())
}

#[context(fn)]
fn ask_names(names: Vec<String>) -> Result<HashMap<String, String>, failure::Error> {
    let mut values = HashMap::new();
    use dialoguer::{theme::CustomPromptCharacterTheme, Input};

    let theme = CustomPromptCharacterTheme::new('>');
    for n in names {
        let input: String = Input::with_theme(&theme).with_prompt(&n).interact()?;
        values.insert(n, input);
    }
    return Ok(values);
}

#[context(fn)]
fn render_template_with_prefix(path: &Path, prefix: &str) -> Result<(), failure::Error> {
    let mut names = pick_names(path, prefix)?;

    let name = path
        .file_name()
        .ok_or(failure::err_msg("could nt find file name"))?;
    let name = name.to_string_lossy().to_string();
    let _removed = names
        .iter()
        .position(|n| n == "name")
        .map(|e| names.remove(e))
        .is_some();

    let mut values = ask_names(names)?;
    values.insert("name".to_string(), name);
    _render_template(path, prefix, &values)?;
    Ok(())
}

#[context(fn)]
fn _render_template(
    path: &Path,
    prefix: &str,
    values: &HashMap<String, String>,
) -> Result<(), failure::Error> {
    use simple_replace_templete_engine::render;
    use std::fs;
    use walkdir::WalkDir;
    for entry in WalkDir::new(path).into_iter().filter_map(|e| e.ok()) {
        let path = entry.path().display().to_string();
        let new_path = render(&path, prefix, &values)?;
        if new_path != path {
            fs::rename(&path, &new_path)?;
        }
        let new_path = Path::new(&new_path);
        if new_path.is_file() {
            let content = fs::read_to_string(&new_path)?;
            let new_content = render(&content, prefix, &values)?;
            if new_content != content {
                fs::write(&new_path, new_content)?;
            }
        }
    }
    Ok(())
}

fn get_git_path(root_path: &Path) -> Result<Vec<PathBuf>, failure::Error> {
    let mut git_paths = vec![];
    for e in root_path.read_dir()? {
        let e = e?;
        let e_path = e.path();
        if e_path.is_dir() && e_path.join(".git").exists() {
            git_paths.push(e_path);
        }
    }
    Ok(git_paths)
}

fn git_update(git_path: &Path) -> Result<(), failure::Error> {
    use std::process::Command;
    println!("{:?}", git_path);
    let out = Command::new("git")
        .args(vec!["pull", "origin", "master"])
        .current_dir(git_path)
        .output()?;
    if !out.status.success() {
        return Err(failure::format_err!("{:?}", out));
    }
    Ok(())
}

impl TemplateConfigLock {
    #[context(fn)]
    fn new(path: &Path) -> Result<Self, failure::Error> {
        let path = Self::init(&path)?;
        let mut s = Self {
            root_path: path.to_path_buf(),
            metas: vec![],
        };
        let lock_path = s.root_path.join(".tpm.lock");
        if lock_path.exists() {
            s.init_from_file(&lock_path)?;
        } else {
            s.reindex()?;
        }
        return Ok(s);
    }

    #[context(fn)]
    fn reindex(&mut self) -> Result<(), failure::Error> {
        println!("reindex");
        let metas = generate_metas(&self.root_path)?;
        self.metas = metas;
        self.save_to_file(&self.root_path.join(".tpm.lock"))?;
        Ok(())
    }

    #[context(fn)]
    fn init(path: &Path) -> Result<PathBuf, failure::Error> {
        if !path.exists() {
            std::fs::create_dir_all(path)?
        }
        std::fs::write(path.join(".tpm"), r#"{"kind":"root"}"#)?;
        let path = path.canonicalize()?;
        Ok(path)
    }

    #[context(fn)]
    fn do_add(&mut self, add: AddKind) -> Result<(), failure::Error> {
        if let AddKind::Local(path) = add {
            return self.do_add_local_file(&path);
        }
        if let AddKind::Git(url) = add {
            return self.do_add_git(&url);
        }

        return Ok(());
    }

    #[context(fn)]
    fn do_update(&mut self) -> Result<(), failure::Error> {
        for git in get_git_path(&self.root_path)? {
            println!("update {:?}", git);
            git_update(&git)?;
        }
        self.reindex()?;
        Ok(())
    }

    #[context(fn)]
    fn do_add_git(&mut self, url: &str) -> Result<(), failure::Error> {
        println!("do_add_git {}", url);
        git_clone_or_update_master(url, &self.root_path)?;
        self.reindex()?;
        Ok(())
    }

    #[context(fn)]
    fn do_add_local_file(&mut self, path: &Path) -> Result<(), failure::Error> {
        if !path.exists() {
            return Err(failure::err_msg("could not find this local template"));
        }
        copy_dir(path, &self.root_path)?;
        self.reindex()?;
        Ok(())
    }

    #[context(fn)]
    fn do_new(&self, id: String, expect_path: String) -> Result<(), failure::Error> {
        let template = self
            .metas
            .iter()
            .find(|t| t.name == id)
            .ok_or_else(|| failure::format_err!("could not find template {}", id))?;
        let expect_path = Path::new(&expect_path);
        copy_dir(Path::new(&template.path), expect_path)?;
        render_template(&expect_path)?;
        Ok(())
    }

    fn do_list_tag(&self) {
        println!("do list tag");
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
        println!("do list template");
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
    #[context(fn)]
    fn init_from_file(&mut self, path: &Path) -> Result<(), failure::Error> {
        if path.exists() {
            let data = std::fs::read_to_string(path)?;
            return self.init_from_str(&data);
        }
        return Ok(());
    }

    #[context(fn)]
    fn init_from_str(&mut self, json_str: &str) -> Result<(), failure::Error> {
        let metas: Vec<Meta> = serde_json::from_str(json_str)?;
        self.metas = metas;
        Ok(())
    }

    #[context(fn)]
    fn into_str(&self) -> Result<String, failure::Error> {
        let res = serde_json::to_string(&self.metas)?;
        Ok(res)
    }

    #[context(fn)]
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
                    .or_insert_with(|| vec![meta]);
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
        #[derive(Debug, Clone, PartialOrd, Ord, Eq, PartialEq)]
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

        let l = s.search(vec!["t3"]);
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

    fn do_assert_fs(root: &Path, mock_fs: Vec<(&str, &str, &str)>) {
        for (kind, path, content) in mock_fs {
            println!("k {} p {} c {}", kind, path, content);
            if kind == "f" {
                let full_path = root.join(path);
                let real_content = std::fs::read_to_string(&full_path).unwrap();
                assert_eq!(real_content, content);
            }
            if kind == "d" {
                let full_path = root.join(path);
                assert_eq!(full_path.is_dir(), true);
            }
        }
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
            .map(|(name, tags, path)| {
                let abs_path = root_path.join(path).canonicalize().unwrap();
                Meta {
                    name: name.to_string(),
                    tag: tags.into_iter().map(|s| s.to_string()).collect(),
                    path: abs_path.to_string_lossy().to_string(),
                }
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
    #[ignore]
    #[test]
    fn test_git_update() {
        println!("test git update");
        let home_dir = dirs::home_dir().unwrap();
        git_update(&home_dir.join(".tpm").join("t"));
        assert_eq!(true, true);
    }

    #[ignore]
    #[test]
    fn test_get_git_path() {
        println!("print all git paht");
        let home_dir = dirs::home_dir().unwrap();
        get_git_path(&home_dir.join(".tpm"));
        assert_eq!(true, true);
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

    #[ignore]
    #[test]
    fn test_git_clone_or_update_master() {
        git_clone_or_update_master(
            "https://github.com/woodgear/t.git",
            Path::new("/home/oaa/.tpm"),
        )
        .unwrap();
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
    #[test]
    fn test_copy_dir() {
        let local_dir_guard = FakeFileSystem::new().temp_dir("test_copy_dir").unwrap();
        let local_path = local_dir_guard.path();
        do_mock_fs(
            local_path,
            vec![
                ("f", "a/.tpm", r#"{"kind":"mutli"}"#),
                ("f", "a/b/.tpm", r#"{"kind":"single","tag":["1"]}"#),
                ("f", "a/c/.tpm", r#"{"kind":"single","tag":["2"]}"#),
            ],
        );
        println!("local path {:?}", local_path);
        copy_dir(&local_path.join("a"), &local_path.join("a1")).unwrap();
        do_assert_fs(
            local_path,
            vec![
                ("f", "a1/.tpm", r#"{"kind":"mutli"}"#),
                ("f", "a1/b/.tpm", r#"{"kind":"single","tag":["1"]}"#),
                ("f", "a1/c/.tpm", r#"{"kind":"single","tag":["2"]}"#),
            ],
        );
    }
    #[ignore]
    #[test]
    fn test_ask_name() {
        use maplit::hashmap;
        println!("{:?}", "test ask name");
        let vals = ask_names(vec!["name".to_owned(), "age".to_owned()]).unwrap();
        assert_eq!(
            vals,
            hashmap! {
                "name".to_owned() => "1  2".to_owned(),
                "age".to_owned() => "12".to_owned(),
            }
        );
    }

    #[test]
    fn test_render_template() {
        let local_dir_guard = FakeFileSystem::new()
            .temp_dir("test_render_template")
            .unwrap();
        let local_path = local_dir_guard.path();
        do_mock_fs(
            local_path,
            vec![
                ("f", "a/.tpm", r#"{"kind":"signle"}"#),
                ("f", "a/b.txt", r#"my name is _t_name_t_ what is you name?"#),
                (
                    "f",
                    "a/c.txt",
                    r#"you name is _t_name_t_ my name is _t_second_name_t_,nice to see you"#,
                ),
            ],
        );
        let names = pick_names(&local_path, "_t_").unwrap();

        assert_eq!(names, vec!["name".to_owned(), "second_name".to_owned()]);
        use maplit::hashmap;
        let values = hashmap! {
            "name".to_string() => "a (with space) b".to_string(),
            "second_name".to_string() => "normal".to_string(),
        };
        _render_template(local_path, "_t_", &values).unwrap();
        do_assert_fs(
            local_path,
            vec![
                ("f", "a/.tpm", r#"{"kind":"signle"}"#),
                (
                    "f",
                    "a/b.txt",
                    r#"my name is a (with space) b what is you name?"#,
                ),
                (
                    "f",
                    "a/c.txt",
                    r#"you name is a (with space) b my name is normal,nice to see you"#,
                ),
            ],
        );
    }
}
