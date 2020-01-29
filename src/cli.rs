pub enum CliConfig {
    Add(String),
    Search(Vec<String>),
    New(String),
}

impl CliConfig {
    fn parse() -> Self {
        unimplemented!()
    }
}
