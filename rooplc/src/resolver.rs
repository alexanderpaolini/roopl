use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use crate::{
    ast::{ImportStmt, Item, ParseError, Program},
    lex,
    parse::{self},
};

type FileId = PathBuf;

#[derive(Debug)]
pub enum ResolveError {
    UnresolvedEntryPoint(String),
    UnresolvedImport(PathBuf),
    ImportCycle(PathBuf),
    ParseError(Vec<ParseError>),
}

#[derive(Debug)]
struct Module {
    program: Program,
}

pub struct ModuleResolver {
    entry: FileId,
    root: PathBuf,

    parsed: HashMap<FileId, Module>,
    parsing: HashSet<FileId>,

    errors: Vec<ResolveError>,
}

impl ModuleResolver {
    pub fn new(entry: String) -> Result<Self, ResolveError> {
        let entry =
            fs::canonicalize(&entry).map_err(|_| ResolveError::UnresolvedEntryPoint(entry))?;

        let root = entry
            .parent()
            .unwrap_or_else(|| Path::new("/"))
            .to_path_buf();

        Ok(Self {
            entry,
            root,
            parsed: HashMap::new(),
            parsing: HashSet::new(),
            errors: Vec::new(),
        })
    }

    pub fn resolve(mut self) -> Result<Program, Vec<ResolveError>> {
        self.resolve_module(self.entry.clone());

        if self.errors.is_empty() {
            Ok(self.merge_programs())
        } else {
            Err(self.errors)
        }
    }

    fn resolve_module(&mut self, id: FileId) {
        if self.parsed.contains_key(&id) {
            return;
        }

        if self.parsing.contains(&id) {
            self.errors.push(ResolveError::ImportCycle(id));
            return;
        }

        self.parsing.insert(id.clone());

        let program = match self.parse_file(&id) {
            Ok(program) => program,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let imports = self.collect_imports(&program);

        for import in imports {
            for target in self.expand_import(&import) {
                self.resolve_module(target);
            }
        }

        self.parsing.remove(&id);
        self.parsed.insert(id.clone(), Module { program });
    }

    fn parse_file(&self, id: &FileId) -> Result<Program, ResolveError> {
        let src = fs::read_to_string(id).map_err(|_| ResolveError::UnresolvedImport(id.clone()))?;
        let toks = lex::lex(src);
        let mut parser = parse::Parser::new(toks);
        let res = parser.parse();

        if let Err(errors) = res {
            return Err(ResolveError::ParseError(errors));
        }

        Ok(res.unwrap())
    }

    fn expand_import(&mut self, import: &ImportStmt) -> Vec<FileId> {
        let is_wildcard = import.path.last().map(|s| s == "*").unwrap_or(false);

        let mut base = self.root.clone();

        let parts = if is_wildcard {
            &import.path[..import.path.len() - 1]
        } else {
            &import.path[..]
        };

        for part in parts {
            base.push(part);
        }

        if is_wildcard {
            fs::read_dir(&base)
                .ok()
                .into_iter()
                .flatten()
                .filter_map(|e| e.ok())
                .map(|e| e.path())
                .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("rp"))
                .filter_map(|p| match fs::canonicalize(&p) {
                    Ok(path) => Some(path),
                    Err(_) => {
                        self.errors.push(ResolveError::UnresolvedImport(p));
                        None
                    }
                })
                .collect()
        } else {
            base.set_extension("rp");
            match fs::canonicalize(&base) {
                Ok(path) => vec![path],
                Err(_) => {
                    self.errors.push(ResolveError::UnresolvedImport(base));
                    Vec::new()
                }
            }
        }
    }

    fn merge_programs(&self) -> Program {
        let mut items = Vec::new();
        for module in self.parsed.values() {
            items.extend(
                module
                    .program
                    .items
                    .clone()
                    .into_iter()
                    .filter(|item| !matches!(item, Item::Import(_))),
            );
        }
        Program { items }
    }

    fn collect_imports(&self, program: &Program) -> Vec<ImportStmt> {
        program
            .items
            .iter()
            .filter_map(|item| match item {
                Item::Import(i) => Some(i.clone()),
                _ => None,
            })
            .collect()
    }
}
