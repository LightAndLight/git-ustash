use clap::{Parser, Subcommand};
use git2::{build::TreeUpdateBuilder, ErrorCode, FileMode, Repository};
use std::{
    fmt::Display,
    fs,
    path::{Path, PathBuf},
};

/// Save and restore unstaged files in Git.
#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /** Save unstaged files.

    Saves unstaged files only, and leaves the index untouched (`git diff --cached` is not affected).
    After `git-ustash save`, `git diff` will report no modifications (only deletions).

    Fails if there is already a saved set of unstaged files.
    */
    Save,
    /** Restore previously-saved unstaged files.

    Restores previously-saved unstaged files only, leaving the index untouched.

    `git-ustash save && git-ustash restore` is equivalent to a no-op.
    */
    Restore,
}

#[derive(Debug)]
enum Error {
    Git2(git2::Error),
    ActiveStash,
    NoActiveStash,
    GitDirNotFound,
}

impl From<git2::Error> for Error {
    fn from(value: git2::Error) -> Self {
        Error::Git2(value)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Git2(err) => err.fmt(f),
            Error::ActiveStash => f.write_str("there is already an active ustash"),
            Error::NoActiveStash => f.write_str("there is no active ustash"),
            Error::GitDirNotFound => f.write_str("not inside a Git repository"),
        }
    }
}

fn main() {
    let cli = Cli::parse();

    fn search_upward_for_entry<P: AsRef<Path>>(cwd: P, entry: &str) -> Option<PathBuf> {
        let mut target_dir = std::fs::canonicalize(cwd.as_ref()).unwrap();
        let mut found = false;

        loop {
            let target_file_exists = {
                target_dir.push(entry);
                let result = target_dir.try_exists().unwrap();
                target_dir.pop();
                result
            };

            if target_file_exists {
                found = true;
                break;
            }

            if !target_dir.pop() {
                break;
            }
        }

        if found {
            Some(target_dir)
        } else {
            None
        }
    }

    let run = || {
        let repo_path = search_upward_for_entry(".", ".git").ok_or(Error::GitDirNotFound)?;
        let repo = Repository::open(repo_path)?;
        match cli.command {
            Command::Save => ustash_save(&repo),
            Command::Restore => ustash_restore(&repo),
        }
    };

    match run() {
        Err(err) => {
            eprintln!("error: {}", err);
            std::process::exit(1);
        }
        Ok(()) => {
            std::process::exit(0);
        }
    }
}

const USTASH_REF: &str = "refs/ustash";

fn ustash_save(repo: &Repository) -> Result<(), Error> {
    match repo.find_reference(USTASH_REF) {
        Ok(_) => Err(Error::ActiveStash),
        Err(err) => match err.code() {
            ErrorCode::NotFound => Ok(()),
            _ => Err(Error::from(err)),
        },
    }?;

    let index = repo.index()?;

    let diff = repo.diff_index_to_workdir(Some(&index), None)?;

    let mut paths = vec![];
    let mut tree_builder = TreeUpdateBuilder::new();
    diff.foreach(
        &mut |delta, _| {
            if let Some(path) = delta.new_file().path() {
                let path_absolute = repo.path().parent().unwrap().join(path);
                match fs::read(&path_absolute) {
                    Ok(content) => match repo.blob(&content) {
                        Err(err) => {
                            panic!("internal error: {}", err);
                        }
                        Ok(oid) => {
                            tree_builder.upsert(path, oid, FileMode::Blob);
                            paths.push((path_absolute, PathBuf::from(path)));
                            true
                        }
                    },
                    Err(err) => {
                        panic!(
                            "internal error: failed to read {}: {}",
                            path_absolute.display(),
                            err
                        );
                    }
                }
            } else {
                panic!("internal error: delta has no path");
            }
        },
        None,
        None,
        None,
    )?;

    let empty_tree_oid = repo.treebuilder(None)?.write()?;
    let empty_tree = repo.find_tree(empty_tree_oid).unwrap();
    let new_tree_id = tree_builder.create_updated(repo, &empty_tree)?;
    let new_tree = repo.find_tree(new_tree_id)?;

    let signature = repo.signature()?;
    let head_ref = repo.head()?;
    let head_commit = head_ref.peel_to_commit()?;
    let _commit_oid = repo.commit(
        Some("refs/ustash"),
        &signature,
        &signature,
        "ustash",
        &new_tree,
        &[&head_commit],
    )?;

    let head_tree = head_commit.tree()?;
    paths.into_iter().for_each(|(path_absolute, path)| {
        let entry_blob = match index.get_path(&path, 0) {
            Some(index_entry) => repo.find_blob(index_entry.id).unwrap(),
            None => {
                let tree_entry = head_tree.get_name(path.to_str().unwrap()).unwrap();
                let entry_object = tree_entry.to_object(repo).unwrap();
                entry_object.peel_to_blob().unwrap()
            }
        };
        std::fs::write(path_absolute, entry_blob.content()).unwrap();
    });

    Ok(())
}

fn ustash_restore(repo: &Repository) -> Result<(), Error> {
    let mut reference = match repo.find_reference(USTASH_REF) {
        Ok(reference) => Ok(reference),
        Err(err) => match err.code() {
            ErrorCode::NotFound => Err(Error::NoActiveStash),
            _ => Err(Error::from(err)),
        },
    }?;

    fn restore_tree_to_workdir(
        repo: &Repository,
        cwd: &mut PathBuf,
        tree: git2::Tree<'_>,
    ) -> Result<(), Error> {
        for entry in &tree {
            let name = entry.name().unwrap();
            let entry_object = entry.to_object(repo)?;

            cwd.push(name);
            match entry_object.kind().unwrap() {
                git2::ObjectType::Tree => {
                    if !cwd.try_exists().unwrap() {
                        std::fs::create_dir(&*cwd).unwrap();
                    }

                    let entry_tree = entry_object.peel_to_tree()?;
                    restore_tree_to_workdir(repo, cwd, entry_tree)?;
                }
                git2::ObjectType::Blob => {
                    let path = &*cwd;
                    let entry_blob = entry_object.peel_to_blob()?;
                    std::fs::write(path, entry_blob.content()).unwrap();
                }
                kind => panic!("internal error: unexpected Git object kind: {}", kind),
            }
            cwd.pop();
        }

        Ok(())
    }

    let commit = reference.peel_to_commit()?;
    let tree = commit.tree()?;
    let mut cwd = PathBuf::from(repo.path().parent().unwrap());
    restore_tree_to_workdir(repo, &mut cwd, tree)?;

    reference.delete()?;

    Ok(())
}
