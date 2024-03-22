use clap::{Parser, Subcommand};
use git2::{build::TreeUpdateBuilder, Error, ErrorCode, FileMode, Repository, Signature};
use std::{fs, path::PathBuf};

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Save,
    Restore,
}

fn main() -> Result<(), Error> {
    let cli = Cli::parse();
    match cli.command {
        Command::Save => wstash_save(),
        Command::Restore => wstash_restore(),
    }
}

fn wstash_save() -> Result<(), Error> {
    let repo = Repository::open(".")?;

    match repo.find_reference("refs/wstash") {
        Err(err) => match err.code() {
            ErrorCode::NotFound => Ok(()),
            _ => Err(err),
        },
        Ok(_) => {
            println!("refs/wstash already exists");
            std::process::exit(1)
        }
    }?;

    let index = repo.index()?;

    let empty_tree_oid = repo.treebuilder(None)?.write()?;
    let empty_tree = repo.find_tree(empty_tree_oid).unwrap();

    let mut tree_builder = TreeUpdateBuilder::new();

    let diff = repo.diff_index_to_workdir(Some(&index), None)?;
    let mut paths = vec![];
    diff.foreach(
        &mut |delta, _| {
            if let Some(path) = delta.new_file().path() {
                match fs::read(path) {
                    Ok(content) => match repo.blob(&content) {
                        Err(err) => {
                            eprintln!("error: {}", err);
                            false
                        }
                        Ok(oid) => {
                            tree_builder.upsert(path, oid, FileMode::Blob);
                            paths.push(PathBuf::from(path));
                            true
                        }
                    },
                    Err(err) => {
                        eprintln!("error: {}", err);
                        false
                    }
                }
            } else {
                eprintln!("internal error: delta has no path");
                false
            }
        },
        None,
        None,
        None,
    )?;

    let new_tree_id = tree_builder.create_updated(&repo, &empty_tree)?;
    let new_tree = repo.find_tree(new_tree_id)?;

    let signature = Signature::now("wstash", "omitted").unwrap();
    let head_ref = repo.head()?;
    let head_commit = head_ref.peel_to_commit()?;
    let _commit_oid = repo.commit(
        Some("refs/wstash"),
        &signature,
        &signature,
        "wstash",
        &new_tree,
        &[&head_commit],
    )?;

    let head_tree = head_commit.tree()?;
    paths
        .into_iter()
        .for_each(|path| match index.get_path(&path, 0) {
            None => {
                let tree_entry = head_tree.get_name(path.to_str().unwrap()).unwrap();
                let entry_object = tree_entry.to_object(&repo).unwrap();
                let entry_blob = entry_object.peel_to_blob().unwrap();
                std::fs::write(path, entry_blob.content()).unwrap();
            }
            Some(index_entry) => {
                let entry_blob = repo.find_blob(index_entry.id).unwrap();
                std::fs::write(path, entry_blob.content()).unwrap();
            }
        });

    Ok(())
}

fn wstash_restore() -> Result<(), Error> {
    let repo = Repository::open(".")?;

    let mut reference = repo.find_reference("refs/wstash")?;
    let commit = reference.peel_to_commit()?;

    let tree = commit.tree()?;
    for entry in &tree {
        let name = entry.name().unwrap();
        let entry_object = entry.to_object(&repo)?;
        let entry_blob = entry_object.peel_to_blob()?;
        std::fs::write(name, entry_blob.content()).unwrap();
    }

    reference.delete().unwrap();

    Ok(())
}
