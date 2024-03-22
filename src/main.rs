use git2::{build::TreeUpdateBuilder, Error, FileMode, Repository};
use std::fs;

fn main() -> Result<(), Error> {
    let repo = Repository::open(".")?;
    let index = repo.index()?;

    let diff = repo.diff_index_to_workdir(Some(&index), None)?;

    let empty_tree_oid = repo.treebuilder(None)?.write()?;
    let empty_tree = repo.find_tree(empty_tree_oid).unwrap();

    let mut tree_builder = TreeUpdateBuilder::new();
    diff.foreach(
        &mut |delta, _| {
            if let Some(path) = delta.new_file().path() {
                println!("Processing unstaged file: {:?}", path);
                match fs::read(path) {
                    Ok(content) => {
                        let oid = repo.blob(&content).unwrap();
                        tree_builder.upsert(path, oid, FileMode::Blob);
                    }
                    Err(e) => println!("Error reading file {:?}: {}", path, e),
                }
            }
            true
        },
        None,
        None,
        None,
    )?;

    let new_tree_id = tree_builder.create_updated(&repo, &empty_tree)?;
    let new_tree = repo.find_tree(new_tree_id)?;

    println!(
        "Created a tree object for unstaged changes with OID: {:?}",
        new_tree
    );

    Ok(())
}
