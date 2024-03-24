# `git-ustash`

Save and restore unstaged changes in Git.

`git-ustash save` saves all unstaged changes, and *only* unstaged changes.
It's an error to call `git-ustash save` while there are previously-saved unstaged changes (i.e. the "max stash size" is 1).

`git-ustash restore` restores the previously-saved unstaged changes.

## Installation

Try using Nix: `nix run github:LightAndLight/git-ustash -- --help`

Add to a Nix Flake:

```nix
{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    git-ustash.url = "github:LightAndLight/git-ustash";
  };
  outputs = { self, nixpkgs, flake-utils, git-ustash }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            git-ustash.packages.${system}.default
          ];
        };
      }
    );
}
```

## Background

This is something I've wanted when writing [pre-commit hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks) that do auto-formatting.
The goal of a pre-commit auto-formatter is to format staged files without affecting any other files.
`git diff --cached --name-only` gives the list of staged files, but if you *just* format them without any other preparation then you can cause issues with unstaged files.
For example, if you use `git add -p` to stage a file using a partial patch, then you will have the rest of the file unstaged.
How to format just the staged copy of the file?
The unstaged version (with the remainder of the changes) is the only one on disk.
Using `git stash --keep-index` almost does the right thing; it gets the unstaged files out of the way so it looks like you are free to work on jus the staged files.
Unfortunately it also stashes the staged files, so that when you auto-format the staged files, re-add them to the index, and pop from the stash, the formatted files are overwritten by the stash.
[This answer on StackOverflow](https://stackoverflow.com/a/71222518/2884502) looked promising, but `git stash push --staged` fails with an error when I use it in the `git add -p` aforementioned situation.
