# Development Environment Setup Repository

Run `setup.sh` to populate user specific dotfiles and emacs config
files.

`setup.sh` makes a symbolic link for each dotfile
(`.path/to/dir-or-file`) listed in `DOTFILES` of `setup.sh`:

```sh
$HOME/.path/to/dir-or-file -> dotfiles/.path/to/dir-or-file
```

Note that a dotfiles can be a directory.

Because all files and directories are symlinked, you can pull or reset
git directory at any time to update or revert your development
environment.
