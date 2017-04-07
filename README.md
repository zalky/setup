# Development Environment Setup Repository

Run `setup.sh` to populate user specific dotfiles and emacs config
files. `setup.sh` makes a symbolic link for each dotfile
(file or directory) listed in `DOTFILES` of `setup.sh`:

```
$HOME/.path/to/dir-or-file -> dotfiles/.path/to/dir-or-file
```

Because all files and directories are symlinked to this version
controlled repo, you can can use git at any time to update, save or
revert changes to your development environment.
