Development Environment Setup Repository
========================================

Run `setup.sh` to populate user specific dotfiles and configure the
development environment, including emacs.

Setup Script
------------

1. Makes a symbolic link: `~/<filename> ->
   ~/.setup/setupfiles/<filename>` for each `<filename>` listed in
   `TARGETLIST`.

2. Initializes and updates git submodules.

3. Performs OS dependent configurations.

   Mac OS X:
       * Installs and/or updates and upgrades Homebrew
       * Install node.js, npm

4. Checks for a supported version of Emacs before performing byte
   compilation of elisp code. Although byte compiliation of elisp code
   should technically be independent of Emacs version, this has not
   been the case in testing.

   Mac OS X: `/Applications/Emacs.app/Contents/MacOS/Emacs-x86_64-10_9`

   Cygwin: `/usr/bin/emacs-w32.exe`

   Linux: Any

4. Byte compiles each elisp package listed in
   `BYTE_COMPILE_ELISP_PACKAGE`.

-----
Notes
-----

Because all installed files and directories are symlinked to
`~/.setup/setupfiles` directory, you can pull or reset git directory
at any time to update or revert your development environment.

-----
TO-DO
-----

1. Make `setup.sh` more flexible in how it handles initial install
   versus re-installs and updates. Specifically:

          * When do you to byte-compile again?
          * When do you update submodules?
          * When do you need to make backups?

   etc...

2. Extend OS specific configurations:

          * Anaconda
          * Python environment
          * Javascript environment

   etc...

3. Is there a way to integrate bzr CEDET package as a submodules?


--------------------------------
Elisp Packages
--------------------------------

List of installed elisp packages:

* CEDET-suite
* Emacs Code Browser




