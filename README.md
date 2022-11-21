# Emacs-IDE (eide)

Emacs-IDE (eide) is a package for [Emacs] that provides IDE features
(Integrated Development Environment).

Although most of these features are already available in Emacs, the purpose of
this package is to integrate them into a user-friendly interface, with
dedicated windows (source files, menu, and ouput), convenient keyboard
shortcuts, and project management.

It is suitable for almost all languages (as long as they are supported by
[Ctags]). [Cscope] provides additional browsing facility for C/C++ files.

* **Version**: 2.3.0
* **Homepage**: <https://software.hjuvi.fr.eu.org/eide/>
* **License**: [GPL-3.0-or-later]
* **Programming language**: [Emacs Lisp]
* **Dependencies**: [Emacs] (>= 25.1), [Ctags], [Cscope]

[Emacs]: https://www.gnu.org/software/emacs/
[Ctags]: http://ctags.sourceforge.net/
[Cscope]: http://cscope.sourceforge.net/
[GPL-3.0-or-later]: https://www.gnu.org/licenses/gpl-3.0.html
[Emacs Lisp]: https://www.gnu.org/software/emacs/manual/elisp.html

## License

Copyright © 2008-2022 Cédric Marie <cedric@hjuvi.fr.eu.org>

This program is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.

## Installation

### From the source code

From the source code, you can create the package (.tar file) and install it:

```
$ ./user-install.sh
```

The package is installed in `~/.emacs.d/elpa/eide-<version>` directory.

Then you must add the following lines in your `~/.emacs`:

```
(package-initialize)
(eide-start)
```

NB:

* `(package-initialize)` might already be present and should not be added in
  that case.
* If you have installed several versions, the package with the higher version
  number will be loaded.
* If you're installing a development version, the package version number is not
  relevant.

If you want to uninstall the package, just execute this other script:

```
$ ./user-uninstall.sh
```

And remove `(eide-start)` from your `~/.emacs`.

### Using Melpa

The package is also available in [Melpa]:

[![MELPA](https://melpa.org/packages/eide-badge.svg)](https://melpa.org/#/eide)
[![MELPA Stable](https://stable.melpa.org/packages/eide-badge.svg)](https://stable.melpa.org/#/eide)

[Melpa]: https://melpa.org

## Features

### Windows layout

When you launch Emacs, it should look like this:

```
┌─────────────────────────────────────────┬───────────────┐
│                                         │               │
│                                         │               │
│       "source" window                   │ "menu" window │
│                                         │               │
│                                         │               │
│                                         │               │
│                                         │               │
│                                         │               │
│                                         │               │
│                                         │               │
│                                         │               │
├─────────────────────────────────────────┴───────────────┤
│                                                         │
│               "output" window                           │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

Emacs-IDE uses dedicated windows for specific content:

* **Source** window is used to display files. It can be split into several
  windows.
* **Menu** window provides the list of open files (with version control
  modified status) and their symbols.
* **Output** window is used to display all temporary buffers (cscope and grep
  results, compilation output, man pages...).

The results of multiple searches are all saved in different temporary buffers,
so that you don't have to execute them again.

### Workspaces and projects

A workspace is a collection of projects. You can use different workspaces if
you don't want to mix some projects in the same list.

A project is defined by its root directory. When you create a project, tags and
cscope list of files (if any C/C++ file) are built for the whole source tree,
unless you decide to create a project without symbols.
In project configuration, you can define commands for compilation, execution,
and debug. The list of open files is saved on exit and restored when you open
the project again (using emacs-desktop).

`F1`-`F12` keys provide shortcuts for search (tags, cscope, and grep),
compilation, and execution. These shorcuts can be customized or just disabled.

You can create filters to exclude files and directories for all search
functions (tags, cscope, and grep).

### Color themes

Emacs-IDE provides two color themes (`eide-dark` and `eide-light`).

## Usage

### Windows and clicks

* `Right click` behaviour depends on the window at mouse position:
  - Over a **source** window, it hides/shows IDE windows ("menu" and "output").
  - Over **menu** window, it displays a menu with project oriented commands. Over
    a directory or a filename, it displays a menu with file oriented commands.
  - Over **output** window, it displays the list of existing temporary buffers in
    order to switch to one of them. `Shift` + `Right click` displays the same list
    but for deletion purpose.

  If some text is selected, it displays a search menu (tags, cscope, grep, and
  man page).

  If some text is selected across several lines, it displays a menu with
  cleaning commands (untabify and indent).

* `Left/right click` on a window mode-line (either "source" or "output") switches
  to previous/next buffer.

* `Middle click` behaviour depends on mouse position:
  - Over **menu** window, it displays a file browser (Dired mode) in the
    directory of the current source file.
  - Over other windows, it pastes the clipboard (standard behaviour).

* `Shift-Mouse wheel up/down` scrolls right/left.

### Configuration

#### Options provided in customization

##### Emacs-IDE group

Emacs-IDE provides some options in customization. To edit them, click on right
button over "menu" window, and select "Customize" in popup menu.

At top level, you will find categories, and one specific option - called
"Override Emacs settings" - that is enabled by default. If you disable it, all
options under "Emacs settings" category will be globally disabled. If you keep
it enabled, you can still disable any specific option under "Emacs settings".

While other categories provide Emacs-IDE related options, "Emacs settings"
category provides options that override standard Emacs behaviour.

##### "Emacs settings" options

Some standard Emacs settings are overridden because it really makes sense to
change these settings when Emacs-IDE is used. Most of them are features for
coding, and are part of what Emacs-IDE aims at providing. Some of them could
just be considered as personal preferences, yet I believe they help for a
better integration of Emacs-IDE, and get rid of some disturbing options that
for some reason were chosen as the default behaviour of Emacs.

The modified standard variables are explicitly mentioned in the customization.
Besides basic variables, overridden settings are:

* `F1`-`F12` key bindings, in order to provide easy access to basic IDE
  features (tags, cscope, grep, compilation...).
* Cscope update policy (`cscope-option-do-not-update-database` function), in
  order to add an automatic mode that will update the database only when a file
  is modified in Emacs.

To save your settings, click on "Save for future sessions", and click on right
button to exit customization.

A few standard settings are also overridden without any option to avoid it,
because keeping the default behaviour would be very annoying when using
Emacs-IDE:

* `inhibit-startup-screen t`
* `tags-revert-without-query t`
* `tags-case-fold-search nil`
* `gdb-many-windows t`
* `revert-without-query (quote (".*"))`

#### Color themes

Emacs-IDE provides two color themes (`eide-dark` and `eide-light`).

To select one of these themes, or any other available theme, click on right
button over "menu" window, and select "Customize themes" in popup menu.

Then you can select the theme you want.

To save your choice, click on "Save theme settings", and click on right button
to exit theme customization.

### Workspaces and projects

A workspace is a collection of projects.
To switch to another workspace, click on right button over "menu" window,
and select "Switch to workspace x".

The directory from which Emacs is launched stands as the root directory. You
can change it during the session: click on right button over "menu" window, and
select "Change root directory" in popup menu.

#### Create a project

To create a project, either launch Emacs from the root directory of the source
code tree, or just launch Emacs and then open project popup menu to change the
root directory.

Then open project popup menu and select:

* "Create a project in this directory" if you want to create a full project
  with tags and cscope databases for code browsing.
* "Create a project without tags/cscope symbols in this directory" if you
  don't want or need symbols for your project.

The project is automatically added to the project list of the current
workspace.
To open your project in future sessions, click on right button over "menu"
window, select "Display projects list", and select your project.

In your project root directory, several files are created:

* `.emacs.desktop`: List of open files.
* `.emacs-ide-project.cfg`: It defines parameters for this project.

If you have created a project with symbols:

* `TAGS`: Tags database.
* `cscope.files`: Cscope list of files (C/C++ files).
* `cscope.out`: Cscope database.

#### Project configuration

To edit the project configuration (`.emacs-ide-project.cfg`), open the project
popup menu and select "Project configuration".
It is created with the default values from the configuration (see 'Projects'
category in customization).

If you delete this file, it will be created again with the default values.
If you delete any parameter in this file, it will be restored with the default
value.

#### Open an existing project

To open an existing project, you can:

* either launch Emacs from your project root directory, with --eide-op option
  (op = open project)
* or launch Emacs, open project popup menu, select 'Change root directory' if
  necessary, then select 'Load the project present in this directory'.
* or launch Emacs, open project popup menu, and select 'Display projects list'
  in order to choose the project.
  NB: If your project is not in the list, use one the the previous methods. The
  project will be automatically added to the project list of the current
  workspace.

#### Tags and cscope update

To update tags or cscope database, open project popup menu and select
the appropriate update action.

When the code is changed:

* Tags database (`TAGS`) needs to be updated.
* Cscope database (`cscope.out`) needs to be updated if 'Emacs settings >
  Search > Update of cscope database' option value is either 'Never (only on
  user request)' or 'When a buffer has been edited or refreshed' (in case a
  file has been modified outside Emacs).

When a file is added or deleted:

* Tags database (`TAGS`) needs to be updated.
* Cscope list of files (`cscope.files`) needs to be updated (`cscope.out` will
  be updated automatically on next search).

### Actions on files (right click on file name)

#### Editing with REF files

When editing a file, you can create a copy, so as to easily switch between the
original and the modified file, and compare them.

The original version of `file` is saved as `file.ref`.
When switching to the original file, `file` becomes `file.new`, and `file.ref`
becomes `file`.

File popup menu actions:

* Backup original file (REF) to work on a copy (NEW): Create a copy of `file`
  (`file.ref`), and set read/write permission on `file`.
* Switch to REF file: Switch to the original version (`file.ref`).
* Discard REF file: Discard the original file, and use the modified file.
* Restore REF file: Discard the modified file, and restore the original file.
* Switch to NEW file: Switch to the modified version (`file.new`).
* Discard NEW file: Discard the modified file, and use the original file.
* Compare REF and NEW files: Compare the original and the modified files.

File name colour:

* green when the modified file is used.
* red when the original file is used.

#### Other actions on files

File popup menu actions:

* Set read/write: Set read/write permission on the file.
* Set read only: Set read only permission on the file.
* Untabify and indent: "Clean" the file (turn tabs into spaces and indent).
* Delete trailing spaces
If "Show version control status" option is set:
* svn diff / git diff
* svn revert / git checkout

File name colour:

* black when the file is read/write.
* grey when the file is read only.
* blue when the file is modified (version control).

### Actions on directories (right click on directory name)

Actions on files (see above) can be applied to several files - open files
located in the same directory - at once.
An action is enabled in popup menu if it is allowed for at least one open file,
and will be applied to all files for which it is allowed.

### Key bindings

#### Standard key bindings

* `Control-x Control-f`: Load a file or open a directory (file browsing)
* `Control-x Control-s`: Save current file
* `Control-s`: Search
* `Alt-%`: Replace
* `Control-_`: Undo
* `Control-g`: Cancel the current command (if Emacs is frozen)

#### New key bindings

Emacs-IDE defines a few key bindings:

* `Alt-←/↓/→` or `Control-left/middle/right click`: Cut/copy/paste
* `Alt-Enter` or `right click` in a "source" window: Hide/show IDE windows
  ("menu" and "output")
* `Control-Alt-Enter`: Enter/exit the project list

If you press `Alt-Enter` to show the "menu" window, it forces the cursor focus
on the "menu" window. You can select a file with `Enter`, and fold/unfold
symbols with `Space`. Press `Alt-Enter` again to hide the "menu" window and
force the focus on the source file again.

#### New key bindings for F1-F12

`F1`-`F12` key bindings can be customized. The default bindings are:

* `F1`: Back from symbol definition
* `F2`: Go to symbol definition (at cursor position, or selected text if any)
* `Shift-F2`: Go to symbol definition (prompt for symbol)
* `F3`: Search symbol in whole project at cursor position
* `Shift-F3`: Search symbol in whole project (prompt for symbol)
* `F4`: Search string in whole project at cursor position
* `Shift-F4`: Search string in whole project (prompt for string)
* `F6`: Search string in current directory at cursor position
* `Shift-F6`: Search string in current directory (prompt for string)
* `F5`: Reload all buffers (and update display)
* `Shift-F5`: Close current buffer
* `F7`: Go to previous instance (cscope/grep) / error (compilation output)
* `F8`: Go to next instance (cscope/grep) / error (compilation output)
* `F9`: Compile (1)
* `Shift-F9`: Compile (2)
* `F10`: Run (1)
* `Shift-F10`: Run (2)
* `F11`: Toggle the fullscreen mode
* `F12`: Open a shell

NB: Additional compile commands (Compile (3) and Compile (4)) - as well as
debug commands - are not available from the keyboard, but only from project
popup menu.

#### New key bindings in "menu" window

* `Enter`: Open file / Go to symbol
* `Space`: Fold/unfold symbols

#### New key bindings in the project list

* `Enter`: Open project
* `Space`: Select/unselect project for comparison
* `Backspace`: Remove project from workspace

#### Standard and new key bindings in a diff session

Let's consider that the diff session displays `file A` on the left and `file B`
on the right.

* `a` (and also `F1`): Copy highlighted region `file A` --> `file B`
* `b` (and also `F2`): Copy highlighted region `file A` <-- `file B`
* `w a`: Save `file A`
* `w b`: Save `file B`
* `!` (and also `F5`): Update diffs
* `Backspace` (and also `F7`): Go to previous diff
* `Space` (and also `F8`): Go to next diff
* `?`: Display help
* `q` (and also `right click`): Quit (y/yes to confirm)

## Support

To report a bug, or suggest an improvement, please send an email to the address
mentioned at the beginning of this file.
