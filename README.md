# Eide (Emacs package)

Eide is a package for [Emacs] that provides IDE features (Integrated
Development Environment).

Although most of these features are already available in Emacs, the purpose of
this package is to integrate them into a user-friendly interface, with
dedicated windows (source files, menu, and ouput), convenient keyboard
shortcuts, and project management.

It is suitable for almost all languages (as long as they are supported by
[Ctags]). [Cscope] provides additional browsing facility for C/C++ files.

* **Version**: 2.2.0+
* **Homepage**: <https://eide.hjuvi.fr.eu.org/>
* **License**: GPLv3 or later
* **Programming language**: Emacs Lisp
* **Dependencies**: [Emacs] (>= 25.1), [Ctags], [Cscope]

[Emacs]: http://www.gnu.org/software/emacs/
[Ctags]: http://ctags.sourceforge.net/
[Cscope]: http://cscope.sourceforge.net/

## License

Copyright © 2008-2021 Cédric Marie <cedric@hjuvi.fr.eu.org>

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

From the source code, you can create the package (.tar file) and install it:

```
$ ./build-package
$ ./install-package
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

## Features

### Windows layout

When you launch Emacs, it should look like this:

```
+-----------------------------------------+---------------+
|                                         |               |
|                                         |               |
|       "source" window                   | "menu" window |
|                                         |               |
|                                         |               |
|                                         |               |
|                                         |               |
|                                         |               |
|                                         |               |
|                                         |               |
|                                         |               |
+-----------------------------------------+---------------+
|                                                         |
|               "output" window                           |
|                                                         |
+---------------------------------------------------------+
```

Eide uses dedicated windows for specific content:

* **Source** window is used to display files. It can be split into several
  windows.
* **Menu** window provides the list of open files (with version control
  modified status) and their symbols.
* **Output** window is used to display all temporary buffers (cscope and grep
  results, compilation output, man pages...).

### Workspaces and projects

A workspace is a collection of projects.

A project is defined by its root directory. When you create a project, tags and
cscope list of files (if any C/C++ file) are built for the whole source tree.
In project configuration, you can define commands for compilation, execution,
and debug. The list of open files is saved on exit and restored when you open
the project again.

F1-F12 keys provide shortcuts for search (tags, cscope, and grep), compilation,
and execution. These shorcuts can be customized or just disabled.

You can create filters to exclude files and directories for all search
functions (tags, cscope, and grep).

### Color themes

Eide provides two color themes (eide-dark and eide-light).

## Usage

### Windows and clicks

Right click behaviour depends on the window at mouse position:

* Over a **source** window, it hides/shows IDE windows ("menu" and "output").
* Over **menu** window, it displays a menu with project oriented commands. Over
  a directory or a filename, it displays a menu with file oriented commands.
* Over **output** window, it displays the list of existing temporary buffers in
  order to switch to one of them. Shift + right click displays the same list
  but for deletion purpose.

### Configuration

#### Options provided in customization

Eide provides some options in customization. To edit them, click on right
button over "menu" window, and select "Customize" in popup menu.
At top level, you will find categories, and one specific option - called
"Override Emacs settings" - that can globally disable all options in "Emacs
settings" category, if disabled.

While other categories provide Eide related options, "Emacs settings"
category provide options that override standard Emacs behaviour:

* F1-F12 key bindings, in order to provide easy access to basic IDE features
  (tags, cscope, grep, compilation...).
* Cscope update policy, in order to add an automatic mode that will update the
  database only when a file is modified in Emacs.

To save your settings, click on "Save for future sessions", and click on right
button to exit customization.

#### Options provided by themes

Eide provides two color themes (eide-dark and eide-light).

It also provides themes to customize some standard Emacs settings. I would
recommend enabling the themes to Emacs beginners, because they enable useful
options for coding and disable a few disturbing options. This is very
subjective of course, but I believe that these settings can be useful for
anyone. Advanced Emacs users may prefer to use their own customization.

To enable these themes, click on right button over "menu" window, and select
"Customize themes" in popup menu.
You can add one of the color themes (`eide-dark` or `eide-light`), and any of
the following themes:

* `eide-browsing`
* `eide-coding`
* `eide-display`
* `eide-settings`

### Workspaces and projects

The directory from which Emacs is launched stands as the root directory. You
can change it during the session: click on right button over "menu" window, and
select "Change root directory" in popup menu.

To create a project, select the directory of the source code tree as the root
directory. Click on right button over "menu" window, and select "Create a
project in this directory". The project is added to your workspace.

To open your project in future sessions, click on right button over "menu"
window, select "Display projects list", and select your project.

### Key bindings

Eide defines a few key bindings:

* `Alt-left/down/right arrows` = cut/copy/paste
* `Ctrl-left/middle/right click` = cut/copy/paste
* `Alt-enter` hides/shows IDE windows ("menu" and "output") (same as
  `right click`)

If you press `Alt-enter` to show the menu window, it forces the cursor focus on
"menu" window. You can select a file with enter, and fold/unfold symbols with
space. Press `Alt-enter` again to hide the "menu" window and force the focus on
source file again.

## Support

To report a bug, or suggest an improvement, please send an email to the address
mentioned at the beginning of this file.
