# Changelog for Emacs-IDE (eide)

## Unreleased

### Changed

* When loading a project (either with --eide-op option, or with "Load the
  project present in this directory" in the menu), automatically switch to its
  workspace, if it is already present in one of them.

### Removed

* Remove actions to set files read/write or read only, because it is not really
  useful anymore (any source code is now always read/write).

### Fixed

* When switching to another workspace, close the open files only if a project
  is loaded, because this project (which is already part of the previous
  workspace) is not supposed to be part of the new workspace. Otherwise, keep
  them open, so that the user can create a new project with these files, in the
  workspace he just chose.

## Version 2.3.0 (2022-01-19)

### Added

* Browse through cscope results with F7/F8.

### Changed

* Move overridden Emacs settings back to the customization. In version 2.0.0,
  these settings were moved to themes, but it turned out to be a bad idea.
  They are now enabled by default, and can be disabled in the customization.
  Only the color themes are kept (eide-dark and eide-light).
  The reasons for this change are:
  - At first installation, it is not really convenient to manually add themes.
  - When themes are disabled, their settings are kept and saved explicitly in
    ~/.emacs.
  - Themes are rarely used for such settings, the main usage is for colors.
* Use customize-themes to choose a color theme by simply selecting a theme in a
  list with checkboxes, without having to enter the name manually. The manual
  method was used because customize-themes failed at selecting multiple themes,
  but this is not required anymore, since themes are used only for colors now.
* Change the default behaviour to indent with spaces (indent-tabs-mode nil),
  with a new option in Emacs settings group. Most languages now recommend to
  use spaces instead of tabs, so this should be the default. Tabs mode is
  preserved for some major modes that require it, such as GNUmakefile.

### Removed

* Remove "project notes" file (.emacs-ide-project.txt) because it is not really
  useful.

## Version 2.2.0 (2020-05-08)

### Changed

* As of Emacs version 25.1, etags API is deprecated and should be replaced with
  xref API.
  - Use xref instead of etags.
  - Require Emacs version >= 25.1 in "install-package" script.
  - Discard "find alternate tag" function (which was mapped on "Shift-F1"),
    because xref displays all definitions to choose from, when several are
    available.
* Remove the customization of the mode-line background color, in eide-dark
  theme, because it made the text for the selected window difficult to read.

### Fixed

* The windows layout is broken when the "menu" or "output" window is split (C-x
  2 or 3) or set full frame (C-x 1) (These commands should be disabled for
  these windows).
* Alt-Enter key binding (to show/hide IDE windows) is ignored in terminal mode.

## Version 2.1.3 (2018-06-26)

### Changed

* In the project configuration, use a specific "source file path replacement"
  option for each compile command.

### Fixed

* When selecting a project with an empty desktop, the "source" window displays
  the menu.
* When loading a project at startup (with --eide-op option), if a global
  desktop file is present in ~ or ~/.emacs.d, it is loaded instead of the one
  of the project.
* The buffer names become inconsistent when the buffers are renamed dynamically
  with a "\<directory\>" suffix.

## Version 2.1.2 (2017-04-20)

### Added

* Add C style option in the project configuration, in order to work on projects
  with different coding rules.

### Changed

* Replace eide-coding theme with new C styles:
  - bsd-4-spaces: bsd style + no tabulations + offset 4.
  - linux-tabs: linux style + tabulations.
* Move hard-coded compilation-scroll-output setting into eide-settings theme,
  and change the value from "first-error" (i.e. stop scrolling on first
  warning/error, which is not always relevant) to "t" (always scroll).

### Fixed

* The windows layout is broken when exiting dired mode.
* C-x C-f starts in the home directory instead of the root directory, when the
  "source" window displays a temporary buffer.
* The buffer list is broken when buffers are renamed (same file names in
  different directories).

## Version 2.1.1 (2015-12-12)

### Added

* Add an option to disable beeping.

### Changed

* When using the project option "compile_error_old_path_regexp", replace the
  compilation path on the fly.

### Fixed

* Mouse-3 key binding (hide/show IDE windows) is overwritten by cscope.
* In the project list, a project can be unexpectedly selected if the user
  clicks in the empty space on the right.
* Undefined window objects are sometimes accessed, or IDE windows are displayed
  twice, especially after having used completions to load a file.

## Version 2.1.0 (2015-02-19)

### Added

* Display an explicit warning message when xcscope.el is missing.
* Add the possibility to create a project without tags/cscope symbols.

### Changed

* At startup, don't open the project that is present in the root directory,
  unless explicitly requested (with the new option --eide-op).

### Removed

* Remove DOS/Unix conversions.

### Performance

* Improve startup and F5 refresh time (Reduce the time that is spent to check
  the version control status).

### Fixed

* Don't override save-buffer (it breaks some other packages) but C-x C-s
  instead.
* Some languages (Perl, Python, ...) are not supported in the menu, because
  imenu parsing is not used correctly.
* The windows layout is broken with Emacs 24.4 (disable desktop-restore-frames
  feature).

## Version 2.0.0 (2014-05-15)

The version number changed to 2.0.0 because:
* It is now distributed as a package.
* It requires Emacs 24.
* Emacs settings have been moved into themes.

### Added

* Add an option (in the project configuration) to modify the path of clickable
  filenames (warnings or errors) in the compilation buffer.
* Add an option to support ANSI escape code in the compilation buffer.

### Changed

* Stop compilation scrolling at the first error.
* Ask user confirmation with y-or-n-p instead of a popup (for mouse or keyboard
  input).
* Don't set the default face height in the color themes.
* Move "Emacs settings" options into themes (not enabled anymore by default).

### Fixed

* Compatibility with Dima Kogan's fork of xcscope.el.
* Missing optional argument (force-same-window) in switch-to-buffer advice
  (Emacs 24).
* F11 key binding is forced to fullscreen mode, whatever its customization.

## Version 1.12 (2014-01-06)

### Added

* Add the possibility to customize F1-F12 keys.
* Add filters in the project configuration to exclude files and directories
  when searching with tags, cscope, or grep.
  Add new commands to temporarily disable these filters.
  Tags and cscope list of files are automatically updated when the filters are
  modified (or disabled/enabled).
* Show the keyboard shortcuts in the global popup menu.

### Changed

* Add an option for case sensitivity of tag search (default is yes, used to be
  disabled).
* Don't try to use selected text when searching with tags (F2), cscope (F3), or
  grep (F4 and F6).
* Cscope dependency is not mandatory anymore at installation time.

### Fixed

* Dired mode breaks the windows layout.
* REF/NEW comparison breaks the "source" windows layout.
* The buffer list is displayed in a "source" window when Emacs is launched with
  several files in arguments.
* Completion breaks the windows layout.
* Multiple updates of the same tags and cscope list of files can be launched in
  parallel.

## Version 1.11 (2013-09-03)

### Added

* Add keyboard shortcuts:
  - Alt-Enter to show/hide the "menu" and "output" windows (same as right
    click).
  - Ctrl-Alt-Enter to enter/exit the project list.
  - In the menu: Enter to select a file or a function and Space to fold/unfold
    the symbols.
  - In the project list: Enter to select a project, Space to select/unselect a
    project for comparison, and Backspace to remove a project from the
    workspace.
* Keep the "source" windows layout when showing/hiding the "menu" and "output"
  windows (requires Emacs 24).

### Fixed

* F5 does not update buffers named file\<n\>.
* The menu becomes read/write when a project is loaded.
* The file status is not updated when saving it if the cursor position in the
  menu is after the file.
* It is impossible to exit the project list when a project is already loaded
  and cscope is not available.

## Version 1.10 (2013-02-07)

### Added

* Add the possibility to change the project name (in the project
  configuration).

### Changed

* Execute external diff commands in subprocesses (in order not to freeze
  Emacs).

### Fixed

* Project creation fails when a desktop has already been loaded.
* Comparison with another project should not be in the action list if the file
  is out of project.
* The default directory is not updated when changing the root directory.

## Version 1.9 (2013-01-17)

### Added

* Add a key binding for fullscreen mode (F11).
* Add uninstall script.
* Add the possibility to switch to another root directory.
* Create project list and add the possibility to switch to another project.
* Create workspaces to manage different project lists.
* Add the possibility to select another project for comparison in the project
  list.
* Add an option to use a specific background color in the menu (default is yes,
  used to be forced).

### Changed

* Enable F4 (global grep) even if there is no current project.
* Add an option to insert a blank line between directories in the menu (default
  is no, used to be forced).
* Add an option to start with maximized frame (default is yes).

### Fixed

* Some C/C++ symbols are not present in the menu.
* The frame size and position are changed at startup.
* The Git status is not shown at startup (F5 is necessary to update the
  status).
* DOS/Unix conversions fail (dos2unix/unix2dos must be replaced with
  fromdos/todos).

Warning: The projects that you have created with previous versions will not
automatically appear in your workspace. You have to open them first (either run
Emacs from the project root directory, or use "Change root directory" command
in the menu).

## Version 1.8 (2012-05-25)

### Added

* Add "Close all files" command.
* Add svn/git blame commands (vc-annotate).

### Changed

* Don't build the windows layout at startup in terminal mode (emacs -nw).
* Use vc-diff for svn/git diff commands.
* Reload all open files with F5 (not only the current file).

### Fixed

* The desktop is not loaded when Emacs-IDE is loaded in a file after init
  (emacs -l).
* Compatibility with Emacs 24.

## Version 1.7 (2011-12-15)

### Added

* Add Git support (status, diff, checkout).
* Add local installation.
* Add options:
  - Show menu bar.
  - Show tool bar.
  - Scroll bar position.
  - Cscope database update (always, never, or auto).
  - Indentation offset.
  - Indentation mode (spaces or tabs).
  - Default tab width.

### Changed

* Change some colors in the dark theme.
* Use Emacs customization (instead of ~/.emacs-ide.cfg) for configuration.

### Fixed

* Unwanted scrolling when clicking in the margin (because of scroll-margin).
* The compilation output doesn't scroll.

Warning: The configuration is not migrated from previous versions. If you have
modified some options, you will have to do it again with the new configuration
system (customization). The old configuration file (~/.emacs-ide.cfg) is not
deleted: you can check the values.

## Version 1.6 (2011-04-04)

### Added

* Add support for imenu folders in the menu.
* Add search for man pages.

### Changed

* Keep the default font (only change the size).
* Distinguish Emacs options from Emacs-IDE options.
* Create tags and cscope list of files in subprocesses.

## Version 1.5 (2010-11-25)

### Added

* Add support for several "source" windows (split).
* Provide Emacs-IDE as a package to install.
* Add an option to keep user's colors.

### Removed

* Remove "Emacs-IDE update" from the menu (This is incompatible with the
  installed package).

## Version 1.4 (2010-07-12)

### Added

* Use gdb graphical interface.

## Version 1.3 (2010-03-30)

### Added

* Add support for spaces in file and directory names.
* Show SVN modified status of files in the menu.
* Add "Emacs-IDE update" in the menu (if Emacs-IDE is under SVN).

### Performance

* Reduce useless disk accesses and menu rebuilds.
* Use recursive grep instead of find/grep.

## Version 1.2 (2009-08-29)

### Changed

* Use ctags instead of etags.
* Use dired mode instead of speedbar for file browsing.

### Removed

* Remove the "toolbar" window (quite useless and not well supported on Emacs
  22).

### Fixed

* Compatibility with Emacs 22.

## Version 1.1 (2009-04-16)

### Added

* Add a command to delete a project.
* Add customizable dark and light color themes.
* Add popup menu on directories (to execute actions on several files at once).

### Changed

* Use a generic project type:
  - Tags are built for all languages.
  - Cscope feature is available if C/C++ files are present.

## Version 1.0 (2008-12-18)

First release.
