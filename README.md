# Command Line Interface File Manager
Clifm is a small file manager written in Haskell with a terminal-based interface. It allows you to explore directories in multiple Panes/Tabs and perform basic operations.

![screenshot](screenshot.png)

> Note: this is still an experiment. Directory navigation will do no harm, but double-check before starting operations on your file system. I take no responsibility for what you do with this software.

## Installation
> Note: You may need to install `ncurses` on your system before using clifm

For ArchLinux the binary from [the latest github release](https://github.com/pasqu4le/clifm/releases/latest) should work.
For other Linux distro the binary may work as well, or you can build from source.

To build from source you will need [GHC](https://www.haskell.org/ghc/) and [cabal-install](http://hackage.haskell.org/package/cabal-install).
Since clifm is on Hackage you can just use:

```
$ cabal install clifm
```
or install from the cloned repository:
```
$ git clone https://github.com/pasqu4le/clifm.git
$ cd clifm
$ cabal install
```

## Features
Clifm is a [brick](https://github.com/jtdaugherty/brick) application, that in turn builds upon [vty](https://github.com/jtdaugherty/vty). As such it supports a large number of terminals, but not on Windows, handles windows resizing and more.

If your terminal supports a mouse you can use it to change Tab/Pane, click a button on the bottom, change your selection or open it (double-click), but only using the keyboard you can perform every possible action. This is the list of all the keybindings:

#### Bottom menu
- L: open Se**l**ection menu
- A: open T**a**b menu
- P: open **P**ane menu
- BackSpace: go **back** to main menu
- Esc/Q: **Q**uit

#### Selection
- Enter: Open directory/run executable file/open readable file in editor
- Ctrl+(X/C): Cut/Copy the selected Item
- Up/Down Arrow: move the selection in the current Tab
- PageUp/PageDown: move the selection in the current Tab by one page at a time
- Home/End: move the selection in the current Tab to beginning or end of list
- Ctrl+R: **R**ename the selected Item
- Ctrl+D: **D**elete the selected Item
- Ctrl+O: **O**pen the selected directory in a New Tab
- S: **S**how info about the selected Item

#### Tabs
- Tab/BackTab: Move to the next/previous tab
- Ctrl+(Left/Right Arrow): Swap current tab's position with the previous/next one
- Ctrl+V: Paste in the current Tab's directory
- Ctrl+S: **S**earch for a file/folder in the current Tab's directory
- K: **K**ill (close) the current Tab
- M: **M**ake a new directory
- T: **T**ouch (create an empty) file
- G: **G**o to another directory
- E: Open **E**mpty Tab
- R: **R**efresh the current Tab
- O: **O**rder by file name/file size/access time/modification time
- I: **I**nvert order

#### Panes
- Left/Right Arrow: Focus on the previous/next Pane
- Ctrl+E: Open **E**mpty Pane
- Ctrl+K: **K**ill (close) the current Pane

The actions above will not work only if a prompt is up, or you try to do something not possible.

## Command line arguments
You can have a list of command line arguments by running `clifm --help`.

#### Starting directory
If you specify nothing `clifm` will open the current directory, but you can select another directory using `--dir-path` or `-d`, for example: `clifm -d "/home"`.

If the directory path is not valid `clifm` will open on an empty tab.

#### Themes
You can load a theme from a file using `--theme` or `-t`, for example: `clifm -t "theme/phosphor.ini"`. If the file does not exists or cannot be loaded `clifm` will use the default theme.

You can use one of the existing themes in the `themes/` folder:
- blackAndWhite.ini
- paper.ini (inverted blackAndWhite)
- phosphor.ini (like old monochrome monitors)
- ocean.ini (very blue)

You can also write and use your own themes: copy the `themes/template.ini` file, fill in the attributes you want to change and delete those you like as default.

Complete explanation from [Brick.Themes](https://hackage.haskell.org/package/brick-0.35/docs/Brick-Themes.html):
> The file format is as follows:
>
> Customization files are INI-style files with two sections, both optional: "default" and "other".
>
> The "default" section specifies three optional fields:
>
> - "default.fg" - a color specification
> - "default.bg" - a color specification
> - "default.style" - a style specification
>
> A color specification can be any of the strings *black*, *red*, *green*, *yellow*, *blue*, *magenta*, *cyan*, *white*, *brightBlack*, *brightRed*, *brightGreen*, *brightYellow*, *brightBlue*, *brightMagenta*, *brightCyan*, *brightWhite*, or *default*.
>
> A style specification can be either one of the following values (without quotes) or a comma-delimited list of one or more of the following values (e.g. "[bold,underline]") indicating that all of the specified styles be used. Valid styles are *standout*, *underline*, *reverseVideo*, *blink*, *dim*, and *bold*.
>
> The other section specifies for each attribute name in the theme the same fg, bg, and style settings as for the default attribute. Furthermore, if an attribute name has multiple components, the fields in the INI file should use periods as delimiters. For example, if a theme has an attribute name ("foo" <> "bar"), then the file may specify three fields:
>
> - foo.bar.fg - a color specification
> - foo.bar.bg - a color specification
> - foo.bar.style - a style specification
>
> Any color or style specifications omitted from the file mean that those attribute or style settings will use the theme's default value instead.
>
> Attribute names with multiple components (e.g. attr1 <> attr2) can be referenced in customization files by separating the names with a dot. For example, the attribute name "list" <> "selected" can be referenced by using the string "list.selected".

#### Threads for directory size computation
Directory size is calculated by visiting a directory tree to sum it's files sizes (using [conduit](http://hackage.haskell.org/package/conduit)) and it may take a while. For this reason directories size will be calculated in different threads. 

You can limit how many of these threads to have by using `--thread-num` or `-n`, for example: `clifm -n 8`.

You are likely to have the best results with as many threads as your processor's cores. The default limit is set to 4.

## TODOs
- wide use of lenses
