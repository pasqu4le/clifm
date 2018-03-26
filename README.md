# Command Line Interface File Manager
Clifm is a small file manager written in Haskell with a command line interface. It allows you to explore directories in multiple tabs and perform basic operations.

[![asciicast](https://asciinema.org/a/LHfaWHtFKzp9KAYGEiGpns3mR.png)](https://asciinema.org/a/LHfaWHtFKzp9KAYGEiGpns3mR)

Note: this is still an experiment and might be unstable. I do not recommend using it as your daily File Manager and I take no responsibility on what you do with it.

## Building and Running
To build clifm, you need [GHC](https://www.haskell.org/ghc/) and [cabal-install](http://hackage.haskell.org/package/cabal-install). Then you can build with:

```
$ cabal build
```
and run the compiled software (should be in `dist/build/clifm/clifm`).

## Features
Clifm is a [brick](https://github.com/jtdaugherty/brick) application, that in turn builds upon [vty](https://github.com/jtdaugherty/vty). As such it supports a large number of terminals, but not on Windows, handles windows resizing and more.

If your terminal supports a mouse you can use it to change tab, click a button on the bottom or change your selection, but only using the keyboard you can perform every possible action. This is the list of all the keybindings:

- Up/Down Arrow keys: move the selection in the current Tab
- PageUp/PageDown keys: move the selection in the current Tab by one page at a time
- Home/End keys: move the selection in the current Tab to beginning or end of list
- Enter: Open the selected directory
- Tab/BackTab: Move to the next/previous tab
- Ctrl+Left/Right Arrow: Swap current tab's position with the previous/next one
- Esc or Ctrl+q: **Q**uit
- Ctrl+x: Cut the selected Item
- Ctrl+c: Copy the selected Item
- Ctrl+v: Paste in the current Tab's directory
- Ctrl+r: **R**ename the selected Item
- Ctrl+d: **D**elete the selected Item
- Ctrl+a: M**a**ke a new directory
- Ctrl+t: **T**ouch (create an empty) file
- Ctrl+g: **G**o to another directory
- Ctrl+n: Open a **N**ew Tab
- Ctrl+o: **O**pen the selected directory in a New Tab
- Ctrl+l: Re**l**oad the current Tab
- Ctrl+k: **K**ill (close) the current Tab

The actions above will not work only if a prompt is up, or you try to do something not possible.

> NOTE: directory size is not guaranteed to be accurate, the function in the `directory` library seems to be filesystem/platform dependent and visiting a directory tree to sum it's files sizes takes way too much time. Until a better solution is found the directory size will still be shown, but do not trust what it says.

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

## TODOs
- treaded IO operations (that do not freeze the UI and can be canceled)
- support for external text editors
- more settings (using command line arguments)
- mc directory comparison
- multi-pane view
- listing more properties
- sorting based on file size or date
- find a way to read correctly a directory size
