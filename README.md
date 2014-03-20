emacs-launcher
==============

A launcher for programs, files, folders, web pages and other, using Emacs (supersedes anything-launcher)

# About Emacs Launcher

Emacs Launcher is nothing more than some simple scripts and elisp files that uses Emacs and the Anything package as a launcher to launch programs, web pages, open common folders and what have you. The possibilities are almost endless (since the commands/sources are programmed using elisp) and Emacs Launcher comes with a couple of convenient example "sources" for most needs. By looking at the example sources and the helper functions they use, you should be able to create your own sources to "launch" pretty much everything. And launching does not just mean opening or starting things, you can of course add commands that does something silently. For example, there is an example command that creates a simple alarm using the operating system scheduler. After installation, Emacs Launcher is capable of doing many things from the start, but you probably want to extend it with your own commands, or command sources (see below for what sources that are available from start).

# Installation instructions

0. Get the files (duh)
1. Start Emacs
2. Press M-x load-file and then press enter
3. Enter the path to the install.el file and press enter
4. Answer the questions given in the installer.

This concludes the installation. 

# So, what did I get and how does it work?

The installation mostly copies a lot of files from the source folder and creates two important script files:

- emacs-launcher.cmd
- runemacs.cmd

emacs-launcher.cmd is, in a sense, the launcher. It is the file you want to make your operating system open as fast as possible (see below for options).

runemacs.cmd is used by emacs-launcher.cmd when Emacs Launcher has not already started. After starting Emacs Launcher and executing a command/launching something, it will be minimized and the next time emacs-launcher.cmd is called, it will just restore Emacs Launcher and ask for new input.

# Configuring how to launch the launcher

Now you have some options on how to launch the launcher (haha!).

## Add shortcut to your Desktop and set a shortcut key

This is probably the most convenient option. Follow these steps:

1. Make a shortcut on your Desktop to the file emacs-launcher.cmd in the installation directory.
2. RMB -> Properties on the shortcut and set a keyboard shortcut in the Shortcut Key field (for example CTRL + ALT + P).
3. Click OK.

## Add a shortcut someplace else

1. Make a shortcut on your Desktop to the file emacs-launcher.cmd in the installation directory.
2. Place it somewhere. The options you have here depends on the operating system you use. In Windows XP and Windows 7 (I think), you can place the shortcut for easy access on the Start Menu. This way you can quickly reach it by pressing the Windows key on your keyboard and then the first letter in the shortcut name (adjust the name of the shortcut accordingly, even a number will work).

## Use some third party program to bind a keyboard shortcut to Emacs Launcher

If the keyboard shortcuts available on shorcuts on the Desktop is not enough, there are plenty of third party tools that can solve the task. A favorite of mine, which I use to start the launcher is AutoHotkey. I actually also use it to hide Emacs Launcher completely, when a command has been executed. I chose however to leave it out since it makes the setup more complicated and it also increases the number of dependencies.

# Sources

What sources that are used listed in the file emacs-launcher.el, so if you want to disable some or add your own, you should modify this file. The sources themselves are found in emacs-launcher-sources.el.

## Sources available after installation

### Special Commands (source anything-c-source-special)


These are the special commands Cancel and Quit, used to either cancel the launcher (minimizing it) or quit the launcher.

### Commands (source anything-c-source-commands)

The most complex of the sources uses the .commands file as input. The syntax of that file is quite complex and supports things like starting programs, launching web pages with clipboard content as input, executing Emacs functions or lambda expressions, etc. You can more or less do anything with this source, but with more typing than in other common cases (see below). The included .commands file contains some examples that should make you understand how to make your own. The commands are parsed and executed by the function run-command defined in emacs-functions.el. There is a command defined in .commands that let you add a new command to start programs and files. There is even a command to let you add files as commands using drag and drop.

### Emacs Bookmarks (source anything-c-source-bookmarks)

Let you open Emacs bookmarks in the operating system (not in Emacs).

### Clips (source anything-c-source-clips)

Search for named text snippets/clips and get the content put on the clipboard. Useful for pieces of text you don't want to type manually or which you forget easily.

### Common Folders (source anything-c-source-common-folders)

Lists and let you open folders from the file .common-folders. You currently have to edit this file manually.

### Web Sites (source anything-c-source-urls)

Similar to anything-c-source-common-folders, but for URLs / web pages. There is a command defined in .commands that let you add a URL easily to this file.

# FAQ

## Why use Anything, and why such an old version of it?

Well, it works for me and it has the capabilities that I need, which is to list different commands matching user input. Feel free to submit patches ;)

## What about anything-launcher.el?

Well, I did not want to scrap it and decided I liked the name Emacs Launcher better and that maybe, in the future, I will, or others will, add other "launcher engines". Probably someome want to use Helm instead.

