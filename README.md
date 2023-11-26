# My Dotfiles!

First, many, many thanks to every other dotfile repository I've ever gawped at
amazement at.

## With GNU Stow

These dotfiles are currently managed using [GNU
Stow](https://www.gnu.org/software/stow/), using a system similar to that
described by [Brandon
Invergo](http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html).
One minor gotcha to note is that, by default, `stow` targets the parent
directory of the stowed files. That default works if you have a "dotfiles"
directory in your home directory. I store my dotfiles a little deeper into my
directory structure, and in multiple places, so I scatter .stowrc files in
several directories. For example, my home directory has this default:

```
--target=/home/danny
--dir=/home/danny/Public/dotfiles
```

Which points to these public dotfiles -- but I have other directories that
store localpackages in ~/.local/ , etc. They have different .stowrc's in them,
which override my global default when I'm using stow in those
directories.

## With Guix Home Services

I'm gradually moving from using Stow to managing my dotfiles (and more) with
Guix's [Home
Services](https://guix.gnu.org/manual/en/html_node/Home-Services.html). I'm
trying to make sure that this configuration works with both for the time being,
but at some point I may switch over completely. Raise an issue if you
see something that is broken on Stow!

The command to set up home servies is:
```
guix home reconfigure guix-home.scm
```

