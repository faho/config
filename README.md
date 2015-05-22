Hi, I'm Fabian, and here's my configuration (some say "dotfiles").

What you will find here is a mish-mash of things I use, things I'm just trying out, and things I no longer use but have forgotten about so thoroughly that I've even forgotten to remove them.

I use it on an archlinux machine, so it might require rather new software (I don't know)

You can try using it whole-cloth, though I'd always recommend you just look for inspiration.

Please note that this very much isn't a collaborative project but a personal one and as such I **will force-push**. In fact, I've just done it again to add this sentence.


So, what's in here?

Emacs
=====

The biggest thing here is an emacs configuration that

- Uses vim-like keys (by using evil, a vim-implementation for emacs) for low-level commands and emacs-like keys for higher-level things

  I don't quite have the brainpower it takes to memorize "C-u 0 C-c C-x C-e", so I use "M-x" for many things.
  Guide-key-mode and hydra help a bunch, though.

- Bootstraps itself from just init.el

  You only need to download init.el, put it into ~/.emacs.d/init.el, and it will download the other files here and all the packages it needs.

- Starts in 1.1 seconds on my system (thanks to the magic of req-package)

  (This is of course only if it doesn't need to download anything)

I use this configuration on a linux system in a console, though it should also work with GUI emacs.

Bash
====

My bash config is reasonably simple, it is mainly aimed at undoing some of the bad defaults bash ships with.

Note that it is currently incomplete as I haven't uploaded all the files yet (still auditing for private info)


Fish
====

Fish (the **F**riendly **I**nteractive **SH**ell) doesn't need much configuration, but setting a few variables is always nice.

It also has a few functions, like "makepkgs", a stupid makepkg-wrapper to try to build multiple packages.

I currently (May 2015) use fish built from git.

Git
====

This isn't much (though the "lg" alias is nice).

Remember to change the name and email-address when you use it.

An infect script
====

This trivial thing just uses Gnu Stow to symlink all _directories_, meaning there's no need to blacklist README.md or any other top-level files.
