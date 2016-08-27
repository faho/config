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
  [Which-key](https://github.com/justbur/emacs-which-key) and [hydra](https://github.com/abo-abo/hydra) help a bunch, though.

- Bootstraps itself from just init.el

  You only need to download init.el, put it into ~/.emacs.d/init.el, and it will download the other files here and all the packages it needs.

- Starts in 0.7 seconds on my system (thanks to the magic of [use-package](https://github.com/jwiegley/use-package))

  (This is of course only if it doesn't need to download anything)

I use this configuration on a linux system in a console, though it should also work with GUI emacs.

Bash
====

My bash config is reasonably simple, it is mainly aimed at undoing some of the bad defaults bash ships with.

Note that it is currently incomplete as I haven't uploaded all the files yet (still auditing for private info)


Fish
====

Fish (the **F**riendly **I**nteractive **SH**ell) doesn't need much configuration, but setting a few variables is always nice.

It also has a few functions, like "makepkgs", a stupid makepkg-wrapper to try to build multiple packages, and `aur`, a stupid aur helper using git.

I currently (August 2016) use fish built from git.

Git
====

This isn't much (though the "lg" alias is nice).

Remember to change the name and email-address when you use it.

An infect script
====

This trivial thing just uses Gnu Stow to symlink all _directories_, meaning there's no need to blacklist README.md or any other top-level files.

License
=======

I don't consider this to be of sufficient originality to warrant copyright, but in case it somehow does, it needs a license.

For "projects" like this I consider it important that you don't need to care about the license (I already do too much of that), that the license doesn't force you to do anything.

Since I don't even want anyone to _need_ to attach a copyright notice or attribution to this or pieces of this, which rules out even the MIT license.

So, since I want to have at least a bit of security, I picked one from the GNU project's list of licenses it calls the "GNU All-Permissive License".

See the LICENSE file (so that bots pick it up and don't count this as another unlicensed repository). PLease note that I wish to apply the "this file" in that license to all files in this repository that don't state another license.
