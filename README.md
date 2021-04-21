<div align="center">
 <h3>Ian's Emacs Configuration (on Windows 10 + WSL Ubuntu 20.04)</h3>
</div>

<hr>

<p align="center">
  <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/EmacsIcon.svg/120px-EmacsIcon.svg.png" />
</p>

### Thanks for dropping by!
This is my personal Emacs 27.2 configuration when I'm using Windows
10 and WSL Ubuntu 20.04. If you prefer an unopinionated Emacs "distro"
that you could build upon, check out [this
page](https://github.com/ianpan870102/yay-evil-emacs).

### Installation
Back up your `~/.emacs.d/` first (if you have one):

```
mv ~/.emacs.d ~/.emacs.d.bak
```

Git clone my configuration to your new `~/.emacs.d/` :
```
# For Emacs 26 and below
git clone https://github.com/ianpan870102/.wsl-emacs.d.git ~/.emacs.d

# For Emacs 27+
git clone https://github.com/ianpan870102/.wsl-emacs.d.git ~/.config/emacs/
```

### "Rolling" Release
I will constantly push new commits since *a real Emacser* is never
completely satisfied with his/her setup ;-)
