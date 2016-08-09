dot
=====

**Tip**: Hard link may be useful to avoid file inconsistencies between
  those in real directories and the repository, e.g.,

    ln dot/.emacs ~/
    ln dot/key_repeat ~/
    ln dracula-theme/emacs/dracula-theme.el .emacs.d/themes/

`fonts.conf` disables hinting.  Move it to,

* Fedora: `~/.config/fontconfig/`
* Ubuntu: `~/.fonts/`

See
[Font configuration](https://wiki.archlinux.org/index.php/font_configuration)
for more details.
