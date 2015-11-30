# emacs-ghq [![melpa badge][melpa-badge]][melpa-link]
[Ghq](https://github.com/motemen/ghq) interface for emacs.

# Usage
## `ghq`
Fetches the repository. equivalent to `ghq get`.

## `ghq-ssh`
Fetches the repository using ssh. equivalent to `ghq get -p`.

## `ghq-list`
Displays a message with the ghq project list paths relative to the root. equivalent to `ghq list`.

## `ghq-list-full-path`
Displays a message with the absolute ghq project list paths. equivalent to `ghq list --full-path`.

## `helm-ghq-list`
List the repositories under `ghq root` using helm.

# Requirements
* have [ghq](https://github.com/motemen/ghq) installed.

[melpa-link]: http://melpa.org/#/ghq
[melpa-badge]: http://melpa.org/packages/ghq-badge.svg
