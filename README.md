# Helm mkr

helm interface for [mkr command](https://github.com/mackerelio/mkr)

# How-to use

1. [install mkr command](https://github.com/mackerelio/mkr#installation).
2. [setup your mackerel.io account](https://github.com/mackerelio/mkr#usage).
3. git clone this repository to your load-path
4. setup your `~/.emacs.d/init.el`.

example of `~/.emacs.d/init.el` is below.
```elisp
(require 'helm-mkr)
(setq mkr-org "your mackerelio org name")
```

