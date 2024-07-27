helm-flymake
============

## Installation:
Add this file in your `load-path' and the following to your Emacs init file:
```elisp
(autoload 'helm-flymake "helm-flymake" nil t)
```

In order for `helm-flymake` to work, `flymake-mode` must be turned on.

```elisp
(flymake-mode)
```

Feel free to turn on flymake only the buffers where you want to use it.

## Commentary:

`helm` interface for `flymake`.
When `flymake-mode` is `t`, ` M-x helm-flymake` lists warning and error
messages in *helm flymake* buffer.

When `Enter/<return>` is pressed the "default" action is executed
moving point to the line of the selected warning/error and closing
the `helm-flymake` mini-buffer.
 
