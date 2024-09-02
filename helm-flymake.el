;;; helm-flymake.el --- helm sources for flymake -*- lexical-binding: t -*-

;; Copyright (C) 2012-2013 Akira Tamamori
;; Copyright (C) 2024 zbelial <zjyzhaojiyang@gmail.com>

;; Author: Akira Tamamori <tamamori5917@gmail.com>

;; Maintainer: zbelial <zjyzhaojiyang@gmail.com>

;; URL: https://github.com/emacs-helm/helm-flymake
;; Version: 0.1.9
;; Package-Requires: ((helm "1.0"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `helm' interface for flymake.
;; When `flymake-mode' is t, M-x `helm-flymake' lists warning and error
;; messages in *helm flymake* buffer.
;; When Enter/<return> is pressed the "default" action is executed
;; moving point to the line of the selected diagnostic and closing
;; the `helm-flymake' mini-buffer.

;;; Installation:
;;
;; Add this file in your `load-path' and the following to your Emacs init file:
;;
;; Note: Not necessary if using ELPA package.
;; (autoload 'helm-flymake "helm-flymake" nil t)

;;; History:
;;
;; Revision 0.1.9
;; * Rewrite most of the code to make it work with latest flymake.
;;
;; Revision 0.1.8
;; * Added "goto line" feature
;; * Sorted warnings and errors by line number such that lowest line is first
;; * Fixed but that showed "nil" in the warning/errors window when there were
;;   a different numbers of warnings compared to errors (and vise-versa).
;;
;; Revision 0.1.7
;; * convert prefix of helm-c-* into helm-*.
;;
;; Revision 0.1.6
;; * fix typo.
;;
;; Revision 0.1.5
;; * remove global variable `helm-c-flymake-err-list'.
;;
;; Revision 0.1.4
;; * revise documents.
;;
;; Revision 0.1.3
;; * add default input, line number at current point when `helm-flymake'
;;   is invoked with prefix argument.
;;
;; Revision 0.1.2
;; * add magic comment `autoload' to `helm-flymake'.
;;
;; Revision 0.1.1
;; * add variable `helm-c-flymake-buffer'.
;;
;; Revision 0.1
;; * Initial revision

;;; Code:

(require 'flymake)
(require 'helm)

(defgroup helm-flymake nil
  "Helm for flymake."
  :group 'helm)

(defcustom helm-flymake-actions
  '(("Goto flymake diagnostic" . helm-flymake-action-goto))
  "Actions for helm-flymake."
  :type '(alist :key-type string :value-type function))

(defun helm-flymake-action-goto (x)
  "Goto where there is any diagnostic."
  (goto-char (flymake-diagnostic-beg x))
  (recenter))

(defvar helm-flymake-source-name "Helm Flymake"
  "Source name of helm flymake.")

(defun helm-flymake--format-type (type)
  (let (face
	display-type
	(type (symbol-name type)))
    (cond
     ((string-suffix-p "note" type)
      (setq display-type "note")
      (setq face 'success))
     ((string-suffix-p "warning" type)
      (setq display-type "warning")
      (setq face 'warning))
     ((string-suffix-p "error" type)
      (setq display-type "error")
      (setq face 'error))
     (t
      (setq display-type "note")
      (setq face 'warning)))
    (propertize (format "%s" display-type) 'face face)))

(defun helm-flymake--transforme-to-candidate (diag)
  (let* (msg
	 (beg (flymake-diagnostic-beg diag))
	 (type (flymake-diagnostic-type diag))
	 (text (flymake-diagnostic-text diag))
	 (line (line-number-at-pos beg)))
    (setq msg (format "%-8d  %-12s    %s"
                      line (helm-flymake--format-type type) text))
    (cons msg diag)))

;;;###autoload
(defun helm-flymake ()
  "Helm interface for flymake."
  (interactive)
  (helm :sources (helm-build-sync-source helm-flymake-source-name
                   :candidates (mapcar #'helm-flymake--transforme-to-candidate
                                       (flymake-diagnostics))
                   :multiline t
                   :action
                   'helm-flymake-actions)
        :quit-if-no-candidate
        (lambda ()
          (message "No flymake diagnostics in this buffer"))
        :buffer "*helm flymake*"))

(provide 'helm-flymake)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-flymake.el ends here
