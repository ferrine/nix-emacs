;;; nix-shell.el --- Utility functions to work with nix-shells

;; Copyright (C) 2024 Max Kochurov

;; Author: Max Kochurov <svenkeidel@gmail.com>
;; Package-Version: 0.1
;; Package-Requires: ((tramp "2.7.2"))
;; Homepage: https://github.com/nix-community/nix-emacs

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;;; Commentary:

;; Useful functions for working with nix-shells

;;; Code:
(require 'nix-sandbox)
(require 'tramp)
(require 'cl-extra)

(defgroup nix-shell nil
  "Nix process environments."
  :prefix "nix-shell-"
  :group 'processes
  :link '(url-link "https://github.com/nix-community/nix-emacs"))

(defcustom nix-shell-ignored-variables
  '("_" "PS1" "SHLVL" "DISPLAY" "PWD" "INSIDE_EMACS" "SHELL")
  "List of environment variables to ignore."
  :type '(repeat string))

(defun nix-shell--filter-vars (vars)
  "Filter out VARS listed in `nix-shell-ignored-variables'."
  (let ((prefix-ignore (seq-do
                        (lambda (e) (s-concat e "="))
                        nix-shell-ignored-variables)))
    (seq-filter (lambda (var)
                (not (seq-contains-p
                      prefix-ignore
                      #'string-prefix-p)))
              vars)))

(defun nix-shell--sandbox-env ( &optional sandbox)
  (when-let* ((this-sandbox (nix-find-sandbox (or sandbox default-directory)))
              (this-sandbox-env
               (split-string (nix-sandbox/nix-shell this-sandbox "env" "-0") "\0" t)))

    (nix-shell--filter-vars this-sandbox-env)))

(defun nix-shell--set-env-variable (var)
  (let* ((pair (s-split-up-to "=" var 1))
         (name (car pair))
         (value (cdr pair)))
    (if (tramp-file-name-p default-directory)
        (nix-shell--tramp-set-remote-env-var
         (tramp-dissect-file-name default-directory)
         name value)
      (setenv name value))))

(defun nix-shell--set-env (vars)
  (seq-do #'nix-shell--set-env-variable vars))


(defun nix-shell--tramp-set-remote-env-var (vec name value &optional append)
  "This code is taken from tramp-sh.el to handle large env variables."
  (let ((command (format "%1$s=%2$s && export %1$s" name value))
         (pipe-buf (tramp-get-remote-pipe-buf vec))
         tmpfile chunk chunksize)
    (tramp-message vec 5 (format "Setting %s environment variable" name))
    (if (tramp-compat-length< command pipe-buf)
        (tramp-send-command vec command)
      ;; Use a temporary file.  We cannot use `write-region' because
      ;; setting the remote path happens in the early connection
      ;; handshake, and not all external tools are determined yet.
      (setq command (concat command "\n")
            tmpfile (tramp-make-tramp-temp-file vec))
      (while (not (string-empty-p command))
        (setq chunksize (min (length command) (/ pipe-buf 2))
              chunk (substring command 0 chunksize)
              command (substring command chunksize))
        (tramp-send-command vec (format
                                 "printf \"%%b\" \"$*\" %s >>%s"
                                 (tramp-shell-quote-argument chunk)
                                 (tramp-shell-quote-argument tmpfile))))
      (tramp-send-command vec (format ". %s" tmpfile))
      (tramp-send-command vec (format "rm -f %s" tmpfile)))))

(provide 'nix-shell)
;;; nix-shell.el ends here
