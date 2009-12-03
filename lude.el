;;; lude.el --- hacks for quickly finding LUDE-related files

;; Copyright (C) 1997, 1998 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Created: 1997-03-03

;; $Id: lude.el,v 1.3 1999/05/22 16:56:22 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; Code:

(defvar lude-soft-directory
  (let ((dirs '("/opt/lude/soft/"
                "/tools/ns/soft/"
                "/opt/local/soft/"
                "/opt/gnu/soft/"
                "/opt/X11/soft/"
                ))
        (dir nil))
    (while dirs
      (cond ((file-exists-p (car dirs))
             (setq dir (car dirs))
             (setq dirs nil))
            (t
             (setq dirs (cdr dirs)))))
    dir))

(defvar lude-soft-re
  (concat "^.*" (regexp-quote lude-soft-directory) "\\([^/]+\\)"))

(defvar lude-soft-file-map
  '(("LUDE"          "install/default/" "install/private/" "install/public/")
    ("IAFA-PACKAGES" . "install/")
    ("history"       . "")))

(defvar lude-soft-ignore-directories
  '("../" "./" "00-catalog/"))

(defvar lude-soft-package-cache nil)
(defvar lude-soft-package-time '(0 0))


;;;###autoload
(defun lude-find-history (package)
  (interactive (list (lude-prompt-package current-prefix-arg)))
  (lude-find-file "history" package))

;;;###autoload
(defun lude-find-LUDE (package)
  (interactive (list (lude-prompt-package current-prefix-arg)))
  (lude-find-file "LUDE" package))

;;;###autoload
(defun lude-find (name package)
  (interactive (list (lude-prompt-name)
                     (lude-prompt-package current-prefix-arg)))
  (lude-find-file name package))

(defun lude-find-file (file-name package)
  (let* ((prefix (concat lude-soft-directory package "/"))
         (infix (or (cdr (assoc file-name lude-soft-file-map)) ""))
         (name nil)
         dirs try)
    (if (stringp infix)
        (setq name (concat prefix infix file-name))
      (setq dirs infix)
      (while dirs
        (setq try (concat prefix (car dirs) file-name))
        (cond ((file-exists-p try)
               (setq name try)
               (setq dirs nil))
              (t
               (setq dirs (cdr dirs))))))
    (find-file name)))

(defun lude-prompt-name (&optional prompt)
  (or prompt (setq prompt "Lude file name: "))
  (completing-read prompt lude-soft-file-map))

(defun lude-prompt-package (&optional prefix-arg)
  (cond ((and (null prefix-arg)
           (lude-current-package)))
        (t
         (lude-package-cache-update)
         (completing-read "Lude package: "
                          lude-soft-package-cache
                          nil t (lude-current-package)))))

(defun lude-package-cache-update ()
  (let* ((attr (file-attributes lude-soft-directory))
         (current-mtime (nth 5 attr)))
    (cond ((and (consp current-mtime)
                (lude-time-> current-mtime lude-soft-package-time))
           (setq lude-soft-package-cache
                 (lude-package-name-completions lude-soft-directory))
           (setq lude-soft-package-time current-mtime)))))

(defun lude-package-name-completions (dir)
  (let* ((names (file-name-all-completions "" dir))
         (remove lude-soft-ignore-directories)
         (new))
  (while names
    (cond ((or (member (car names) remove)
               (not (char-equal (aref (car names) (1- (length (car names))))
                                ?/))))
          (t
           (setq new (cons (cons (substring (car names) 0 -1) nil) new))))
    (setq names (cdr names)))
  new))

(defun lude-current-package ()
  (save-match-data
    (and (string-match lude-soft-re default-directory)
         (substring default-directory (match-beginning 1) (match-end 1)))))

(defun lude-time-> (a b)
  (or (> (car a) (car b))
      (and (= (car a) (car b))
           (> (car (cdr a)) (car (cdr b))))))

(provide 'lude)

;;; lude.el ends here
