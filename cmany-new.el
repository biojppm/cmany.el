;; cmany.el --- cmany integration for Emacs. -*-coding: utf-8 -*-

;; Copyright (C) 2017 Joao Paulo Magalhaes <dev@jpmag.me>

;; Author:      Joao Paulo Magalhaes <dev@jpmag.me>
;; Created:     2017-03-20
;; Version:     0.1
;; Keywords:    cmany, Cmake, IDE, Languages, Tools, rtags
;; URL:         http://github.com/biojppm/cmany.el

;; This file is not part of GNU Emacs.

;; This file adds facilities to Emacs for interacting
;; with cmany (http://github.com/biojppm/cmany.git ).

;; This extension is licensed under the MIT License.
;;
;; You should have received a copy of the MIT License
;; along with this program. If not, see <http://github.com/biojppm/cmany.el/blob/master/LICENSE.txt

;;; Depends:

;; cmany.el uses facilities from projectile and rtags, if they are
;; available (tested via featurep). These are NOT hard dependencies.
;; cmany.el has the following hard dependencies: term-run

;;; Install:

;; Put this file somewhere on your Emacs-Lisp `load-path' and add the
;; following into your ~/.emacs startup file.
;;
;;      <at standards TAB position explain what lisp code is needed>
;;      (autoload 'example-install "example" "" t)
;;      (autoload 'example-mode    "example" "" t)

;;; Commentary:

;; cmany.el provides emacs integration for cmany
;; (http://github.com/biojppm/cmany.git ), a batch-build tool
;; and workflow simplifier for cmake-based projects.

;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
(require 'eieio)
(require 'transient)

;; https://www.reddit.com/r/emacs/comments/f3o0v8/anyone_have_good_examples_for_transient/
;; https://www.reddit.com/r/emacs/comments/ciyqib/how_to_set_transient_infix_params_default_values/
;; https://gist.github.com/abrochard/dd610fc4673593b7cbce7a0176d897de


;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------

(defgroup cmany nil
  "Customizations for cmany.el."
  :prefix "cmany-"
  :group 'ide
  )

(defcustom cmany-log-to-messages 1
  "Whether cmany should log to the *messages* buffer."
  :group 'cmany
  :type 'boolean
  :safe #'booleanp
  )

(defcustom cmany-log-to-buffer 1
  "Whether cmany should log to the *cmany* buffer."
  :group 'cmany
  :type 'boolean
  :safe #'booleanp
  )

(defcustom cmany-log-with-date 1
  "Whether cmany should log to a *cmany* buffer."
  :group 'cmany
  :type 'boolean
  :safe #'booleanp
  )

(defcustom cmany-dbg 1
  "Whether cmany should log additional debug info."
  :group 'cmany
  :type 'boolean
  :safe #'booleanp
  )


;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;; utility functions

(defun cmany--fmt-date () (format-time-string "%Y%m%d-%H%M%S.%3N"))

(defun cmany--dbg (&rest args)
  (when cmany-dbg (cmany--log args))
  )

(defun cmany--log (fmt &rest args)
  (when cmany-log-to-messages
    (if cmany-log-with-date
        (message
         (apply 'format (concat "cmany[%s]: %s " fmt) (current-buffer) (cmany--fmt-date) args))
      (message
       (apply 'format (concat "cmany[%s]: " fmt) (current-buffer) args))
      )
    )
  (when cmany-log-to-buffer
    (let ((b (current-buffer)))
      (with-current-buffer (get-buffer-create "*cmany*")
        ;;(end-of-buffer)
        (if cmany-log-with-date
            (insert (apply 'format (concat "%s: [%s] " fmt "\n") (cmany--fmt-date) b args))
          (insert (apply 'format (concat "[%s] " fmt "\n") b args)))
        )
      )
    )
  )

(defun cmany--visit-buffer (name)
  "Create or visit a buffer with the given name."
  (when (not (get-buffer name))
    (split-window-sensibly (selected-window))
    ;(other-window 1)
    (get-buffer-create name)
    ;;(term (getenv "SHELL"))
    )
  (switch-to-buffer-other-window name)
  )

(defun cmany--write-file (file data)
  "http://stackoverflow.com/a/36196312/5875572"
  (with-temp-file file
    (prin1 data (current-buffer))
    )
  )

(defun cmany--read-file (file symbol)
  "http://stackoverflow.com/a/36196312/5875572"
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((v (read (current-buffer))))
      ;;(cmany--log "symbol: %s: %s" symbol v)
      (set symbol v)
      )
    )
  )

(defun cmany--get-cmd-output (workdir cmd)
  "Run cmd and return the output in a string if it returns 0;
  otherwise return an empty string."
  (let ((d default-directory))
    (cmany--dbg "cmd exec: %s" cmd)
    (cmany--dbg "        : currdir=%s" d)
    (cmany--dbg "        : workdir=%s" workdir)
    (cd workdir)
    (with-temp-buffer
      (cmany--dbg "cmd exec dir: %s (was at %s)" (pwd) d)
      (let ((p (call-process-shell-command cmd nil (current-buffer))))
        (cmany--dbg "cmd return: %d" p)
        (cmany--dbg "cmd output: %s" (buffer-string))
        (if (eq p 0)
            (progn (setq -cmcs (buffer-string)))
          (progn (setq -cmcs ""))
          )
        )
      )
    (cd d)
    -cmcs
    )
  )

(defun cmany--get-cmany-output (cmd &rest more-args)
  (let* ((base-cmd (cmany--format-cmd cmd))
         (full-cmd (concat base-cmd more-args))
         )
    ;;(cmany--dbg "cmd base: %s" base-cmd)
    ;;(cmany--dbg "cmd full: %s" full-cmd)
    (cmany--get-cmd-output cmany-proj-dir full-cmd)
    )
  )

(defun cmany--get-cmany-lines (cmd &rest more-args)
  (let ((out (apply 'cmany--get-cmany-output cmd more-args)))
    (split-string out "\n")
    )
  )

(defun cmany--str-not-empty (symbol)
  "is there a smarter way to check this?"
  (and (boundp symbol)
       (not (equal (symbol-value symbol) nil))
       (not (string-equal (symbol-value symbol) ""))
       )
  )

(defun cmany--var-not-nil (symbol)
  "is there a smarter way to check this?"
  (and (boundp symbol)
       (not (equal (symbol-value symbol) nil))
       )
  )


;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------

(defun cmany-set-project (proj-dir)
  (interactive)
  (cmany--set-project proj-dir)
  )

(defun cmany-get-project ()
  (interactive)
  (cmany--get-project)
  )

(defun cmany--set-project (proj-dir)
  (cmany--log "set-project: %s" proj-dir)
  )

(defun cmany--get-project ()
  (cmany--log "get-project: %s")
  )


;;-----------------------------------------------------------------------------

(defun cmany--set-proj-variable (varname varvalue)
  (cmany--log "set-proj-variable: %s %s" varname varvalue)
  )

(defun cmany--get-proj-variable (varname)
  (cmany--log "get-proj-variable: %s" varname)
  (concat "varvalue:" varname)
  )


;;-----------------------------------------------------------------------------
(defun cmany--test-function (&rest args)
  (interactive
   (list (transient-args 'cmany-exec)))
  (message "cmany------- args %s" args))

(defun cmany--configure (&rest args)
  (interactive
   (list (transient-args 'cmany-exec)))
  (cmany--log "configure-----" args)
  (cmany--test-function 'cmany 'configure args)
  )

(defun cmany--build (&rest args)
  (interactive
   (list (transient-args 'cmany-exec)))
  (cmany--log "build-----" args)
  (cmany--test-function 'cmany 'build args)
  )

(defun cmany--install (&rest args)
  (interactive
   (list (transient-args 'cmany-exec)))
  (cmany--log "install-----" args)
  (cmany--test-function 'cmany 'install args)
  )


;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;; adapted from magit-transient.el

(defclass cmany--transient-variable (transient-infix)
  ((scope       :initarg :scope)
   (default     :initarg :default     :initform nil)
   )
  )

(defclass cmany--transient-variable:switch (cmany--transient-variable)
  ()
  )

(defclass cmany--transient-variable:option (cmany--transient-variable)
  ()
  )

(defclass cmany--transient-variable:-multivalue (cmany--transient-variable)
  ((choices     :initarg :choices)
   (fallback    :initarg :fallback    :initform nil))
  )

(defclass cmany--transient-variable:radiobtn (cmany--transient-variable:-multivalue) ())

(defclass cmany--transient-variable:checkbox (cmany--transient-variable:-multivalue) ())

;;; Methods
;;;; Init

(cl-defmethod transient-init-scope ((obj cmany--transient-variable))
  (cmany--log "crl init-scope")
  (oset obj scope
        (cond (transient--prefix (oref transient--prefix scope))
              ((slot-boundp obj 'scope) (funcall (oref obj scope) obj))
              )
        )
  )

(cl-defmethod transient-init-value ((obj cmany--transient-variable))
  (cmany--log "crl init-value")
  (let ((variable (format (oref obj variable)
                          (oref obj scope))))
    (cmany--log "crl init-value: variable=%s" variable)
    (oset obj variable variable)
    (oset obj value
          (cond ((oref obj multi-value)
                 (message "variable is multivalue")
                 (cmany--get-proj-variable variable))
                (t
                 (magit-git-string "config" "--local" variable)))
          )
    )
  )

;;;; Read

(cl-defmethod transient-infix-read ((obj cmany--transient-variable:radiobtn))
  (cmany--log "crl infix-read:radiobtn")
  (let ((choices (oref obj choices)))
    (when (functionp choices)
      (cmany--log "crl infix-read choices is function, calling...")
      (setq choices (funcall choices)))
    (cmany--log "crl infix-read %s" choices)
    (if-let ((value (oref obj value)))
        (cadr (member value choices))
      (car choices))
    )
  )

;;;; Set

(cl-defmethod transient-infix-set ((obj cmany--transient-variable) value)
  (cmany--log "crl infix-set <- %s" value)
  (let ((variable (oref obj variable)))
    (oset obj value value)
    ;;(if (oref obj multi-value)
    ;;    (magit-set-all value variable)
    ;;  (magit-set value variable))
    (transient--redisplay)
    (unless (or value transient--prefix)
      (cmany--log "crl infix-set: Unset %s" variable))
    )
  )


;;;; Draw

(cl-defmethod transient-format-description ((obj cmany--transient-variable))
  (cmany--log "crl format-description")
  (or (oref obj description)
      (oref obj variable)))

(cl-defmethod transient-format-value ((obj cmany--transient-variable))
  (cmany--log "crl format-value")
  (if-let ((value (oref obj value)))
      (if (oref obj multi-value)
          (if (cdr value)
              (mapconcat (lambda (v)
                           (concat "\n     "
                                   (propertize v 'face 'transient-value)))
                         value "")
            (propertize (car value) 'face 'transient-value))
        (propertize (car (split-string value "\n"))
                    'face 'transient-value))
    (propertize "unset" 'face 'transient-inactive-value)))

(cl-defmethod transient-format-value ((obj cmany--transient-variable:radiobtn))
  (cmany--log "crl format-value:choices")
  (let* ((variable (oref obj variable))
         (choices  (oref obj choices))
         (local    (concat "config" "--local--"  variable))
         (global   (concat "config" "--global--" variable))
         (default  (oref obj default))
         (fallback (oref obj fallback))
         (fallback (and fallback
                        (when-let ((val (concat "fallback" fallback)))
                          (concat fallback ":" val)))))
    (when (functionp choices)
      (setq choices (funcall choices)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat (lambda (choice)
                  (propertize choice 'face (if (equal choice local)
                                               (if (member choice choices)
                                                   'transient-value
                                                 'font-lock-warning-face)
                                             'transient-inactive-value)))
                (if (and local (not (member local choices)))
                    (cons local choices)
                  choices)
                (propertize "|" 'face 'transient-inactive-value))
     (and (or global fallback default)
          (concat
           (propertize "|" 'face 'transient-inactive-value)
           (cond (global
                  (propertize (concat "global:" global)
                              'face (cond (local
                                           'transient-inactive-value)
                                          ((member global choices)
                                           'transient-value)
                                          (t
                                           'font-lock-warning-face))))
                 (fallback
                  (propertize fallback
                              'face (if local
                                        'transient-inactive-value
                                      'transient-value)))
                 (default
                   (propertize (concat "default:" default)
                               'face (if local
                                         'transient-inactive-value
                                       'transient-value))))))
     (propertize "]" 'face 'transient-inactive-value))))


;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------

(define-infix-argument cmany--transient:proj-dir ()
  :description "proj dir"
  :class 'transient-option ;;'cmany--transient-variable:option
  :shortarg "-p"
  :argument "--proj-dir="
  :history-key "-cmany-proj-dir"
  :command (lambda (&rest args) (cmany--log "set-proj-dir"))
  )

(define-infix-argument cmany--transient:build-dir ()
  :description "build dir"
  :class 'transient-option ;;'cmany--transient-variable:option
  :shortarg "-b"
  :argument "--build-dir="
  :history-key "-cmany-build-dir"
  :command (lambda (&rest args) (cmany--log "set-build-dir"))
  :reader (lambda (&rest args) (cmany--log "get-build-dir"))
  )

(define-infix-argument cmany--transient:install-dir ()
  :description "install dir"
  :class 'transient-option ;;'cmany--transient-variable:option
  :shortarg "-i"
  :argument "--install-dir="
  :history-key "-cmany-install-dir"
  :command (lambda (&rest args) (cmany--log "set-install-dir"))
  :reader (lambda (&rest args) (cmany--log "get-install-dir"))
  )


(define-infix-argument cmany--transient:jobs ()
  :description "jobs"
  :class 'transient-switch ;;'cmany--transient-variable:option
  :shortarg "-j"
  :argument "--jobs="
  :history-key "-cmany-jobs"
  :command (lambda (&rest args) (cmany--log "set-cmany-jobs"))
  :reader (lambda (&rest args) (cmany--log "get-cmany-jobs"))
  )

(define-infix-argument cmany--transient:continue ()
  :description "continue"
  :class 'transient-switch ;;'cmany--transient-variable:switch
  :shortarg "--c"
  :argument "--continue"
  :history-key "-cmany-continue"
  :command (lambda (&rest args) (cmany--log "set-cmany-continue"))
  :transient (lambda (&rest args) (cmany--log "set-cmany-continue."))
  )

(define-infix-argument cmany--transient:verbose ()
  :description "verbose"
  :class 'transient-switch ;;'cmany--transient-variable:switch
  :shortarg "--v"
  :argument "--verbose"
  :history-key "-cmany-verbose"
  :command (lambda (&rest args) (cmany--log "set-cmany-verbose"))
  :transient (lambda (&rest args) (cmany--log "set-cmany-verbose."))
  )


(define-infix-argument cmany--transient:cmake-vars ()
  :description "cmake vars"
  :class 'transient-option
  ;;:class 'cmany--transient-variable:option
  :shortarg "-V"
  :argument "--cmake-vars="
  :history-key "-cmany-flags-cmake-vars"
  )

(define-infix-argument cmany--transient:defines ()
  :description "defines"
  :class 'transient-option ;;'cmany--transient-variable:option
  :shortarg "-D"
  :argument "--defines="
  :history-key "-cmany-flags-defines"
  )

(define-infix-argument cmany--transient:cxxflags ()
  :description "cxx flags"
  :class 'transient-option ;;'cmany--transient-variable:option
  :shortarg "-X"
  :argument "--cxxflags="
  :history-key "-cmany-flags-cxxflags"
  )

(define-infix-argument cmany--transient:cflags ()
  :description "c flags"
  :class 'transient-option ;;'cmany--transient-variable:option
  :shortarg "-C"
  :argument "--cflags="
  :history-key "-cmany-flags-cflags"
  )


(defun cmany--transient-build-item-scope (&optional obj)
  (cmany--log "build item scope.")
  (cmany--log "build item scope: obj=%s" obj)
  )

;;(setq cmany--proj-systems nil)
;;(define-infix-command cmany--transient-build-item:systems ()
;;  ;;:scope 'cmany--transient-build-item-scope
;;  :description "systems"
;;  :class 'cmany--transient-variable:radiobtn
;;  :variable cmany--proj-systems
;;  :fallback nil
;;  :choices #'(lambda () '("linux" "windows"))
;;  :default "default-systems"
;;  :history-key "-cmany--systems"
;;  )


(define-infix-argument cmany--transient-build-item:systems ()
  :description "systems"
  :class 'transient-option ;;'cmany--transient-variable:radiobtn
  :shortarg "-s"
  :argument "--systems="
  )

(define-infix-argument cmany--transient-build-item:architectures ()
  :description "architectures"
  :class 'transient-option ;;'cmany--transient-variable:option
  :shortarg "-a"
  :argument "--architectures="
  )

(define-infix-argument cmany--transient-build-item:compilers ()
  :description "compilers"
  :class 'transient-option ;;'cmany--transient-variable:option
  :shortarg "-c"
  :argument "--compilers="
  )

(define-infix-argument cmany--transient-build-item:types ()
  :description "build types"
  :class 'transient-option ;;'cmany--transient-variable:option
  :shortarg "-t"
  :argument "--build-types="
  )

(define-infix-argument cmany--transient-build-item:variants ()
  :description "variants"
  :class 'transient-option ;;'cmany--transient-variable:option
  :shortarg "-v"
  :argument "--variants="
  )


(define-transient-command cmany-configure ()
  "run cmany configure"
  [
   ["Flags"
    (cmany--transient:cmake-vars)
    (cmany--transient:defines)
    (cmany--transient:cxxflags)
    (cmany--transient:cflags)
    ]
   ["Directories"
    (cmany--transient:proj-dir)
    (cmany--transient:build-dir)
    (cmany--transient:install-dir)
    ]
   ]
  [
   ["Build Items"
    ("s" cmany--transient-build-item:systems)
    (cmany--transient-build-item:architectures)
    (cmany--transient-build-item:compilers)
    (cmany--transient-build-item:types)
    (cmany--transient-build-item:variants)
    ]
   ["Arguments"
    (cmany--transient:jobs)
    (cmany--transient:continue)
    (cmany--transient:verbose)
    ]
   ]
  ["Actions"
   ("c" "configure" cmany--configure)
   ]
  )

(define-transient-command cmany-build ()
  "run cmany build"
  [
   ["Flags"
    (cmany--transient:cmake-vars)
    (cmany--transient:defines)
    (cmany--transient:cxxflags)
    (cmany--transient:cflags)
    ]
   ["Directories"
    (cmany--transient:proj-dir)
    (cmany--transient:build-dir)
    (cmany--transient:install-dir)
    ]
   ]
  [
   ["Build Items"
    ("s" cmany--transient-build-item:systems)
    (cmany--transient-build-item:architectures)
    (cmany--transient-build-item:compilers)
    (cmany--transient-build-item:types)
    (cmany--transient-build-item:variants)
    ]
   ["Arguments"
    (cmany--transient:jobs)
    (cmany--transient:continue)
    (cmany--transient:verbose)
    ]
   ]
  ["Actions"
   ("b" "build" cmany--build)
   ]
  )

(define-transient-command cmany-install ()
  "run cmany build"
  [
   ["Flags"
    (cmany--transient:cmake-vars)
    (cmany--transient:defines)
    (cmany--transient:cxxflags)
    (cmany--transient:cflags)
    ]
   ["Directories"
    (cmany--transient:proj-dir)
    (cmany--transient:build-dir)
    (cmany--transient:install-dir)
    ]
   ]
  [
   ["Build Items"
    ("s" cmany--transient-build-item:systems)
    (cmany--transient-build-item:architectures)
    (cmany--transient-build-item:compilers)
    (cmany--transient-build-item:types)
    (cmany--transient-build-item:variants)
    ]
   ["Arguments"
    (cmany--transient:jobs)
    (cmany--transient:continue)
    (cmany--transient:verbose)
    ]
   ]
  ["Actions"
   ("i" "install" cmany--install)
   ]
  )

(define-transient-command cmany-exec ()
  "Run cmany"
  ["Actions"
   ("c" "configure" cmany-configure)
   ("b" "build" cmany-build)
   ("i" "install" cmany-install)
   ]
  )

(global-set-key (kbd "C-c C-m") 'cmany-exec)
(global-set-key (kbd "C-c m c") 'cmany-configure)
(global-set-key (kbd "C-c m b") 'cmany-build)
(global-set-key (kbd "C-c m i") 'cmany-install)

(defun cmany----reload ()
  (interactive)
  (byte-compile-file buffer-file-name)
  (eval-buffer)
  (with-current-buffer (get-buffer-create "*cmany*")
    (erase-buffer)
    )
  )
