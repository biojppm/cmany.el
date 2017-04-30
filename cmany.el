;; cmany.el --- cmany integration for Emacs. -*-coding: utf-8 -*-

;; Copyright (C) 2017 Joao Paulo Magalhaes <dev@jpmag.me>

;; Author:      Joao Paulo Magalhaes <dev@jpmag.me>
;; Created:     2017-03-20
;; Version:     0.1
;; Keywords:    cmany, Cmake, IDE, Languages, Tools, rtags
;; URL:         http://github.com/biojppm/cmany.el.git

;; This file is not part of GNU Emacs.

;; This file adds facilities to Emacs for interacting
;; with cmany (http://github.com/biojppm/cmany.git ).

;; This extension is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; This extension is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information.

;;; Depends:

;; cmany.el uses facilities from projectile and rtags, if they are
;; available (via featurep). These are NOT hard dependencies.
;; cmany.el has the following hard dependencies: term-run

;;; Install:

;; Put this file along your Emacs-Lisp `load-path' and add the following
;; into your ~/.emacs startup file.
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
(require 'term-run)

;;-----------------------------------------------------------------------------

(defgroup cmany nil
  "Customizations for cmany.el."
  :prefix "cmany-"
  :group 'ide
  )

(defcustom cmany-build-dir-prefix "build/"
  "the path (relative to the project dir) under which the build
directories should be placed. This applies only to newly created
build trees."
  :group 'cmany
  :type 'string
  :safe #'stringp
  )

(defcustom cmany-build-before-run 1
  "Whether cmany should build the target before running it."
  :group 'cmany
  :type 'boolean
  :safe #'booleanp
  )

(defcustom cmany-rtags-enabled 1
  "Whether cmany should announce the current build directory to rtags."
  :group 'cmany
  :type 'boolean
  :safe #'booleanp
  )

;;-----------------------------------------------------------------------------
(defvar cmany-proj-dir nil
  "The directory where the current CMakeLists.txt project is located."
  )

(defvar cmany-build-dir nil
  "The directory of the current cmany build."
  )

(defvar cmany-target ""
  "The current active target"
  )

(defvar cmany-work-dir nil
  "The directory where the current active target should be debugged/run."
  )

(defconst cmany-cmd-default "cmany {cmd} -E -c clang++ -t Debug {projdir} {target}"
  "The default value for cmany-cmd"
  )

(defvar cmany-cmd cmany-cmd-default
  "The command form to use when calling cmany."
  )

;;-----------------------------------------------------------------------------
(defvar cmany--last-build nil
  "The last build command used by cmany."
  )
(defvar cmany--last-configure nil
  "The last configure command used by cmany."
  )
(defvar cmany--last-debug nil
  "The last debug command used by cmany."
  )
(defvar cmany--current-project nil
  ""
  )

(defvar cmany--with-global-mode nil
  ""
  )
;;-----------------------------------------------------------------------------
(defvar cmany-mode-map
   (let ((map (make-sparse-keymap)))

     (define-key map (kbd "C-c m x ;") 'cmany-switch-proj)
     (define-key map (kbd "C-c m x !") 'cmany-restore-or-guess)
     (define-key map (kbd "C-c m x ?") 'cmany-wizard)
     (define-key map (kbd "C-c m x p") 'cmany-set-proj-dir)
     (define-key map (kbd "C-c m x b") 'cmany-set-build-dir)
     (define-key map (kbd "C-c m x w") 'cmany-set-work-dir)
     (define-key map (kbd "C-c m x t") 'cmany-set-target)
     (define-key map (kbd "C-c m x c") 'cmany-set-cmd)

     (define-key map (kbd "C-c m A") 'cmany-rtags-announce-build-dir)

     (define-key map (kbd "C-c m c") 'cmany-configure)
     (define-key map (kbd "C-c m C") 'cmany-configure-with-prompt)
     (define-key map (kbd "C-c m b") 'cmany-build)
     (define-key map (kbd "C-c m B") 'cmany-build-with-prompt)
     (define-key map (kbd "C-c m d") 'cmany-debug)
     (define-key map (kbd "C-c m D") 'cmany-debug-with-prompt)
     (define-key map (kbd "C-c m r") 'cmany-run)
     (define-key map (kbd "C-c m R") 'cmany-run-with-prompt)

     (define-key map (kbd "C-c m e") 'cmany-edit-cache)

     (define-key map (kbd "C-c m s p") 'cmany-shell-at-proj)
     (define-key map (kbd "C-c m s b") 'cmany-shell-at-build)
     (define-key map (kbd "C-c m s w") 'cmany-shell-at-work)

     map)
   "Key map for the Emacs Lisp cmany environment."
   )

(easy-menu-define cmany-menu cmany-mode-map
  "cmany Mode Menu"
  '("cmany"
    ;;["Documentation" cmany-doc :help "Get documentation for symbol at point"]
    ;;["Run Tests" cmany-test :help "Run test at point, or all tests in the project"]
    ["Configure"             cmany-configure             :keys "C-c m c"   :help "call cmany configure"]
    ["Configure w/prompt"    cmany-configure-with-prompt :keys "C-c m C"   :help "call cmany configure with interactive prompt for the configure command"]
    ["Build"                 cmany-build                 :keys "C-c m b"   :help "call cmany build"]
    ["Build w/prompt"        cmany-build-with-prompt     :keys "C-c m B"   :help "call cmany build with interactive prompt for the build command"]
    ["Debug"                 cmany-debug                 :keys "C-c m d"   :help "open a gdb session with the current target"]
    ["Debug w/prompt"        cmany-debug-with-prompt     :keys "C-c m D"   :help "open a gdb session with the current target with interactive prompt for the build command"]
    ["Run"                   cmany-run                   :keys "C-c m r"   :help "run the current active target"]
    ["Run w/prompt"          cmany-run-with-prompt       :keys "C-c m R"   :help "run the current active target with interactive prompt for the build command"]
    ["Edit cache"            cmany-edit-cache            :keys "C-c m e"   :help "edit the cmake cache of the current project"]
    ["Open shell: proj dir"  cmany-shell-at-proj         :keys "C-c m s p" :help "open a shell session at the current project directory"]
    ["Open shell: build dir" cmany-shell-at-build        :keys "C-c m s d" :help "open a shell session at the current build directory"]
    ["Open shell: work dir"  cmany-shell-at-work         :keys "C-c m s w" :help "open a shell session at the current work directory"]
    "---"
    ("Project params"
    ["Switch project"        cmany-switch-proj      :keys "C-c m x ;" :help "Open a previous project"]
    ["Restore or guess"      cmany-restore-or-guess :keys "C-c m x !" :help "Based on the current buffer, restore project parameters from a previous session, or guess if no session exists"]
    ["Wizard"                cmany-wizard           :keys "C-c m x ?" :help "Run an interactive prompt sequence to configure the project params"]
    ["Set project directory" cmany-set-proj-dir     :keys "C-c m x p" :help "Set the root of the current project"]
    ["Set build directory"   cmany-set-build-dir    :keys "C-c m x b" :help "Set the current build directory"]
    ["Set target"            cmany-set-target       :keys "C-c m x t" :help "Set the current target"]
    ["Set work directory"    cmany-set-work-dir     :keys "C-c m x w" :help "Set the current work directory"]
    ["Set command"           cmany-set-cmd          :keys "C-c m x c" :help "Set the current cmany command"]
    ["rtags: announce directory" cmany-rtags-announce-build-dir :keys "C-c m A" :help "Announce a build directory to the rtags daemon"])
    )
  )

;;;###autoload
(define-minor-mode cmany-mode
  "cmany.el: simple and batched cmake integration"
  :group cmany
  :lighter " cmany"
  :keymap cmany-mode-map
  :after-hook (cmany--mode-hook)
  )

;;;###autoload
(define-globalized-minor-mode global-cmany-mode
  cmany-mode
  (lambda() (cmany--global-mode-hook))
  :group 'cmany
  :keymap cmany-mode-map
  )

(provide 'cmany-mode)
(provide 'global-cmany-mode)

(defun cmany--is-special-buffer (&optional bufname)
  (when (not bufname)
    (setq bufname (buffer-name (current-buffer)))
    )
  (or
   (and (string-prefix-p "*" bufname) (string-suffix-p "*" bufname))
   (and (string-prefix-p " *" bufname) (string-suffix-p "*" bufname))
   )
  )

(defun cmany--mode-hook ()
  "called every time cmany-mode is set"
  (message "mode-hook 0")
  (if cmany--with-global-mode
      (progn
        (cmany--log "mode-hook 1")
        (if (cmany--is-special-buffer)
            (progn
              (cmany--log "mode-hook 2.1: ignoring special buffer %s"
                          (buffer-name (current-buffer)))
              )
            (progn
              (cmany--log "mode-hook 2.2: (possibly) restoring from buffer %s"
                          (buffer-name (current-buffer)))
              (cmany-restore-possibly)
              )
            )
        (cmany--log "mode-hook 4")
        )
    (progn
      (cmany--log "mode-hook 5")
      (cmany-restore-or-guess)
      (cmany--log "mode-hook 6")
      )
    )
  (cmany--log "mode-hook 7")
  )

(defun cmany--global-mode-hook ()
  "called when global-cmany-mode is set"
  (cmany--log "global cmany-mode? %s" (buffer-name (current-buffer)))
  (setq cmany--with-global-mode t)
  (if (cmany--is-special-buffer)
      (progn (cmany--log "no, buffer is special: %s"
                         (buffer-name (current-buffer)))
             )
    (progn
      (when (not cmany-proj-dir)
        (cmany--log "enabling global cmany-mode. current buffer %s" (current-buffer))
        (cmany--log "                          . current file %s" (buffer-file-name))
        (cmany-mode 1)
        )
      )
    )
  )

;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;; utility functions

(defun cmany--log (fmt &rest args)
  (message (apply 'format (concat "cmany[%s]: " fmt) (current-buffer) args))
  (let ((b (current-buffer)))
    (with-current-buffer (get-buffer-create "*cmany*")
      ;;(end-of-buffer)
      (insert (apply 'format (concat "cmany[%s]: " fmt "\n") b args))
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

(defun cmany--write-to-file (file data)
  "http://stackoverflow.com/a/36196312/5875572"
  (with-temp-file file
    (prin1 data (current-buffer))
    )
  )

(defun cmany--read-from-file (file symbol)
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
     (cmany--log "cmd exec: %s" cmd)
     (cd workdir)
     (with-temp-buffer
       (cmany--log "cmd exec dir: %s (was at %s)" (pwd) d)
       (let ((p (call-process-shell-command cmd nil (current-buffer))))
         (cmany--log "cmd return: %d" p)
         (cmany--log "cmd output: %s" (buffer-string))
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
(defun cmany--format-cmd (which-cmd &optional target)
  "return a formatted cmany command based on the current proj/build"
  ;; http://stackoverflow.com/a/17325791/5875572
  (let* ((cmd (replace-regexp-in-string (regexp-quote "{cmd}") which-cmd cmany-cmd nil 'literal)))
    (setq cmd (replace-regexp-in-string (regexp-quote "{projdir}") cmany-proj-dir cmd nil 'literal))
    (when (not target)
      (setq target "")
      )
    (setq cmd (replace-regexp-in-string (regexp-quote "{target}") target cmd nil 'literal))
    cmd
    )
  )

;;-----------------------------------------------------------------------------
(defun cmany--get-cmany-output (cmd &rest more-args)
  (let* ((base-cmd (cmany--format-cmd cmd))
         (full-cmd (concat base-cmd more-args))
         )
    ;;(cmany--log "cmd base: %s" base-cmd)
    ;;(cmany--log "cmd full: %s" full-cmd)
    (cmany--get-cmd-output cmany-proj-dir full-cmd)
    )
  )

(defun cmany--get-cmany-lines (cmd &rest more-args)
  (let ((out (apply 'cmany--get-cmany-output cmd more-args)))
    (split-string out "\n")
    )
  )

;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;; Guessing functions

(defun cmany--guess-proj-dir ()
  (let ((r "")
        (gotcml nil))
    ;; if projectile is available, turned on, and we're in a project,
    ;; get the current projectile project root
    (cmany--log "guess-proj-dir: begin")
    (when (and
           (featurep 'projectile)
           (bound-and-true-p projectile-mode)
           (projectile-project-p))
      (setq r (file-truename (projectile-project-p)))
      )
    (if (cmany--str-not-empty 'r)
        (progn
          (cmany--log "guess-proj-dir: after projectile: %s" r)
          ;; we got project root through projectile
          (setq r (file-name-as-directory r))
          ;; check if there's a CMakeLists.txt there
          (if (file-exists-p (concat r "CMakeLists.txt"))
              (progn
                ;; yep, this is what we want
                (cmany--log "guess-proj-dir: proj dir from projectile: %s" r)
                (setq gotcml t)
                )
            (progn
              (cmany--log "guess-proj-dir: no CMakeLists.txt at proj dir from projectile: %s" r)
              )
            )
          )
      (progn
        (cmany--log "guess-proj-dir: after projectile: (no dir)")
        ;; projectile root not available; go up in the fs tree to find CMakeLists.txt
        (when (buffer-file-name)
          (setq r (locate-dominating-file (buffer-file-name) "CMakeLists.txt"))
          (when r
            (setq r (file-truename r))
            )
        )
        (if (cmany--str-not-empty 'r)
            (progn
              ;; yep, found a CMakeLists.txt
              (cmany--log "guess-proj-dir: proj dir from locate-dominating-file: %s" r)
              (setq r (file-name-as-directory r))
              (setq gotcml t)
              )
          (progn
            ;; otherwise, try the current directory
            (when (buffer-file-name)
              (setq r (file-name-directory (buffer-file-name)))
            )
            (if (and r (file-exists-p (concat r "CMakeLists.txt")))
                (progn
                  (cmany--log "guess-proj-dir: proj dir from current dir: %s" r)
                  (setq gotcml t)
                  )
              (progn
                (cmany--log "guess-proj-dir: no CMakeLists.txt at current dir %s" r)
                )
              )
            )
          )
        )
      )
    (when (not gotcml)
      ;; Is r a subdir of any known cmany project?
      (dolist (p (cmany--get-known-projects))
        (let ((rel (file-relative-name r p)))
          (cmany--log "guess-proj-dir: is %s a subdir of %s? %s" r p rel)
          (when (not (file-name-absolute-p rel))
            (cmany--log "guess-proj-dir: %s is a subdir of %s" r p)
            (setq gotcml t)
            )
          )
        )
      (when (not gotcml)
        (setq r nil)
        )
      )
    (cmany--log "guess-proj-dir: end %s" r)
    r ;; return the result
    )
  )

(defun cmany--get-default-proj-dir ()
  (if (cmany--str-not-empty 'cmany-proj-dir)
      ;; if there's a current cmany-proj-dir, use it
      (progn
        (cmany--log "cmany-proj-dir already defined: %s" cmany-proj-dir)
        cmany-proj-dir)
      ;; otherwise, make a guess
    (cmany--guess-proj-dir)
    )
  )

(defun cmany--guess-build-dir ()
  (interactive)
  (let* ((bds (cmany--get-cmany-lines "show_build_dirs"))  ;; extract the list of current builds
         (bd (car bds)) ;; pick the first
         )
    (setq bd (file-truename bd))
    (cmany--log "build directory guess: %s" bd)
    (file-name-as-directory bd)
    )
  )

(defun cmany--work-dir-or-default ()
  (if (cmany--str-not-empty 'cmany-work-dir)
      cmany-work-dir
      cmany-build-dir
      )
  )

;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;; Prompting functions

(defun cmany--exec-prompt-proj-dir ()
  (interactive)
  (let* ((prompt "cmany proj dir: ")
         (dn (file-name-directory (cmany--get-default-proj-dir)))
         (bn (file-name-base dn))
         (result (ido-read-directory-name prompt dn bn nil bn))
         )
    (setq result (file-truename result))
    (cmany--log "prompt for %s%s" prompt result)
    (file-name-as-directory result)
    )
  )

(defun cmany--exec-prompt-build-dir ()
  (interactive)
  (let* ((prompt "cmany build dir: ")
         (dn (file-name-directory (cmany--guess-build-dir)))
         (bn (file-name-base dn))
         (result (ido-read-directory-name prompt dn bn nil bn))
         )
    (setq result (file-truename result))
    (cmany--log "prompt for %s%s" prompt result)
    (file-name-as-directory result)
    )
  )

(defun cmany--exec-prompt-work-dir ()
  (interactive)
  (let* ((prompt "cmany work dir: ")
         (dn (file-name-directory (cmany--work-dir-or-default)))
         (bn (file-name-base dn))
         (result (ido-read-directory-name prompt dn bn nil bn))
         )
    (setq result (file-truename result))
    (cmany--log "prompt for %s%s" prompt result)
    (file-name-as-directory result)
    )
  )

;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;; RTags
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html

(defun cmany-rtags-start ()
  "start the rtags daemon"
  (interactive)
  (when (and cmany-rtags-enabled (featurep 'rtags))
    ;;(if (not (boundp 'cmany--rtags-rdm))
    ;;    (progn
    ;;      (cmany--log "starting rdm")
    ;;      (setq cmany--rtags-rdm (start-process "cmany-rtags-rdm" "*RTags Log*" "rdm")))
    ;;  (progn (cmany--log "rdm is already running")))
    ;;)
    (rtags-start-process-unless-running)
    )
  )

(defun cmany-rtags-announce-build-dir (&optional dir)
  (interactive
   (list (if cmany-rtags-enabled
             (ido-read-directory-name "build dir: " cmany-build-dir)
             ""
           )
         )
   )
  (when (and cmany-rtags-enabled (featurep 'rtags))
    (cmany-rtags-start)
    (start-process (concat "cmany-rtags-rc" " " dir) "*RTags Log*" "rc" "-J" dir)
    )
  )


;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;; Config functions

;;;###autoload
(defun cmany-load-configs ()
  "loads configs"
  (interactive)
  (let ((fn (concat user-emacs-directory "cmany.save")))
    (if (file-exists-p fn)
        (progn
          (cmany--log "loading configs from %s" fn)
          (cmany--read-from-file fn 'cmany--configs)
          ;(message "cmany loaded configs: %s" cmany--configs)
          )
      (progn
        (cmany--log "configs file not found: %s" fn)
        (cmany--log "starting with empty config")
        (setq cmany--configs ())
        )
    )
    (cmany--get-known-projects)
    )
  )

;;;###autoload
(defun cmany-load-configs-if-none ()
  "loads configs if they are not yet available"
  (interactive)
  (when (not (cmany--var-not-nil 'cmany--configs))
    (cmany-load-configs)
    )
  )

;;;###autoload
(defun cmany-save-configs ()
  (interactive)
  (cmany-load-configs-if-none)
  (when (not (boundp 'cmany--configs))
    (setq cmany--configs ())
    )
  (let ((pd (file-truename cmany-proj-dir))
        (pc
         `(("cmany-build-dir" . ,cmany-build-dir)
           ("cmany-target" . ,cmany-target)
           ("cmany-cmd" . ,cmany-cmd)
           ("cmany-work-dir" . ,cmany-work-dir)
           ("cmany--last-configure" . ,cmany--last-configure)
           ("cmany--last-build" . ,cmany--last-build)
           ("cmany--last-debug" . ,cmany--last-debug)
           )
         )
        (tmp ()) ;; we'll put the new list here
        )
    (add-to-list 'tmp (cons pd pc)) ;; put the new config in first place
    (dolist (c cmany--configs)
      (when (not (string-equal (car c) pd))
        (add-to-list 'tmp c) ;; add more configs if they're not the same
        )
      )
    (setq cmany--configs tmp) ;; done
    )
  (cmany--write-to-file
   (concat user-emacs-directory "cmany.save")
   cmany--configs)
  (setq cmany--known-projects ())
  (cmany--get-known-projects)
  )

;;;###autoload
(defun cmany-restore-config (dir)
  (interactive
   (list
    (file-name-as-directory
     (ido-read-directory-name "cmake proj dir to restore: " (cmany--guess-proj-dir)))))
  (cmany--log "restore-config: begin: %s" dir)
  (if (and dir (boundp 'cmany--configs))
      (progn
        (cmany--log "restore-config: configs available")
        (setq dir (file-truename dir))
        (let ((c (cdr (assoc dir cmany--configs))))
          (if c
              (progn
                (cmany--log "restore-config: found config for %s: %s" dir c)
                (cmany-set-proj-dir dir t)
                (cmany-set-build-dir (cdr (assoc "cmany-build-dir" c)) t)
                (cmany-set-target (cdr (assoc "cmany-target" c)) t)
                (cmany-set-cmd (cdr (assoc "cmany-cmd" c)) t)
                (cmany-set-work-dir (cdr (assoc "cmany-work-dir" c)) t)
                (setq cmany--last-configure (cdr (assoc "cmany--last-configure" c)))
                (setq cmany--last-build (cdr (assoc "cmany--last-build" c)))
                (setq cmany--last-debug (cdr (assoc "cmany--last-debug" c)))
                (cmany--log "restore-config: end. found config for %s" dir)
                t ;; return true to signal loaded config
                )
            (progn
              (cmany--log "restore-config: end. no config was found for %s" dir)
              nil ;; no config was found for this dir
              )
            )
          )
        )
    (progn
      (when (not dir)
        (cmany--log "restore-config: end. no dir with CMakeLists.txt was found")
        )
      (when (not (boundp 'cmany--configs))
        (cmany--log "restore-config: end. no configs available")
        )
      nil ;; no configs are available
      )
    )
  )

(defun cmany--clear-last-commands ()
  "clears the last stored commands. call whenever a build param changes."
  (setq cmany--last-build "")
  (setq cmany--last-configure "")
  (setq cmany--last-debug "")
  )

(defun cmany--get-known-projects ()
  (when (or (not (boundp 'cmany--known-projects))
            (not cmany--known-projects))
    (setq cmany--known-projects (list))
    (when (boundp 'cmany--configs)
      (dolist (p cmany--configs)
        (add-to-list 'cmany--known-projects (car p))
        )
      )
    )
  cmany--known-projects
  )

;;-----------------------------------------------------------------------------

;;;###autoload
(defun cmany-switch-proj ()
  (interactive)
  (let ((p (ido-completing-read "choose a project: " (cmany--get-known-projects))))
    (when p (cmany-restore-config p))
    )
  )

;;;###autoload
(defun cmany-set-proj-dir (&optional dir no-save)
  "set the project dir used by cmany"
  (interactive
   (list (call-interactively 'cmany--exec-prompt-proj-dir)
         nil))
  (cmany--log "set proj dir: %s" dir)
  (setq cmany-proj-dir dir)
  (cmany--clear-last-commands)
  (when (not no-save)
    (cmany-load-configs-if-none)
    (cmany-save-configs)
    )
  )

;;;###autoload
(defun cmany-set-build-dir (&optional dir no-save)
  "set the build dir used by cmany"
  (interactive
   (list (call-interactively 'cmany--exec-prompt-build-dir)
         nil))
  (cmany--log "set build dir: %s" dir)
  (setq cmany-build-dir dir)
  (cmany--clear-last-commands)
  (when (not no-save)
    (cmany-load-configs-if-none)
    (cmany-save-configs)
    )
  (cmany-rtags-announce-build-dir cmany-build-dir)
  )

;;;###autoload
(defun cmany-set-cmd (&optional cmd no-save)
  "set the cmany command form"
  (interactive
   (list (read-string "cmany command: " cmany-cmd)
         nil))
  (cmany--log "set command: %s" cmd)
  (setq cmany-cmd cmd)
  (cmany--clear-last-commands)
  (when (not no-save)
    (cmany-load-configs-if-none)
    (cmany-save-configs)
    )
  )

;;;###autoload
(defun cmany-set-target (&optional tgt no-save)
  "set the current active target for building and running/debugging"
  (interactive
   (list (ido-completing-read
          "cmany current target: "
          (cmany--get-cmany-lines "show_targets") nil nil cmany-target)
         nil))
  (cmany--log "set target: %s" tgt)
  (setq cmany-target tgt)
  (cmany--clear-last-commands)
  (when (not no-save)
    (cmany-load-configs-if-none)
    (cmany-save-configs)
    )
  )

;;;###autoload
(defun cmany-set-work-dir (&optional dir no-save)
  "set the work directory for the current active target"
  (interactive
   (list (call-interactively 'cmany--exec-prompt-work-dir)
         nil))
  (cmany--log "set work dir: %s" dir)
  (setq cmany-work-dir dir)
  (cmany--clear-last-commands)
  (when (not no-save)
    (cmany-load-configs-if-none)
    (cmany-save-configs)
    )
  )


;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-configure-with-prompt (cmd)
  (interactive
   (list
    (read-string
     "enter configure cmd: "
     (if (cmany--str-not-empty 'cmany--last-configure)
         (progn cmany--last-configure)
         (progn (cmany--format-cmd "configure"))
         )
     )))
  (setq cmany--last-configure cmd)
  (cmany-save-configs)
  (let ((d default-directory))
    (cd cmany-proj-dir)
    (compile cmd)
    (cd d)
    )
  )

;;;###autoload
(defun cmany-configure()
  (interactive)
  (if (cmany--str-not-empty 'cmany--last-configure)
      (cmany-configure-with-prompt cmany--last-configure)
    (call-interactively 'cmany-configure-with-prompt)
    )
  )

;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-build-with-prompt (cmd)
  "build the current target"
  (interactive
   (list
    (read-string
     "enter build cmd: "
     (if (cmany--str-not-empty 'cmany--last-build)
         (progn cmany--last-build)
         (progn (cmany--format-cmd "build" cmany-target))
         )
     )))
  (setq cmany--last-build cmd)
  (cmany-save-configs)
  (let ((d default-directory))
    (cd cmany-proj-dir)
    (compile cmd)
    (cd d)
    )
  )

;;;###autoload
(defun cmany-build()
  (interactive)
  (if (cmany--str-not-empty 'cmany--last-build)
      (cmany-build-with-prompt cmany--last-build)
      (call-interactively 'cmany-build-with-prompt)
    )
  )

;;-----------------------------------------------------------------------------

;;;###autoload
(defun cmany-debug-with-prompt (cmd &optional workdir)
  "debug the current target"
  (interactive
   (list
    (read-string
     "enter gdb cmd: "
     (if (cmany--str-not-empty 'cmany--last-debug)
         (progn cmany--last-debug)
         (progn (format "gdb -i=mi %s" (concat cmany-build-dir cmany-target)))
         )
     )
    (let* ((def (cmany--work-dir-or-default))
           (dn (file-name-directory def))
           (bn (file-name-base def)))
      (ido-read-directory-name "work dir: " dn bn nil bn)
      )
    )
   )
  (setq cmany--last-debug cmd)
  (cmany-save-configs)
  (if (not workdir)
      (setq workdir cmany-build-dir))
  (if cmany-build-before-run
      (progn
        (setq cmany--dbg-work-dir workdir)
        (setq cmany--dbg-cmd cmd)
        ;; WTF??? when this is uncommented our function never gets called.
        ;; gotta learn why.
        ;;(setq cmany--dbg-old-fn (symbol-function compilation-exit-message-function))
        (setq compilation-exit-message-function 'cmany--dbg-after-compile)
        (compile (concat "cd " cmany-proj-dir " ; " (cmany--format-cmd "build" cmany-target)))
        ;;(setq compilation-exit-message-function cmany--dbg-old-fn)
        )
    (progn
      (let ((d default-directory))
        (cd workdir)
        (call-interactively (gdb cmpcmd))
         (cd d)
          )
      )
    )
  )

(defun cmany--dbg-after-compile (status code msg)
  (cmany--log "compilation finished with status %s" status)
  (cmany--log "compilation finished with code %d" code)
  (cmany--log "compilation finished with msg %s" msg)
  (if (and (eq status 'exit) (zerop code))
      (progn
        (let ((d default-directory))
          (when (not (equal d cmany--dbg-work-dir))
            (cmany--log "changing to directory %s" cmany--dbg-work-dir)
            )
          (cd cmany--dbg-work-dir)
          (gdb cmany--dbg-cmd)
          (when (not (equal d cmany--dbg-work-dir))
            (cmany--log "returning to directory %s" d)
            )
          (cd d)
          )
        )
    (progn
      (tooltip-show "\n            :-(            \n\nCompilation failed.\n"))
    )
  )

;;;###autoload
(defun cmany-debug()
  (interactive)
  (if (cmany--str-not-empty 'cmany--last-debug)
      (cmany-debug-with-prompt cmany--last-debug)
    (call-interactively 'cmany-debug-with-prompt)
    )
  )

;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-run-with-prompt (cmd &optional workdir)
  "run the current target"
  (interactive
   (list
    (read-string
     "enter cmd: "
     (if (cmany--str-not-empty 'cmany--last-run)
         (progn cmany--last-run)
         (progn (concat cmany-build-dir cmany-target))
       )
     )
    (ido-read-directory-name "work dir: " (cmany--work-dir-or-default))
    )
   )
  (setq cmany--last-run cmd)
  (when (not workdir)
    (setq workdir (cmany--work-dir-or-default))
    )
  (if cmany-build-before-run
      (setq cmd (concat
                 "cd " cmany-proj-dir " ; " (cmany--format-cmd "build" cmany-target)
                 " && cd " workdir " ; " cmd
                 )
            )
    (setq cmd (concat "cd " workdir " ; "  cmd))
    )
  (compile cmd)
  )

;;;###autoload
(defun cmany-run()
  (interactive)
  (if (cmany--str-not-empty 'cmany--last-run)
      (cmany-run-with-prompt cmany--last-run)
    (call-interactively 'cmany-run-with-prompt)
    )
  )

;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-edit-cache ()
  "interactively edit the cmake cache for the current build
  directory using either ccmake or cmake-gui"
  (interactive)
  (let ((d default-directory))
    (cmany-save-configs)
    (cd cmany-build-dir)
    (if (and nil (executable-find "ccmake"))
        (progn
          (cmany--log "editing cache via ccmake at %s" cmany-build-dir)
          (term-run "ccmake" (cmany--visit-buffer "*ccmake*") ".")
          )
      (progn
        (if (executable-find "cmake-gui")
            (progn (cmany--log "editing cache via cmake-gui at %s" cmany-build-dir)
                   (start-process "cmake-gui" nil "cmake-gui" "."))
          (progn (error "ERROR: could not find ccmake or cmake-gui in the exec-path: %s" exec-path))
          )
        )
      )
    (cd d)
    )
  )

;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-shell-at-proj ()
  "open a shell in the current project dir"
  (interactive)
  (assert (file-exists-p cmany-proj-dir))
  (let ((d default-directory))
    (cmany-save-configs)
    (cd cmany-proj-dir)
    (shell)
    (cd d)
    )
  )

;;;###autoload
(defun cmany-shell-at-build ()
  "open a shell in the current build dir"
  (interactive)
  (assert (file-exists-p cmany-build-dir))
  (if (not))
  (let ((d default-directory))
    (cmany-save-configs)
    (cd cmany-build-dir)
    (shell)
    (cd d)
    )
  )

;;;###autoload
(defun cmany-shell-at-work ()
  "open a shell in the current work dir"
  (interactive)
  (assert (file-exists-p cmany-work-dir))
  (if (not))
  (let ((d default-directory))
    (cmany-save-configs)
    (cd cmany-work-dir)
    (shell)
    (cd d)
    )
  )

;;-----------------------------------------------------------------------------

;;;###autoload
(defun cmany-show-configuration (&optional msg)
  (interactive (list nil))

  (cmany--log "----------------------------")
  (when (cmany--str-not-empty 'msg) (cmany--log "%s:" msg))
  (cmany--log "  cmany-proj-dir: %s" cmany-proj-dir)
  (cmany--log "  cmany-build-dir: %s" cmany-build-dir)
  (cmany--log "  cmany-cmd: %s" cmany-cmd)
  (cmany--log "  cmany-target: %s" cmany-target)
  (cmany--log "  cmany-work-dir: %s" cmany-work-dir)
  (cmany--log "----------------------------")

  (message "cmany: %scurrent config: %s%s"
           (if (cmany--str-not-empty 'msg) "" (format "%s: " msg))
           cmany-build-dir
           (if (string-equal cmany-target "") "" (format " [%s]" cmany-target))
           )
  )

;;;###autoload
(defun cmany-wizard ()
  "interactively configure the cmany params: project dir, cmd
form, build dir and active target"
  (interactive)
  (call-interactively 'cmany-set-proj-dir)
  (call-interactively 'cmany-set-cmd)
  (call-interactively 'cmany-set-build-dir)
  (call-interactively 'cmany-set-target)
  (call-interactively 'cmany-set-work-dir)

  (cmany-show-configuration "configuration from wizard")
  )

;;;###autoload
(defun cmany-guess (&optional dir)
  "make automatic guesses the cmany params"
  (interactive (list (cmany--guess-proj-dir)))
  (when (not dir)
    (setq dir (cmany--guess-proj-dir)))
  (cmany-set-proj-dir dir t)
  (cmany-set-cmd cmany-cmd-default t)
  (cmany-set-build-dir (cmany--guess-build-dir) t)
  (cmany-set-target "" t)
  (cmany-set-work-dir cmany-build-dir t)

  (cmany-show-configuration "guessed configuration")
  )

;;;###autoload
(defun cmany-restore-possibly ()
  "make automatic guesses of the cmany params"
  (interactive)
  (cmany--log "cmany-restore-possibly: begin")
  (cmany-load-configs-if-none)
  (cmany--log "cmany-restore-possibly: configs loaded!")
  (let ((dir (cmany--guess-proj-dir)))
    (cmany--log "cmany-restore-possibly: guessed directory: %s" dir)
    (let ((restore-ok  (cmany-restore-config dir)))
      (if restore-ok
          (progn
            (cmany-show-configuration "restored configuration")
            (cmany--log "cmany-restore-possibly: end. restored configuration: %s" dir)
            dir
            )
        (progn
          (cmany--log "cmany-restore-possibly: end. no luck with dir: %s" dir)
          nil
          )
        )
      )
    )
  )

;;;###autoload
(defun cmany-restore-or-guess ()
  "make automatic guesses of the cmany params"
  (interactive)
  (cmany--log "cmany-restore-or-guess: begin")
  (let ((dir (cmany-restore-possibly)))
    (when (not dir)
      (cmany--log "cmany-restore-or-guess: really need to guess")
      (cmany-guess)
      )
    )
  (cmany--log "cmany-restore-or-guess: end")
  )
