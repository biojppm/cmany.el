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

;;-----------------------------------------------------------------------------
(defvar cmany-mode-map
   (let ((map (make-sparse-keymap)))

     (define-key map (kbd "C-c m ?") 'cmany-wizard)
     (define-key map (kbd "C-c m !") 'cmany-restore-or-guess)
     (define-key map (kbd "C-c m P") 'cmany-set-proj-dir)
     (define-key map (kbd "C-c m D") 'cmany-set-build-dir)
     (define-key map (kbd "C-c m T") 'cmany-set-target)
     (define-key map (kbd "C-c m W") 'cmany-set-work-dir)
     (define-key map (kbd "C-c m K") 'cmany-set-cmd)

     (define-key map (kbd "C-c m A") 'cmany-rtags-announce-build-dir)

     (define-key map (kbd "C-c m C") 'cmany-configure)
     (define-key map (kbd "C-c m c") 'cmany-configure-again)
     (define-key map (kbd "C-c m B") 'cmany-build)
     (define-key map (kbd "C-c m b") 'cmany-build-again)
     (define-key map (kbd "C-c m R") 'cmany-run)
     (define-key map (kbd "C-c m r") 'cmany-run-again)
     (define-key map (kbd "C-c m G") 'cmany-debug)
     (define-key map (kbd "C-c m g") 'cmany-debug-again)

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
    ["Configure"             cmany-configure       :keys "C-c m C"   :help "call cmany configure using the current project params"]
    ["Configure again"       cmany-configure-again :keys "C-c m c"   :help "call cmany configure using the settings of the previous configure"]
    ["Build"                 cmany-build           :keys "C-c m B"   :help "call cmany build using the current project params"]
    ["Build again"           cmany-build-again     :keys "C-c m b"   :help "call cmany build using the settings of the previous build"]
    ["Run"                   cmany-run             :keys "C-c m R"   :help "run the current active target"]
    ["Run again"             cmany-run-again       :keys "C-c m r"   :help "run the current active target using the settings of the previous run"]
    ["Debug"                 cmany-debug           :keys "C-c m G"   :help "open a gdb session with the current target"]
    ["Debug-again"           cmany-debug-again     :keys "C-c m g"   :help "open a gdb session with the current target using the settings of the previous debug"]
    ["Edit cache"            cmany-edit-cache      :keys "C-c m e"   :help "edit the cmake cache using the current project params"]
    ["Open shell: proj dir"  cmany-shell-at-proj   :keys "C-c m s p" :help "open a shell session at the current project directory"]
    ["Open shell: build dir" cmany-shell-at-build  :keys "C-c m s d" :help "open a shell session at the current build directory"]
    ["Open shell: work dir"  cmany-shell-at-work   :keys "C-c m s w" :help "open a shell session at the current work directory"]
    "---"
    ("Project params"
    ["Wizard"                cmany-wizard           :keys "C-c m ?" :help "Run an interactive wizard to configure the project params"]
    ["Restore or guess"      cmany-restore-or-guess :keys "C-c m !" :help "Based on the current buffer, restore project parameters from a previous session, or guess if no session exists"]
    ["Set project directory" cmany-set-proj-dir     :keys "C-c m P" :help "Set the current project directory"]
    ["Set build directory"   cmany-set-build-dir    :keys "C-c m D" :help "Set the current build directory"]
    ["Set target"            cmany-set-target       :keys "C-c m T" :help "Set the current target"]
    ["Set work directory"    cmany-set-work-dir     :keys "C-c m W" :help "Set the current work directory"]
    ["Set command"           cmany-set-target       :keys "C-c m K" :help "Set the current cmany command"]
    ["rtags: announce directory" cmany-rtags-announce-build-dir :keys "C-c m A" :help "Announce a build directory to the rtags daemon"])
    )
  )

;;;###autoload
(define-minor-mode cmany-mode
  "cmany.el: simple and batched cmake integration"
  :group cmany
  :lighter " cmany"
  :keymap cmany-mode-map
  :after-hook (cmany-restore-or-guess)
  )

;;;###autoload
(define-globalized-minor-mode global-cmany-mode
  cmany-mode
  (lambda (arg)
    (cmany-mode arg))
  :group 'cmany
  )

;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;; utility functions

(defun cmany--log (fmt &rest args)
  ;;(message (apply 'format (concat "cmany[%s]: " fmt "\n") (current-buffer) args))
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
(defun cmany--guess-proj-dir ()
  (let ((r "")
        (gotcml nil))
    ;; if projectile is available, turned on and we're in a project,
    ;; get the current projectile project root
    (cmany--log "AQUI 0")
    (when (and
           (featurep 'projectile)
           (bound-and-true-p projectile-mode)
           (projectile-project-p))
      (setq r (file-truename (projectile-project-p)))
      )
    (cmany--log "AQUI 1")
    (if (cmany--str-not-empty 'r)
        (progn
          ;; we got project root through projectile
          (setq r (file-name-as-directory r))
          ;; check if there's a CMakeLists.txt there
          (if (file-exists-p (concat r "CMakeLists.txt"))
              (progn
                ;; yep, this is what we want
                (cmany--log "proj dir from projectile: %s" r)
                (setq gotcml t)
                )
            (progn
              (cmany--log "no CMakeLists.txt at proj dir from projectile: %s" r)
              )
            )
          )
      (progn
        ;; projectile root not available; go up in the fs tree to find CMakeLists.txt
        (setq r (file-truename (locate-dominating-file (buffer-file-name) "CMakeLists.txt")))
        (if (cmany--str-not-empty 'r)
            (progn
              ;; yep, found a CMakeLists.txt
              (cmany--log "proj dir from locate-dominating-file: %s" r)
              (setq r (file-name-as-directory r))
              (setq gotcml t)
              )
          (progn
            ;; otherwise, try the current directory
            (setq r (file-name-directory (buffer-file-name)))
            (if (file-exists-p (concat r "CMakeLists.txt"))
                (progn
                  (cmany--log "proj dir from current dir: %s" r)
                  (setq gotcml t)
                  )
              (progn
                (cmany--log "no CMakeLists.txt at current dir %s" r)
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
          (cmany--log "is %s a subdir of %s? %s" r p rel)
          (when (not (file-name-absolute-p rel))
            (cmany--log "%s is a subdir of %s" r p)
            (setq gotcml t)
            )
          )
        )
      (when (not gotcml)
        (setq r nil)
        )
      )
    (cmany--log "AQUI 2")
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
  (let* ((pfx (concat cmany-proj-dir cmany-build-dir-prefix)) ;; the full path to the builds dir
         (bds (cmany--get-cmany-lines "show_builds"))  ;; extract the list of current builds
         (bdf (car bds)) ;; pick the first
         (bd (concat pfx bdf)))
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

(defun cmany--exec-prompt-proj-dir ()
  (interactive)
  (let* ((prompt "cmany proj dir: ")
         (dn (file-name-directory (cmany--get-default-proj-dir)))
         (bn (file-name-base dn))
         (result (ido-read-directory-name prompt dn bn nil bn))
         )
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
    (cmany--log "prompt for %s%s" prompt result)
    (file-name-as-directory result)
    )
  )

;;-----------------------------------------------------------------------------
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html

(defun cmany-rtags-start ()
  "start the rtags daemon"
  (interactive)
  (when (and cmany-rtags-enabled (featurep 'rtags))
    ;;(if (not (boundp 'cmany--rtags-rdm))
    ;;    (progn
    ;;      (cmany--log "starting rdm")
    ;;      (setq cmany--rtags-rdm (start-process "cmany-rtags-rdm" "*rdm*" "rdm")))
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
    (start-process (concat "cmany-rtags-rc" " " dir) "*rdm*" "rc" "-J" dir)
    )
  )

;;-----------------------------------------------------------------------------

;;;###autoload
(defun cmany-load-configs ()
  "loads configs"
  (interactive)
  (let ((fn (concat user-emacs-directory "cmany.save")))
    (when (file-exists-p fn)
          (cmany--log "loading configs from %s" fn)
          (cmany--read-from-file fn 'cmany--configs)
          ;(message "cmany loaded configs: %s" cmany--configs)
          )
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
  ;;(cmany--log "configs before: %s" cmany--configs)
  (let ((pc
         `(("cmany-build-dir" . ,cmany-build-dir)
           ("cmany-target" . ,cmany-target)
           ("cmany-cmd" . ,cmany-cmd)
           ("cmany-work-dir" . ,cmany-work-dir))
         ))
    (when (not (boundp 'cmany--configs))
        (setq cmany--configs ())
      )
    ;; http://emacs.stackexchange.com/questions/9328/looking-for-a-simple-way-to-update-an-alist-without-introducing-degeneracies
    ;; to update (key,val) from an alist...
    ;; ... remove (key,val)
    (setq cmany--configs (assq-delete-all cmany-proj-dir cmany--configs))
    ;;(cmany--log "configs before: %s" cmany--configs)
    ;; ... and set it with the new value
    (add-to-list 'cmany--configs (cons cmany-proj-dir pc))
    ;;(setq cmany--configs `((,cmany-proj-dir ,pc)))
    )
  ;;(cmany--log "configs after: %s" cmany--configs)
  (cmany--write-to-file
   (concat user-emacs-directory "cmany.save")
   cmany--configs)
  )

;;;###autoload
(defun cmany-restore-config (dir)
  (interactive
   (list
    (file-name-as-directory
     (ido-read-directory-name "cmake proj dir to restore: " (cmany--guess-proj-dir)))))
  (if (boundp 'cmany--configs)
      (progn
        (let ((c (cdr (assoc dir cmany--configs))))
          (if c
              (progn
                (cmany-set-proj-dir dir t)
                (cmany-set-build-dir (cdr (assoc "cmany-build-dir" c)) t)
                (cmany-set-target (cdr (assoc "cmany-target" c)) t)
                (cmany-set-cmd (cdr (assoc "cmany-cmd" c)) t)
                (cmany-set-work-dir (cdr (assoc "cmany-work-dir" c)) t)
                t ;; return true to signal loaded config
                )
            nil ;; no config was found for this dir
            )
          )
        )
    (progn
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

;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-set-proj-dir (&optional dir no-save)
  "set the project dir used by cmany"
  (interactive
   (list (call-interactively 'cmany--exec-prompt-proj-dir)
         nil))
  (cmany-load-configs-if-none)
  (cmany--log "set proj dir: %s" dir)
  (setq cmany-proj-dir dir)
  (cmany--clear-last-commands)
  (when (not no-save) (cmany-save-configs))
  )

;;;###autoload
(defun cmany-set-build-dir (&optional dir no-save)
  "set the build dir used by cmany"
  (interactive
   (list (call-interactively 'cmany--exec-prompt-build-dir)
         nil))
  (cmany-load-configs-if-none)
  (cmany--log "set build dir: %s" dir)
  (setq cmany-build-dir dir)
  (cmany--clear-last-commands)
  (when (not no-save) (cmany-save-configs))
  (cmany-rtags-announce-build-dir cmany-build-dir)
  )

;;;###autoload
(defun cmany-set-cmd (&optional cmd no-save)
  "set the cmany command form"
  (interactive
   (list (read-string "cmany command: " cmany-cmd)
         nil))
  (cmany-load-configs-if-none)
  (cmany--log "set command: %s" cmd)
  (setq cmany-cmd cmd)
  (cmany--clear-last-commands)
  (when (not no-save) (cmany-save-configs))
  )

;;;###autoload
(defun cmany-set-target (&optional tgt no-save)
  "set the current active target for building and running/debugging"
  (interactive
   (list (ido-completing-read
          "cmany current target: "
          (cmany--get-cmany-lines "show_targets") nil nil cmany-target)
         nil))
  (cmany-load-configs-if-none)
  (cmany--log "set target: %s" tgt)
  (setq cmany-target tgt)
  (cmany--clear-last-commands)
  (when (not no-save) (cmany-save-configs))
  )

;;;###autoload
(defun cmany-set-work-dir (&optional dir no-save)
  "set the work directory for the current active target"
  (interactive
   (list (call-interactively 'cmany--exec-prompt-work-dir)
         nil))
  (cmany-load-configs-if-none)
  (cmany--log "set work dir: %s" dir)
  (setq cmany-work-dir dir)
  (cmany--clear-last-commands)
  (when (not no-save) (cmany-save-configs))
  )

;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-configure (cmd)
  (interactive
   (list
    (read-string
     "enter configure cmd: "
     (if (cmany--str-not-empty 'cmany--last-configure)
         (progn cmany--last-configure)
         (progn (cmany--format-cmd "configure"))
         )
     )))
  (cmany-save-configs)
  (setq cmany--last-configure cmd)
  (let ((d default-directory))
    (cd cmany-proj-dir)
    (compile cmd)
    (cd d)
    )
  )

;;;###autoload
(defun cmany-configure-again()
  (interactive)
  (if (cmany--str-not-empty 'cmany--last-configure)
      (cmany-configure cmany--last-configure)
    (error "cmany-configure was not run yet")
    )
  )

;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-build (cmd)
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
  (cmany-save-configs)
  (setq cmany--last-build cmd)
  (let ((d default-directory))
    (cd cmany-proj-dir)
    (compile cmd)
    (cd d)
    )
  )

;;;###autoload
(defun cmany-build-again()
  (interactive)
  (if (cmany--str-not-empty 'cmany--last-build)
      (cmany-build cmany--last-build)
    (error "cmany-build was not run yet")
    )
  )

;;-----------------------------------------------------------------------------

;;;###autoload
(defun cmany-debug (cmd &optional workdir)
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
  (cmany-save-configs)
  (setq cmany--last-debug cmd)
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
          (cd cmany--dbg-work-dir)
          (gdb cmany--dbg-cmd)
          (cd d)
          )
        )
    (progn
      (tooltip-show "\n            :-(            \n\nCompilation failed.\n"))
    )
  )

;;;###autoload
(defun cmany-debug-again()
  (interactive)
  (if (cmany--str-not-empty 'cmany--last-debug)
      (cmany-debug cmany--last-debug)
    (error "cmany-debug was not run yet")
    )
  )

;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-run (cmd &optional workdir)
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
(defun cmany-run-again()
  (interactive)
  (message "AGAIN")
  (if (cmany--str-not-empty 'cmany--last-run)
      (cmany-run cmany--last-run)
    (error "cmany-run was not run yet")
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
    (if (executable-find "ccmake")
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
(defun cmany-show-configuration (&optional msg full)
  (interactive (list nil t))
  (if full
      (progn
        (cmany--log "----------------------------")
        (when (not (equal msg nil)) (cmany--log "%s:" msg))
        (cmany--log "  cmany-proj-dir: %s" cmany-proj-dir)
        (cmany--log "  cmany-build-dir: %s" cmany-build-dir)
        (cmany--log "  cmany-cmd: %s" cmany-cmd)
        (cmany--log "  cmany-target: %s" cmany-target)
        (cmany--log "  cmany-work-dir: %s" cmany-work-dir)
        (cmany--log "----------------------------")
        )
    (progn
      (cmany--log "%scurrent config: %s%s"
                  (if (equal msg nil) "" (format "%s: " msg))
                  cmany-build-dir
                  (if (string-equal cmany-target "") "" (format " [%s]" cmany-target)))
      )
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

  (cmany-show-configuration "configuration from wizard")
  )

;;;###autoload
(defun cmany-restore-or-guess ()
  "make automatic guesses of the cmany params"
  (interactive)
  (cmany--log "guessing configuration...")
  (cmany-load-configs-if-none)
  (cmany--log "configs loaded!")
  (setq dir (cmany--guess-proj-dir))
  (if (cmany-restore-config dir)
      (progn
        (cmany-show-configuration "restored configuration" t)
        )
    (progn
      (cmany--log "really need to guess")
      (cmany-set-proj-dir dir t)
      (cmany-set-cmd cmany-cmd-default t)
      (cmany-set-build-dir (cmany--guess-build-dir) t)
      (cmany-set-target "" t)
      (cmany-show-configuration "guessed configuration" t)
      )
    )
  (cmany-show-configuration)
  )

(provide 'cmany-mode)
