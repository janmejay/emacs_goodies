(require 'easymenu)
(require 'cl)

(require 'higlight-errors nil t)

(require 'test-runner-image)

;; when running from other buffer, set default-dir in run
;; make clean on exit
;; http://www.emacswiki.org/cgi-bin/emacs/unit-test.el

(defface test-case-success-face
  '((t (:background "dark olive green")))
  "Face used for displaying a successful test result."
  :group 'test-faces)

(defface test-case-success-but-modified-face
  '((t (:background "dark olive green")))
  "Face used for displaying a successful test result in a modified buffer."
  :group 'test-faces)

(defface test-case-failure-face
  '((t (:background "firebrick")))
  "Face used for displaying a failed test result."
  :group 'test-faces)

(defface test-case-undetermined-face
  '((t (:background "orange")))
  "Face used for displaying a unknown test result."
  :group 'test-faces)

(defface test-case-failure-overlay-face
  '((t (:background "firebrick")))
  "Face used to highlight a failing line in a test."
  :group 'test-faces)

(defface test-case-failure-working-overlay-face
  '((t (:background "orange red")))
  "Face used to highlight a failing line in a test that is currently re-run."
  :group 'test-faces)

(defvar test-case-run-after-saving nil
  "Run the test automatically after saving.")

(defvar test-case-save-before-running nil
  "Save the test automatically before running it.")

(defvar test-case-call-compilation-finish-functions t
  "Whether to run `compilation-finish-functions' after test case compilation.")

(defvar test-case-success-state 'unknown
  "The state of the current buffer test.
This is either 'unknown, 'unrunnable, 'running, 'failure or 'success.")
(make-variable-buffer-local 'test-case-success-state)

(defvar test-case-type nil
  "The test type of the current buffer.")
(make-variable-buffer-local 'test-case-type)

(defvar test-case-compile-command nil)
(make-variable-buffer-local 'test-case-compile-command)

(defvar test-case-run-command nil)
(make-variable-buffer-local 'test-case-run-command)

(defconst test-compilation-buffer-name "*Test compilation %s*")

(defconst test-run-buffer-name "*Test run %s*")

(defvar test-case-lighter nil
  "The mode-line string used by ``test-case-mode'' to represent the test type.")
(make-variable-buffer-local 'test-case-lighter)

(defvar test-runner-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-ctr" 'run-test)
    (define-key map "\C-ctc" 'test-case-configure)
    (define-key map "\C-cta" 'run-all-tests)
    map)
  "Keymap used in by test-runner.")

(defvar test-runner-menu nil)

(defvar test-case-process nil
  "The process object for this buffer's compilation/run")
(make-variable-buffer-local 'test-case-process)

(defvar test-run-finished-function nil
  "Functions to call when a test run finishes.
It is called with two arguments, the buffer or list of buffers that were run
and a result value that is either 'unrunnable, 'failure or 'success.")

(define-minor-mode test-case-mode
  "A minor mode for test buffers."
  nil test-case-lighter nil nil
  (if test-case-mode
      (progn
        (setq test-case-type (or (test-case-scan-type) 'unknown))
        ;; copy, so we can change the color just for this buffer
        (set (make-local-variable 'mode-line-buffer-identification)
             (list (copy-sequence (car mode-line-buffer-identification))))
        (when (and test-case-run-after-saving
                   (not (eq test-case-type 'unknown)))
          (run-test))
        (unless (prog1 test-buffer-list
                (add-to-list 'test-buffer-list (current-buffer)))
          (test-runner-mode 1))
        (add-hook 'kill-buffer-hook '(lambda () (test-case-mode 0)) nil t)
        (test-case-update-lighter))
    (test-case-abort)
    (test-case-set-state 'unknown t)
    (kill-local-variable 'test-case-type)
    (kill-local-variable 'test-case-success-state)
    (kill-local-variable 'mode-line-buffer-identification)
    (setq test-buffer-list (delq (current-buffer) test-buffer-list))
    (unless test-buffer-list
      (test-runner-mode 0)))
  (test-runner-update-menu))

(defun enable-test-case-mode-if-test ()
  "Turns on ``test-case-mode'' if this buffer is a recognized test."
  (when (test-case-scan-type)
    (test-case-mode 1)))

;; use defadvice instead of hook, so we can determine which saves are
;; interactive
(defadvice basic-save-buffer (after run-test-maybe activate)
  "Run a test if `test-case-run-after-saving' is set and save was interactive"
  (test-case-abort)
  (when (and test-case-mode
             (not (eq test-case-type 'unknown))
             test-case-run-after-saving
             (not ad-return-value))
    (run-test)))

(defvar test-case-registered-types nil
  "A list of registered test types.")

(defun test-case-register-type (name buffer-tester run-command-generator
                                     &optional
                                     compile-command-generator target-finder
                                     parse-error-func)
  "Register a new test type."
  (assert (functionp buffer-tester))
  (assert (functionp run-command-generator))
  (assert (or (null compile-command-generator)
              (functionp compile-command-generator)))
  (assert (or (null target-finder)
              (functionp target-finder)))
  (let ((old test-case-registered-types)
        (entry (list name buffer-tester run-command-generator
                     compile-command-generator target-finder parse-error-func))
        (new))
    (while old
      (push (if (equal (caar old) name)
                (prog1 entry (setq entry nil))
              (car old)) new)
      (pop old))
    (when entry
      (push entry new))
    (setq test-case-registered-types (nreverse new))))

(defun test-case-get-name ()
  (if (eq test-case-type 'unknown)
      (if test-case-run-command "manual" "unknown")
    (car test-case-type)))

(defun test-case-get-target ()
  (assert (not (eq test-case-type 'unknown)))
  (let ((func (nth 4 test-case-type)))
    (when func (funcall func (file-name-nondirectory (buffer-file-name))))))

(defun test-case-get-compile-command ()
  (if (stringp test-case-compile-command)
      test-case-compile-command
    (let ((command (nth 3 test-case-type)))
      (if command
          (funcall command (current-buffer))
        ""))))

(defun test-case-get-run-command ()
  (or test-case-run-command
      (assert (not (eq test-case-type 'unknown)))
      (funcall (nth 2 test-case-type) (current-buffer))))

(defun test-case-configure ()
  "Interactively set `test-case-compile-command' and `test-case-run-command'."
  (interactive)
  (let* ((compile-command (test-case-get-compile-command))
         (run-command (test-case-get-run-command))
         (prompt (concat "Compile command: "
                         (if (equal "" compile-command)
                             "(none)"
                           (case test-case-compile-command
                             ('nil "(default) make or ")
                             ('nomake "(no make) "))
                           compile-command)
                         "\nRun command: "
                         (unless test-case-run-command "(default) ")
                         (if (functionp run-command)
                             "(elisp command)"
                           run-command)
                         "\n"))
         (new-compile-command
          (test-case-read-from-minibuffer
           (concat prompt "New compile command: ") compile-command
           test-case-compile-command
           '(("(default)" . nil) ("default" . nil)
             ("(no make)" . nomake) ("no make" . nomake))))
         (new-run-command
          (test-case-read-from-minibuffer
           (concat prompt "New run command: ") run-command
           test-case-run-command
           '(("(default)" . nil) ("default" . nil)))))
    (setq test-case-compile-command new-compile-command)
    (setq test-case-run-command new-run-command)
    (test-case-update-lighter)))

(defun test-case-can-make ()
  "Test if there is a makefile that can generate target."
  (and (or (file-exists-p "Makefile")
           (file-exists-p "makefile"))
       (let* ((target (test-case-get-target))
              (tmp (when (file-exists-p target) (make-temp-name target)))
              (result))
         (when tmp
           ;; hide the target, so make will give a meaningful return value
           (rename-file target tmp))
         (setq result (= 0 (shell-command (concat "make -sn " target))))
         (when tmp
           (rename-file tmp target))
         result)))

(defun file-name-relative (file-name &optional base-dir result-as-list)
  (unless base-dir (setq base-dir default-directory))
  (unless (listp file-name)
    (setq file-name (split-string file-name "/" t)))
  (unless (listp base-dir)
    (setq base-dir (split-string base-dir "/" t)))
  (while (and file-name base-dir)
    (pop file-name)
    (pop base-dir))
  (let ((res))
    (if base-dir
        (dotimes (i (length base-dir))
          (push ".." res))
      (setq res file-name))
    (if result-as-list
        res
      (if res
          (mapconcat 'identity res "/")
        "."))))

(defun test-case-needs-compiling (type)
  (assert (not (eq test-case-type 'unknown)))
  (let* ((out-file-finder (nth 4 test-case-type))
         (out-file (funcall out-file-finder (buffer-file-name))))
    (test-case-have-executable (buffer-file-name) out-file)))

(defun test-case-run ()
  (let ((out-buffer (get-buffer-create (format test-run-buffer-name
                                               (buffer-name))))
        (command (test-case-get-run-command))
        (inhibit-read-only t))
    (with-current-buffer out-buffer (erase-buffer))
    (unless (or command (equal command ""))
      (error "No test run command specified"))
    (message "Running %s test %s ..." (test-case-get-name) (buffer-name))
    (if (functionp command)
        ;; lisp function
        (funcall command out-buffer)
      (test-case-execute 'test-case-run-done out-buffer
                         "/bin/sh" "-c" command))))

(defun test-case-fontify (out-buffer &optional buffer)
  (when (fboundp 'hl-errors-add)
    (hl-errors-clear (or buffer (current-buffer))
                     'test-case-failure-working-overlay-face)
    (let ((func (nth 5 test-case-type)))
      (when (and func out-buffer)
        (let* ((res (funcall func out-buffer buffer))
               (lines (car res))
               (errors (cdr res)))
          (assert (equal (length lines) (length errors)))
          (while lines
            (hl-errors-add (string-to-int (car lines)) (car errors) buffer
                           'test-case-failure-overlay-face)
            (pop lines)
            (pop errors))
          t)))))

(defun test-case-run-done (result output &optional msg)
  (message "Running %s test %s %s" (test-case-get-name)
           (buffer-name) (if result "succeeded" "failed"))
  (test-case-set-state (if result 'success 'failure))
  ;; this looks a bit fishy to me
  (if (buffer-live-p output)
      (if (or (test-case-fontify output) result)
          (kill-buffer output)
        (with-current-buffer output (setq buffer-read-only t))
        (display-buffer output))
    (test-case-fontify nil))
  (test-case-signal)
  (test-case-update-result-on-change)
  (when test-run-finished-function
    (run-hook-with-args test-run-finished-function (current-buffer) result)))

(defun test-case-get-make-command ()
  (when (or (file-exists-p "Makefile")
            (file-exists-p "makefile"))
    (let ((target (test-case-get-target)))
      (when (file-exists-p target)
        (delete-file target))
      (when (= 0 (call-process-shell-command (concat "make -sn " target)))
        (concat "make " target)))))

(defun test-case-compile ()
  ;; transform all red lines to yellow lines
  (when (fboundp 'hl-errors-transform)
    (hl-errors-transform 'test-case-failure-overlay-face
                         'test-case-failure-working-overlay-face
                         (current-buffer)))
  (let ((command (test-case-get-compile-command)))
    (if (equal command "")
        ;; no compile needed
        (test-case-run)
      (let ((out-buffer (get-buffer-create (format test-compilation-buffer-name
                                                 (buffer-name))))
            (inhibit-read-only t))
        (with-current-buffer out-buffer (erase-buffer))
        ;; check for target file age first, because try-make will delete it
        (let ((needs-compiling (test-case-needs-compiling test-case-type))
              (make-command (test-case-get-make-command)))
          (when make-command
            (message "Make available for test %s ..." (test-case-get-name))
            (setq command make-command))
          (message "Compiling %s test %s ..." (test-case-get-name)
                   (buffer-name))
          (test-case-execute 'test-case-compilation-done out-buffer
                             "/bin/sh" "-c" command))))))

(defun test-case-compilation-done (result output msg)
  (message "Compiling %s test %s %s" (test-case-get-name)
           (buffer-name) (if result "succeeded" "failed"))
  (when (buffer-live-p output)
    (when test-case-call-compilation-finish-functions
      (run-hook-with-args 'compilation-finish-functions output msg))
    (when (buffer-live-p output)
      (if result
          (kill-buffer output)
        (with-current-buffer output (compilation-mode))
        (display-buffer output))))
  (if result
      (test-case-run)
    (test-case-set-state 'unrunnable)
    (test-case-fontify nil)))

(defun test-case-abort (&optional dont-signal)
  "Abort the currently running `test-case-mode' run or compilation process."
  (interactive)
  (if (not test-case-process)
      (when (interactive-p)
        (message "No test running."))
    (process-put test-case-process 'caller nil)
    (process-put test-case-process 'result-func nil)
    (process-kill-without-query test-case-process)
    (setq test-case-process nil)
    (message "Test run aborted.")
    (test-case-set-state 'unknown dont-signal)))

(defun run-test (&optional dont-abort)
  "Run a test contained in a buffer."
  (interactive)
  (assert (not (eq 'unknown test-case-type)))
  (if (and test-case-process
           dont-abort)
      (message "Test '%s' is already being run!" (buffer-name))
    (test-case-abort t)
    (when (buffer-modified-p)
      (if (not (or test-case-save-before-running
                   (yes-or-no-p (format "Save file '%s'? "
                                        (buffer-file-name)))))
          (error "Test run aborted")
        (let ((test-case-run-after-saving nil))
          (save-buffer))
        (message "Saving test '%s'" (buffer-file-name))))
    (test-case-set-state 'running)
    (test-case-compile)))

(defun test-case-update-result-on-change ()
  "Call `test-case-update-result' as soon as the buffer changes."
  (add-hook 'after-change-functions 'test-case-update-result nil t))

(defun test-case-update-result (beg end len)
  (remove-hook 'after-change-functions 'test-case-update-result t)
  (test-case-signal))

(defun test-case-get-face ()
  "Return the appropriate face corresponding to `test-case-success-state'."
  (case test-case-success-state
    ((unrunnable failure) 'test-case-failure-face)
    (running 'test-case-undetermined-face)
    (success (if (buffer-modified-p) 'test-case-success-but-modified-face
               'test-case-success-face))
    ))

(defadvice list-buffers-noselect (after store-buffer-list
                                        (&optional files-only buffer-list)
                                        activate)
  "Store the requested buffer-list in local variable `Buffer-menu-buffer-list'."
  (with-current-buffer ad-return-value
    (defvar Buffer-menu-buffer-list nil
      "The buffers the current buffer-menu lists.")
    (set (make-local-variable 'Buffer-menu-buffer-list) buffer-list)))

(defun test-case-signal ()
  "Signal the buffer's `test-case-success-state'."
  (assert test-case-mode)
  (add-text-properties 0 (length (car mode-line-buffer-identification))
                       (list 'face (test-case-get-face))
                       (car mode-line-buffer-identification))
  (force-mode-line-update)
  (let ((buffer-list (get-buffer "*Buffer List*")))
    (when buffer-list
      (with-current-buffer buffer-list
        (list-buffers-noselect Buffer-menu-files-only
                               Buffer-menu-buffer-list))))
  )

(defadvice Buffer-menu-buffer+size (after fontify-test-results
                                          (name size &optional name-props
                                                size-props) activate)
  (let ((buffer (get-buffer name)))
    (when (and buffer (buffer-local-value 'test-case-mode buffer))
      (let ((face (with-current-buffer buffer (test-case-get-face))))
        (add-text-properties 0 (length ad-return-value)
                             `(face ,face) ad-return-value)))))

(defun test-case-scan-type ()
  (let ((result)
        (list test-case-registered-types))
    (when (buffer-file-name)
      (while (and list (null result))
        (let* ((type (car list))
               (name (car type))
               (test (cadr type)))
          (if (funcall test (current-buffer))
              (setq result type)
            (setq list (cdr list))))))
    result))

(defun test-case-update-lighter ()
  (setq test-case-lighter
        (if (eq test-case-type 'unknown)
            (if test-case-run-command "manual" "unknown")
          (concat " " (test-case-get-name)
                  (when (or test-case-compile-command
                            test-case-run-command) "!")))))

(defun grep-buffer (buffer regexp)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward regexp nil t)
          ; match, always return something, even if nothing was matched in ()
          (or (match-string-no-properties 1) "")
        ; no match
        nil))))

(defun grep-buffer-list (regexp &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let ((result))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (push (match-string-no-properties 1) result)))
      result)))

(defun file-newer-than (a b)
  (let* ((date-a (nth 5 (file-attributes a)))
         (date-b (nth 5 (file-attributes b)))
         (diff (- (car date-a) (car date-b))))
    (if (= 0 diff)
        (> (cadr date-a) (cadr date-b))
      (> diff 0))))

(defun test-case-have-executable (file-name executable-name)
  "Test if EXECUTABLE-NAME doesn't exist or is older that FILE-NAME."
  (not (and (file-exists-p executable-name)
            (file-newer-than executable-name file-name))))

(defun test-case-execute (result-func out-buffer command &rest arguments)
  (assert (null test-case-process))
  (buffer-disable-undo out-buffer)
  (if (not (fboundp 'start-process))
      (let ((res-code
             (eval `(call-process "java" nil out-buffer nil ,@arguments))))
        (result-func (= 0 res-code) run-buffer
                     (= 0 "finished\n"
                        (format "exited abnormally with code %d\n" res-code))))
    (setq test-case-process
          (eval `(start-process "test-case-process" out-buffer command
                                ,@arguments)))
    (process-put test-case-process 'caller (current-buffer))
    (process-put test-case-process 'result-func result-func)
    (set-process-sentinel test-case-process 'test-case-sentinel)))

(defun test-case-sentinel (proc msg)
  "Sentinel for test run buffers."
  (when (memq (process-status proc) '(exit signal))
    (let ((buffer (process-buffer proc))
          (caller (process-get proc 'caller))
          (result-func (process-get proc 'result-func))
          (result (= 0 (process-exit-status proc))))
      (when (and caller result-func)
        (with-current-buffer caller
          (when test-case-process
            (assert (eq test-case-process proc))
            (setq test-case-process nil)
            (delete-process proc)
            (funcall result-func result buffer msg)))))))

(defun test-case-set-state (new-state &optional dont-signal)
  "Set `test-case-success-state' to NEW-STATE."

  ;; add/remove buffer from `test-runner-running-tests
  (if (eq new-state 'running)
      (unless (eq test-case-success-state 'running)
        (push (current-buffer) test-runner-running-tests))
    (when (eq test-case-success-state 'running)
      (setq test-runner-running-tests
            (delq (current-buffer) test-runner-running-tests))))

  ;; add/remove buffer from `test-runner-failed-tests
  (if (or (eq new-state 'failure)
          (eq new-state 'unrunnable))
      (unless (or (eq test-case-success-state 'failure)
                  (eq test-case-success-state 'unrunnable))
        (push (current-buffer) test-runner-failed-tests))
    (when (or (eq test-case-success-state 'failure)
              (eq new-state 'unrunnable))
      (setq test-runner-failed-tests
            (delq (current-buffer) test-runner-failed-tests))))

  (test-runner-update-tooltip)

  (setq test-case-success-state new-state)
  (unless dont-signal
    (test-case-signal)))

(defvar test-buffer-list nil
  "A list of all buffers that are in `test-case-mode'.")

(defun build-menu-run-buffer ()
  "Collect a menu with all possible tests."
  (let* ((tests
          (mapcar '(lambda (buffer)
                     (vector (buffer-name buffer)
                             `(lambda () (interactive)
                                (with-current-buffer ,buffer (run-test)))))
                  test-buffer-list))
         (test-list `(["all" 'run-all-tests] "-" ,@tests)))
    `("run" ,@test-list)))

(defun build-menu-switch-entry (buffer)
  "Build a \"Tests/Switch To\" menu entry."
  (vector (buffer-name buffer) `(lambda () (interactive)
                                  (switch-to-buffer ,buffer)) t))

(defun build-menu-switch (buffers)
  "Build the \"Tests/Switch To\" menu."
  (cons "switch to"
        (mapcar '(lambda (buffer)
                   (vector (buffer-name buffer)
                           `(lambda () (interactive)
                              (with-current-buffer ,buffer (run-test)))))
                test-buffer-list)))

(defun build-menu ()
  "Build the main menu."
  (list "Test"
        (build-menu-run-buffer)
        (build-menu-switch test-buffer-list)
        "-"
        '["run" run-test :active test-case-type]
        '["configure ..." test-case-configure :active test-case-type]
        ;; hack
        '["configure c++ linker ..." test-case-configure-linker
          :visible (equal (test-case-get-name) "CxxTest")]
        ;;'run-test] ;; :active test-case-type]
        ))

(defun test-runner-update-menu ()
  (easy-menu-define test-runner-menu test-runner-keymap
    "Test runner commands" (build-menu))
  (easy-menu-add test-runner-menu))

(define-minor-mode test-runner-mode
  "A minor mode for accessing open tests from any buffer."
  nil nil test-runner-keymap :global t
  (if test-runner-mode
      (test-runner-install-dot)
    (test-runner-remove-dot)))

(defvar test-runner-failed-tests nil
  "A list of all buffers containing failed tests.")

(defvar test-runner-running-tests nil
  "A list of all buffers containing running tests.")

(defun run-all-tests ()
  "Run all buffers in `test-case-mode'."
  (interactive)
  (dolist (test test-buffer-list)
    (with-current-buffer test (run-test t))))

(provide 'test-runner-base)
