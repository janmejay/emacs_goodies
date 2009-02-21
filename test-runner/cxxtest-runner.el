(require 'test-runner-base)

(defvar cxxtestgen-executable "cxxtestgen.pl")

(defvar test-case-c++-default-params nil
  "The default c++ command parameters for test case compiles.
This should contain library and include paths for every test case.")

(defvar test-case-c++-objects nil
  "A list of object files to link this test against.")
(make-variable-buffer-local 'test-case-c++-objects)

(define-key test-runner-keymap "\C-cto" 'test-case-configure-linker)

(defun test-case-read-from-minibuffer (prompt default default-res special-vals)
  (let ((res (completing-read prompt special-vals nil nil default)))
    (if (equal res default)
        ;; unchanged
        default-res
      ;; check for special value
      (while special-vals
        (let ((pair (pop special-vals)))
          (when (equal (car pair) res)
            (setq res (cdr pair))
            (setq special-vals nil))))
      res)))

(defun test-case-configure-linker (&optional buffer)
  "Configure the C++ linker used by `test-case-mode'"
  (interactive)
  (let* ((objs (test-case-get-linker-options buffer))
         (prompt (concat "Objects: "
                         (unless test-case-c++-objects "(auto) ")
                         (if (equal objs "")
                             "(none)"
                           objs)
                         "\n"))
         (new-objs (test-case-read-from-minibuffer
                    (concat prompt "New objects: ")
                    objs test-case-c++-objects
                    '(("(auto)" . nil) ("auto" . nil)))))
    (setq test-case-c++-objects
          (if (stringp new-objs) (split-string new-objs) new-objs))
    (when (equal test-case-c++-objects (test-case-guess-required-objects))
      (setq test-case-c++-objects nil))))

(defun test-case-cxxtest-get-output-file (file)
  (substring file 0 -2))

(defun test-case-guess-required-objects (&optional buffer)
  (let ((headers
         (grep-buffer-list "#include\s+[<\"]\\([^>\"]+\\)\\.h.?.?[>\"]" buffer))
        (result))
    (dolist (header headers)
      (when (file-exists-p (concat header ".cpp"))
          (push (concat header ".o") result)))
      result))

(defun test-case-cxxtest-needs-compiling (&optional buffer)
  (let ((objs (or test-case-c++-objects
                  (test-case-guess-required-objects buffer)))
        (file (buffer-file-name buffer)))
    (while (and objs
                (or (not (file-exists-p (car objs)))
                    (file-newer-than file (car objs))))
      (setq objs (cdr objs)))
  objs))

(defun test-case-get-linker-options (&optional buffer)
  (let ((objs (or test-case-c++-objects
                  (test-case-guess-required-objects buffer))))
    (mapconcat 'identity objs " ")))

(defun test-case-cxxtest-make-run-command (buffer)
  (let* ((file-name (file-name-nondirectory (buffer-file-name))))
    (concat "./" (test-case-cxxtest-get-output-file file-name))))

(defun test-case-run-async-cxxtest (run-buffer)
  (test-case-execute 'test-case-run-done run-buffer
                     (test-case-cxxtest-make-run-command (current-buffer))))

(defun test-case-cxxtest-make-compile-command (&optional buffer)
  (let ((file-name (file-name-nondirectory (buffer-file-name buffer)))
        (opts (test-case-get-linker-options buffer)))
    (concat cxxtestgen-executable " --error-printer "
            file-name " | g++"
            (when test-case-c++-default-params " ") test-case-c++-default-params
            (when opts " ") opts
            " -xc++ - -o " (test-case-cxxtest-get-output-file file-name))))

(defun test-case-compile-async-cxxtest (compile-buffer)
  (test-case-execute 'test-case-compilation-done compile-buffer "/bin/sh" "-c"
                     (test-case-cxxtest-make-compile-command (current-buffer))))

(defun test-case-is-cxxtest (buffer)
  "Determine if this buffer is a cxxtest test case."
  (and (or (string-match "\.h.?.?$" (buffer-file-name buffer))
           (string-match "\.c[px+]\\{2,2\\}$" (buffer-file-name buffer)))
       ;; TODO: this allows mixing of < and "
       (grep-buffer buffer "#include\s+\\([<\"]\\)cxxtest[/\\]TestSuite.h[>\"]")
       (or (grep-buffer buffer (concat ":\s*\\(public\\|private\\|protected\\)"
                                       "\s*CxxTest::TestSuite"))
           (and (grep-buffer buffer (concat ":\s*\\(public\\|private\\|"
                                            "protected\\)\s*TestSuite"))
                (or (grep-buffer buffer (concat "using\s+namespace\s+CxxTest;"))
                    (grep-buffer buffer (concat "using\s+CxxTest.TestSuite;"))))
           )
  ))

(test-case-register-type "CxxTest" 'test-case-is-cxxtest
                         'test-case-cxxtest-make-run-command
                         'test-case-cxxtest-make-compile-command
                         'test-case-cxxtest-get-output-file)


(provide 'cxxtest-runner)
