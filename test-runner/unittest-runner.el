(require 'test-runner-base)

(setq unittest-executable "python ")
(setq unittest-default-options "")

(defun build-unittest-runner-command-for (file-name)
  (concat unittest-executable file-name))

(defun build-unittest-command-options ()
  rspec-default-options)

(defun test-case-unittest-make-run-command (buffer)
  (fset 'builder-for-unittest-runner-command 'build-unittest-runner-command-for)
  (builder-for-unittest-runner-command (buffer-file-name (current-buffer))))

(defun test-case-is-unittest (buffer)
  "Determine if this buffer is a rspec test case."
  (let ((file_name (buffer-file-name buffer)))
    (or (string-match "\_test.py$" file_name))))

(test-case-register-type "import unittest" 'test-case-is-unittest
                         'test-case-unittest-make-run-command)

(provide 'unittest-runner)
