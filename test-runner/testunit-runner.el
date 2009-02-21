(require 'test-runner-base)

(setq testunit-executable "ruby")

(defun build-testunit-runner-command-for (file-name)
  (concat testunit-executable " " file-name))

(defun test-case-testunit-make-run-command (buffer)
  (fset 'builder-for-testunit-runner-command 'build-testunit-runner-command-for)
  (let ((file-name (buffer-file-name (current-buffer))))
    (load-emacs-project-file-for file-name)
    (builder-for-testunit-runner-command file-name)))

(defun test-case-is-testunit (buffer)
  "Determine if this buffer is a testunit test case."
  (let ((file_name (buffer-file-name buffer)))
    (or (string-match "\_test.rb$" file_name))))

(test-case-register-type "Test::Unit" 'test-case-is-testunit
                         'test-case-testunit-make-run-command)

(provide 'testunit-runner)
