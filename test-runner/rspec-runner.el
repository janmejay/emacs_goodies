(require 'test-runner-base)

(setq rspec-executable "spec")
(setq rspec-default-options " -f n ")

(defun build-rspec-runner-command-for (file-name)
  (concat rspec-executable (build-rspec-command-options) file-name))

(defun build-rspec-command-options ()
  (if run-rspec-block (concat rspec-default-options " -l " (number-to-string (line-number-at-pos)) " ") rspec-default-options))

(setq run-rspec-block nil)

(defun toggle-run-current-rspec-block ()
  (interactive)
  (setq run-rspec-block (not run-rspec-block)))

(defun test-case-rspec-make-run-command (buffer)
  (fset 'builder-for-rspec-runner-command 'build-rspec-runner-command-for)
  (let ((file-name (buffer-file-name (current-buffer))))
    (load-emacs-project-file-for file-name)
    (builder-for-rspec-runner-command file-name)))

(defun test-case-is-rspec (buffer)
  "Determine if this buffer is a rspec test case."
  (let ((file_name (buffer-file-name buffer)))
    (or (string-match "\_spec.rb$" file_name))))

(test-case-register-type "describe" 'test-case-is-rspec
                         'test-case-rspec-make-run-command)

(provide 'rspec-runner)
