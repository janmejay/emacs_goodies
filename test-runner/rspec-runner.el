(require 'test-runner-base)

(setq rspec-executable "spec")
(setq rspec-default-options " -f n ")

(defun build-rspec-runner-command-for (file-name)
  (concat rspec-executable (build-rspec-command-options) file-name))

(defun build-rspec-command-options ()
  (if run-rspec-block (concat rspec-default-options " -l " (number-to-string (line-number-at-pos)) " ") rspec-default-options))

(setq run-rspec-block nil)

(defun run-current-rspec-block ()
  (interactive)
  (setq run-rspec-block t)
  (run-test)
  (setq run-rspec-block nil))

(defun file-path-and-exec-dir (file-name)
  (let* ((project-root (emacs-proj-root))
         (abs-filename (file-truename file-name)))
    (concat "(cd " project-root " && " (build-rspec-runner-command-for abs-filename) ")")))

(defun test-case-rspec-make-run-command (buffer)
  (fset 'builder-for-rspec-runner-command 'build-rspec-runner-command-for)
  (file-path-and-exec-dir (buffer-file-name (current-buffer))))


(defun test-case-is-rspec (buffer)
  "Determine if this buffer is a rspec test case."
  (let ((file_name (buffer-file-name buffer)))
    (or (string-match "\_spec.rb$" file_name))))

(test-case-register-type "describe" 'test-case-is-rspec
                         'test-case-rspec-make-run-command)

(provide 'rspec-runner)
