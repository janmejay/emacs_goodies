(require 'test-runner-base)

(setq rspec-executable "spec -f n")

(defun build-rspec-runner-command-for (file-name)
  (cond ((numberp block-selection-for-rspec-buffer) (concat rspec-executable " -l " (number-to-string block-selection-for-rspec-buffer) " " file-name))
        (t (concat rspec-executable " " file-name))))

(setq block-selection-for-rspec-buffer nil)

(defun toggle-spec-block-identification-for-selective-running ()
  (interactive)
  (cond ((numberp block-selection-for-rspec-buffer) (setq block-selection-for-rspec-buffer nil))
        (t (setq block-selection-for-rspec-buffer (line-number-at-pos)))))

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
