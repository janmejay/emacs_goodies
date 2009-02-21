(require 'test-runner-base)

(defun test-case-run-elk-test (out-buffer &optional buffer)
  (require 'elk-test)
  (unless buffer (setq buffer (current-buffer)))
  (let* ((elk-test-list)
         (elk-test-map (make-hash-table :test 'equal))
         (elk-test-run-on-define nil)
         (inhibit-read-only t)
         (buffer-name (buffer-name buffer))
         (success t)
         (parse-res (condition-case err (eval-buffer buffer) (error err))))
    (if parse-res
          (progn
            (setq success nil)
            (with-current-buffer out-buffer
              (insert "Parsing buffer \'" buffer-name "\' failed:\n"
                      (format "%s" parse-res) "\n")))
      (dolist (test elk-test-list)
        (message "run %s" test)
        (let ((results (run-elk-test test)))
          (message "assert %s" results)
          (when results
            (message "assert")
            (setq success nil)
            (with-current-buffer out-buffer
              (insert "test \'" test "\' failed:\n")
              (dolist (result results)
                (insert "* " result)))))))
    (test-case-run-done success out-buffer)))

(defun test-case-is-elk-test (buffer)
  "Determine if this buffer is a testunit test case."
  (and (string-match "\.el$" (buffer-file-name buffer))
       (grep-buffer buffer "(\s*deftest \"\\([^\"]*\\)\"")))

(test-case-register-type ".elk-test" 'test-case-is-elk-test
                         (lambda (foo) 'test-case-run-elk-test))

(provide 'elk-test-runner)
