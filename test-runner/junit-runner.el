(require 'test-runner-base)

(defvar junit-jar-location nil
  "Location of the JUnit .jar file.")

(defun java-class-from-file-name (file-name)
  "Return the Java class name that the buffer's file name implies."
  (substring (file-name-nondirectory file-name) 0 -5))

(defun make-classpath (&rest locations)
  (mapconcat 'identity locations ":"))

(defun test-case-junit-get-output-file (file)
  (concat (substring file 0 -5) ".class"))

(defun grep-package-from-buffer (buffer)
  (or (grep-buffer buffer "package\s+\\([[:alnum:].]+\\);") ""))

(defun get-base-dir-from-package (package &optional path)
  (assert package)
  (unless path
    (setq path default-directory))
  (if (equal package "")
      (directory-file-name path)
    (let* ((packages (nreverse (split-string package "\\." t)))
           (dirs (cdr (nreverse (split-string path "/"))))
           (res (file-name-relative dirs packages t)))
      (if res
          (mapconcat 'identity (reverse res) "/")
        "."))))

;; deprecated
(defun get-base-dir-from-buffer (&optional buffer)
  (get-base-dir-from-package
   (grep-package-from-buffer buffer)))
;;    (file-name-directory (buffer-file-name buffer))))

(defun test-case-junit-needs-compiling (buffer)
  (let ((file-name (buffer-file-name buffer)))
    (test-case-have-executable file-name
                               (test-case-junit-get-output-file file-name))))

(defun test-case-junit-make-run-command (buffer)
  (let* ((package (grep-package-from-buffer buffer))
         (file-name (buffer-file-name buffer))
         (base-dir (get-base-dir-from-package package
                                              (file-name-directory file-name)))
         (cp (make-classpath junit-jar-location base-dir))
         (class-name (java-class-from-file-name file-name)))
    (concat "java -cp " cp " org.junit.runner.JUnitCore " package
            (unless (equal package "") ".")
            class-name)))

;; TODO It would be good to check for "0 tests run"
(defun test-case-run-async-junit (run-buffer)
  (test-case-execute 'test-case-run-done run-buffer "/bin/sh" "-c"
                     (test-case-junit-make-run-command (current-buffer))))

(defun test-case-junit-make-compile-command (buffer)
  (unless junit-jar-location
    (error "`junit-jar-location' not specified"))
  (let* ((base-dir (get-base-dir-from-buffer buffer))
         (cp (make-classpath junit-jar-location base-dir))
         (file-name (buffer-file-name buffer)))
    (concat "javac -cp " cp " -d " base-dir " " file-name)))

(defun test-case-compile-async-junit (compile-buffer)
  (test-case-execute 'test-case-compilation-done compile-buffer "/bin/sh" "-c"
                     (test-case-junit-make-compile-command (current-buffer))))

(defun test-case-is-junit (buffer)
  "Determine if this buffer is a JUnit test case."
  (and (string-match "\.java$" (buffer-file-name buffer))
       (or (grep-buffer buffer "import\s+junit\s*\\.framework\s*\\.")
           (grep-buffer buffer "import\s+org\.junit"))
       (or (grep-buffer buffer "extends\s+TestCase") (grep-buffer buffer "@Test")) ))

;; (grep-buffer-list "junit\\.framework\\.AssertionFailedError: \\(.+\\)"
;;                   (get-buffer "*Test run Test.java*"))


;; (grep-buffer-list (concat "(" (regexp-quote "Test.java") ":\\(.+\\))")
;;                   (get-buffer "*Test run Test.java*"))

(defun test-case-junit-parse-errors (out-buffer &optional buffer)
  (let ((errors
         (grep-buffer-list "junit\\.framework\\.AssertionFailedError: \\(.+\\)"
                           out-buffer))
        (lines
         (grep-buffer-list (concat "(" (regexp-quote (buffer-name buffer))
                                                     ":\\(.+\\))")
                           out-buffer)))
    `(,lines . ,errors)))

(test-case-register-type "JUnit" 'test-case-is-junit
                         'test-case-junit-make-run-command
                         'test-case-junit-make-compile-command
                         'test-case-junit-get-output-file
                         'test-case-junit-parse-errors)

(provide 'junit-runner)

