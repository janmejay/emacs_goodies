(defvar test-runner-dot-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      '(lambda () (interactive)
         (switch-to-buffer (list-buffers-noselect t test-buffer-list))))
    map)
  "Keymap used for test-runner dot in mode-line.")

(defvar test-runner-dot-tooltip nil)

(defun test-runner-make-dot (color)
  "Return an image representing a dot whose color is COLOR."
  (propertize " "
              'help-echo 'test-runner-dot-tooltip
              'keymap test-runner-dot-keymap
              'display
              `(image :type xpm
                      :data ,(format "/* XPM */
static char * data[] = {
\"18 13 3 1\",
\" 	c None\",
\"+	c #000000\",
\".	c %s\",
\"                  \",
\"       +++++      \",
\"      +.....+     \",
\"     +.......+    \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"     +.......+    \",
\"      +.....+     \",
\"       +++++      \",
\"                  \"};"
                                     color)
                      :ascent center)))

(defconst test-runner-dot-position 2
  "The position in the mode-line `test-runner-install-dot' puts the dot.")

(defun test-runner-install-dot (&optional position)
  "Install the dot in the mode-line at `test-result-dot-position' or POSITION."
  (let ((mode-line (default-value 'mode-line-format))
        (pos (or position test-runner-dot-position))
        (res))
    (dotimes (i (min pos (length mode-line)))
      (push (car mode-line) res)
      (pop mode-line))
    (push `(test-runner-failed-tests
            ,(test-runner-make-dot "firebrick")
            (test-runner-running-tests
              ,(test-runner-make-dot "orange")
              ,(test-runner-make-dot "dark olive green")))
          res)
    (while mode-line
      (push (car mode-line) res)
      (pop mode-line))
    (setq-default mode-line-format (nreverse res)))
  (force-mode-line-update t))

(defun test-runner-remove-dot ()
  "Remove the dot installed by `test-runner-install-dot' from the mode-line."
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (while mode-line
      (let ((item (car mode-line)))
        (unless (eq (car-safe item) 'test-runner-failed-tests)
          (push item res)))
      (pop mode-line))
    (setq-default mode-line-format (nreverse res)))
  (force-mode-line-update t))

(defun test-runner-update-tooltip ()
  "Update the tooltip on the mode-line result dot in `test-runner-mode'"
  (let ((tooltip))
    (when test-runner-running-tests
      (setq tooltip (concat "Running tests: "
                            (mapconcat '(lambda (x) (format "%s" x))
                                       test-runner-running-tests " ") "\n")))
    (when test-runner-failed-tests
      (setq tooltip (concat tooltip "Failed tests: "
                            (mapconcat '(lambda (x) (format "%s" x))
                                       test-runner-failed-tests " ") "\n"))
      (setq test-runner-dot-tooltip (or tooltip "All tests successful.")))))

(provide 'test-runner-image)
