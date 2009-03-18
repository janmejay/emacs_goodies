(require 'eltest)

(deftest "get-base-dir-from-package"
  (assert-equal "foo" (get-base-dir-from-package "bar" "foo/bar/"))
  (assert-equal "." (get-base-dir-from-package "foo.bar" "foo/bar/"))
  (assert-equal "42" (get-base-dir-from-package "foo.bar" "42/foo/bar/"))
  (assert-equal "/42/4" (get-base-dir-from-package "foo.bar" "/42/4/foo/bar/"))
  (assert-equal "/foo/bar" (get-base-dir-from-package "" "/foo/bar/"))
  (assert-equal ".." (get-base-dir-from-package "bla.foo.bar" "foo/bar/"))
  (assert-equal "../.." (get-base-dir-from-package "foo.bar" ""))
  )

(deftest "file-name-relative"
  (let ((default-directory "/foo/bar/"))
    (assert-equal "." (file-name-relative "/foo/bar"))
    (assert-equal "." (file-name-relative "/foo/bar/"))
    (assert-equal ".." (file-name-relative "/foo"))
    (assert-equal ".." (file-name-relative "/foo/"))
    (assert-equal "../.." (file-name-relative "/"))
    (assert-equal "42" (file-name-relative "/foo/bar/42"))
    (assert-equal "42" (file-name-relative "/foo/bar/42/"))
    (assert-equal "42/43" (file-name-relative "/foo/bar/42/43")))
  (let ((default-directory "/foo/bar"))
    (assert-equal "." (file-name-relative "/foo/bar"))
    (assert-equal "." (file-name-relative "/foo/bar/"))
    (assert-equal ".." (file-name-relative "/foo"))
    (assert-equal ".." (file-name-relative "/foo/"))
    (assert-equal "../.." (file-name-relative "/"))
    (assert-equal "42" (file-name-relative "/foo/bar/42"))
    (assert-equal "42" (file-name-relative "/foo/bar/42/"))
    (assert-equal "42/43" (file-name-relative "/foo/bar/42/43"))))