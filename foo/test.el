;;; dignified-elpa-package.el --- package helper  -*- lexical-binding:t -*-

(require 'package)

(defsubst dignified-elpa-package-where ()
  (directory-file-name (expand-file-name default-directory)))

(defsubst dignified-elpa-package-desc ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "dignified-elpa.el" (dignified-elpa-package-where)))
    (package-buffer-info)))

(defun dignified-elpa-package-name ()
  (concat "dignified-elpa-" (package-version-join
			      (package-desc-version
			       (dignified-elpa-package-desc)))))

(defun dignified-elpa-package-inception ()
  (let ((pkg-desc (dignified-elpa-package-desc))
	(pkg-dir (expand-file-name (dignified-elpa-package-name)
				   (dignified-elpa-package-where))))
    (ignore-errors (delete-directory pkg-dir t))
    (make-directory pkg-dir t)
    (copy-file (expand-file-name "dignified-elpa.el" (dignified-elpa-package-where))
	       (expand-file-name "dignified-elpa.el" pkg-dir))
    (package--make-autoloads-and-stuff pkg-desc pkg-dir)))
