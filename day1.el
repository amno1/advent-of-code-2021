;;; day1.el ---                                      -*- lexical-binding: t; -*-
(with-temp-buffer
  (insert-file-contents-literally "./input1")
  (let* ((list (map 'list #'string-to-number (split-string (buffer-string))))
         (parts (cl-maplist #'(lambda (p)
                                (and p (cdr p) (cddr p)
                                     (list (car p) (cadr p) (caddr p)))) list))
         (sums (cl-mapcar #'(lambda (p) (apply #'+ p)) parts)))
    (message "P1: %s" (cl-count 't (cl-mapcar '< list (cdr list))))
    (message "P2: %s" (cl-count 't (cl-mapcar #'< sums (cdr sums))))))
