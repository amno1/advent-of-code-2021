;;; day1.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


;;; Part I
(defvar day1-input nil)
(defun day1-input (file-name)
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (map 'list #'string-to-number (split-string (buffer-string)))))

(setq day1-input (day1-input "input.1"))

(defun count-increases1 (list)
  (let ((increases 0))
    (while (cdr list)
      (if (> (cadr list) (car list))
          (setq increases (1+ increases)))
      (setq list (cdr list)))
    increases))

(defun count-increases2 (list)
  (let (increases)
    (while (cdr list)
      (if (> (cadr list) (car list))
          (push (cadr list) increases))
      (setq list (cdr list)))
    (length increases)))

(defun count-increases3 (list)
  (cl-loop for current in list
           for next in (cdr list)
           while (cdr list)
           if (> next current) collect next into increases
           finally (return (length increases))))

(defun count-increases4 (list)
  (cl-loop for (current next) on list
           while (and current next)
           if (> next current) collect next into increases
           finally (return (length increases))))

(defun count-increases5 (list)
  (cl-count 't (map 'list '< list (cdr list))))

(defun count-increases6 (list)
  (cl-count 't (cl-mapcar '< list (cdr list))))

(defun count-increases7 (list)
  (length (remove nil (map 'list '< list (cdr list)))))

(defun count-increases8 (list)
  (cl-count-if-not #'null (map 'list '< list (cdr list))))

;;; Part II
(defun sliding-windows (list-to-partition n)
  (let ((l list-to-partition)
        window windows)
    (while l
      (setq window (cond ((> (length l) n) (cl-subseq l 0 n))
                         ((= (length l) n) l)))
      (push window windows)
      (setq l (cdr l)))
    (nreverse windows)))

(defun count-increase-proc ()
  (let* ((partitions (sliding-windows day1-input 3))
         (sums (cl-mapcar '(lambda (p) (apply '+ p)) partitions)))
    (cl-count 't (cl-mapcar '< sums (cdr sums)))))

(defun count-increase-func ()
  (let* ((day1-input
          (with-temp-buffer
            (insert-file-contents-literally "./input1")
            (map 'list #'string-to-number (split-string (buffer-string)))))
         (partitions (cl-maplist #'(lambda (p)
                                 (and p (cdr p) (cddr p)
                                      (list (car p) (cadr p) (caddr p))))
                              day1-input))
         (sums (cl-mapcar #'(lambda (p) (apply #'+ p)) partitions)))
    (cl-count 't (cl-mapcar #'< sums (cdr sums)))))

(provide 'day1)
;;; day1.el ends here
