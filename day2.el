;;; day2.el ---                                      -*- lexical-binding: t; -*-

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
(let ((h 0) (d 0)
      (steps (with-temp-buffer
               (insert-file-contents-literally "./input2")
               (split-string (buffer-string)))))
  
  (defun forward (incr) (cl-incf h incr))
  (defun down (incr) (cl-incf d incr))
  (defun up (incr) (cl-decf d incr))
  
  (while (cdr steps)
    (funcall (intern-soft (car steps)) (string-to-number (cadr steps)))
    (setq steps (cddr steps)))

  (message "Moved forward %s steps and down %s steps." h d)
  (message "Total movement %s steps." (* h d)))

;;; Part II
(let ((h 0) (d 0) (a 0)
      (steps (with-temp-buffer
               (insert-file-contents-literally "./input.2")
               (split-string (buffer-string)))))
  
  (defun forward (incr) (cl-incf h incr) (cl-incf d (* a incr)))
  (defun down (incr) (cl-incf a incr))
  (defun up (incr) (cl-decf a incr))
  
  (while (cdr steps)
    (funcall (intern-soft (car steps)) (string-to-number (cadr steps)))
    (setq steps (cddr steps)))

  (message "Moved forward %s steps and down %s steps." h d)
  (message "Total movement %s steps." (* h d)))

(provide 'day2)
;;; day2.el ends here
