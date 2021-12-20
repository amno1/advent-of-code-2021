;;; day3.el ---                                      -*- lexical-binding: t; -*-

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
(let* ((matrix (with-temp-buffer
                 (insert-file-contents-literally "./input.3")
                 (mapcar '(lambda (s) (append s nil))
                         (split-string (buffer-string)))))
       (transpose (apply 'mapcar* 'list matrix))
       (ones (mapcar '(lambda (line) (count ?1 line)) transpose))
       (zeros (mapcar '(lambda (line) (count ?0 line)) transpose))
       (gamma (mapconcat 'identity
               (cl-mapcar '(lambda (x y) (if (> x y) "1" "0")) ones zeros)))
       (epsilon (mapconcat 'identity
                 (cl-mapcar '(lambda (x y) (if (< x y) "1" "0")) ones zeros))))
  (message "Power consumption is %s."
           (* (string-to-number gamma 2) (string-to-number epsilon 2))))

;;; Part II
(defun gas-rating (&optional input position co2)
  (let* ((matrix (or input (with-temp-buffer
                             (insert-file-contents-literally "./input3")
                             (mapcar #'(lambda (s) (append s nil))
                                     (split-string (buffer-string))))))
         (i (or position 0))
         (transpose (apply #'cl-mapcar 'list matrix))
         (num1 (mapcar #'(lambda (line) (count ?1 line)) transpose))
         (num0 (mapcar #'(lambda (line) (count ?0 line)) transpose)))
    (if (>= (elt num1 i) (elt num0 i))
        (dolist (l matrix)
          (if (char-equal (if co2 ?1 ?0) (elt l i))
              (setq matrix (delete l matrix))))
      (dolist (l matrix)
        (if (char-equal (if co2 ?0 ?1) (elt l i))
            (setq matrix (remove l matrix)))))
    (if (= (length matrix) 1)
        (string-to-number (mapconcat 'identity matrix) 2)
      (gas-rating matrix (1+ i) co2))))

(defun life-rating () (* (gas-rating) (gas-rating nil nil t)))

(provide 'day3)
;;; day3.el ends here
