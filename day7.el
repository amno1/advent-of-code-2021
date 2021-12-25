;;; day7.el ---                                      -*- lexical-binding: t; -*-

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

;; two alternative calculations for part 2:
;; (ceiling (* 0.5 cost)))) <-- multiply by 1/2 instead of right shift
;; (dotimes (i d) (cl-incf cost i)) <-- increment in loop, exact but slow

;;; Code:
(defvar crab-input 
         (with-temp-buffer
           (insert-file-contents "./input7")
            (mapcar #'string-to-number
                    (split-string (buffer-string) ","))))

(defsubst distance-cost1 (p1 p2) (abs (- p1 p2)))
(defun distance-cost2 (p1 p2)
  (let* ((d (distance-cost1 p1 p2))
         (cost (lsh (+ (* d d) d) -1)))
    cost))

(defun count-cost (&optional p2)
  (let* ((costs (make-vector (cl-reduce #'max crab-input) 0)))
    (dotimes (cost (length costs))
      (dolist (i crab-input)
        (cl-incf (aref costs cost)
                 (if p2 (distance-cost2 i cost)
                   (distance-cost1 i cost)))))
    (cl-reduce 'min costs)))

(defun part-1 ()
  (message "P1: %s" (count-cost)))

(defun part-2 ()
  (message "P2: %s" (count-cost 'p2)))

(provide 'day7)
;;; day7.el ends here
