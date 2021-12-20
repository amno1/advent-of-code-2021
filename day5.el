;;; day5.el ---                                      -*- lexical-binding: t; -*-

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
(defun gen-straights (x1 y1 x2 y2)
  (cond ((= x1 x2)
         (mapcar (lambda (c) (cons x1 c))
                 (number-sequence (min y1 y2) (max y1 y2))))
        ((= y1 y2)
         (mapcar (lambda (c) (cons c y1))
                 (number-sequence (min x1 x2) (max x1 x2))))))

(defun gen-diagonals (x1 y1 x2 y2)
  (cond ((= (abs (- y2 y1)) (abs (- x2 x1)))
         (let ((steps (abs (- x1 x2))) points x y)
           (setq x x1 y y1)
           (dotimes (_ steps)
             (push (cons x y) points)
             (if (> x x2) (cl-decf x) (cl-incf x))
             (if (> y y2) (cl-decf y) (cl-incf y)))
           (push (cons x2 y2) points)
           points))))

(defun gen-points (coords &optional part2)
  (let ((x1 (nth 0 coords)) (x2 (nth 2 coords)) 
        (y1 (nth 1 coords)) (y2 (nth 3 coords)))
    (if part2
        (gen-diagonals x1 y1 x2 y2)
      (gen-straights x1 y1 x2 y2))))

(defun gen-lines (input &optional part2)
  (let* ((crds (seq-partition input 4)))
    (apply #'append
           (remove nil (mapcar (lambda (l) (gen-points l part2)) crds)))))

(defun update-point (point table)
  (let ((v (or (gethash point table) 0)))
    (puthash point (1+ v) table)))

(defun update-table (input table &optional part2)
  (let ((coords (gen-lines input part2)))
    (mapcar (lambda (p) (update-point p table)) coords)))

(defun count-intersections (table)
  (let ((intersections 0))
    (maphash (lambda (_ v) (if (> v 1) (cl-incf intersections))) table)
    intersections))

(defun read-input ()
  (let (input)
    (with-temp-buffer
      (insert-file-contents "./input5")
      (goto-char 1)
      (while (re-search-forward "[0-9]+" nil t)
        (push (string-to-number (match-string 0)) input)))
    (nreverse input)))

(let ((table (make-hash-table :test 'equal))
      (input (read-input)))
  (update-table input table)
  (message "P1: %s" (count-intersections table))
  (update-table input table 'part2)
  (message "P2: %s" (count-intersections table)))

(provide 'day5)
;;; day5.el ends here
