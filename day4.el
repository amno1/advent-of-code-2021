;;; day4.el ---                                      -*- lexical-binding: t; -*-

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
(defun get-line (&optional sep)
  (map 'list 'string-to-number
       (split-string (buffer-substring
                      (line-beginning-position) (line-end-position)) sep)))

(defun bingo (seq container)
  (car (sort (mapcar #'(lambda (x) (gethash x container)) seq) '>)))

(defun sum-unmarked (winner drawn)
  (let ((seen (cl-subseq drawn 0 (1+ (car winner))))
        (board (apply 'concatenate 'list (cdr winner))))
    (apply #'+ (cl-remove-if #'(lambda (x) (seq-contains-p seen x)) board))))

(defun first-winner (boards positions)
  (let ((last-winner most-positive-fixnum)
        (winner-board (car boards)) tmp)
    (dolist (board boards)
      (dolist (row board)
        (setq tmp (bingo row positions))
        (if (< tmp last-winner)
            (setq winner-board board last-winner tmp)))
      (dolist (col (apply 'cl-mapcar 'list board))
        (setq tmp (bingo col positions))
        (if (< tmp last-winner)
            (setq winner-board board last-winner tmp))))
    (cons last-winner winner-board)))

(defun last-winner (boards positions)
  (let ((winner (first-winner boards positions)))
    (if (= (length boards) 1) winner
      (last-winner (remove (cdr winner) boards) positions))))

(with-temp-buffer
  (insert-file "./input4")
  (goto-char 1)
  (let ((numbers (vconcat (get-line ",")))
        (positions (make-hash-table :size (length numbers)))
        boards)
    (dotimes (i (length numbers))
      (puthash (aref numbers i) i positions))
    (forward-line)
    (while (not (eobp))
      (let (board)
        (forward-line)
        (dotimes (__ 5)
          (push (get-line) board)
          (forward-line))
        (push (nreverse board) boards)))
    (setq boards (nreverse boards)))
  (let ((first (first-winner boards positions))
        (last (last-winner boards positions)))
    (message "P1: %s" (* (sum-unmarked first numbers)
                         (aref numbers (car first))))
    (message "P2: %s" (* (sum-unmarked last numbers)
                         (aref numbers (car last))))))

(provide 'day4)
;;; day4.el ends here
