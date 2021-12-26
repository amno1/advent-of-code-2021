;;; day8.el ---                                      -*- lexical-binding: t; -*-

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

(defun string-intersect (s1 s2)
  (concat (seq-intersection s1 s2)))

(let ((1s 0) (4s 0) (7s 0) (8s 0))
  (with-temp-buffer
    (insert-file-contents "./input8")
    (while (search-forward "|" nil t)
      (let ((tokens
             (split-string
              (buffer-substring-no-properties (point)
                                              (line-end-position)))))
        (dolist (token tokens)
          (cond ((= (length token) 2) (cl-incf 1s))
                ((= (length token) 3) (cl-incf 7s))
                ((= (length token) 4) (cl-incf 4s))
                ((= (length token) 7) (cl-incf 8s))))))
    (message "P1: %s" (+ 1s 4s 7s 8s))))

(let ((answer 0))
  (with-temp-buffer
    (insert-file-contents "./input8")
    (while (search-forward "|" nil t)
      (let ((digits (make-vector 10 nil))
            (tokens
             (seq-sort-by
              #'length #'< 
              (split-string
               (buffer-substring-no-properties (line-beginning-position)
                                               (line-end-position)))))
            (outputs (split-string
                      (buffer-substring-no-properties
                       (point) (line-end-position))))
            output 5s 6s)
        (setq tokens (cl-map 'list (lambda (s) (seq-sort #'< s)) tokens)
              outputs (cl-map 'list (lambda (s) (seq-sort #'< s)) outputs))
        (dolist (token tokens)
          (cond ((= (length token) 2) (aset digits 1 token))
                ((= (length token) 3) (aset digits 7 token))
                ((= (length token) 4) (aset digits 4 token))
                ((= (length token) 5) (push token 5s))
                ((= (length token) 6) (push token 6s))
                ((= (length token) 7) (aset digits 8 token))))
        (let ((d (aref digits 1)))
          (dolist (s 6s)
            (when (= 1 (length (string-intersect s d)))
              (delete s 6s)
              (aset digits 6 s)
              (dolist (str 5s)
                (when (= 5 (length (string-intersect s str)))
                  (delete str 5s)
                  (aset digits 5 str))))))
        (let ((d (aref digits 5)))
          (dolist (s 6s)
            (cond ((= 5 (length (string-intersect s d)))
                   (aset digits 9 s))
                  ((= 4 (length (string-intersect s d)))
                   (aset digits 0 s)))))
        (let ((d (aref digits 4)))
          (dolist (s 5s)
            (cond ((= 3 (length (string-intersect s d)))
                   (aset digits 3 s))
                  ((= 2 (length (string-intersect s d)))
                   (aset digits 2 s)))))
        (dolist (o outputs)
          (let ((l (length o)))
            (dotimes (i 10)
              (and (= l (length (aref digits i)))
                   (equal o (aref digits i))
                   (push i output)))))
        (setq output (string-join (cl-map 'list #'number-to-string (nreverse output))))
        (cl-incf answer (string-to-number output)))))
  (message "P2: %s" answer))

  (provide 'day8)
;;; day8.el ends here
