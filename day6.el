;;; day6.el ---                                      -*- lexical-binding: t; -*-

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

;; at least 3: solutions
;; 1. naive use list and update entire list for each step
;; This would be very cpu intensive, i.e. slow and would need a lot of memory,
;; so I didn't coded this approach. Would be very straightforward to implement.

;; 2. naive build tree and calculate depth first, only one branch at a time in
;; memory and not iterating entire list all the time.
;; It is relatively simple to code; just one recursive function, and is
;; plausible to calculate, albeit somewhat slow. Coded for illustration of run
;; time difference.

;; (defun read-input (steps)
;;   (with-temp-buffer
;;     (insert-file-contents "./input6.test")
;;     (mapcar (lambda (x) (cons x steps))
;;             (mapcar #'string-to-number (split-string (buffer-string) ",")))))

;; (defun simulate-fishes (days)
;;   (let ((input (read-input days))
;;         (count 0))
;;     (cl-labels ((sim-fish ()
;;                   (let* ((fish (pop input))
;;                          (steps (cdr fish)))
;;                     (cl-incf count)
;;                     (while (> steps 0)
;;                       (cl-decf steps)
;;                       (cond ((= (car fish) 0)
;;                              (setf (car fish) 6)
;;                              (push (cons 8 steps) input))
;;                             (t (cl-decf (car fish)))))
;;                     (if input (sim-fish) count))))
;;       (sim-fish))))

;; 3. only track states [0 - 8] and calculate count for each state at each
;; iteration. Takes almost no ram, only does few integer operations and is very
;; fast and is the strategy used to calculate the solution.

;;; Code:
(defun read-input ()
  (let ((input
         (with-temp-buffer
           (insert-file-contents "./input6")
           (sort
            (mapcar #'string-to-number
                    (split-string (buffer-string) ",")) #'< )))
        (counts (make-vector 8 0)) tmp n)
      (while input
        (setq n (pop input))
        (cl-incf (aref counts n)))
      (dotimes (i 8)
        (unless (= (aref counts i) 0)
          (push (cons i (aref counts i)) tmp)))
      (cl-sort tmp #'< :key #'car)))

(defun simulate-fishes (days)
  (let ((input (read-input))
        (count 0))
    (dotimes (_i days)
      (let ((fish (if (= (caar input) 0) (pop input))))
        (dolist (i input) (cl-decf (car i)))
        (when fish
          (push (cons 8 (cdr fish)) input)
          (if (assoc '6 input)
              (cl-incf (cdr (assoc '6 input)) (cdr fish))
            (push (cons 6 (cdr fish)) input))
          (setq input (cl-sort input #'< :key #'car)))))
    (mapc (lambda (x) (cl-incf count (cdr x))) input)
    count))

(progn
  (message "P1: %s" (simulate-fishes 80))
  (message "P2: %s" (simulate-fishes 256)))
  
(provide 'day6)
;;; day6.el ends here
