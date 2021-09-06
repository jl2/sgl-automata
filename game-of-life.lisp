;; game-of-life.lisp

;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :sgl-automata)

(defclass game-of-life (2d-cellular-automata)
  ())


(defun create-game-of-life (width height
                            &key
                              (max-instances (* width height))
                              (initial-data (make-array (* width height)
                                                        :element-type 'bit
                                                        :initial-contents
                                                        (loop for i below (* width height)
                                                              collecting (random 2)))))
  "Create a cellular automata of the specified size."
  (make-instance 'game-of-life
                 :width width
                 :height height
                 :current-board-idx 0
                 :max-instances max-instances
                 :current-board-data initial-data
                 :next-board-data (make-array (list (* width height))
                                              :initial-element 0
                                              :element-type 'bit)))

(defmethod compute-next ((object game-of-life))
  (with-slots (instance-count width height current-board-idx current-board-data next-board-data) object
    (loop
      ;; Calculate the quad location
      for i fixnum from 0 below width
      do
         (loop
           for j fixnum from 0 below height
           for ncount = (count-neighbors object i j)
           do
              (cond ((and (is-off object i j)
                          (= ncount 3))
                     (turn-on object i j))
                    ((and (is-on object i j)
                          (or (= ncount 2)
                              (= ncount 3)))
                     (turn-on object i j))
                    (t
                     (turn-off object i j)))))
    (rotatef next-board-data current-board-data)
    (incf current-board-idx)))
