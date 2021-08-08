;; sgl-automata.lisp
;;
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

(setf sgl:*shader-dirs*
      (adjoin (asdf:system-relative-pathname :sgl-automata "shaders/") sgl:*shader-dirs*))

(defclass cellular-automata (instanced-opengl-object)
  ((style :initform (make-style "automata" "sgl-automata-vertex.glsl" "point-fragment.glsl"))
   (max-instances :initform 1000 :initarg :max-instances)
   (width :initform 50))
  )

(defmethod update ((object cellular-automata) elapsed-seconds)
  (with-slots (buffers instance-count max-instances width) object
    (when (< instance-count max-instances)
      (let ((buffer (get-buffer object :obj-transform)))
        (with-slots (pointer) buffer
          (sgl:fill-pointer-offset (vec3 (1- (/ (random width) (/ width 2.0))) (1- (/ (random width) (/ width 2.0))) 0.0)
                                   pointer
                                   (* instance-count 3)))
        (incf instance-count)
        (reload buffer)
        (format t "Instance-count is now ~a~%" instance-count))))
  )
(defmethod initialize-buffers ((object cellular-automata) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))
  (with-slots (width) object
    (let ((fw (/ 1.0f0 width)))
      (set-buffer object
                  :vertices
                  (make-instance
                   'attribute-buffer
                   :pointer (to-gl-array
                             :float
                             28
                             (list 0.0f0 0.0f0 0.0f0
                                   0.1f0 0.8f0 0.1f0 1.0f0

                                   fw 0.0f0 0.0f0
                                   0.1f0 0.8f0 0.1f0 1.0f0

                                   fw fw 0.0f0
                                   0.1f0 0.8f0 0.1f0 1.0f0

                                   0.0f0 fw 0.0f0
                                   0.8f0 0.8f0 0.1f0 1.0f0))
                   :stride nil
                   :attributes '(("in_position" . :vec3)
                                 ("in_color" . :vec4))
                   :usage :static-draw
                   :free t))))
  (set-buffer object
              :indices
              (make-instance
               'index-buffer
               :idx-count 6
               :pointer (to-gl-array :unsigned-int 6 #(0 1 2 0 2 3))
               :stride nil
               :usage :static-draw
               :free t))
  (with-slots (max-instances instance-count) object
    (setf instance-count 0)
    (set-buffer object
                :obj-transform (make-instance
                                'instance-buffer
                                :pointer (to-gl-array :float
                                                      (* max-instances 3)
                                                      (vec3 0.0 0.0 0.0))
                                :stride nil
                                :attributes '(("translation" . :vec3))
                                :usage :static-draw
                                :free nil))))
