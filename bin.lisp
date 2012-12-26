;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Queue API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Single-item queue
(defun queue-singleton (k v)
    (list k v nil nil))

(defun queue-key (queue)
    (car queue))

(defun queue-value (queue)
    (cadr queue))

(defun queue-left (queue)
    (caddr queue))

(defun queue-right (queue)
    (cadddr queue))

(defun queue-size (queue)
    (if (endp queue)
        0
        (+
            1
            (queue-size (queue-left queue))
            (queue-size (queue-right queue)))))

(defun queue-insert (queue k v)
    (declare (xargs :measure (queue-size queue)))
    (if (endp queue)
        (queue-singleton k v)
        (if (> (queue-key queue) k)
            (list
                (queue-key queue)
                (queue-value queue)
                (queue-insert (queue-left queue) k v)
                (queue-right queue))
            (list
                (queue-key queue)
                (queue-value queue)
                (queue-left queue)
                (queue-insert (queue-right queue) k v)))))
