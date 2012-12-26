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
        (if (= (queue-key queue) k)
            queue
            (if (< k (queue-key queue))
                (list
                    (queue-key queue)
                    (queue-value queue)
                    (queue-insert (queue-left queue) k v)
                    (queue-right queue))
                (list
                    (queue-key queue)
                    (queue-value queue)
                    (queue-left queue)
                    (queue-insert (queue-right queue) k v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Queue properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Check that all items in the queue are larger or smaller than a given item

(defun queue-all-smaller (queue x)
    (if (endp queue)
        t
        (and
            (< (queue-key queue) x)
            (queue-all-smaller (queue-left queue) x)
            (queue-all-smaller (queue-right queue) x))))

(defun queue-all-larger (queue x)
    (if (endp queue)
        t
        (and
            (> (queue-key queue) x)
            (queue-all-larger (queue-left queue) x)
            (queue-all-larger (queue-right queue) x))))

; A general validity check for the queue...

(defun queue-valid (queue)
    (if (endp queue)
        t
        (and
            (integerp (queue-key queue))
            (queue-all-smaller (queue-left queue) (queue-key queue))
            (queue-all-larger (queue-right queue) (queue-key queue))
            (queue-valid (queue-left queue))
            (queue-valid (queue-right queue)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Queue theorems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A simple queue with one element is valid (easy)

(defthm queue-singleton-valid
    (implies
        (integerp k)
        (queue-valid (queue-singleton k v))))

; Two utility theorems to make `queue-insert-valid` work

(defthm queue-insert-smaller
    (implies
        (and
            (< k x)
            (queue-all-smaller queue x))
        (queue-all-smaller (queue-insert queue k v) x)))

(defthm queue-insert-larger
    (implies
        (and
            (> k x)
            (queue-all-larger queue x))
        (queue-all-larger (queue-insert queue k v) x)))

; A theorem that insertion preserves validity

(defthm queue-insert-valid
    (implies
        (and
            (queue-valid queue)
            (integerp k))
        (queue-valid (queue-insert queue k v))))
