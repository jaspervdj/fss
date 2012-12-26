;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Queue API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Single-item queue

(defun queue-singleton (k v)
    (list k v nil nil))

; Some accessors for queues, this improves readability

(defun queue-key (queue)
    (car queue))

(defun queue-value (queue)
    (cadr queue))

(defun queue-left (queue)
    (caddr queue))

(defun queue-right (queue)
    (cadddr queue))

; Check if a queue is empty

(defun queue-empty (queue)
    (endp queue))

; Count the number of nodes in the queue. Useful as measure, to check that
; recursion ends at some point.

(defun queue-size (queue)
    (if (queue-empty queue)
        0
        (+
            1
            (queue-size (queue-left queue))
            (queue-size (queue-right queue)))))

; Insert a new item into the queue.

(defun queue-insert (k v queue)
    (declare (xargs :measure (queue-size queue)))
    (if (queue-empty queue)
        (queue-singleton k v)
        (if (= (queue-key queue) k)
            queue
            (if (< k (queue-key queue))
                (list
                    (queue-key queue)
                    (queue-value queue)
                    (queue-insert k v (queue-left queue))
                    (queue-right queue))
                (list
                    (queue-key queue)
                    (queue-value queue)
                    (queue-left queue)
                    (queue-insert k v (queue-right queue)))))))

; Find the minimum node of the queue. This is simply the leftmost node. It does
; not work for empty queues.

(defun queue-find-min (queue)
    (if (queue-empty (queue-left queue))
        queue
        (let
            ((left-min (queue-find-min (queue-left queue))))
            (if (< (queue-key left-min) (queue-key queue))
                left-min
                queue))))

; Delete the minimum node of the queue.

(defun queue-delete-min (queue)
    (if (queue-empty (queue-left queue))
        ; No left child, hence this node has the smallest key, and the updated
        ; queue is simply the right child.
        (queue-right queue)
        (list
            (queue-key queue)
            (queue-value queue)
            (queue-delete-min (queue-left queue))
            (queue-right queue))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Queue properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Check that all items in the queue are larger or smaller than a given item

(defun queue-all-smaller (x queue)
    (if (queue-empty queue)
        t
        (and
            (< (queue-key queue) x)
            (queue-all-smaller x (queue-left queue))
            (queue-all-smaller x (queue-right queue)))))

(defun queue-all-larger (x queue)
    (if (queue-empty queue)
        t
        (and
            (> (queue-key queue) x)
            (queue-all-larger x (queue-left queue))
            (queue-all-larger x (queue-right queue)))))

; A general validity check for the queue...

(defun queue-valid (queue)
    (if (queue-empty queue)
        t
        (and
            (integerp (queue-key queue))
            (queue-all-smaller (queue-key queue) (queue-left queue))
            (queue-all-larger (queue-key queue) (queue-right queue))
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
            (queue-all-smaller x queue))
        (queue-all-smaller x (queue-insert k v queue))))

(defthm queue-insert-larger
    (implies
        (and
            (> k x)
            (queue-all-larger x queue))
        (queue-all-larger x (queue-insert k v queue))))

; A theorem that insertion preserves validity

(defthm queue-insert-valid
    (implies
        (and
            (queue-valid queue)
            (integerp k))
        (queue-valid (queue-insert k v queue))))

; If 'x' is smaller than all elements in the queue... it should also be smaller
; than the `queue-find-min` result

(defthm queue-find-min-bounded
    (implies
        (and
            (not (queue-empty queue))
            (queue-all-larger x queue))
        (< x (queue-key (queue-find-min queue)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Playing around/tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(skip-proofs
    (defun test-tree ()
        (queue-insert 10 "Internet"
            (queue-insert 15 "Food"
                (queue-insert 12 "Women"
                    (queue-insert 17 "Shelter"
                        (queue-singleton 14 "Beer")))))))
