;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Queue API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Empty queue
(defun empty ()
    nil)

; Check if empty
(defun emptyp (queue)
    (endp queue))

; Insert item into queue
(defun insert (queue k v)
    (cons (cons k v) queue))

; Single-item queue
(defun singleton (k v)
    (insert (empty) k v))

; Priority is the first element
(defun node-priority (node)
    (car node))

; And value the second
(defun node-value (node)
    (cdr node))

; Find minimum value
(defun find-min-node (queue)
    (if (emptyp (cdr queue))
        (car queue)
        (let
            (
                (cdr-min (find-min-node (cdr queue)))
                (car-min (car queue)))
            (if (< (node-priority car-min) (node-priority cdr-min))
                car-min
                cdr-min))))

; Find the minimum value
(defun find-min (queue)
    (node-value (find-min-node queue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Verification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Check the structure of a queue: (integer, value) tuples in a list
(defun valid-queue (queue)
    (if (emptyp queue)
        t
        (and
            (consp (car queue))
            (integerp (node-priority (car queue)))
            (valid-queue (cdr queue)))))

; The empty queue should be valid
(defthm empty-valid
    (valid-queue (empty)))

; Insert an item into a queue, queue remains valid
(defthm insert-valid
    (implies
        (and
            (valid-queue queue)
            (integerp k))
        (valid-queue (insert queue k v))))

; Singleton queues should also be valid...
(defthm singleton-valid
    (implies
        (integerp k)
        (valid-queue (singleton k v))))

; Checks that all priorities in the queue are at least as large as the given
; one...
(defun is-min-priority (queue min-priority)
    (if (emptyp queue)
        t
        (and
            (<= min-priority (node-priority (car queue)))
            (is-min-priority (cdr queue) min-priority))))

; Combine find-min and is-min
(defthm find-min-is-min-priority
    (implies
        (and
            (valid-queue queue)
            (not (emptyp queue)))
        (is-min-priority queue (node-priority (find-min-node queue)))))
