;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Queue API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Empty queue

(defun queue-empty ()
    nil)

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

(defun queue-null (queue)
    (endp queue))

; Count the number of nodes in the queue. Useful as measure, to check that
; recursion ends at some point.

(defun queue-size (queue)
    (if (queue-null queue)
        0
        (+
            1
            (queue-size (queue-left queue))
            (queue-size (queue-right queue)))))

; Insert a new item into the queue.

(defun queue-insert (k v queue)
    (declare (xargs :measure (queue-size queue)))
    (if (queue-null queue)
        (queue-singleton k v)
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
                (queue-insert k v (queue-right queue))))))

; Find the minimum node of the queue. This is simply the leftmost node. It does
; not work for empty queues.

(defun queue-find-min (queue)
    (if (queue-null (queue-left queue))
        queue
        (let
            ((left-min (queue-find-min (queue-left queue))))
            (if (< (queue-key left-min) (queue-key queue))
                left-min
                queue))))

; Delete the minimum node of the queue.

(defun queue-delete-min (queue)
    (if (queue-null (queue-left queue))
        ; No left child, hence this node has the smallest key, and the updated
        ; queue is simply the right child.
        (queue-right queue)
        (list
            (queue-key queue)
            (queue-value queue)
            (queue-delete-min (queue-left queue))
            (queue-right queue))))

; Merge two queues

(defun queue-merge (q1 q2)
    (if (queue-null q1)
        q2
        (queue-insert (queue-key q1) (queue-value q1)
            (queue-merge (queue-left q1)
                (queue-merge (queue-right q1) q2)))))

; Change the priority of every item which equals 'v' to 'k'

(defun queue-change-priority (k v queue)
    (if (queue-null queue)
        queue
        (queue-insert
            (if (equal v (queue-value queue))
                k
                (queue-key queue))
            (queue-value queue)
            (queue-merge
                (queue-change-priority k v (queue-left queue))
                (queue-change-priority k v (queue-right queue))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Queue properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Check that all elements in the queue are less than a given item 'x'

(defun queue-all-lt (x queue)
    (if (queue-null queue)
        t
        (and
            (< (queue-key queue) x)
            (queue-all-lt x (queue-left queue))
            (queue-all-lt x (queue-right queue)))))

; Check that all elements in the queue are greater or equal to a given item 'x'

(defun queue-all-get (x queue)
    (if (queue-null queue)
        t
        (and
            (>= (queue-key queue) x)
            (queue-all-get x (queue-left queue))
            (queue-all-get x (queue-right queue)))))

; Check that the queue contains a key 'k' with value 'v'. This is a slow
; operation, only meant for proving stuff.

(defun queue-contains (k v queue)
    (if (queue-null queue)
        nil
        (or
            (and
                (= (queue-key queue) k)
                (equal (queue-value queue) v))
            (queue-contains k v (queue-left queue))
            (queue-contains k v (queue-right queue)))))

; A general validity check for the queue...

(defun queue-valid (queue)
    (if (queue-null queue)
        t
        (and
            (integerp (queue-key queue))
            (queue-all-lt (queue-key queue) (queue-left queue))
            (queue-all-get (queue-key queue) (queue-right queue))
            (queue-valid (queue-left queue))
            (queue-valid (queue-right queue)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Queue theorems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The empty queue is valid

(defthm queue-empty-valid
    (queue-valid (queue-empty)))

; A singleton queue with one element is valid (easy)

(defthm queue-singleton-valid
    (implies
        (integerp k)
        (queue-valid (queue-singleton k v))))

; A singleton queue contains the element we inserted

(defthm queue-singleton-contains
    (queue-contains k v (queue-singleton k v)))

; Two utility theorems to make `queue-insert-valid` work

(defthm queue-insert-smaller
    (implies
        (and
            (< k x)
            (queue-all-lt x queue))
        (queue-all-lt x (queue-insert k v queue))))

(defthm queue-insert-larger
    (implies
        (and
            (>= k x)
            (queue-all-get x queue))
        (queue-all-get x (queue-insert k v queue))))

; Another utility: if we delete the minimum from a queue, this does not change
; the maximum value...

(defthm queue-delete-min-all-smaller
    (implies
        (and
            (integerp x)
            (queue-all-lt x queue)
            (not (queue-null queue)))
        (queue-all-lt x (queue-delete-min queue))))

; A theorem that insertion preserves validity

(defthm queue-insert-valid
    (implies
        (and
            (queue-valid queue)
            (integerp k))
        (queue-valid (queue-insert k v queue))))

; A theorem that if a queue contains a pair, and we insert an item, the queue
; still contains the first pair

(defthm queue-insert-already-contains
    (implies
        (and
            (queue-valid queue)
            (queue-contains k v queue)
            (integerp k)
            (integerp x))
        (queue-contains k v (queue-insert x y queue))))

; If we insert an item, the resulting queue must contain that pair

(defthm queue-insert-contains
    (implies
        (and
            (queue-valid queue)
            (integerp k))
        (queue-contains k v (queue-insert k v queue))))

; A theorem that deletion preserves validity

(defthm queue-delete-min-valid
    (implies
        (and
            (not (queue-null queue))
            (queue-valid queue))
        (queue-valid (queue-delete-min queue))))

; Utility: transitivity of queue-all-get

(defthm queue-all-get-transitivity
    (implies
        (and
            (<= y x)
            (queue-all-get x queue))
        (queue-all-get y queue)))

; Utility: all elements are larger than the minimum (this one takes a while to
; prove...)

(defthm queue-find-min-all-get
    (implies
        (and
            (not (queue-null queue))
            (queue-valid queue))
        (queue-all-get
            (queue-key (queue-find-min queue))
            queue)))

; If 'x' is smaller than all elements in the queue... if we delete the minimum
; element from the queue, this must still hold.

; (defthm queue-delete-min-all-larger
;     (implies
;         (and
;             (not (queue-null queue))
;             (queue-all-get x queue))
;         (queue-all-get x (queue-delete-min queue))))

; If we delete the minimum from a tree, all elements in the tree must be greater
; or equal to this minimum.

(defthm queue-find-min-delete-min
    (implies
        (and
            (not (queue-null queue))
            (queue-valid queue))
        (queue-all-get
            (queue-key (queue-find-min queue))
            (queue-delete-min queue))))

; After deleting the minimum, the tree must be smaller in size

(defthm queue-delete-min-smaller-size
    (implies
        (and
            (not (queue-null queue))
            (queue-valid queue))
        (<
            (queue-size (queue-delete-min queue))
            (queue-size queue))))

; Merging preserves validity

(defthm queue-merge-valid
    (implies
        (and
            (queue-valid q1)
            (queue-valid q2))
        (queue-valid (queue-merge q1 q2))))

; Merging should preserve the contained elements

(defthm queue-merge-contains-right
    (implies
        (and
            (integerp k)
            (queue-valid q1)
            (queue-valid q2)
            (queue-contains k v q2))
        (queue-contains k v (queue-merge q1 q2))))

(defthm queue-merge-contains-left
    (implies
        (and
            (integerp k)
            (queue-valid q1)
            (queue-valid q2)
            (queue-contains k v q1))
        (queue-contains k v (queue-merge q1 q2))))

; Changing the priority of an item preserves validity

(defthm queue-change-priority-valid
    (implies
        (and
            (integerp k)
            (queue-valid queue))
        (queue-valid (queue-change-priority k v queue))))

; Suppose the queue contains a value 'v' with priority 'k'. If we change the
; priority to 'x', the queue should now contain a value 'v' with priority 'x'.

; TODO: additionally, it should NOT contain ('k', 'v')
(defthm queue-change-priority-contains
    (implies
        (and
            (integerp k)
            (integerp x)
            (queue-valid queue)
            (queue-contains k v queue))
        (queue-contains x v (queue-change-priority x v queue))))

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
