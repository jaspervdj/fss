;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Usage stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; - (set-ld-redefinition-action '(:doit . :overwrite) state)
; - (ld "binomial-heap.lisp" :ld-skip-proofsp t)
; - :program

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Binomial trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tree-singleton (k v)
    (list 0 k v nil))

(defun tree-rank (tree)
    (car tree))

(defun tree-root-key (tree)
    (cadr tree))

(defun tree-root-value (tree)
    (caddr tree))

(defun tree-children (tree)
    (cadddr tree))

; Private
(defun tree-add-child (tree child)
    (list
        (+ (tree-rank tree) 1)
        (tree-root-key tree)
        (tree-root-value tree)
        (cons child (tree-children tree))))

; tree1 and tree2 must have the same rank. We should probably check this
; somewhere.
(defun tree-merge (tree1 tree2)
    (if (< (tree-root-key tree1) (tree-root-key tree2))
        (tree-add-child tree1 tree2)
        (tree-add-child tree2 tree1)))

; Check that the ordering of children is reversed. This is caused by the way we
; merge trees, but it's not necessarily a bad thing.
; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Binomial heaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun heap-empty ()
    nil)

(defun heap-singleton (k v)
    (list (tree-singleton k v)))

(defun heap-is-empty (heap)
    (not heap))

; The two heaps MUST be sorted
(defun heap-merge (heap1 heap2)
    (declare (xargs :measure (length heap1)))
    (cond
        ((heap-is-empty heap1) heap2)
        ((heap-is-empty heap2) heap1)

        ; Neither are empty. The ranks are equal and we need to merge
        ((= (tree-rank (car heap1)) (tree-rank (car heap2)))
            (heap-merge
                (heap-merge
                    (list (tree-merge (car heap1) (car heap2)))
                    (cdr heap1))
                (cdr heap2)))

        ; Head of tree 1 goes first
        ((< (tree-rank (car heap1)) (tree-rank (car heap2)))
            (cons (car heap1) (heap-merge (cdr heap1) heap2)))

        ; Head of tree 2 goes first
        (t
            (cons (car heap2) (heap-merge heap1 (cdr heap2))))))

(defun heap-insert (heap k v)
    (heap-merge heap (heap-singleton k v)))

; Auxiliary function which starts with a min-tree. We only need to check the
; roots of the different trees.
;
; Because we want to reuse this function for heap-delete-min, this function
; works *like* (but not really) a zipper; we return:
;
;   (reversed left part of list, min element, right part of list)
;
; TODO

(defun heap-min (heap)
    (tree-root-value (heap-min-tree (car heap) (cdr heap))))

; (defun heap-delete-min

(defun heap-from-list (ls)
    (if ls
        (heap-insert
            (heap-from-list (cdr ls))
            (car (car ls))
            (cadr (car ls)))
        (heap-empty)))

; Checks that all trees in the heap are sorted by rank.
(defun heap-check-sorted (heap)
    (if (heap-is-empty heap)
        t
        (if (heap-is-empty (cdr heap))
            t
            (and
                (< (tree-rank (car heap)) (tree-rank (cadr heap)))
                (heap-check-sorted (cdr heap))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Testing crap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-tree ()
    (tree-merge
        (tree-merge
            (tree-singleton 10 "Gilles")
            (tree-singleton 2 "Javache"))
        (tree-merge
            (tree-singleton 5 "Pipi")
            (tree-singleton 6 "Kaka"))))

(defun test-heap ()
    (heap-from-list
        (list
            '(10 "Gilles")
            '(2 "Javache")
            '(5 "Pipi")
            '(0 "VERY IMPORTANT GUY")
            '(6 "Kaka"))))
