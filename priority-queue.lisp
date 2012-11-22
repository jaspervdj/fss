;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Usage stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; - (set-ld-redefinition-action '(:doit . :overwrite) state)
; - (ld "priority-queue.lisp" :ld-skip-proofsp t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Binomial trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tree-singleton (k v)
    (list 0 k v NIL))

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
