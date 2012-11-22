;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Usage stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; - (set-ld-redefinition-action '(:doit . :overwrite) state)
; - (ld "priority-queue.lisp" :ld-skip-proofsp t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Red-black trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *red* 0)
(defconst *black* 1)

(defun singleton (k v)
    (list *black* k v NIL NIL))

(defun node-color (node)
    (car node))

(defun node-key (node)
    (cadr node))

(defun node-value (node)
    (nth 2 node))

(defun node-left (node)
    (nth 3 node))

(defun node-right (node)
    (nth 4 node))

(defun node-insert (node k v)
    (if node
        (if (< k (node-key node))
            (list
                (node-color node)
                (node-key node)
                (node-value node)
                (node-insert (node-left node) k v)
                (node-right node))
            (list
                (node-color node)
                (node-key node)
                (node-value node)
                (node-left node)
                (node-insert (node-right node) k v)))
        (list *red* k v NIL NIL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-tree ()
    (node-insert
        (node-insert
            (node-insert
                (singleton 1 "Hi")
                2 "sup")
            5 "Kaka")
        0 "Pipi"))
