;; Using https://www.emacswiki.org/emacs/widget-demo.el as an example this file
;; contains an example for how to create a tree-widget from an alist. The code
;; works for alist containing lists of symbols only, however it is easy to adapt
;; to support alists which containing elements of any type.

;; Although the tree-widget itself can have a :tag and :value property, the
;; latter property can not be in the :expander because or some reason it does
;; not get passed (maybe because it sets an 'external' value and not an
;; 'internal' as explained in
;; https://www.gnu.org/software/emacs/manual/html_mono/widget.html#Basic-Types)
(defun widget-tree-widget (alist)
  `(tree-widget
    :node (item
           :tag ,(symbol-name (car alist))
           :value ,(cdr alist))
    :expander widget-tree-expand))

(defun widget-tree-expand (tree)
  (let ((alist (widget-get (tree-widget-node tree) :value)))
    (mapcar (lambda (x)
              (if (listp x)
                  (widget-tree-widget x)
                `(item :tag ,(symbol-name x))))
            alist)))

(defun alist-to-tree-widget (alist)
  (widget-create (widget-tree-widget (append '(root) alist))))

(alist-to-tree-widget '((a (b (c d)) (e f))))

[-] root
 `-[-] a
    |-[-] b
    |  `-[-] c
    |     `-  d
    `-[-] e
       `-  f
