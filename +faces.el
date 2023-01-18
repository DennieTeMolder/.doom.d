;;; $DOOMDIR/+faces.el -*- lexical-binding: t; -*-

;;* Undefined builtin capture-names
(defface tree-sitter-hl-face:float
  '((t :inherit tree-sitter-hl-face:number))
  "Face for floats."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:boolean
  '((t :inherit font-lock-warning-face
     :weight bold))
  "Face for booleans."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:conditional
  '((t :inherit font-lock-keyword-face))
  "Face for conditionals (if, else, switch)."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:repeat
  '((t :inherit font-lock-keyword-face))
  "Face for repeats (for, while)."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:namespace
  '((t :inherit font-lock-doc-face
     :slant italic))
  "Face for namespaces (:: in R)."
  :group 'tree-sitter-hl-faces)
