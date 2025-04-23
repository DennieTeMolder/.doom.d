;;; $DOOMDIR/+faces.el -*- lexical-binding: t; -*-

;;* Undefined builtin capture-names
(defface tree-sitter-hl-face:float
  '((t :inherit tree-sitter-hl-face:number))
  "Face for floating point numbers."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.function
  '((t :inherit font-lock-keyword-face))
  "Face for function arguments."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:namespace
  '((t :inherit font-lock-doc-face
     :slant italic))
  "Face for namespaces (:: in R)."
  :group 'tree-sitter-hl-faces)
