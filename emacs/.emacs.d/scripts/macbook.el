;; Macbook Air specific configuration options
(defun is-macbook ()
  "Return `t` if running on the macbook."
  (string-equal (system-name) "Christians-MacBook-Air.local"))

;; Unbind some mac-specific bindings
(when (is-macbook)
 (progn
   (define-key global-map [?\s-,] nil)
   (define-key global-map [?\s-'] nil)
   (define-key global-map [?\s-`] nil)
   (define-key global-map [?\s-~] nil)
   (define-key global-map [?\s--] nil)
   (define-key global-map [?\s-:] nil)
   (define-key global-map [?\s-?] nil)
   (define-key global-map [?\s-^] nil)
   (define-key global-map [?\s-&] nil)
   (define-key global-map [?\s-C] nil)
   (define-key global-map [?\s-D] nil)
   (define-key global-map [?\s-E] nil)
   (define-key global-map [?\s-L] nil)
   (define-key global-map [?\s-M] nil)
   (define-key global-map [?\s-S] nil)
   (define-key global-map [?\s-a] nil)
   (define-key global-map [?\s-c] nil)
   (define-key global-map [?\s-d] nil)
   (define-key global-map [?\s-e] nil)
   (define-key global-map [?\s-f] nil)
   (define-key global-map [?\s-g] nil)
   (define-key global-map [?\s-h] nil)
   (define-key global-map [?\s-H] nil)
   (define-key global-map [?\s-j] nil)
   (define-key global-map [?\s-k] nil)
   (define-key global-map [?\s-l] nil)
   (define-key global-map [?\s-m] nil)
   (define-key global-map [?\s-n] nil)
   (define-key global-map [?\s-o] nil)
   (define-key global-map [?\s-p] nil)
   (define-key global-map [?\s-q] nil)
   (define-key global-map [?\s-s] nil)
   (define-key global-map [?\s-t] nil)
   (define-key global-map [?\s-u] nil)
   (define-key global-map [?\s-v] nil)
   (define-key global-map [?\s-w] nil)
   (define-key global-map [?\s-x] nil)
   (define-key global-map [?\s-y] nil)
   (define-key global-map [?\s-z] nil)
   (define-key global-map [?\s-|] nil)
   (define-key global-map [s-kp-bar] nil)))



;; Make sure left alt = Super, left cmd = Meta,
;; and right alt is alt gr
(when (is-macbook)
 (progn
   ;; (setq mac-option-key-is-meta nil
   (setq mac-option-modifier 'super
         mac-command-modifier 'meta
         mac-right-option-modifier nil)))
