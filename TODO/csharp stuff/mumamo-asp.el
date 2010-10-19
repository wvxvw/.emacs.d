
;; (defun mumamo-chunk-asp (pos min max)
;;   "Find <% ... %>.  Return range and 'asp-js-mode.
;; See `mumamo-find-possible-chunk' for POS, MIN and MAX."
;;   (mumamo-find-possible-chunk pos min max
;;                               'mumamo-search-bw-exc-start-asp
;;                               'mumamo-search-bw-exc-end-asp
;;                               'mumamo-search-fw-exc-start-asp
;;                               'mumamo-search-fw-exc-end-asp))

;; (defun mumamo-search-bw-exc-start-asp (pos min)
;;   (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min "<%")))
;;     (and exc-start
;;          (<= exc-start pos)
;;          (cons exc-start 'asp-mode))))
                                
;; (defun mumamo-search-bw-exc-end-asp (pos min)
;;   (mumamo-chunk-end-bw-str-inc pos min "%>"))

;; (defun mumamo-search-fw-exc-start-asp (pos max)
;;   (mumamo-chunk-start-fw-str-inc pos max "<%"))

;; (defun mumamo-search-fw-exc-end-asp (pos max)
;;   (mumamo-chunk-end-fw-str-inc pos max "%>"))
