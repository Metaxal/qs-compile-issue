#lang at-exp racket/base
(require
  drracket/tool ; necessary to build a drracket plugin
  racket/class
  racket/gui/base
  racket/unit
  "utils.rkt")

(provide tool@)

;;; Run with
;;; export PLTSTDERR=debug@trycompile && drracket &

;=======================;
;=== DrRacket Plugin ===;
;=======================;

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define script-menu-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)

        (define menu-bar (send this get-menu-bar))

        (define scripts-menu
          (new menu% [parent menu-bar] [label "Tr&yCompile"]))
        
        (void (new menu-item% [parent scripts-menu]
                   [label "Tr&y to compile"]
                   [callback (λ _ (try-compile))]))
        ))

    (define (phase1) (void))
    (define (phase2) (void))

    (drracket:get/extend:extend-unit-frame script-menu-mixin)

    ;; On startup
    (try-compile)))
