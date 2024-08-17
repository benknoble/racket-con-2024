#lang at-exp slideshow

(require slideshow/text
         slideshow/code
         slideshow/step
         slideshow/play
         racket/runtime-path
         file/glob
         racket/draw
         racket/class
         pict/face
         syntax/parse/define
         qi)

(define-syntax-parse-rule (screen-only e:expr)
  (let ([v e])
    (cond
      [printing? (ghost v)]
      [else v])))

(define (screen-clickback p t)
  (screen-only (clickback p t)))

(define-runtime-path here ".")

(define arrow-size 10)

(define (color-code col p)
  (define q (inset p 2))
  (define h (pict-height q))
  (define w (pict-width q))
  (~> (w h 5) filled-rounded-rectangle
      (colorize col) (cc-superimpose p) (refocus p)))

(define-syntax interact-gui-easy
  (syntax-parser
   [(_ {~optional {~seq #:alt alt:expr}}
       e:expr ... t:expr)
    #`(interactive
       {~? alt (blank)}
       (λ (f)
         #,(syntax-local-introduce #'(local-require racket/gui/easy
                                                    racket/gui/easy/operator))
         e ...
         (embed f t)
         void))]))

(define (strikethrough p)
  (define line
    (~> (p) (-< pict-width pict-height) hline))
  (cc-superimpose p line))

(define column
  (ghost
   (rectangle (/ (pict-width titleless-page) 2)
              (pict-height titleless-page))))

(define 1/3column
  (ghost
   (rectangle (/ (pict-width titleless-page) 3)
              (pict-height titleless-page))))

(define 2/3column (hc-append 1/3column 1/3column))

(slide
 #:title "Frosthaven Manager: Built by the Community"
 (~> (titleless-page)
     (cc-superimpose @t{D. Ben Knoble})))
