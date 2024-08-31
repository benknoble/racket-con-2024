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

;; Lead with Cantrill quote?

;; Got to spend _some_ time showing off the various fun things FHM does. See
;; list of libs for inspiration. Also programmable game data.

;; Dates:
;; initial code: May 2022
;; first game: February 2023
;; Server: started + functional in April 2023
;; ?? Scenarios completed

;; Got some pics of us playing the game (Discord): numbering is roughly reverse chronological
;; TODO: screenshots of app, server page

;; Empirical Software Engineering: We don't know what we don't know.
;; Qualitative: I know that I've enjoyed building in Racket! The complicated
;; stuff isn't less complicated, but it seems accessible and it's been pleasant
;; for my brain.

;; Also enjoyed Racket _because of the community_. Expound: what does that mean?
;; Why?

;; Building in the open
;; Sharing knowledge back (also: emulating Alex Harsányi, sharing lessons from working on a large project)
;;   - e.g., https://alex-hhh.github.io/2020/05/markdown-view.html
;; Responsible consumers (stewards)

;; Call for ideas: how can I return the favors?
