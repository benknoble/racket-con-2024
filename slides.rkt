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
         qi
         net/sendurl
         frosthaven-manager/monster-db)

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

(define (linkto url)
  (clickback
   (hyperlinkize @t[url])
   (thunk (send-url url))))

(define games
  (for/list ([f (reverse (sort (glob (build-path here "fh-play-??.*")) path<?))])
    (~> (f) bitmap (scale-to-fit titleless-page))))

(define screens
  (for/list ([f (sort (append (glob (build-path here "fh-mobile-??.*"))
                              (glob (build-path here "fh-desktop-??.*")))
                      path<?)])
    (~> (f) bitmap (scale-to-fit titleless-page))))

(define projector
  (~> ("fh-play-projector.jpg")
      (build-path here _)
      bitmap (scale-to-fit titleless-page)))

(define docs
  (~> ("docs.png") (build-path here _) bitmap))

(slide
 #:title "Frosthaven Manager: Built by the Community"
 @para[#:align 'center]{D. Ben Knoble}
 @para[#:align 'center]{@small{@t{He/Him/His}}})

(slide
 #:title "About Ben"
 @item{Tar Heel}
 @item{Luddite @(small @t{more to come soon})}
 @item{Vim, Git, shell aficionado}
 @item{I believe that simple questions have complex answers}
 @subitem{@linkto{https://benknoble.github.io/about/#me}}
 'next
 @item{Personal capacity/Affiliation disclaimer}
 'next
 @item{I do get paid to write Racket!})

(slide
 #:title "About this talk"
 @item{Based on me and my experience building a relatively large, in-use Racket
             program}
 @item{I'm necessarily going to skim}
 @subitem{Let's chat after!}
 @item{Looking forward to future community and collaboration})

(define spoiler-face
  (scale-to-fit (face 'surprised) (t "XX\nXX")))

(slide
 #:title "Frosthaven"
 spoiler-face
 (para (small @t{(Minor)}) @t{ Spoilers ahead!}
       #:align 'center)
 spoiler-face)

;; Got some pics of us playing the game (Discord): numbering is roughly reverse
;; chronological
(for ([g games])
  (slide #:title "Playing Frosthaven" g))

(slide #:title "Playing Frosthaven… with the Manager" projector)

(for ([s screens])
  (slide #:title "Using Frosthaven Manager" s))

(slide
 #:title "Frosthaven Manager"
 @item{First commit: March 2022}
 @subitem{Now: 4k lines of Scribble, 10.5k lines of code and tests}
 @item{First game: February 2023}
 @item{Local web server: April 2023}
 @subitem{Now: largest single module}
 @item{Approx. 41 scenarios completed}
 @item{Infinite fun})

(slide
 #:title "Why build it?"
 'next
 @item{Story time: official app uncertain}
 'next
 @item{Great excuse to try building a GUI}
 'next
 @item{Joy of software: I enjoy Racket}
 @subitem{Complexity not gone, but accessible})

;; Got to spend _some_ time showing off the various fun things FHM does. See
;; list of libs for inspiration. Also programmable game data.

(slide
 #:title "Features"
 'alts
 (list
  (list
   @item{Portable GUI app}
   @subitem{@tt{racket/gui/easy} + @tt{raco distribute} + GitHub Actions}
   @linkto{https://github.com/benknoble/frosthaven-manager/releases})
  (list
   @item{Save & Load}
   @subitem{@tt{racket/serialize} + @tt{racket/fasl}}
   (hc-append 10
              (vc-append @t{Problem}
                         (desktop-machine 1 '(plt)))
              (arrow 20 0)
              (let ([icon (file-icon 70 80 "green")])
                (cc-superimpose icon (scale-to-fit @t{Save} icon)))
              (arrow 20 0)
              @t{Fix, compile}
              (arrow 20 0)
              (vc-append @t{Load}
                         (desktop-machine 1 '(plt)))))
  (list
   @item{Undo}
   @subitem{Observable subscription})
  (list
   @item{Start a server: play by phone or tablet}
   @subitem{@tt{web-server} + Server-sent events from observables}
   (let ([base (~> (titleless-page)
                   (-< pict-width (~> pict-height (* 2/3)))
                   rectangle)]
         [server (desktop-machine 1 '(plt))]
         [clients (map (thunk* (desktop-machine 1)) (range 4))])
     (~> (base)
         (cc-superimpose server)
         (pin-over 0 0 (first clients))
         (pin-over (- (pict-width base)
                      (pict-width (second clients)))
                   0
                   (second clients))
         (pin-over 0
                   (- (pict-height base)
                      (pict-height (third clients)))
                   (third clients))
         (pin-over (- (pict-width base)
                      (pict-width (second clients)))
                   (- (pict-height base)
                      (pict-height (third clients)))
                   (fourth clients))
         (pin-arrows-line
          20 _
          server lt-find
          (first clients) rc-find)
         (pin-arrows-line
          20 _
          server rt-find
          (second clients) lc-find)
         (pin-arrows-line
          20 _
          server lb-find
          (third clients) rc-find)
         (pin-arrows-line
          20 _
          server rb-find
          (fourth clients) lc-find))))
  (list
   @item{Customize the game: make your own loot cards, monsters, and AoE
                   patterns}
   @subitem{@tt{#lang}, of course! @tt{megaparsack}, etc.}
   (~> (default-monster-db)
       file->lines
       (take 23)
       (string-join "\n")
       codeblock-pict
       (scale-to-fit (rectangle (pict-width titleless-page)
                                (* 2/3 (pict-height titleless-page))))))
  (list
   @item{Documentation: User manual, Programmer reference, etc.}
   @subitem{Scribble + GitHub Actions/Pages}
   @subitem{@linkto{https://benknoble.github.io/frosthaven-manager/}}
   (~> (docs)
       (scale-to-fit (rectangle (pict-width titleless-page)
                                (* 2/3 (pict-height titleless-page))))))))

(slide
 #:title "Built by the community"
 @item{Multiple commit co-authors}
 @item{Language Server + Vim community}
 @subitem{Renewed interest in Vim plugins}
 @item{Dozens of Discourse and Discord Q&As}
 @item{You! We wouldn't play our game with our app without community like you.})

;; Community: Lead with Cantrill quote?

(slide
 #:title "Community"
 @it{Everybody needs to get their a** handed to them intellectually}
 @para[#:align 'right]{
    @small{@t{—Bryan Cantrill, }}
    @small{@linkto{https://softwaremisadventures.com/p/bryan-cantrill-oxide}}}
 @para{In the best way possible, there is where I get my intellectual butt handed to me.})

(slide
 #:title "Community"
 @it{We got a lot of adults that are not behaving very well right now.}
 @para[#:align 'right]{@small{@it{—idem.}}}
 @para{Racketeers behave so well! Thanks community moderators.})

;; Also enjoyed Racket _because of the community_. Expound: what does that mean?
;; Why?
(slide
 #:title "Community"
 @item{Earlier, I said I enjoy Racket}
 @item{Programming @it{and} community}
 @subitem{Working together})

;; What's special about community? Warning: rough draft of some ideas incoming.

;; Democratization
;; Education: Racket's educational roots (I'll let the Profs in the room tell
;; you more about the challenges there): but we know there are challenges bring
;; people into the elite fold of technologists. Racket has a head start on
;; tackling those, in my opinion: how do we do more? Put people in control of
;; their digital life.

;; Building in the open
;; Sharing knowledge back (also: emulating Alex Harsányi, sharing lessons from
;; working on a large project)
;;   - e.g., https://alex-hhh.github.io/2020/05/markdown-view.html
;; Responsible consumers (stewards)

;; Call for ideas: how can I return the favors?
;; extract useful components ("GUI utils", curlique, ?)

;; What's next for me?
;; - perf, profiling
;; - searching a large state space for good enough combinations
