.POSIX:
SHELL = /bin/sh

slides.pdf: slides.rkt
	racket slides.rkt -D -o $@
