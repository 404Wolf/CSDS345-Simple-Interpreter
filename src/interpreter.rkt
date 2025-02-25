#lang racket

(require "simpleParser.rkt")

(define interpret-stdin (lambda () (interpret (read-line))))

(define interpret (lambda (filename) (printf filename)))
