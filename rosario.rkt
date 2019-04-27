#lang br/quicklang

; todo: fix line by line expansions into list of objects
; then have meta-handle convert list to object, each line
; refer

; TO MAKE EVALUATE, REMOVE THE QUOTE IN FRONT OF 'HANDLE-EXPR!


(define (found? identifier variables)
  (if (empty? variables)
      #f
      (if (eq? (first (first variables)) identifier)
          (second (first variables))
          (found? identifier (rest variables)))))

(define (get-line-of-code code line)
  (if (empty? code)
      #f
      (if (eq? (last (first code)) line)
          (reverse (rest (reverse (first code))))
          (get-line-of-code (rest code) line))))

(define (resolve identifier namespace)
  (if (not (or (number? identifier) (string? identifier)))
      (found? identifier namespace)
      identifier))

(define (++ variables var)
  (map (lambda (x) (if (eq? (first x) var)
                       (list (first x) (+ 1 (second x)))
                       x))
       variables))

(define (interpret code (variables '()) (line 0))
  (if (>= line (length code))
      (void)
      (let* [(current-line (first (get-line-of-code code line)))
             (command (first current-line))]
        (cond [(eq? command 'hi) (begin (display (resolve (second current-line) variables))
                                        (interpret code variables (+ line 1)))]
              [(eq? command '<-) (interpret code (cons (list (second current-line)
                                                             (third current-line))
                                                       variables) (+ line 1))]
              [(eq? command '++) (interpret code (++ variables (second current-line)) (+ line 1))]
              [(eq? command 'label) (interpret code (cons (list (second current-line)
                                                                line)
                                                          variables) (+ line 1))]
              [(eq? command '!=) (interpret code variables
                                            (if (eq? (resolve (second current-line) variables)
                                                     (resolve (third current-line) variables))
                                                (+ line 2) ; if eq skip next line
                                                (+ line 1)))]
              [(eq? command 'goto) (interpret code variables (+ (found? (second current-line) variables) 1))]
              [else (interpret code variables (+ line 1))]))))

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums
    (format-datums '~a src-lines))
  (define module-datum `(module rosario-mod "rosario.rkt"
                          ,@src-datums))
  (datum->syntax #f module-datum))
(provide read-syntax)



(define-macro (sex86-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     (interpret (let ((code (filter (lambda (x) (not (void? x)))
                                    (list 'HANDLE-EXPR ...))))
                  
                  (for/list ([i code]
                             [j (range (length code))])
                    (list i j))))))
(provide (rename-out [sex86-module-begin #%module-begin]))
