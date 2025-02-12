(define-module (aoc logging)
  #:export (*log-level* log-init log))

;; Map log levels to priorities
(define (log-level-map level)
  (case level
    ((DEBUG) 1)
    ((INFO) 2)
    ((WARN) 3)
    ((ERROR) 4)
    ((CRIT) 5)
    ((ALERT) 6)
    ((EMERG) 7)))

(define *log-level* 'EMERG)

(define (log-init level)
  (set! *log-level* level))

(define (log level msg)
  (if (>= (log-level-map level) (log-level-map *log-level*))
      (display (format #f "~a\n" msg))))

