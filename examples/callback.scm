(import (rnrs) (pffi))

(define libcallback
  (open-shared-object "tests/callback.so"))

(define cb-init
  (foreign-procedure libcallback cb_init () void))
(define register-callback
  (foreign-procedure libcallback register_callback (char callback) void))
(define event-loop
  (foreign-procedure libcallback event_loop () void))

(define ouch
  (c-callback void ((char c))
    (lambda (c)
      (display "Ouch! Hit by '")
      (display c) (display "'") (newline)
      (flush-output-port (current-output-port)))))
(define rats
  (c-callback void ((char c))
    (lambda (c)
      (display "Rats! Received '")
      (display c) (display "'") (newline)
      (flush-output-port (current-output-port)))))

(cb-init)
(register-callback (char->integer #\a) ouch)
(register-callback (char->integer #\c) rats)
(register-callback (char->integer #\e) ouch)

(display "start loop") (newline)
(flush-output-port (current-output-port))

(event-loop)
