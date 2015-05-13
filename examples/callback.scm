(import (rnrs) (pffi))

(define libcallback
  (open-shared-object "callback.so"))

(define cb-init
  (foreign-procedure libcallback void cb_init ()))
(define register-callback
  (foreign-procedure libcallback void register_callback 
		     (char (callback void (char)))))
(define event-loop
  (foreign-procedure libcallback void event_loop ()))

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
