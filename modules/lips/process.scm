;;;(lips process)
;;This module contains the core text processor.
;;All it does is read a file, recognize escapes, and call eval.

(define-module (lips process)
	       #:export (process))

(define (process iport oport)

  (define (printable? s)
    (or (number? s)
        (string? s)
        (list?   s)))
  
  (define line-number 0)
  (define first-character #t)
  ;line-number -> for error reporting
  ;first-character -> are we processing the first character of the line?
  ;			If so and it is a macro that produces no printable output
  ;			and nothing else is on the line
  ;			we will remove the following newline
  ;we're going to loop over all the characters
  ;in the file looking for escapes
  ;cic -> current input character
  (let loop ((cic (read-char iport)))
      (cond 
	;if we find a tilda then check to see if the next character
	;is also a tilda. If it is, then we output a tilda and eat the
	;tilda from input.
	((eq? cic #\~)
		(if (eq? (peek-char iport) #\~)
		  (begin
		    (display "~" oport)
		    (read-char iport))
		  ;if the next character is not a tilda then read in a s-exp
		  (let ((expr (read iport)))
		    (cond 					
			;if expr is a list then evaluate and print output
			;if we're at the beginning of a line and the character
			;following the expression is a newline then, if the expression
			;doesn't evaluate to something printable, ignore the newline
		      ((list? expr) (let ((output (primitive-eval expr))) 
				      (if (printable? output)
					(display output oport)
					(if (and first-character
					      (eq? (peek-char iport) #\newline))
					  (read-char iport)))))
			;if expr is a symbol then look it up in the
			;parameters alist
		      ((symbol? expr) (let ((value (assq-ref parameters expr)))
					(if value
					  (display value oport)
					  (error "No defined parameter: line:" line-number  expr))))

		      (else		(display expr oport))))))

	((eq? cic #\newline)					
		(display cic oport)
		(set! first-character #t)
		(set! line-number (1+ line-number)))

	((eof-object? cic))

	(else
	 	(if first-character 
		  (set! first-character #f))
	  	(display cic oport)))

      (if (not (eof-object? cic))
        (loop (read-char iport)))))

