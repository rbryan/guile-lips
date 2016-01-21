(use-modules (ice-9 format))

(define line-number 0)					;line number for error reporting
(define line-output-chars 0)				;number of unprocessed characters output
							; to line so far

(define parameters					;alist of parameters that is extended as 
 '()) 							;parameters are added

(define finish-hooks					;list of functions to call when the end of the
  '())							;file is reached

(define (useage)
  (display "Usage:\t lips < input.txt > output.txt | lips input.txt output.txt\n")
  (exit 1))

(define (main)
  (let* ((args (command-line))
	(argc (length args)))
    (cond
      ((= argc 3) (set-current-input-port (open-file (list-ref args 1) "r"))
		  (set-current-output-port (open-file (list-ref args 2) "w")))
      ((= argc 1))
      (else (useage)))
    (process (current-input-port) (current-output-port))))

(define (include file)
  (let ((fport (open-file file "r")))
    (if fport
      (do ((c (read-char fport) (read-char fport)))
	((eof-object? c) (close-port fport))
	(display c (current-output-port))))))

(define (add-finish-hook hook)
  (set! finish-hooks (append finish-hooks (list hook))))

(define (call-finish-hooks)
  (map (lambda (hook)
	 (let ((output (hook)))
	   (if (printable? output)
	     (display output)))) finish-hooks))

(define (define-parameter sym expr)
  (set! parameters (assq-set! parameters sym expr)))

(define (printable? s)
  (or (number? s)
      (string? s)
      (list?   s)))

(define (process iport oport)
  (let loop ((cic ;current input character		;we're going to loop over all the characters
	       	 (read-char iport)))			; in the file looking for escapes
      (cond 
	((eq? cic #\~)						;if we find a tilda
		(if (eq? (peek-char iport) #\~) 		;then check if the next character is a tilda
		  (begin					;if it is we output a tilda and eat the next tilda
		    (display "~" oport)
		    (set! line-output-chars (1+ line-output-chars))
		    (read-char iport))
		  (let ((expr (read iport)))			;else, read in the next valid lisp expression
		    (cond 					
		      						;if expr is a list then evaluate and print output
		      ((list? expr) (let ((output (primitive-eval expr))) 
				      (if (printable? output)
					(display output oport))))
		      						;if expr is a symbol then look it up in the
								;parameters alist
		      ((symbol? expr) (let ((value (assq-ref parameters expr)))
					(if value
					  (display value oport)
					  (error "No defined parameter: line:" line-number  expr))))

		      (else		(display expr oport))))))

	((eq? cic #\newline)					
	 	(if (not (zero? line-output-chars))
		  (display cic oport))
		(set! line-number (1+ line-number))
		(set! line-output-chars 0))

	((eof-object? cic) 
	 	(call-finish-hooks)
	 	(close-port (current-output-port))
		(close-port (current-input-port))
		(exit 0))

	(else
	  	(set! line-output-chars (1+ line-output-chars))
	  	(display cic oport)))
      (loop (read-char iport))))

(main)
