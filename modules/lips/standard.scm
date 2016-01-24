(define-module (lips standard)
	       #:use-module (lips process)
	       #:export (parameters
			  finish-hooks
			  hook-stack
			  push
			  pop
			  add-finish-hook
			  call-finish-hooks
			  define-parameter)
	       #:replace (include))



;alist of parameters defined using define-parameter
(define parameters
 '())

;list of functions to call just before exiting
;finish-hooks are called at the end of the file they are written in.
;If you would like to define finish hooks in a separate file then
;you must load that file with (load) not include.
(define finish-hooks
  '())

;when include is called the hooks for the current
;are pushed to this stack and a new list is started
;for the included file.
(define hook-stack
  '())

(define (push l stack)
  (set! stack (cons l stack)))

(define (pop stack)
  (if (nil? stack)
    stack
    (let ((l (car stack)))
      (set! stack (cdr stack))
      l)))

(define (include file)
  (let ((fport (open-file file "r")))
    (if fport
      (begin
	(push finish-hooks hook-stack)
	(set! finish-hooks '())
        ;ideally this should output to the port
        ;that the (process) call that called (include) outputs to.
        ;This should work fine in just about every case, but if the user
        ;decides they want to call process themselves (can't imagine why
        ;they would) and what they call it on calls include 
        ;then it will be a problem.
        (process fport (current-output-port))
	(set! finish-hooks (pop hook-stack)))
      (error "Include couldn't open file:" file))))

(define (add-finish-hook hook)
  (set! finish-hooks (append finish-hooks (list hook))))

(define (call-finish-hooks)
  (for-each (lambda (hook)
	      (hook)) finish-hooks))

(define (define-parameter sym expr)
  (set! parameters (assq-set! parameters sym expr)))

