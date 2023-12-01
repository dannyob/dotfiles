;; Sets a list of options and their corresponding values,
;; automatically quoting each option, transforming it into a symbol.
;; In other words, this can be used just like @code{set}, but without
;; needing to quote each option manually.

;; @example
;; (setq border-px 10
;;       border-color \"#00FF00\")
;; @end example
(define-syntax setq-args
  (syntax-rules ()
    ((setq-args)
     (syntax-error "Missing arguments to setq"))
    ((setq-args option)
     (syntax-error "Missing value to option in setq"))
    ((setq-args option exp)
     `(option ,exp))
    ((setq-args option exp rest ...)
     (append `(option ,exp) (setq-args rest ...)))))

(define-syntax setq
  (syntax-rules ()
    ((setq)
     (syntax-error "Missing arguments to setq"))
    ((setq option)
     (syntax-error "Missing value to option in setq"))
    ((setq rest ...)
     (apply set (setq-args rest ...)))))

(define* (dwl:run-async fn #:optional (callback #f))
  "Evalutes FN asynchronously, without blocking the main thread.
CALLBACK will be executed once FN has finished its execution, being
passed the potential return value from FN. If no callback is provided,
the return value will be ignored.

For thread safety, FN should not make use of dwl-guile bindings, although
some bindings can be used without issue, such as @code{dwl:spawn}. Instead,
try to move dwl-guile calls to the callback."
  ((@ (ice-9 futures) make-future)
   (lambda ()
     ;; This is a really hacky (but easy) solution for ensuring thread safety
     ;; while still allowing for dwl-guile bindings to be called asynchronously.
     ;; Essentially, we are executing a shell command from the Guile context
     ;; in order to send a command to the main thread Guile context, via
     ;; the Wayland socket. A lot of overhead, but speed is not of great concern
     ;; when doing async calls (it is still quite fast).
     (dwl:spawn dwl:%binary-path "-e" (object->string (callback (fn)))))))

(define* (dwl:start-repl-server)
  "Starts a local Guile REPL server, listening on a UNIX socket at path
@path{/tmp/dwl-guile.socket}. This REPL allows you to execute expressions
in the dwl-guile context, just like @code{dwl-guile -e \"<exp\"}, but with
a more user-friendly interface.

The preferred way of connecting to the REPL server is using Geiser in Emacs.
You can connect to the server by calling @code{geiser-connect-local}, and
specifying the UNIX-socket path.

Note that this needs to be explicitly called in order for the REPL server to
be started!
"
  (use-modules (system repl server))

  ;; REPL socket path is dependent on the type of build, i.e. stable or devel.
  ;; Therefore, this variable is set during the initial configuration load in C.
  (define (kill-server)
    (when (file-exists? dwl:%repl-socket-path)
      (delete-file dwl:%repl-socket-path)
      (stop-server-and-clients!)))

  (unless (file-exists? dwl:%repl-socket-path)
    (begin
      (spawn-server (make-unix-domain-server-socket #:path dwl:%repl-socket-path))
      (add-hook! dwl:hook-quit kill-server))))

(define* (dwl:list-options)
  "Lists all available options that can be configured using the @code{set}
procedure."
  (hash-fold
   (lambda (key value acc)
     ;; Discard value since it just contains C-related metadata
     (cons key acc))
   '()
   dwl:%metadata))

(define* (dwl:list-keysyms)
  "Lists all available keysyms and their respective keycode that can be used
when binding keys and buttons using the @code{bind} procedure."
  (define (iterator key value acc)
     (cons `(,key ,value) acc))

  (hash-fold iterator
             (hash-fold iterator '() dwl:%keycodes)
             dwl:%keycodes-mouse))

(define* (dwl:show-options)
  "Same as @code{dwl:list-options}, but the list of options are printed
in a readable format."
  (for-each
   (lambda (option) (display (string-append (symbol->string option) "\n")))
   (sort-list (dwl:list-options)
              (lambda (x y) (string< (symbol->string x)
                                     (symbol->string y))))))

(define* (dwl:show-keysyms)
  "Same as @code{dwl:list-keysyms}, but the list of keysyms are printed
in a readable format."
  (for-each
   (lambda (pair)
     (display (string-append (car pair) " = " (number->string (cadr pair)) "\n")))
   (sort-list (dwl:list-keysyms)
              (lambda (x y) (< (cadr x) (cadr y))))))

(define* (dwl:set-tty-keys modifiers #:optional (ttys 12))
  "Helper procedure for binding all ttys to MODIFIERS + F[1-TTYS]."
  (for-each
   (lambda (v)
     (set-keys (string-append modifiers "-<F" (number->string v) ">") `(dwl:chvt ,v)))
   (iota ttys 1)))

(define* (dwl:set-tag-keys view-modifiers move-modifiers #:optional (tags 9))
  "Helper procedure for adding bindings for viewing and moving clients to
tags 1-TAGS. The key modifiers used for viewing and moving can be set by
VIEW-MODIFIERS, and MOVE-MODIFIERS, respectively."
  (for-each
   (lambda (t)
     (set-keys (string-append view-modifiers "-" (number->string t)) `(dwl:view ,t)
               (string-append move-modifiers "-" (number->string t)) `(dwl:tag ,t)))
   (iota tags 1)))

;; Set required options.
;; These can not be inhibited, but they can easily be overridden if needed.
(setq tags (map number->string (iota 9 1)))

;; Define layouts before monitor rules to make sure layout is available.
(set-layouts 'default "[]=" 'dwl:tile)

;; There must be a default monitor rule (i.e. with name == NULL)
(set-monitor-rules '((masters . 1)
                     (master-factor . 0.55)
                     (scale . 2) 
                     (transform . TRANSFORM-NORMAL)
                     (layout . default)))

(set-xkb-rules '((options . "compose:prsc,caps:hyper")))
(load "./keys.scm")
