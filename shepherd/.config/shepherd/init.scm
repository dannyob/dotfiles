(use-modules (shepherd service)
             ((ice-9 ftw) #:select (scandir)))

;; Send shepherd into the background
(perform-service-action (lookup-running 'shepherd) 'daemonize)

     ;; Load all the files in the directory 'init.d' with a suffix '.scm'.
     (for-each
       (lambda (file)
         (load (string-append "init.d/" file)))
       (scandir (string-append (dirname (current-filename)) "/init.d")
                (lambda (file)
                  (string-suffix? ".scm" file))))

