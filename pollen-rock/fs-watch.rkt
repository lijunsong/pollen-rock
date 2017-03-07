#lang racket

(require openssl/md5)
(provide file-watch)

(define (file-checksum filepath)
  (with-input-from-file filepath
    (lambda ()
      (md5 (current-input-port)))))

(define (directory-list-all folderpath)
  (define (list-all-files folder)
    (for/list [(f (directory-list folder #:build? #t))]
      (if (directory-exists? f)
          (list-all-files f)
          f)))
  (if (not (directory-exists? folderpath))
      (list)
      (flatten (list-all-files folderpath))))

;; can't watch added new files for now.
;; removing file event will be ignored.
(define/contract (folder-watch folderpath callback)
  (-> path-string? (-> path-string? any/c) any/c)
  (define files (directory-list-all folderpath))
  (define file-cache
    (make-hash
     (map (lambda (f)
            (cons f (file-or-directory-modify-seconds f)))
          files)))
  (let loop ()
    (define evts
      (for/list [(file-modtime (hash->list file-cache))]
        (define fname (car file-modtime))
        (define modtime (cdr file-modtime))
        (handle-evt (filesystem-change-evt fname)
                    (lambda (_)
                      (cond [(file-exists? fname)
                             (define last-mod
                               (file-or-directory-modify-seconds fname))
                             (unless (= last-mod modtime)
                               (callback fname)
                               (hash-set! file-cache fname last-mod))]
                            [else
                             (hash-remove! file-cache fname)])
                      (loop)))))
    (apply sync evts)))

(define (file-watch filepath callback [last-seen-seconds #f] [noexit #f])
  (define cur-mod (file-or-directory-modify-seconds filepath))
  (cond [(or (not last-seen-seconds)
             (= last-seen-seconds cur-mod))
         (let loop ([last-mod cur-mod])
           (sync
            (handle-evt
             (filesystem-change-evt filepath)
             (lambda (_)
               (define cur-mod
                 (with-handlers
                     ([exn:fail?
                       (lambda _
                         ;; sometimes the file will be replaced in a
                         ;; non-idempotent way, so sleep a short time
                         ;; waiting for the real change.
                         (sleep 0.3)
                         (file-or-directory-modify-seconds filepath))])
                   (file-or-directory-modify-seconds filepath)))
               (cond [(not (file-exists? filepath))
                      (error 'file-watch "file is removed. Not yet implemented.")]
                     [(= last-mod cur-mod)
                      (loop cur-mod)]
                     [noexit
                      (callback filepath cur-mod)
                      (loop cur-mod)]
                     [else
                      (callback filepath cur-mod)])))))]
        [(< last-seen-seconds cur-mod)
         (callback filepath cur-mod)]
        [else
         ;; last-seen-seconds is greater than cur-mod. No idea why,
         ;; send back cur-mode to let the frontend update its time
         (callback filepath cur-mod)]))

;(folder-watch "." (lambda (f) (println (format "file ~a changed." f))))
;(file-watch "1" (lambda (f) (println (format "file ~a changed." f))) #t)
