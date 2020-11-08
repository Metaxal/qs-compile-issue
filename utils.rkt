#lang racket/base
(require racket/path
         racket/file
         racket/runtime-path
         errortrace/errortrace-lib
         compiler/cm
         compiler/compilation-path)

(provide (all-defined-out))

(define-logger trycompile)

(define current-erase-bad-zos? (make-parameter #f))

;=============================;
;=== Compilation utilities ===;
;=============================;

(define version-bytes (string->bytes/utf-8 (version)))
(define vm-bytes (string->bytes/utf-8 (symbol->string (system-type 'vm))))

(define (zo-file src-file)
  (get-compilation-bytecode-file src-file #:modes '("compiled")))

; Based on 'read-linklet-bundle-or-directory':
; https://github.com/racket/racket/blob/master/racket/src/expander/compile/read-linklet.rkt#L9
; and 'get-cached-compiled':
; https://github.com/racket/racket/blob/master/racket/src/expander/run/cache.rkt#L76
(define (zo-version source-or-zo-file)
  ; We (only) use "compiled" as modes, because by default DrRacket would place zos in
  ; compiled/errortrace, but the compile-zos used in compile-user-scripts places them in
  ; "compiled".
  (define zof
    (if (path-has-extension? source-or-zo-file #".zo")
      source-or-zo-file
      (zo-file source-or-zo-file)))
  (and (file-exists? zof)
       (parameterize ([read-accept-compiled #t])
         (call-with-input-file*
             zof
           (lambda (in)
             (read-bytes 2 in) ; consume "#~"
             (define vers-len (min 63 (read-byte in)))
             (define vers (read-bytes vers-len in))
             (define vm-len (min 63 (read-byte in)))
             (define vm (read-bytes vm-len in))
             (list vers vm))))))

;; Is the zo file for the given source file having the same version as
;; the current (dr)racket one?
;; Returns #t also if source-file is not compiled (in which case it can still be
;; run by racket)
(define (compiled-for-current-version? source-file)
  (define zov (zo-version source-file))
  (or (not zov) ; no zo file
      (equal? (list version-bytes vm-bytes)
              zov)))


(define (try-compile-files files)
  ; Synchronous version:
  (define err-str-port (open-output-string))
  (define cmc (make-caching-managed-compile-zo))
  (parameterize ([current-error-port err-str-port]
                 [current-namespace (make-base-empty-namespace)])
    (for ([f (in-list files)])
      (with-handlers* ([exn:fail?
                        (λ (e) (errortrace-error-display-handler (exn-message e) e))])
        (log-trycompile-info "File: ~a" (path->string f))
        (log-trycompile-info "  Compiled for ~a (current is ~a ~a)" (zo-version f) version-bytes vm-bytes)
        (define c? (compiled-for-current-version? f))
        (define zo (zo-file f))
        (when (and (not c?) zo (file-exists? zo) (current-erase-bad-zos?))
          (delete-file zo)) ; Bad fix for compilation that should happen but doesn't
        (log-trycompile-info "  Compiling." )
        (cmc f)
        (log-trycompile-info "  Compiled for ~a (after compiling)" (zo-version f)))))
  (define err-str (get-output-string err-str-port))
  (log-trycompile-info err-str))

;===================;
;=== Try compile ===;
;===================;

(define-runtime-path script-dir "scripts")
(define try-file1 (build-path script-dir "test-compile.rkt"))
(define try-file2 (build-path script-dir "test-compile-cs.rkt"))

(define (try-compile)
  (make-directory* (build-path script-dir "compiled"))
  ;; Copy old zo files to 'compiled' directory.
  (copy-file (build-path script-dir "compiled-old" "test-compile_rkt--7.7.0.901.zo")
             (zo-file try-file1)
             #t)
  (copy-file (build-path script-dir "compiled-old" "test-compile-cs_rkt--7.8.0.6_cs.zo")
             (zo-file try-file2)
             #t)
  ;; Try to compile them
  (try-compile-files (list try-file1 try-file2)))

;============;
;=== Main ===;
;============;

;;; Run with
;;; export PLTSTDERR=debug@trycompile && racket -l qs-compile-issue/utils -- --erase
;;; Then
;;; export PLTSTDERR=debug@trycompile && racket -l qs-compile-issue/utils

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (current-erase-bad-zos? (member "--erase" args))
  (when (current-erase-bad-zos?)
    (log-trycompile-info "Erasing bad zos before compiling"))
  (try-compile))
