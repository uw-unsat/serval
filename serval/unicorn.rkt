#lang racket/base

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define
         ffi/unsafe/define/conventions
         setup/dirs
         "unicorn/const/unicorn.rkt"
         "unicorn/engine.rkt"
         "unicorn/arm.rkt"
         "unicorn/arm64.rkt"
         "unicorn/x86.rkt")

(provide (except-out (all-defined-out)
                     arch->engine check define-unicorn))

(define unicorn-lib
  (ffi-lib "libunicorn"
   #:get-lib-dirs (lambda () (append (get-lib-search-dirs)
                                     (list "/usr/local/lib" "/usr/lib" "/opt/homebrew/lib")))))

(define-ffi-definer define-unicorn unicorn-lib
  #:make-c-id convention:hyphen->underscore)

(define (arch->engine arch)
  (case arch
    [(arm) arm-engine]
    [(arm64) arm64-engine]
    [(x86) x86-engine]))

(define-fun-syntax _uc_engine
  (syntax-id-rules ()
    [_ (type: _pointer
        pre: (x => (engine-ptr x)))]))

(define _uc_cb_hookcode_t
  (_fun _pointer _uint64 _uint32 _pointer -> _void))

(define (check v)
  (unless (equal? v 'ok)
    (error v)))

(define-unicorn uc-version
  (_fun [major : (_ptr o _uint)]
        [minor : (_ptr o _uint)]
        -> _uint))

(define-unicorn uc-arch-supported
  (_fun [arch : _uc_arch]
        -> _stdbool))

(define-unicorn uc-close
  (_fun [uc : _uc_engine]
        -> [r : _uc_err])
  #:wrap (deallocator))

(define-unicorn uc-open
  (_fun [arch : _uc_arch]
        [mode : _uc_mode]
        [ucptr : (_ptr o _pointer)]
        -> [r : _uc_err]
        -> (begin (check r) ((arch->engine arch) ucptr mode)))
  #:wrap (allocator uc-close))

(define-unicorn uc-errno
  (_fun [uc : _uc_engine] -> _uc_err))

(define-unicorn uc-strerror
  (_fun [code : _uc_err] -> _string))

(define-unicorn uc-reg-write
  (_fun [uc : _uc_engine]
        [regid : _?]
        [_int = (cast regid (engine-reg-enum uc) _int)]
        [type : _? = (engine-reg-type uc regid)]
        [value : (_ptr i type)]
        -> [r : _uc_err]
        -> (check r)))

(define-unicorn uc-reg-read
  (_fun [uc : _uc_engine]
        [regid : _?]
        [_int = (cast regid (engine-reg-enum uc) _int)]
        [type : _? = (engine-reg-type uc regid)]
        [value : (_ptr io type) = 0] ; use io rather than o to zeroize value
        -> [r : _uc_err]
        -> (begin (check r) value)))

(define-unicorn uc-mem-write
  (_fun [uc : _uc_engine]
        [address : _uint64]
        [bytes : _bytes]
        [_size = (bytes-length bytes)]
        -> [r : _uc_err]
        -> (check r)))

(define-unicorn uc-mem-read
  (_fun [uc : _uc_engine]
        [address : _uint64]
        [bytes : (_bytes o size)]
        [size : _size]
        -> [r : _uc_err]
        -> (begin (check r) bytes)))

(define-unicorn uc-emu-start
  (_fun [uc : _uc_engine]
        [start : _uint64]
        [until : _uint64]
        [timeout : _uint64]
        [count : _size]
        -> [r : _uc_err]
        -> (check r)))

(define-unicorn uc-emu-stop
  (_fun [uc : _uc_engine]
        -> [r : _uc_err]
        -> (check r)))

(define-unicorn uc-hook-add/code
  (_fun [uc : _uc_engine]
        [hh : (_ptr o _size)]
        [_int = (cast 'code _uc_hook _int)]
        [callback : _uc_cb_hookcode_t]
        [user_data : _pointer = #f]
        [start : _uint64 = 1]
        [end : _uint64 = 0]
        -> [r : _uc_err]
        -> (begin (check r) hh))
  #:c-id uc_hook_add)

(define-unicorn uc-hook-del
  (_fun [uc : _uc_engine]
        [hh : _size]
        -> [r : _uc_err]
        -> (check r)))

(define-unicorn uc-mem-map
  (_fun [uc : _uc_engine]
        [address : _uint64]
        [size : _size]
        [perm : _uc_prot]
        -> [r : _uc_err]
        -> (check r)))
