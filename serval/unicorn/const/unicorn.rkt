#lang racket/base

(require ffi/unsafe)

(provide (all-defined-out))

(define _uc_arch
  (_enum '(arm = 1
           arm64 = 2
           mips = 3
           x86 = 4
           ppc = 5
           sparc = 6
           m68k = 7
           max = 8)))

(define _uc_err
  (_enum '(ok = 0
           nomem = 1
           arch = 2
           handle = 3
           mode = 4
           version = 5
           read-unmapped = 6
           write-unmapped = 7
           fetch-unmapped = 8
           hook = 9
           insn-invalid = 10
           map = 11
           write-prot = 12
           read-prot = 13
           fetch-prot = 14
           arg = 15
           read-unaligned = 16
           write-unaligned = 17
           fetch-unaligned = 18
           hook-exist = 19
           resource = 20
           exception = 21)))

(define _uc_mem
  (_enum '(read = 16
           write = 17
           fetch = 18
           read-unmapped = 19
           write-unmapped = 20
           fetch-unmapped = 21
           write-prot = 22
           read-prot = 23
           fetch-prot = 24
           read-after = 25)))

(define _uc_hook
  (_enum '(intr = 1
           insn = 2
           code = 4
           block = 8
           mem-read-unmapped = 16
           mem-write-unmapped = 32
           mem-fetch-unmapped = 64
           mem-read-prot = 128
           mem-write-prot = 256
           mem-fetch-prot = 512
           mem-read = 1024
           mem-write = 2048
           mem-fetch = 4096
           mem-read-after = 8192
           mem-unmapped = 112
           mem-prot = 896
           mem-read-invalid = 144
           mem-write-invalid = 288
           mem-fetch-invalid = 576
           mem-invalid = 1008
           mem-valid = 7168)))

(define _uc_mode
  (_bitmask '(little-endian = 0
              big-endian = 1073741824
              arm = 0
              thumb = 16
              mclass = 32
              v8 = 64
              micro = 16
              mips3 = 32
              mips32r6 = 64
              mips32 = 4
              mips64 = 8
              x86-16 = 2
              x86-32 = 4
              x86-64 = 8
              ppc32 = 4
              ppc64 = 8
              qpx = 16
              sparc32 = 4
              sparc64 = 8
              v9 = 16)))

(define _uc_prot
  (_bitmask '(none = 0
              read = 1
              write = 2
              exec = 4
              all = 7)))
