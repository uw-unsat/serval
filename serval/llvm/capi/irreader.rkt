#lang racket/base

(require ffi/unsafe
         ffi/unsafe/alloc
         "core.rkt")

(provide LLVMParseIRInContext)

; allocate a new _LLVMMemoryBufferRef, which is consumed by this function
(define-llvm LLVMParseIRInContext
  (_fun [_LLVMContextRef = (LLVMGetGlobalContext)]
        [data : _?]
        [_LLVMMemoryBufferRef = (LLVMCreateMemoryBufferWithMemoryRangeCopy data "" 0)]
        [m : (_ptr o (_or-null _LLVMModuleRef))]
        [msg : (_ptr o _pointer)]
        -> [r : _LLVMBool]
        -> (begin (check r msg) m))
  #:wrap (allocator LLVMDisposeModule))

; no allocator here
(define-llvm LLVMCreateMemoryBufferWithMemoryRangeCopy
  (_fun [data : _bytes]
        [length : _size = (bytes-length data)]
        [buffer-name : _string]
        [requires-null-terminator? : _LLVMBool]
        -> _LLVMMemoryBufferRef))
