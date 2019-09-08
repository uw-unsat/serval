#lang racket/base

(require ffi/unsafe
         ffi/unsafe/alloc
         "core.rkt")

(provide (all-defined-out))

(define _LLVMByteOrdering
  (_enum
    '(big = 0
      little)))

(struct _data-layout (pointer module) #:transparent)

(define _LLVMTargetDataRef
  (make-ctype _pointer
              _data-layout-pointer
              #f))

(define-llvm LLVMGetModuleDataLayout
  (_fun (m : _LLVMModuleRef)
        -> (cptr : _pointer)
        -> (_data-layout cptr m)))

(define-llvm LLVMCopyStringRepOfTargetData
  (_fun _LLVMTargetDataRef
        -> _LLVMMessageRef))

(define-llvm LLVMByteOrder
  (_fun _LLVMTargetDataRef
        -> _LLVMByteOrdering))

(define-llvm LLVMPointerSize
  (_fun _LLVMTargetDataRef
        -> _uint))

(define-llvm LLVMABISizeOfType
  (_fun _LLVMTargetDataRef
        _LLVMTypeRef
        -> _ullong))

(define-llvm LLVMOffsetOfElement
  (_fun _LLVMTargetDataRef
        _LLVMTypeRef
        _uint
        -> _ullong))
