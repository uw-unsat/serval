#lang racket

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define)

(provide (all-defined-out))

; locate libLLVM

(define LLVM_CONFIG (getenv "LLVM_CONFIG"))

(define llvm-config-path
  (cond
    [LLVM_CONFIG (find-executable-path LLVM_CONFIG)]
    [else (ormap find-executable-path (list "llvm-config" "/usr/local/opt/llvm/bin/llvm-config"))]))

(unless llvm-config-path
  (raise-user-error "ffi-lib: cannot locate \"llvm-config\" (LLVM is requied)"))

(define llvm-libdir (string-trim (with-output-to-string (lambda () (system* llvm-config-path "--libdir")))))

(define llvm-lib (ffi-lib "libLLVM" #:get-lib-dirs (lambda () (list llvm-libdir))))

(define-ffi-definer define-llvm llvm-lib)


; enum

(define _LLVMOpcode
  (_enum
    '(ret            = 1
      br             = 2
      switch         = 3
      indirectbr     = 4
      invoke         = 5
      ; removed 6 due to API changes
      unreachable    = 7
      callbr         = 67

      ; Standard Unary Operators
      fneg           = 66

      ; Standard Binary Operators
      add            = 8
      fadd           = 9
      sub            = 10
      fsub           = 11
      mul            = 12
      fmul           = 13
      udiv           = 14
      sdiv           = 15
      fdiv           = 16
      urem           = 17
      srem           = 18
      frem           = 19

      ; Logical Operators
      shl            = 20
      lshr           = 21
      ashr           = 22
      and            = 23
      or             = 24
      xor            = 25

      ; Memory Operators
      alloca         = 26
      load           = 27
      store          = 28
      getelementptr  = 29

      ; Cast Operators
      trunc          = 30
      zext           = 31
      sext           = 32
      fptoui         = 33
      fptosi         = 34
      uitofp         = 35
      sitofp         = 36
      fptrunc        = 37
      fpext          = 38
      ptrtoint       = 39
      inttoptr       = 40
      bitcast        = 41
      addrspacecast  = 60

      ; Other Operators
      icmp           = 42
      fcmp           = 43
      phi            = 44
      call           = 45
      select         = 46
      va_arg         = 49
      extractelement = 50
      insertelement  = 51
      shufflevector  = 52
      extractvalue   = 53
      insertvalue    = 54

      ; Atomic operators
      fence          = 55
      cmpxchg        = 56
      atomicrmw      = 57

      ; Exception Handling Operators
      resume         = 58
      landingpad     = 59
      cleanupret     = 61
      catchret       = 62
      catchpad       = 63
      cleanuppad     = 64
      catchswitch    = 65)))

(define _LLVMTypeKind
  (_enum
    '(void = 0
      half
      float
      double
      x86-fp80
      fp128
      ppc-fp128
      label
      integer
      function
      struct
      array
      pointer
      vector
      metadata
      x86-mmx
      token)))

(define _LLVMValueKind
  (_enum
    '(argument = 0
      basic-block
      memory-use
      memory-def
      memory-phi
      ;
      function
      global-alias
      global-ifunc
      global-variable
      block-address
      constant-expr
      constant-array
      constant-struct
      constant-vector
      ;
      undef-value
      constant-aggregate-zero
      constant-data-array
      constant-data-vector
      constant-int
      constant-fp
      constant-pointer-null
      constant-token-none
      ;
      metadata-as-value
      inline-asm
      ;
      instruction)))

(define _LLVMIntPredicate
  (_enum
    '(eq = 32
      ne
      ugt
      uge
      ult
      ule
      sgt
      sge
      slt
      sle)))


; LLVM message - convert to string and dispose

(define (pointer->list cptr [offset 0])
  (define b (ptr-ref cptr _byte offset))
  (if (zero? b)
      null
      (cons b (pointer->list cptr (add1 offset)))))

(define (pointer->string cptr)
  (bytes->string/utf-8 (list->bytes (pointer->list cptr))))

(define _LLVMMessageRef
  (let ()
    (define (c->racket x)
      (begin0
        (pointer->string x)
        (@LLVMDisposeMessage x)))
    (make-ctype _pointer
                #f ; no racket->c
                c->racket)))

(define-llvm @LLVMDisposeMessage
  (_fun _pointer
        -> _void)
  #:c-id LLVMDisposeMessage)


; primitive types
(define _LLVMBool _bool)
(define-cpointer-type _LLVMContextRef #:tag 'LLVMContextRef)
(define-cpointer-type _LLVMMemoryBufferRef #:tag 'LLVMMemoryBufferRef)

; Conceptually a pointer of the following types holds a reference to a context.
; For simplicity, we assume a global context.
(define-cpointer-type _LLVMModuleRef #:tag 'LLVMModuleRef)
(define-cpointer-type _LLVMTypeRef #:tag 'LLVMTypeRef)
(define-cpointer-type _LLVMValueRef #:tag 'LLVMValueRef)

; mimic the C++ hierarchy

(define (value-> v type)
  (cast v _LLVMValueRef type))

(define-syntax (define-llvmref-type stx)
  (syntax-case stx ()
    [(_ type super)
     #'(define-llvmref-type type super #f #f)]
    [(_ type super racket-to-c c-to-racket)
     (with-syntax ([isa (datum->syntax #'type (string->symbol (format "LLVMIsA~a" (syntax->datum #'type))))]
                   [name (datum->syntax #'type (string->symbol (format "_LLVM~aRef" (syntax->datum  #'type))))])
       (syntax/loc stx
         (begin
           (define-llvm isa (_fun _LLVMValueRef -> _LLVMBool))
           (define-cpointer-type name super
             racket-to-c
             (lambda (x)
               (when (not (and x (isa x)))
                 (raise "LLVM type error"))
               (if c-to-racket (c-to-racket x) x))))))]))

(define-llvmref-type GlobalValue _LLVMValueRef)
(define-llvmref-type Function _LLVMValueRef)
(define-llvmref-type GlobalVariable _LLVMValueRef)

; the pointer value of LLVMBasicBlockRef is not the same as that of LLVMValueRef
; need conversions using LLVMBasicBlockAsValue/LLVMValueAsBasicBlock
(define-cpointer-type _LLVMBasicBlockRef #:tag 'LLVMBasicBlockRef)

(define-llvm LLVMBasicBlockAsValue
  (_fun _LLVMBasicBlockRef
        -> _LLVMValueRef))

(define-llvm LLVMValueAsBasicBlock
  (_fun _LLVMValueRef
        -> _LLVMBasicBlockRef))

(define-llvmref-type User _LLVMValueRef)

(define-llvmref-type Constant _LLVMUserRef)

(define-llvmref-type ConstantInt _LLVMConstantRef)

(define-llvmref-type ConstantExpr _LLVMConstantRef)

(define-llvmref-type Instruction _LLVMUserRef)

(define-llvmref-type Argument _LLVMValueRef)


(define (check v msg)
  (when v
    (error 'llvm (pointer->string msg))))

(define-llvm LLVMGetGlobalContext
  (_fun -> _LLVMContextRef))

(define-llvm LLVMDisposeModule
  (_fun _LLVMModuleRef
        -> _void)
  #:wrap (deallocator))

(define-llvm LLVMDumpModule
  (_fun _LLVMModuleRef
        -> _void))

(define-llvm LLVMTypeOf
  (_fun _LLVMValueRef
        -> _LLVMTypeRef))

(define-llvm LLVMGetValueKind
  (_fun _LLVMValueRef
        -> _LLVMValueKind))

(define-llvm LLVMGetValueName2
  (_fun _LLVMValueRef
        [length : (_ptr o _size)]
        -> (cptr : _bytes)
        -> (apply bytes (cblock->list cptr _uint8 length))))

(define-llvm LLVMSetValueName2
  (_fun _LLVMValueRef
        [name : _bytes]
        [_size = (bytes-length name)]
        -> _void))

(define-llvm LLVMDumpValue
  (_fun _LLVMValueRef
        -> _void))

(define-llvm LLVMPrintValueToString
  (_fun _LLVMValueRef
        -> _LLVMMessageRef))

(define-llvm LLVMIsDeclaration
  (_fun _LLVMGlobalValueRef
        -> _LLVMBool))

(define-llvm LLVMGetFirstFunction
  (_fun _LLVMModuleRef
        -> (_or-null _LLVMFunctionRef)))

(define-llvm LLVMGetNextFunction
  (_fun _LLVMFunctionRef
        -> (_or-null _LLVMFunctionRef)))

(define-llvm LLVMGetTypeKind
  (_fun _LLVMTypeRef
        -> _LLVMTypeKind))

(define-llvm LLVMPrintTypeToString
  (_fun _LLVMTypeRef
        -> _LLVMMessageRef))

(define-llvm LLVMGetIntTypeWidth
  (_fun _LLVMTypeRef
        -> _uint))

; Struct

(define-llvm LLVMCountStructElementTypes
  (_fun _LLVMTypeRef
        -> _uint))

(define-llvm LLVMStructGetTypeAtIndex
  (_fun _LLVMTypeRef
        _uint
        -> _LLVMTypeRef))

; Sequential Types

(define-llvm LLVMGetElementType
  (_fun _LLVMTypeRef
        -> _LLVMTypeRef))

(define-llvm LLVMGetArrayLength
  (_fun _LLVMTypeRef
        -> _uint))

; AllocaInst/LoadInst/StoreInst/GlobalValue
(define-llvm LLVMGetAlignment
  (_fun _LLVMValueRef
        -> _uint))

(define-llvm LLVMGetFirstGlobal
  (_fun _LLVMModuleRef
        -> (_or-null _LLVMGlobalVariableRef)))

(define-llvm LLVMGetNextGlobal
  (_fun _LLVMGlobalVariableRef
        -> (_or-null _LLVMGlobalVariableRef)))

(define-llvm LLVMGetInitializer
  (_fun _LLVMGlobalVariableRef
        -> (_or-null _LLVMValueRef)))

(define-llvm LLVMCountBasicBlocks
  (_fun _LLVMFunctionRef
        -> _uint))

(define-llvm LLVMGetFirstBasicBlock
  (_fun _LLVMFunctionRef
        -> (_or-null _LLVMBasicBlockRef)))

(define-llvm LLVMGetNextBasicBlock
  (_fun _LLVMBasicBlockRef
        -> (_or-null _LLVMBasicBlockRef)))

(define-llvm LLVMGetEntryBasicBlock
  (_fun _LLVMFunctionRef
        -> _LLVMBasicBlockRef))

(define-llvm LLVMGetFirstInstruction
  (_fun _LLVMBasicBlockRef
        -> (_or-null _LLVMInstructionRef)))

(define-llvm LLVMGetNextInstruction
  (_fun _LLVMInstructionRef
        -> (_or-null _LLVMInstructionRef)))

(define-llvm LLVMCountParams
  (_fun _LLVMFunctionRef
        -> _uint))

(define-llvm LLVMGetParam
  (_fun _LLVMFunctionRef
        _uint
        -> _LLVMArgumentRef))

(define-llvm LLVMGetFirstParam
  (_fun _LLVMFunctionRef
        -> (_or-null _LLVMArgumentRef)))

(define-llvm LLVMGetNextParam
  (_fun _LLVMArgumentRef
        -> (_or-null _LLVMArgumentRef)))

(define-llvm LLVMGetInstructionOpcode
  (_fun _LLVMValueRef
        -> _LLVMOpcode))

(define-llvm LLVMGetICmpPredicate
  (_fun _LLVMValueRef
        -> _LLVMIntPredicate))

(define-llvm LLVMGetOperand
  (_fun _LLVMUserRef
        _uint
        -> _LLVMValueRef))

(define-llvm LLVMGetNumOperands
  (_fun _LLVMUserRef
        -> _int))

(define-llvm LLVMConstIntGetSExtValue
  (_fun _LLVMConstantIntRef
        -> _llong))

(define-llvm LLVMIsConstantString
  (_fun _LLVMValueRef
        -> _LLVMBool))

(define-llvm LLVMGetAsString
  (_fun _LLVMValueRef
        [length : (_ptr o _size)]
        -> (cptr : _bytes)
        -> (apply bytes (cblock->list cptr _uint8 length))))

(define-llvm LLVMGetConstOpcode
  (_fun _LLVMValueRef
        -> _LLVMOpcode))

(define-llvm LLVMGetAllocatedType
  (_fun _LLVMValueRef
        -> _LLVMTypeRef))

(define-llvm LLVMCountIncoming
  (_fun _LLVMInstructionRef
        -> _uint))

(define-llvm LLVMGetIncomingValue
  (_fun _LLVMInstructionRef
        [index : _uint]
        -> _LLVMValueRef))

(define-llvm LLVMGetIncomingBlock
  (_fun _LLVMInstructionRef
        [index : _uint]
        -> _LLVMBasicBlockRef))

(define-llvm LLVMGetNumIndices
  (_fun _LLVMValueRef
        -> _uint))

(define-llvm LLVMGetIndices
  (_fun [v : _LLVMValueRef]
        [length : _? = (LLVMGetNumIndices v)]
        -> (cblock : _pointer)
        -> (cblock->list cblock _uint length)))
