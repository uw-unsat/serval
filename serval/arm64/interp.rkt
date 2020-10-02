#lang rosette

; The encodings and semantics are organized following the ARM manual.
;
; The semantics closely follows the pseudocode in the manual, but carefully
; avoids the use of integers in order to avoid integers in SMT constraints.
; The only exception is for instruction fields that will be concrete upon use,
; such as datasize (sf for 32/64-bit instructions).

(require
  "interp/arithmetic-immediate.rkt"
  "interp/arithmetic-shifted-register.rkt"
  "interp/atomic.rkt"
  "interp/bit-operation.rkt"
  "interp/bitfield-move.rkt"
  "interp/conditional-branch.rkt"
  "interp/divide.rkt"
  "interp/load-store-register.rkt"
  "interp/load-store-register-pair.rkt"
  "interp/logical-immediate.rkt"
  "interp/logical-shifted-register.rkt"
  "interp/move-wide-immediate.rkt"
  "interp/multiply.rkt"
  "interp/shift-register.rkt"
  "interp/unconditional-branch-immediate.rkt"
  "interp/unconditional-branch-register.rkt")

(provide (all-from-out
  "interp/arithmetic-immediate.rkt"
  "interp/arithmetic-shifted-register.rkt"
  "interp/atomic.rkt"
  "interp/bit-operation.rkt"
  "interp/bitfield-move.rkt"
  "interp/conditional-branch.rkt"
  "interp/divide.rkt"
  "interp/load-store-register.rkt"
  "interp/load-store-register-pair.rkt"
  "interp/logical-immediate.rkt"
  "interp/logical-shifted-register.rkt"
  "interp/move-wide-immediate.rkt"
  "interp/multiply.rkt"
  "interp/shift-register.rkt"
  "interp/unconditional-branch-immediate.rkt"
  "interp/unconditional-branch-register.rkt"))
