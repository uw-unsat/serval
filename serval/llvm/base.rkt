#lang racket/base

(provide (all-defined-out))

(struct module (globals functions) #:transparent)

(struct value (name type) #:transparent)

(struct function value (arguments blocks) #:transparent)

(struct argument value () #:transparent)

(struct basic-block value (instructions) #:transparent)

(struct instruction value (opcode operands attributes) #:transparent)

(struct array-offset (index size) #:transparent)
(struct struct-offset (value) #:transparent)

(struct asm (template constraint) #:transparent)

(struct nullptr () #:transparent)

(struct undef (type) #:transparent)
