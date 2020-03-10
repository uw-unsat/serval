#!/usr/bin/env python3

import fileinput, re

STATE_NONE        = 0
STATE_NAME        = 1
STATE_INSNS_START = 2
STATE_INSNS       = 3
STATE_AUX         = 4
STATE_DATA        = 5
STATE_RESULT      = 6

def emit(name, result, insns):
    print(f'\n\n  (bpf-test-case "{name}"')
    print(f"    #:result {racketify(result)}", end='')
    for insn in insns:
        comment = ''
        if insn.endswith("*/"):
            i = insn.index("/*")
            comment = f" ; {insn[i+2:-2].strip()}"
            insn = insn[:i].strip()
        insn = insn.rstrip(",")
        print(f"\n    {racketify(insn)}{comment}", end='')
    print(")", end='')

def racketify(s):
    i = s.find('(')
    # function
    if i >= 0 and s.endswith(')'):
        args = s[i+1:-1]
        if args:
            args = args.split(",")
            args = [" " + racketify(x.strip()) for x in args]
        return "(" + s[:i] + "".join(args) + ")"
    # remove number suffix (including for 0x...)
    if re.match("[0-9]+", s):
        suffixes = ["ULL", "LL", "UL", "U"]
        for suffix in suffixes:
            if s.endswith(suffix):
                s = s[:-len(suffix)]
                break
    # replace hex prefix
    if s.startswith("0x"):
        return "#x" + s[2:]
    return s

def main():
    print('#lang racket\n\n(require "bpftests.rkt")\n\n(define test_bpf-tests (test-suite+ "Tests for BPF"', end='')

    state = STATE_NONE
    for line in fileinput.input():
        if state == STATE_NONE:
            if line.startswith("\t{"):
                state = STATE_NAME
            continue

        line = line.strip()
        if state == STATE_NAME:
            if not line.startswith('"'):
                continue
            name = line.strip(',"')
            state = STATE_INSNS_START
            continue

        if state == STATE_INSNS_START:
            state = STATE_INSNS
            insns = []
            continue

        if state == STATE_INSNS:
            if line == "},":
                state = STATE_AUX
            else:
                insns.append(line)
            continue

        if state == STATE_AUX:
            if line == "INTERNAL,":
                state = STATE_DATA
            else:
                state = STATE_NONE
            continue

        if state == STATE_DATA:
            state = STATE_RESULT
            continue

        if state == STATE_RESULT:
            result = re.match(r"\{\s*\{\s*0,\s*([^\}]+)\s*\}\s*\}", line).group(1).strip()
            if result.startswith("(u32)"):
                result = result[5:].strip()
            emit(name, result, insns)
            state = STATE_NONE
            continue

    print('))\n\n(module+ test\n  (time (run-tests test_bpf-tests)))')


if __name__ == '__main__':
    main()
