#!/usr/bin/env python

import argparse, re, sys
from collections import OrderedDict

def generate(out, d, typ, prefixes):
    for prefix in prefixes:
        out.write("\n(define _%s\n  (_%s '(" % (prefix.lower(), typ))
        first = True
        for key, value in d.items():
            if not key.startswith(prefix):
                continue
            if first:
                first = False
            else:
                out.write("\n" + ' ' * (len(typ) + 7))
            key = key[len(prefix)+1:].lower().replace('_', '-')
            # patch numeric as they cannot be used as symbols
            if key.isdigit():
                assert prefix == 'UC_MODE'
                key = 'x86-' + key
            out.write("%s = %s" % (key, value))
        out.write(")))\n")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--enum', action='append', default=[])
    parser.add_argument('--bitmask', action='append', default=[])
    parser.add_argument('infile', nargs='?', type=argparse.FileType('r'), default=sys.stdin)
    parser.add_argument('outfile', nargs='?', type=argparse.FileType('w'), default=sys.stdout)
    args = parser.parse_args()

    pattern = re.compile(r'^([A-Z0-9_]+)\s*=\s*([0-9]+)')
    d = OrderedDict()
    for s in args.infile:
        m = pattern.match(s.strip())
        if m:
          d.update({m.group(1): m.group(2)})

    out = args.outfile
    out.write('#lang racket/base\n\n(require ffi/unsafe)\n\n(provide (all-defined-out))\n')

    generate(out, d, "enum", args.enum)
    generate(out, d, "bitmask", args.bitmask)

if __name__ == '__main__':
    main()
