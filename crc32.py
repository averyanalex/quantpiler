#!/usr/bin/env python

import quantpiler

poly = 0xEDB8_8320

def table(ch: quantpiler.Expr) -> quantpiler.Expr:
    table = 0

    for i in range(8):
        cur_ch = ch >> i
        table = ((cur_ch ^ table) & 1).ternary((table >> 1) ^ poly, table >> 1)
    
    return table

size = 32

inp = quantpiler.argument("input", size)
value = 0xFFFF_FFFF

for i in range(size // 8):
    byte = (inp >> (i * 8)) & 0xFF
    ch = (byte ^ value) & 0xFF
    value = table(ch) ^ (value >> 8)

circ = value.compile()
qc = quantpiler.circuit_to_qiskit(circ)
print(qc.draw())
