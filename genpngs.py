#!/usr/bin/env python

import quantpiler

x_len = 4
x = quantpiler.argument("x", x_len)

a = 6
# N = 2**4

prod = 1
for i in range(x_len):
    prod = ((x >> i) & 1).ternary(prod * a**(2**i), prod) & 0b1111

circ = prod.compile()

qc = quantpiler.circuit_to_qiskit(circ)
qc.draw("mpl").savefig("example.png")

qc = quantpiler.circuit_to_qiskit(circ, rev=True)
qc.draw("mpl").savefig("example-rev.png")

a = quantpiler.argument("a", 2)
b = quantpiler.argument("b", 4)

r = a ^ b + 3
circ = r.compile()
qc = quantpiler.circuit_to_qiskit(circ)
qc.draw("mpl").savefig("guide.png")
