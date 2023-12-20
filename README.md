# Сompiler of classical algorithms into oracles for quantum computing

## Achievements:
- CRC32 hash function (4 byte input) - 411 qubits.

## Architecture:
1. Parsing source code in AST (planned).
2. Construction of a single large expression.
3. Construction of an even larger logical expression for each bit of the output result of the algorithm.
4. Expression optimization (minimizing unique logic operations and qubit allocations).
5. Generation of a quantum circuit from a DAG of logical operations.

## Authors:
- Alexander Averyanov - author
- Evgeny Kiktenko - mentor
- Dmitry Ershov - helped with the optimizer design

## Example:
```python
import quantpiler

x_len = 3
x = quantpiler.argument("x", x_len)

a = 5
# N = 2**4

prod = 1
for i in range(x_len):
    prod = ((x >> i) & 1).ternary(prod * a**(2**i), prod)

prod = prod & 0b1111

circ = prod.compile()
qc = quantpiler.circuit_to_qiskit(circ)

qc.draw("mpl")
```
![Resulting circuit](https://raw.githubusercontent.com/averyanalex/quantpiler/main/example.png)
