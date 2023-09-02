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
