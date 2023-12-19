from .quantpiler import *

from qiskit.circuit import QuantumCircuit, Qubit, QuantumRegister


def circuit_to_qiskit(c: Circuit) -> QuantumCircuit:
    qubits = []
    regs = []
    for qubit, qubit_descs in c.qubits_map_list():
        full_desc = []
        for desc in qubit_descs:
            full_desc.append(f"{desc.reg.name()}_{desc.index}")
        full_desc.sort()
        q = Qubit()
        qubits.append(q)
        reg = QuantumRegister(name="-".join(full_desc), bits=[q])
        regs.append(reg)

    qc = QuantumCircuit(*regs)

    for gate in c.gates:
        controls = []
        for control in gate.controls:
            if control[1]:
                qc.x(qubits[control[0].index])
            controls.append(qubits[control[0].index])
        if controls:
            qc.mcx(controls, qubits[gate.target.index])
        else:
            qc.x(qubits[gate.target.index])
        for control in gate.controls:
            if control[1]:
                qc.x(qubits[control[0].index])
    return qc
