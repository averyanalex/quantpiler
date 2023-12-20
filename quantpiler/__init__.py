from .quantpiler import *

import qiskit


def circuit_to_qiskit(c: Circuit, rev=False) -> qiskit.circuit.QuantumCircuit:
    qubits = {}
    args = []
    ancs = []
    rets = []
    for qubit, qubit_descs in c.qubits_map_list():
        full_desc = []
        for desc in qubit_descs:
            full_desc.append(f"{desc.reg.name()}_{desc.index}")
        full_desc.sort()
        q = qiskit.circuit.Qubit()
        qubits[qubit.index] = q

        reg_name = "-".join(full_desc)
        reg = qiskit.circuit.QuantumRegister(name=reg_name, bits=[q])

        if "ret" in reg_name:
            rets.append(reg)
        elif "anc" in reg_name:
            ancs.append(reg)
        else:
            args.append(reg)

    args.sort(key=lambda r: r.name)
    ancs.sort(key=lambda r: r.name)
    rets.sort(key=lambda r: r.name)

    qc = qiskit.circuit.QuantumCircuit(*(args + ancs + rets))

    gates = c.gates
    if rev:
        gates.reverse()

    for gate in gates:
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

    qc = qiskit.transpile(qc)
    return qc
