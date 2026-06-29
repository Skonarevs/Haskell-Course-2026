// Quantum teleportation of |1> from qubit 0 to qubit 2.
// Alice shares a Bell pair (qubits 1 and 2) with Bob.
init 3

apply X [0]

apply H [1]
apply CNOT [1, 2]

apply CNOT [0, 1]
apply H [0]

measure 0 -> m0
measure 1 -> m1

if m1
  apply X [2]
end

if m0
  apply Z [2]
end

measure 2 -> teleported
print teleported
