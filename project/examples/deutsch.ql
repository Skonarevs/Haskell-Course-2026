// Deutsch algorithm with balanced oracle f(x) = x.
// A balanced function always yields result = 1.
init 2

apply X [1]
apply H [0]
apply H [1]

apply CNOT [0, 1]

apply H [0]

measure 0 -> result
print result
