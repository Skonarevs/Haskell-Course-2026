// Deutsch algorithm with constant oracle f(x) = 0.
// A constant function always yields result = 0.
init 2

apply X [1]
apply H [0]
apply H [1]

apply H [0]

measure 0 -> result
print result
