// Bell state creation and measurement
init 2

apply H    [0]
apply CNOT [0, 1]

measure 0 -> m0
measure 1 -> m1

print m0
print m1

// m0 and m1 should always agree (both 0 or both 1)
