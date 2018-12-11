program main

use iso_fortran_env, only: dp => real64
use ieee_arithmetic, only: ieee_is_finite

use mcc, only: simulate_annealing, get_resistance

implicit none

real(dp), parameter :: pi = acos(-1.d0)

integer, parameter :: n_nodes = 10
integer, parameter :: n_resistors = 20
integer, parameter :: n_steps = 10000

integer, allocatable :: circuit(:, :)

allocate (circuit(2, n_resistors))
circuit(1, :) = 1
circuit(2, :) = 2
call simulate_annealing(circuit, n_nodes, pi, 1d-3, 'linear', n_steps)
print *, pi, get_resistance(circuit)

end program
