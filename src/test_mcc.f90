program main

use iso_fortran_env, only: dp => real64
use ieee_arithmetic, only: ieee_is_finite

use mcc, only: simulate_annealing, get_resistance, random_integer

implicit none

real(dp), parameter :: pi = acos(-1.d0)

integer, parameter :: n_nodes = 10
integer, parameter :: n_resistors = 20
integer, parameter :: n_steps = 10000

integer, allocatable :: circuit(:, :)
integer :: i_resistor, i_end

allocate (circuit(2, n_resistors))

do
    do i_resistor = 1, n_resistors
        do i_end = 1, 2
            call random_integer(circuit(i_end, i_resistor), n_nodes)
        end do
    end do
    if (ieee_is_finite(get_resistance(circuit))) exit
end do
call simulate_annealing(circuit, n_nodes, pi, n_steps)
print *, pi, get_resistance(circuit)

end program
