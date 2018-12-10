module mcc

use iso_fortran_env, only: dp => real64

implicit none

private
public :: simulate_annealing, get_resistance, random_integer

contains

subroutine simulate_annealing(circuit, n_nodes, target_resistance, n_steps)
    integer, intent(inout) :: circuit(:, :)
    integer, intent(in) :: n_nodes
    real(8), intent(in) :: target_resistance
    integer, intent(in) :: n_steps

    integer, allocatable :: circuit_best(:, :), circuit_previous(:, :)
    real(8) :: energy, energy_best, energy_previous
    integer :: i_step
    real(8) :: temperature, rand

    do i_step = 0, n_steps
        temperature = 1d-3*(1d0-dble(i_step)/n_steps)
        energy = abs(get_resistance(circuit)-target_resistance)
        call random_number(rand)
        if (rand < exp((energy_previous-energy)/temperature) .or. i_step == 0) then
            energy_previous = energy
            circuit_previous = circuit
            if (energy < energy_best .or. i_step == 0) then
                energy_best = energy
                circuit_best = circuit
                print *, 'steps:', i_step, 'E:', energy
            end if
        end if
        circuit = switch_resistor(circuit_previous, n_nodes)
    end do
    circuit = circuit_best

end subroutine

function switch_resistor(circuit, n_nodes) result(switched)
    integer, intent(in) :: circuit(:, :)
    integer :: switched(2, size(circuit, 2))

    integer :: i_node, j_node, n_nodes, i_resistor

    call random_integer(i_resistor, size(circuit, 2))
    call random_integer(i_node, n_nodes)
    call random_integer(j_node, n_nodes-1)
    if (j_node == i_node) j_node = n_nodes
    switched = circuit
    switched(:, i_resistor) = [i_node, j_node]
end function

real(8) function get_resistance(circuit) result(res)
    integer, intent(in) :: circuit(:, :)

    real(8), allocatable :: work(:, :)
    real(8) :: current
    integer :: i_node, j_node, k_node, n_nodes, i_resistor

    n_nodes = maxval(circuit)
    allocate (work(n_nodes, n_nodes), source=0d0)
    do i_resistor = 1, size(circuit, 2)
        i_node = circuit(1, i_resistor)
        j_node = circuit(2, i_resistor)
        work(i_node, j_node) = work(i_node, j_node) - 1d0
        work(j_node, i_node) = work(i_node, j_node)
    end do
    forall (i_node = 1:n_nodes) work(i_node, i_node) = -sum(work(:, i_node))
    do k_node = n_nodes, 3, -1
        forall (i_node = 1:n_nodes, j_node = 1:n_nodes)
            work(i_node, j_node) = work(i_node, j_node) &
                - work(i_node, k_node)*work(k_node, j_node)/work(k_node, k_node)
        end forall
    end do
    current = (work(1, 1)-work(1, 2))/2
    res = 1d0/current
end function

subroutine random_integer(rand, nmax)
    integer, intent(out) :: rand
    integer, intent(in) :: nmax

    real(8) :: rand_real

    call random_number(rand_real)
    rand = 1 + int(rand_real*nmax)
end subroutine

end module
