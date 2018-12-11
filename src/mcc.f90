module mcc

use iso_fortran_env, only: dp => real64
use ieee_arithmetic, only: ieee_is_finite

implicit none

contains

subroutine simulate_annealing( &
    circuit, &
    n_nodes, &
    target_resistance, &
    temperature_start, &
    anneal_kind, &
    n_steps, &
    trajectory &
)
    integer, intent(inout) :: circuit(:, :)
    integer, intent(in) :: n_nodes
    real(8), intent(in) :: target_resistance
    real(8), intent(in) :: temperature_start
    character(len=*), intent(in) :: anneal_kind
    integer, intent(in) :: n_steps
    real(8), intent(out), optional :: trajectory(n_steps)

    integer, allocatable :: circuit_best(:, :), circuit_previous(:, :)
    real(8) :: energy, energy_best, energy_previous
    integer :: i_step
    real(8) :: temperature, rand

    temperature = temperature_start
    circuit_previous = circuit
    circuit_best = circuit
    energy_previous = abs(get_resistance(circuit)-target_resistance)
    energy_best = energy_previous
    i_step = 0
    do while (i_step < n_steps)
        circuit = switch_resistor(circuit_previous, n_nodes)
        energy = abs(get_resistance(circuit)-target_resistance)
        if (.not. ieee_is_finite(energy)) cycle
        i_step = i_step + 1
        call random_number(rand)
        if (exp((energy_previous-energy)/temperature) > rand) then
            energy_previous = energy
            circuit_previous = circuit
            if (energy < energy_best) then
                energy_best = energy
                circuit_best = circuit
                print *, 'steps:', i_step, 'E:', energy, 'T:', temperature
            end if
        end if
        if (present(trajectory)) trajectory(i_step) = energy_previous
        select case (anneal_kind)
        case ('linear')
            temperature = temperature - temperature_start/n_steps
        case ('constant')
        end select
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
    if (current < 0) current = 0d0
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
