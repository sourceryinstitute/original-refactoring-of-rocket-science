module state_class_test
    use kind_parameters, only: rkind
    use state_interface, only: state_t
    use Vegetables_m, only: Result_t, TestItem_t, assertEquals, describe, it

    implicit none
    private

    public :: test_state_class
contains
    function test_state_class() result(tests)
        type(TestItem_t) :: tests

        tests = describe( &
                "a state", &
                [it( &
                        "can produce an array of its values in the order time, mass, energy, burn_depth", &
                        check_values_as_array)])
    end function

    pure function check_values_as_array() result(result_)
        type(Result_t) :: result_

        type(state_t) :: state
        real(rkind), allocatable :: values(:,:)

        state = state_t(time = 1.0_rkind, mass = 1.0_rkind, energy = 1.0_rkind, burn_depth = 1.0_rkind)

        values = state%row_vector()

        result_ = &
                assertEquals(state%time(), values(1,1)) &
                .and. assertEquals(state%mass(), values(1,2)) &
                .and. assertEquals(state%energy(), values(1,3)) &
                .and. assertEquals(state%burn_depth(), values(1,4))
    end function
end module
