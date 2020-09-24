module state_class_test
    use kind_parameters, only: rkind
    use state_interface, only: state_t
    use Vegetables_m, only: &
            Input_t, &
            Result_t, &
            TestItem_t, &
            assertEquals, &
            describe, &
            example, &
            fail, &
            it

    implicit none
    private

    public :: test_state_class

    type, extends(Input_t) :: state_input_t
        type(state_t) :: state
    end type
contains
    function test_state_class() result(tests)
        type(TestItem_t) :: tests

        type(state_t) :: state

        state = state_t(time = 1.0_rkind, mass = 1.0_rkind, energy = 1.0_rkind, burn_depth = 1.0_rkind)

        tests = describe( &
                "a state", &
                [it( &
                        "can produce an array of its values in the order time, mass, energy, burn_depth", &
                        [example(state_input_t(state))], &
                        check_values_as_array)])
    end function

    pure function check_values_as_array(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        real(rkind), allocatable :: values(:,:)

        select type (input)
        type is (state_input_t)
            values = input%state%row_vector()

            result_ = &
                    assertEquals(input%state%time(), values(1,1)) &
                    .and. assertEquals(input%state%mass(), values(1,2)) &
                    .and. assertEquals(input%state%energy(), values(1,3)) &
                    .and. assertEquals(input%state%burn_depth(), values(1,4))
        class default
            result_ = fail("expected a state_input_t")
        end select
    end function
end module
