module state_class_test
    use kind_parameters, only: rkind
    use state_interface, only: state_t
    use Vegetables_m, only: &
            Generated_t, &
            Generator_t, &
            Input_t, &
            Result_t, &
            ShrinkResult_t, &
            TestItem_t, &
            assertEquals, &
            describe, &
            fail, &
            Generated, &
            getRandomDoublePrecisionWithRange, &
            it, &
            SimplestValue

    implicit none
    private

    public :: test_state_class

    type, extends(Input_t) :: state_input_t
        type(state_t) :: state
    end type

    type, extends(Generator_t) :: state_generator_t
    contains
        procedure :: generate => generate_state
        procedure, nopass :: shrink => shrink_state
    end type
contains
    function test_state_class() result(tests)
        type(TestItem_t) :: tests

        tests = describe( &
                "a state", &
                [it( &
                        "can produce an array of its values in the order time, mass, energy, burn_depth", &
                        state_generator_t(), &
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

    function generate_state(self) result(generated_value)
        class(state_generator_t), intent(in) :: self
        type(Generated_t) :: generated_value

        real(rkind) :: time
        real(rkind) :: mass
        real(rkind) :: energy
        real(rkind) :: burn_depth

        time = getRandomDoublePrecisionWithRange(0.1_rkind, 1.0e12_rkind)
        mass = getRandomDoublePrecisionWithRange(0.1_rkind, 1.0e12_rkind)
        energy = getRandomDoublePrecisionWithRange(0.1_rkind, 1.0e12_rkind)
        burn_depth = getRandomDoublePrecisionWithRange(0.1_rkind, 1.0e12_rkind)

        generated_value = Generated(state_input_t(state_t( &
                time, mass, energy, burn_depth)))
    end function

    function shrink_state(input) result(shrunk)
        class(Input_t), intent(in) :: input
        type(ShrinkResult_t) :: shrunk

        select type (input)
        type is (state_input_t)
            shrunk = SimplestValue(input)
        class default
            error stop "type mismatch"
        end select
    end function
end module
