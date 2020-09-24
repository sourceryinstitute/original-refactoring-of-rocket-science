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

    type, extends(Input_t) :: state_pair_input_t
        type(state_t) :: state_1
        type(state_t) :: state_2
    end type

    type, extends(Generator_t) :: state_generator_t
    contains
        procedure :: generate => generate_state
        procedure, nopass :: shrink => shrink_state
    end type

    type, extends(Generator_t) :: state_pair_generator_t
    contains
        procedure :: generate => generate_state_pair
        procedure, nopass :: shrink => shrink_state_pair
    end type
contains
    function test_state_class() result(tests)
        type(TestItem_t) :: tests

        tests = describe( &
                "a state", &
                [it( &
                        "can produce an array of its values in the order time, mass, energy, burn_depth", &
                        state_generator_t(), &
                        check_values_as_array), &
                it( &
                        "the components are added together when adding 2 states", &
                        state_pair_generator_t(), &
                        check_adding_states)])
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

    pure function check_adding_states(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(state_t) :: total

        select type (input)
        type is (state_pair_input_t)
            total = input%state_1 + input%state_2
            result_ = &
                    assertEquals(input%state_1%time() + input%state_2%time(), total%time()) &
                    .and. assertEquals(input%state_1%mass() + input%state_2%mass(), total%mass()) &
                    .and. assertEquals(input%state_1%energy() + input%state_2%energy(), total%energy()) &
                    .and. assertEquals(input%state_1%burn_depth() + input%state_2%burn_depth(), total%burn_depth())
        class default
            result_ = fail("expected a state_pair_input_t")
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

    function generate_state_pair(self) result(generated_value)
        class(state_pair_generator_t), intent(in) :: self
        type(Generated_t) :: generated_value

        real(rkind) :: time_1
        real(rkind) :: time_2
        real(rkind) :: mass_1
        real(rkind) :: mass_2
        real(rkind) :: energy_1
        real(rkind) :: energy_2
        real(rkind) :: burn_depth_1
        real(rkind) :: burn_depth_2

        time_1 = getRandomDoublePrecisionWithRange(0.1_rkind, 1.0e12_rkind)
        time_2 = getRandomDoublePrecisionWithRange(0.1_rkind, 1.0e12_rkind)
        mass_1 = getRandomDoublePrecisionWithRange(0.1_rkind, 1.0e12_rkind)
        mass_2 = getRandomDoublePrecisionWithRange(0.1_rkind, 1.0e12_rkind)
        energy_1 = getRandomDoublePrecisionWithRange(0.1_rkind, 1.0e12_rkind)
        energy_2 = getRandomDoublePrecisionWithRange(0.1_rkind, 1.0e12_rkind)
        burn_depth_1 = getRandomDoublePrecisionWithRange(0.1_rkind, 1.0e12_rkind)
        burn_depth_2 = getRandomDoublePrecisionWithRange(0.1_rkind, 1.0e12_rkind)

        generated_value = Generated(state_pair_input_t( &
                state_t(time_1, mass_1, energy_1, burn_depth_1), &
                state_t(time_2, mass_2, energy_2, burn_depth_2)))
    end function

    function shrink_state_pair(input) result(shrunk)
        class(Input_t), intent(in) :: input
        type(ShrinkResult_t) :: shrunk

        select type (input)
        type is (state_pair_input_t)
            shrunk = SimplestValue(input)
        class default
            error stop "type mismatch"
        end select
    end function
end module
