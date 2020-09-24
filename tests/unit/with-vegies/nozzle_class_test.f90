module nozzle_class_test
    use kind_parameters, only: rkind
    use nozzle_interface, only: nozzle_t
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

    public :: test_nozzle_class

    type, extends(Input_t) :: nozzle_input_t
        type(nozzle_t) :: nozzle
    end type

    type, extends(Generator_t) :: nozzle_generator_t
    contains
        procedure :: generate => generate_nozzle
        procedure, nopass :: shrink => shrink_nozzle
    end type
contains
    function test_nozzle_class() result(tests)
        type(TestItem_t) :: tests

        tests = describe( &
                "a nozzle", &
                [it( &
                        "has zero thrust at zero pressure", &
                        nozzle_generator_t(), &
                        check_thrust_at_zero)])
    end function

    pure function check_thrust_at_zero(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        select type (input)
        type is (nozzle_input_t)
            result_ = assertEquals(0.0_rkind, input%nozzle%thrust(0.0_rkind))
        class default
            result_ = fail("expected a nozzle_input_t")
        end select
    end function

    function generate_nozzle(self) result(generated_value)
        class(nozzle_generator_t), intent(in) :: self
        type(Generated_t) :: generated_value

        real(rkind) :: diameter
        real(rkind) :: C_f

        diameter = getRandomDoublePrecisionWithRange(0.1_rkind, 1.0e12_rkind)
        C_f = getRandomDoublePrecisionWithRange(0.1_rkind, 1.0e12_rkind)
        generated_value = Generated(nozzle_input_t(nozzle_t(diameter, C_f)))
    end function

    function shrink_nozzle(input) result(shrunk)
        class(Input_t), intent(in) :: input
        type(ShrinkResult_t) :: shrunk

        select type (input)
        type is (nozzle_input_t)
            shrunk = SimplestValue(input)
        class default
            error stop "type mismatch"
        end select
    end function
end module
