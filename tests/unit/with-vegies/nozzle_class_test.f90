module nozzle_class_test
    use kind_parameters, only: rkind
    use nozzle_interface, only: nozzle_t
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

    public :: test_nozzle_class

    type, extends(Input_t) :: nozzle_input_t
        type(nozzle_t) :: nozzle
    end type
contains
    function test_nozzle_class() result(tests)
        type(TestItem_t) :: tests

        type(nozzle_t) :: nozzle

        nozzle = nozzle_t(diameter = 1.0_rkind, C_f = 1.0_rkind)

        tests = describe( &
                "a nozzle", &
                [it( &
                        "has zero thrust at zero pressure", &
                        [example(nozzle_input_t(nozzle))], &
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
end module
