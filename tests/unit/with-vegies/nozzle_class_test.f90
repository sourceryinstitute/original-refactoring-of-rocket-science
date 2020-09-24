module nozzle_class_test
    use kind_parameters, only: rkind
    use nozzle_interface, only: nozzle_t
    use Vegetables_m, only: Result_t, TestItem_t, assertEquals, describe, it

    implicit none
    private

    public :: test_nozzle_class
contains
    function test_nozzle_class() result(tests)
        type(TestItem_t) :: tests

        tests = describe( &
                "a nozzle", &
                [it("has zero thrust at zero pressure", check_thrust_at_zero)])
    end function

    pure function check_thrust_at_zero() result(result_)
        type(Result_t) :: result_

        type(nozzle_t) :: nozzle

        nozzle = nozzle_t(diameter = 1.0_rkind, C_f = 1.0_rkind)

        result_ = assertEquals(0.0_rkind, nozzle%thrust(0.0_rkind))
    end function
end module
