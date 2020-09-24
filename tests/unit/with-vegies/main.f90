program main
    use nozzle_class_test, only: &
            nozzle_class_nozzle_class => test_nozzle_class
    use Vegetables_m, only: TestItem_t, testThat, runTests

    implicit none

    call run()
contains
    subroutine run()
        type(TestItem_t) :: tests
        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = nozzle_class_nozzle_class()
        tests = testThat(individual_tests)

        call runTests(tests)
    end subroutine run
end program
