program main
  use kind_parameters, only : rkind
  implicit none

  interface

    function reference_rocket() result(output)
      import rkind
      real(rkind), allocatable :: output(:,:)
    end function

    function rocket() result(output)
      import rkind
      real(rkind), allocatable :: output(:,:)
    end function

  end interface

  associate( &
    reference_output => reference_rocket(), &
    rocket_output => rocket() &
  )
    associate( error => rocket_output - reference_output)
      block
        real(rkind), parameter :: tolerance = 1.E-6_rkind
        if (maxval(abs(error)) > tolerance) error stop "Test failed."
      end block
    end associate
  end associate

  print *,"Test passed."

end program
