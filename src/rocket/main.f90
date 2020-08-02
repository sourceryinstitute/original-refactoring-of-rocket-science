program main
  use command_line_interface, only : command_line
  use kind_parameters, only : rkind
  implicit none

  type(command_line) :: rocket_launch_command

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
    if (rocket_launch_command%argument_present([ character(len=len("--verbose")) :: "--verbose", "-v", "/verbose", "/v"])) then
      call print_results(rocket_output)
    end if

    associate( error => rocket_output - reference_output)
      block
        real(rkind), parameter :: tolerance = 1.E-6_rkind
        if (maxval(abs(error)) > tolerance) error stop "Test failed."
      end block
    end associate
  end associate

  print *,"Test passed."

contains

  subroutine print_results(results)
    real(rkind), intent(in) :: results(:,:)
    integer i
    do i=1,size(results,1)
      print *,results(i,:)
    end do
  end subroutine

end program
