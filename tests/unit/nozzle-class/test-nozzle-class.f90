program main
  use assertions_interface, only: assert
  use kind_parameters, only: rkind
  use nozzle_interface, only: nozzle_t
  use universal_constants, only: PI

  implicit none

  type(nozzle_t) :: nozzle

  real(rkind) :: diameter

  call random_number(diameter)

  nozzle = nozzle_t(diameter = diameter, C_f = 1.7_rkind)

  call assert( &
      abs(nozzle%diameter() - diameter) < 1.0e-6_rkind, &
      "the given diameter is preserved")
  call assert( &
      abs(nozzle%area() - diameter**2 / 4.0_rkind * PI) < 1.0e-6_rkind, &
      "the area is calculated in accordance with the given diameter")
  call assert(abs(nozzle%thrust(1.0_rkind) - diameter**2 / 4 * PI * 1.7_rkind) < 1.0e-6_rkind, "thrust")

  print *," Test passed."

end program main
