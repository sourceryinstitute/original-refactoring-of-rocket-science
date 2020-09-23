program main
  use assertions_interface, only: assert
  use kind_parameters, only: rkind
  use nozzle_interface, only: nozzle_t
  use universal_constants, only: PI

  implicit none

  type(nozzle_t) :: nozzle

  call nozzle%define("rocket.inp")

  call assert(abs(nozzle%diameter() - 0.05_rkind) < 1.0e-6_rkind, "diameter")
  call assert(abs(nozzle%area() - 0.05_rkind**2 / 4 * PI) < 1.0e-6_rkind, "area")
  call assert(abs(nozzle%thrust(1.0_rkind) - 0.05_rkind**2 / 4 * PI * 1.7_rkind) < 1.0e-6_rkind, "thrust")

  print *," Test passed."

end program main
