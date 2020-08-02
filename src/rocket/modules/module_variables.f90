module mod1
use kind_parameters, only : rkind
implicit none
save

  ! define propellent grain geom; simple outward burning cylinder.
  ! outer diameter is inhibited since this is a cast propellent meaning
  ! it was poured into the tube/chamber and only the inner diameter
  ! burns when ignited
  real(rkind), parameter :: id=0.25_rkind  ! inner diameter of propellant
  real(rkind), parameter :: od=0.5_rkind  ! outder diameter
  real(rkind), parameter :: length=1.0_rkind ! propellant grain length
  real(rkind), parameter :: rref=0.05_rkind  ! propellant burn rate at Pref  (m/s)

  ! gas properties
  real(rkind), parameter :: mw=28_rkind   ! kg/mol
  real(rkind), parameter :: rgas=8314._rkind/mw

  ! combustion parameters
  real(rkind), parameter :: rhos=2000_rkind ! kg/m3
  real(rkind), parameter :: n=.4 ! burn rate exponent
  real(rkind), parameter :: psipa=6894.76_rkind ! pascals per psi (constant)
  real(rkind), parameter :: pref=3000_rkind*psipa ! reference pressure (constant)

  ! boundary condition
  real(rkind), parameter :: pamb=101325._rkind ! atmospheric pressure

  ! calculate time related values
  real(rkind), parameter ::  dt=0.001_rkind
  real(rkind), parameter  ::  tmax=15.0_rkind ! time to stop calculating performance

  real(rkind), parameter :: pi=3.14159_rkind

real(rkind):: cp,cv,g,vol,dia,cf,db
real(rkind):: thrust, area, r, surf,mdotgen,mdotout,edotgen,edotout,tflame, energy
real(rkind):: mdotos, edotos, texit, dsigng,p,t
real(rkind):: mcham,echam,time
integer:: nsteps
end module
