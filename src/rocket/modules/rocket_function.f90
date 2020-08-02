function rocket() result(output)
use mod1
use kind_parameters, only : rkind
implicit none

integer  i
real(rkind), allocatable :: output(:,:)
! this is a basic function for modeling a single-stage
! rocket motor flow out of a nozzle, assuming
! a thrust coefficient and ignoring the complexities of
! what happens to thrust at low pressures, i.e. shock in the nozzle


! define initial variables and constants
! gas variables
  cp=1500_rkind ! j/kg/K
  mw=28_rkind   ! kg/mol
  tflame=4000._rkind ! Kelvin

! define geometry
  vol= 1.0_rkind ! cubic meters
  dia=0.05_rkind ! meters
  cf=1.7_rkind ! nozzle thrust coefficient

! define propellent grain geom; simple outward burning cylinder.
! outer diameter is inhibited since this is a cast propellent meaning
! it was poured into the tube/chamber and only the inner diameter
! burns when ignited
  id=0.25_rkind  ! inner diameter of propellant
  od=0.5_rkind  ! outder diameter
  length=1.0_rkind ! propellant grain length
  rref=0.05_rkind  ! propellant burn rate at Pref  (m/s)
  rhos=2000_rkind ! kg/m3
  n=.4 ! burn rate exponent
  psipa=6894.76_rkind ! pascals per psi (constant)
  pref=3000_rkind*psipa ! reference pressure (constant)
  db=0_rkind ! initial burn distance

! calculate time related values
  dt=0.001_rkind
  tmax=15.0_rkind ! time to stop calculating performance
  nsteps=nint(tmax/dt) ! number of time steps
! preallocate an output file for simulation infomration
  allocate(output(nsteps,4))
  output=0_rkind ! initialize to zero

  thrust=0_rkind
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11

! now begin calculating and initializing
! gas variables
  rgas=8314._rkind/mw
  cv=cp-rgas
  g=cp/cv
  pi=3.14159_rkind
  area=pi/4.0_rkind*dia**2.0_rkind

  t=300._rkind ! initial temp (Kelvin)
  pamb=101325._rkind ! atmospheric pressure
  p=pamb  ! initial chamber pressure

!  calculate initial mass and energy in the chamber
  mcham=p*vol/rgas/t
  echam=mcham*cv*t
  time=0._rkind
  do i=1,nsteps

   call calmdotgen
   call massflow
  ! [mdot,engy,dsign]= massflow(p1,pamb,t1,tamb,cp,cp,rgas,rgas,g,g,area)
   call addmass
   call calct
   call calcp
   call calcthrust
   output(i,:)=[time,p,t, thrust]
   time=time+dt
  enddo

  end function
