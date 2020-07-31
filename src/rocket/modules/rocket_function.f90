function rocket() result(output)
use mod1
implicit none

integer  i
real(8), allocatable :: output(:,:)
! this is a basic function for modeling a single-stage
! rocket motor flow out of a nozzle, assuming
! a thrust coefficient and ignoring the complexities of
! what happens to thrust at low pressures, i.e. shock in the nozzle


! define initial variables and constants
! gas variables
  cp=1500d0 ! j/kg/K
  mw=28d0   ! kg/mol
  tflame=4000.d0 ! Kelvin

! define geometry
  vol= 1.0d0 ! cubic meters
  dia=0.05d0 ! meters
  cf=1.7d0 ! nozzle thrust coefficient

! define propellent grain geom; simple outward burning cylinder.
! outer diameter is inhibited since this is a cast propellent meaning
! it was poured into the tube/chamber and only the inner diameter
! burns when ignited
  id=0.25d0  ! inner diameter of propellant
  od=0.5d0  ! outder diameter
  length=1.0d0 ! propellant grain length
  rref=0.05d0  ! propellant burn rate at Pref  (m/s)
  rhos=2000d0 ! kg/m3
  n=.4 ! burn rate exponent
  psipa=6894.76d0 ! pascals per psi (constant)
  pref=3000d0*psipa ! reference pressure (constant)
  db=0d0 ! initial burn distance

! calculate time related values
  dt=0.001d0
  tmax=15.0d0 ! time to stop calculating performance
  nsteps=nint(tmax/dt) ! number of time steps
! preallocate an output file for simulation infomration
  allocate(output(nsteps,4))
  output=0d0 ! initialize to zero

  thrust=0d0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11

! now begin calculating and initializing
! gas variables
  rgas=8314d0/mw
  cv=cp-rgas
  g=cp/cv
  pi=3.14159d0
  area=pi/4.0d0*dia**2.0d0

  t=300d0 ! initial temp (Kelvin)
  pamb=101325d0 ! atmospheric pressure
  p=pamb  ! initial chamber pressure

!  calculate initial mass and energy in the chamber
  mcham=p*vol/rgas/t
  echam=mcham*cv*t
  time=0d0
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

  do i=1,nsteps-1
    print *, output(i,:)
  enddo
  end function
