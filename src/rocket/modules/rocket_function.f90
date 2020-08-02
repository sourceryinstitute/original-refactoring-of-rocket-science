function rocket() result(output)
use mod1, only : &
  area, cp, cv, db, dia, dt, echam, g, mcham, nsteps, p, pamb, pi, rgas, t, thrust, time, tmax, vol
use kind_parameters, only : rkind
implicit none

integer  i
real(rkind), allocatable :: output(:,:)
! this is a basic function for modeling a single-stage
! rocket motor flow out of a nozzle, assuming
! a thrust coefficient and ignoring the complexities of
! what happens to thrust at low pressures, i.e. shock in the nozzle


! define initial variables
  vol= 1.0_rkind ! cubic meters
  db=0_rkind ! initial burn distance

! calculate time related values
  nsteps=nint(tmax/dt) ! number of time steps
! preallocate an output file for simulation infomration
  allocate(output(nsteps,4))
  output=0_rkind ! initialize to zero

  thrust=0_rkind
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11

! now begin calculating and initializing
! gas variables
  cv=cp-rgas
  g=cp/cv
  area=pi/4.0_rkind*dia**2.0_rkind

  t=300._rkind ! initial temp (Kelvin)
  p=pamb  ! initial chamber pressure

!  calculate initial mass and energy in the chamber
  mcham=p*vol/rgas/t
  echam=mcham*cv*t
  time=0._rkind

  do i=1,nsteps

   call calmdotgen
   call massflow   ! [mdot,engy,dsign]= massflow(p1,pamb,t1,tamb,cp,cp,rgas,rgas,g,g,area)
   call addmass
   call calct
   call calcp
   call calcthrust

   output(i,:)=[time,p,t, thrust]
   time=time+dt
  enddo

  end function
