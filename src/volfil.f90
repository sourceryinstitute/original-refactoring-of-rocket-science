    module kind_parameters
      implicit none

      private
      public :: DP

      integer, parameter :: precision=15, range=307
      integer, parameter :: DP = selected_real_kind(precision, range)

    end module

    module subdeclare
      !! this probram calculates a simple, single chamber blowdown/combustion simulation.
      !! conservation of Mass/Momentum using modern fortran fortran 95/2003 techniques
      use kind_parameters, only : DP

      implicit none

      real, parameter:: Ru=8314._DP
      real, parameter:: pamb=101325._DP
      real, parameter:: tamb=300._DP
      real, parameter:: pi=3.14159_DP
      real, parameter:: pref=20.7E6_DP

      type flags
          real(DP) :: temp,dt,tmax
          integer  ::nsteps=selected_int_kind(12)
      end type flags

      type gasprop
          real(DP) :: cp,cv,h,e,mw,rgas,g
      end type gasprop

      type chamber_internal
          real(DP) :: M, E,P,T,vol
      end type chamber_internal

      type flow
          real(DP):: diam, mdoto, edoto
      end type flow

      type combustion
          real(DP):: mdotgen, edotgen, tflame, mpkg, genmass, genheight, gendiam, rhosolid, ntabs, voltab, mtab,db,rref,r,n,summ
      end type combustion


    contains




    ! only called once to find # of tablets
    subroutine ntabs(tablets)
    type(combustion),intent(inout)::tablets
    tablets%voltab=tablets%genheight*pi*0.25*tablets%gendiam**2
    tablets%mtab=tablets%voltab*tablets%rhosolid
    tablets%ntabs=tablets%genmass/tablets%mtab
    end subroutine




    subroutine getgasproperties(xp,cham)  ! all f(T)
    type(gasprop),intent(inout)::xp
    type(chamber_internal),intent(in)::cham
    real(DP)::T
    T=cham%T
    xp%h=xp%cp*T
    xp%e=xp%cv*T
    xp%rgas=xp%cp-xp%cv
    xp%g=xp%cp/xp%cv
    end subroutine getgasproperties



    subroutine calctp(a,b)  ! calculate flow burning into chamber
    type(chamber_internal),intent(inout) :: a
    type(gasprop), intent(in)::b
    a%T=a%E/a%M/b%cv ! calculate temp, assuming constant specific heat
    a%P=a%M*b%rgas*a%T/a%vol ! calculate ideal gas pressure
    end subroutine calctp



    subroutine calmdotgen(chamcond,comb,gp,flag)
    type(gasprop),intent(in) :: gp
    type(chamber_internal), intent(in):: chamcond
    type(combustion), intent(inout) :: comb
    type(flags), intent(in)::flag
    real(DP) :: surf,dist,h,r
    ! real(DP):: mdotgen, edotgen, tflame, mpkg, genmass, genheight, gendiam, rhosolid, ntabs, voltab, mtab,db,rref,r,n)
    comb%r=comb%rref*(chamcond%p/pref)**comb%n ! forget about conditioning temperature for now
    comb%db=comb%db+comb%r*flag%dt
    r=comb%gendiam/2.
    dist=comb%db
    h=comb%genheight
    surf=comb%ntabs*(2*pi*(r-dist)*(h-2*dist)+2*pi*(r-dist)**2)
    if(dist>r) surf=0.
    if(dist>h/2) surf=0.
    comb%mdotgen=comb%r*surf*comb%rhosolid ! amount of solid combusted
    comb%summ=comb%summ+comb%mdotgen*flag%dt ! keepting track of how much has burned
    if(comb%summ>comb%genmass) comb%mdotgen=0.
   ! now factor in the gas yield
    !1real(DP):: mdotgen, edotgen, tflame, mpkg, genmass, genheight, gendiam, rhosolid, ntabs, voltab, mtab,db,rref,r,n
    comb%mdotgen=comb%mdotgen*comb%mpkg*gp%mw/1d3 ! amount of gas created vs solids
    comb%edotgen=comb%mdotgen*gp%cp*comb%tflame ! ENTHALPY
    end subroutine calmdotgen



    subroutine massflow(ch,gas,flo)  ! calculatte flow exiting chamber
    type(chamber_internal),intent(in)::ch
    type(gasprop), intent(in)::gas
    type(flow), intent(out) :: flo
    real(dp) :: pcrit,pratio,p1,p2,ax,tx,gx,rx,px,cstar,facx,term1,term2,mdtx
    gx=gas%g
    pcrit=(2/(gx+1))**(gx/(gx-1))
    p1 = ch%P
    p2 = pamb
    ax = pi*.25*flo%diam**2.0
    if(p2>p1) error stop "negative flow"
    ! assuming always positive flow
    tx = ch%T
    pratio=p1/p2

    rx= gas%rgas
    px = p1

    IF((1. / pratio) .LT. pcrit) then
        ! choked flow
        cstar = sqrt((1.0d0 / gx) * ((gx + 1.0d0) / 2.0d0) ** ((gx + 1.0d0) / (gx - 1.0d0)) * rx * tx)
        mdtx = px * ax / cstar
    else
        ! unchoked flow
        facx = pratio ** ((gx - 1.0d0) / gx)
        term1 = SQRT(gx * rx * tx / facx)
        term2 = SQRT((facx - 1.0D0) / (gx - 1.0D0))
        mdtx = SQRT(2.0d0) * px / pratio / rx / tx * facx * term1 * term2 * ax

    endif
    flo%mdoto=mdtx
    flo%edoto=mdtx*gas%h
    end subroutine



    subroutine addmass(cham,cmb,flo,flg)  ! update mass and energy balance in the chamber
    type(chamber_internal),intent(inout) :: cham
    type(combustion) ,intent(in):: cmb
    type(flow),intent(in) :: flo
    type(flags),intent(in) :: flg
    cham%M=cham%M+(cmb%mdotgen-flo%mdoto)*flg%dt
    cham%E=cham%E+(cmb%edotgen-flo%edoto)*flg%dt
    end subroutine addmass

    ! end contains

    end module



program volfil
  use kind_parameters, only : DP
  use subdeclare
  implicit none

  type(chamber_internal)::cham
  type(combustion)::comb
  type(gasprop)::gp
  type(flags)::flag
  type(flow)::flo

  integer :: nsteps,i
  real(DP) :: time
  open(unit=20,file='volfil.inp')
  read(20,*);read(20,*)
  read(20,*) flag%dt,flag%tmax; read(20,*)
  read(20,*) gp%cp,gp%mw; read(20,*)
  gp%rgas=gp%cp-gp%cv; gp%cv=gp%cp-Ru/gp%mw ! set a value for rgas
  gp%g=gp%cp/gp%cv;
  read(20,*) cham%P,cham%T, cham%vol; read(20,*)
  read(20,*) flo%diam;read(20,*)
  read(20,*) comb%tflame,comb%mpkg, comb%genmass, comb%genheight, comb%gendiam, comb%rhosolid, comb%rref, comb%n
  close(20)
  nsteps=(flag%tmax)/(flag%dt)
  comb%db=0.d0 !initialized burn distance to zero
  cham%T=300;cham%M=.03

  cham%E=cham%M*cham%T*gp%cv
  call ntabs(comb)
  call getgasproperties(gp,cham)
  call calctp(cham,gp)
  time=0.d0

  block
    integer, parameter :: skip=50, first=1, num_variables=3
    integer :: step, last
    real(DP) :: output(num_variables,nsteps)

    last=nsteps-1
    do i = first, last
      time=time+flag%dt

      call calmdotgen(cham, comb, gp,flag)
      call massflow(cham, gp, flo)
      call addmass(cham,comb,flo,flag)
      call getgasproperties(gp,cham)
      call calctp(cham,gp)
      print*, [time, cham%P,  cham%T]
      output(:,i)=[time, cham%P,  cham%T]
    end do

    open(unit=20,file='volfil.out',status='unknown')

    do step = first, last, skip
      write(20,'(3e15.5)') output(:,step)
    end do
  end block
  close(20)

  print *,"Test passed."
end program