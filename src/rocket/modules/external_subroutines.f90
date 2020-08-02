  subroutine calmdotgen
  use mod1, only : &
    r, rref, p, pref, n, &
    surf, pi, id, db, length, &
    id, od, mdotgen, rhos, r, surf, edotgen, mdotgen,cp, tflame, &
    dt, vol
  use kind_parameters, only : rkind
  implicit none
  !integer i

  r=rref*(p/pref)**n ! calculate burn rate

  surf=pi*(id+2_rkind*db)*length
  if((id/2.0_rkind+db).gt.od/2.0_rkind) then
      surf=0.0_rkind ! burned out to wall
      r=0.0_rkind ! stop adding to burn distance because surface area becomes negative
  endif
  !if(i==1) surf=pi*id*length! no burn distance

  mdotgen=rhos*r*surf
  edotgen=mdotgen*cp*tflame
  db=db+r*dt ! increment burn distance
  vol=vol+r*surf*dt ! increment the volume due to burn back
  end subroutine




  subroutine massflow
   USE mod1, only : mdotos, edotos, pamb, area, t, g, rgas, p, cp, dsigng, texit
   use kind_parameters, only : rkind
   implicit none
   REAL (rkind)::mdtx,engyx
   REAL (rkind)::tx,gx,rx,px,cpx,pcrit,facx,term1,term2,pratio,cstar,ax,hx
   REAL (rkind):: p1,p2

   mdotos=0.
   edotos=0.  ! initially set them to zero prior to running this loop

            p1=p
            p2=pamb
            ax=area
              IF(p1.GT.p2) THEN
                          dsigng=1.0_rkind
                          tx=t
                          gx=g
                          rx=rgas
                          px=p
                          cpx=cp
                          hx=cp*t
                          pratio=p1/p2
                  else
                          dsigng=-1.0_rkind
                          tx=300_rkind
                          gx=g
                          rx=rgas
                          px=pamb
                          cpx=cp
                          hx=cp*300_rkind
                          pratio=p2/p1
                  end if

                  pcrit=(2._rkind/(gx+1._rkind))**(gx/(gx-1._rkind))
                  IF((1._rkind/pratio).LT.pcrit) then
                          ! choked flow
                          cstar=sqrt((1._rkind/gx)*((gx+1._rkind)/2._rkind)**((gx+1._rkind)/(gx-1._rkind))*rx*tx)
                          mdtx=px*ax/cstar
                  else
                          ! unchoked flow
                          facx=pratio**((gx-1._rkind)/gx)
                          term1=SQRT(gx*rx*tx/facx)
                          term2=SQRT((facx-1._rkind)/(gx-1._rkind))
                          mdtx=SQRT(2._rkind)*px/pratio/rx/tx*facx*term1*term2*ax
                  end if


                   engyx=mdtx*hx  ! reformulate based on enthalpy of the chamber

                   mdotos=mdtx*dsigng
                   edotos=engyx*dsigng

                   texit=tx


         end subroutine

  subroutine addmass
    use mod1, only : &
      mcham, mdotgen, dt, mdotos, &
      echam, edotgen,     edotos
    implicit none
    mcham=mcham+mdotgen*dt-mdotos*dt
    echam=echam+edotgen*dt-edotos*dt
  end subroutine

  subroutine calct
     use mod1, only : t, echam, mcham, cv
     implicit none
    t=echam/mcham/cv
  end subroutine

  subroutine calcp
    use mod1, only : p, mcham, rgas, t, vol
    implicit none
    p=mcham*rgas*t/vol
  end subroutine

  subroutine calcthrust
    use mod1, only : thrust, p, area, cf
    implicit none
    thrust=p*area*cf
  end subroutine
