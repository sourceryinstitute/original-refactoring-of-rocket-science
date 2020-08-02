module mod1
use kind_parameters, only : rkind
implicit none
save
real(rkind):: cp,cv,g,rgas,mw,vol,dia,cf,id,od,length,rref,rhos,psipa,pref,db,dt,tmax
real(rkind):: thrust, area, r, surf,mdotgen,mdotout,edotgen,edotout,tflame, energy
real(rkind):: pi, mdotos, edotos, texit, dsigng,pamb,p,t
real(rkind):: mcham,echam,time,n
integer:: nsteps
end module
