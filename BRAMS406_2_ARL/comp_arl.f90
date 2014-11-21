!----------------------------------------------------------------
! University of Houston
! Air Quality Modeling and Monitoring Center
! 218 Old Science Building
! Houston, TX 77204-5048
! USA
!
! email: sbkim@math.uh.edu (Dr. Seung-Bum Kim)
!        dwbyun@math.uh.edu (Dr. Daewon W. Byun)
!----------------------------------------------------------------

subroutine comp_ARL(n1,n2,n3,zt)
!----------------------------------------------------------------
! Name:     comp_arl
! Purpose:  compute met. variables needed for RAMS ARL packed data
! Revised:  06 Aug 2002  Created for RAMS2ARL (S.-B. Kim)
! Note:     this version is F90 for use with RAMS v4.3
!
!---------------------------------------------------------------
!
include 'param.inc' 
include 'fields.inc' 

real, dimension(80) :: zt

real, parameter ::             &
       rgas    = 287.          &
    ,  cp      = 1004.         &
    ,  cpor    = cp/rgas       &
    ,  p00     = 1.e5 

!-------------------------------------------------------------------------
!-----RH2M : Relative Humidity at 2 m
!
do j=1,n3
do i=1,n2
  valthet= THETA(2,i,j)
  valpp= PP(2,i,j)
  valpi0= PI0(2,i,j)
  valtemp= valthet*(valpp+valpi0)/cp
  valprs=((valpi0 + valpp)/cp)**cpor * p00
  valrvs= RS(valprs, valtemp) ! ./lib/misc_lib.F90
  valrv = RV(2,i,j) ! kg/kg
  RH2M(i,j)=100.*valrv/valrvs
enddo
enddo
!
!-----PRSS: sfc pressure in mb 
!
do j=1,n3
do i=1,n2
!-->Compute hydrostatically from half level 2 
  valpp=PP(2,i,j)
  valpi0=PI0(2,i,j)
  valprs=(valpp+valpi0)
  avar = ((valprs/cp)**3.4965)*1000. 
  valrho2=DN0(2,i,j)
  valrho1=DN0(1,i,j)
  valrhos=(valrho2+valrho1)*.5
  valdlp=valrhos*9.81*ZT(2)*0.01
  avar=avar+valdlp
  PRSS(I,J)=avar
enddo
enddo
!
!-----T02M: temperature at 2m in Kelvin
!
do j=1,n3
do i=1,n2
  valthet= THETA(1,i,j)
  valpp= PP(1,i,j)
  valpi0= PI0(1,i,j)
  T02M(I,J)=valthet*(valpp+valpi0)/cp + 0.95
enddo
enddo
!
!-------------------------------------------------------------------------
! test
!do j=1,n3
!do i=1,n2
!  write(30,*) i,j,rh2m(i,j)
!  write(32,*) i,j,shgt(i,j)
!  write(33,*) i,j,tpp1(i,j)
!  write(34,*) i,j,prss(i,j)
!  write(35,*) i,j,t02m(i,j)
!  write(36,*) i,j,ustr(i,j)
!  write(37,*) i,j,tstr(i,j)
!  write(38,*) i,j,qstr(i,j)
!  write(39,*) i,j,umof(i,j)
!  write(40,*) i,j,vmof(i,j)
!  write(41,*) i,j,dswf(i,j)
!  write(42,*) i,j,dlwf(i,j)
!enddo
!enddo
!
return
end 
