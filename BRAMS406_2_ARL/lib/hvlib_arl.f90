!############################# Change Log ##################################
! 2.3.0.5
!
! 000921 MJB RAMS_varlib read_SPEC ##
!            Cleaned up screen output. ##
! 000915 MJB RAMS_varlib read_SPEC ##
!            Modified for HYPACT input. ##
! 000830 CJT RAMS_varlib ##
!            Removed interface.h. ##
! 000829 CJT RAMS_varlib ##
!            Changed from passing arguments to using an_header module. ##
! 000828 MJB RAMS_varlib ##
!            Replaced c dynamic allocations to f90. ##
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!  Mission Research Corporation / *ASTeR Division
!###########################################################################

subroutine RAMS_varlib(cvar,n1,n2,n3,ngrd,a,b,flnm  &
                 ,cdname,cdunits,icoor)
implicit none
include 'rcommons.h'

integer :: n1,n2,n3,ngrd,icoor
integer :: i,j
real :: xtr(n1),ytr(n2)

integer :: memsiz4,memsiz6

character*(*) cvar,flnm,cdname,cdunits

real :: a(*),b(*)

real, allocatable, save :: c(:),d(:),e(:),f(:),o(:)
integer :: lv,lv2,idim_type,irecind,irecsize,irecsizep,ind,ispec
integer :: memsave4,ierr,kp
character cspec*3

integer, external :: RAMS_getvar, lastchar, irfree, iralloc
integer :: ierr_getvar,ifound,ivar_type
common /getvar/ierr_getvar,ifound,ivar_type
data memsave4/0/

real, allocatable :: pv1(:,:,:),pv2(:,:,:),pv3(:,:,:),pv4(:,:,:)
real, allocatable :: pv5(:,:,:)


memsiz4=nxyzpm
memsiz6=nxyzpm*nclouds

if (allocated(c)) deallocate (c)
if (allocated(d)) deallocate (d)
if (allocated(e)) deallocate (e)
if (allocated(f)) deallocate (f)
if (allocated(o)) deallocate (o)
allocate(c(memsiz4))
allocate(d(memsiz4))
allocate(e(memsiz4))
allocate(f(memsiz4))
allocate(o(memsiz6))

lv=lastchar(cvar)
lv2=min(lv,index(cvar,':')-1)  ! for HYPACT fields
!print*,'===> varlib- ',cvar,n1,n2,n3,ngrd

ivar_type=0
ierr_getvar=0
ierr=0
ifound=0

!JCL
!print*,'in RAMS_varlib:',cvar(1:lv)

! LOAD GRID-POINT LOCATION VECTORS

do i = 1,n1
  xtr(i) = xtn(i,ngrd)/1000.
enddo
do j = 1,n2
  ytr(j) = ytn(j,ngrd)/1000.
enddo

! 3D VELOCITY AND VORTICITY VARIABLES

if(cvar(1:lv).eq.'u') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   cdname='u;'
   cdunits='m/s;'

elseif(cvar(1:lv).eq.'v') then
   ivar_type=3
   ierr= RAMS_getvar('VP',idim_type,ngrd,a,b,flnm)
   cdname='v;'
   cdunits='m/s;'

elseif(cvar(1:lv).eq.'ue') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_rotate(n1,n2,n3,a,c,ngrd)
   cdname='ue;'
   cdunits='m/s;'

elseif(cvar(1:lv).eq.'ve') then
   ivar_type=3
   ierr= RAMS_getvar('VP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('UP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_rotate(n1,n2,n3,c,a,ngrd)
   cdname='ve;'
   cdunits='m/s;'

elseif(cvar(1:lv).eq.'ue_avg') then
   ivar_type=3
   ierr=RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr=RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_rotate(n1,n2,n3,a,c,ngrd)
   call RAMS_comp_avgu(n1,n2,n3,a)
   cdname='ue_avg;'
   cdunits='m/s;'

elseif(cvar(1:lv).eq.'ve_avg') then
   ivar_type=3
   ierr=RAMS_getvar('VP',idim_type,ngrd,a,b,flnm)
   ierr=RAMS_getvar('UP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_rotate(n1,n2,n3,c,a,ngrd)
   call RAMS_comp_avgv(n1,n2,n3,a)
   cdname='ve_avg;'
   cdunits='m/s;'

elseif(cvar(1:lv).eq.'w') then
   ivar_type=3
   ierr= RAMS_getvar('WP',idim_type,ngrd,a,b,flnm)
   cdname='w;'
   cdunits='m/s;'

elseif(cvar(1:lv).eq.'wcms') then
   ivar_type=3
   ierr= RAMS_getvar('WP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_wcms(n1,n2,n3,a)
   cdname='w;'
   cdunits='cm/s;'

elseif(cvar(1:lv).eq.'w_avg') then
   ivar_type=3
   ierr=RAMS_getvar('WP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_avgw(n1,n2,n3,a)
   cdname='w_avg;'
   cdunits='m/s;'

elseif(cvar(1:lv).eq.'speed') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_speed(n1,n2,n3,a,c)
   cdname='speed;'
   cdunits='m/s;'

elseif(cvar(1:lv).eq.'speed_mph') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_speed(n1,n2,n3,a,c)
   call RAMS_comp_mults(n1,n2,n3,a,2.237)
   cdname='speed;'
   cdunits='mph;'

elseif(cvar(1:lv).eq.'tempf2m') then

   ivar_type=2
   ierr=RAMS_getvar('UP',idim_type,ngrd  &
         ,c,b,flnm)
   ierr=RAMS_getvar('VP',idim_type,ngrd  &
         ,d,b,flnm)
   call RAMS_comp_speed(n1,n2,n3,c,d)
   ierr=RAMS_getvar('THETA',idim_type,ngrd  &
         ,d,b,flnm)

!        Get topo
   ierr= RAMS_getvar('TOPT',idim_type,ngrd  &
         ,e,b,flnm)

   !print*,'=============tempf2m:',nnxp(ngrd),nnyp(ngrd),npatch
   allocate (pv1(nnxp(ngrd),nnyp(ngrd),npatch) )
   allocate (pv2(nnxp(ngrd),nnyp(ngrd),npatch) )
   allocate (pv3(nnxp(ngrd),nnyp(ngrd),npatch) )
   allocate (pv4(nnxp(ngrd),nnyp(ngrd),npatch) )
   allocate (pv5(nnxp(ngrd),nnyp(ngrd),npatch) )

!           Get ustar
   ierr = RAMS_getvar('USTAR',idim_type,ngrd,pv1,b,flnm)
        
!           Get net roughness
   ierr = RAMS_getvar('NET_Z0',idim_type,ngrd   &
        ,pv2,b,flnm)
        
!           Get patch canopy temperature
   ierr = RAMS_getvar('CAN_TEMP',idim_type,ngrd   &
        ,pv3,b,flnm)
        
!           Get % coverage
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,pv4,b,flnm)

!           Get tstar
   ierr = RAMS_getvar('TSTAR',idim_type,ngrd,pv5,b,flnm)

   call RAMS_reduced_temp(nnxp(ngrd),nnyp(ngrd)  &
      ,nnzp(ngrd),npatch  &
      ,a,c,pv1,pv5,2.,ztn(2,ngrd)  &
      ,pv2,pv4,pv3,d,e  &
      ,zmn(nnzp(1)-1,1))

   !print*,'===========done'
  
   deallocate (pv1,pv2,pv3,pv4,pv5)

   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,1,a,c)
   call RAMS_comp_tempF(n1,n2,1,a)

   cdname='temp - 2m AGL;'
   cdunits='F;'
   !print*,'===========out'

elseif(cvar(1:lv).eq.'speed10m') then

   ivar_type=2
   ierr=RAMS_getvar('UP',idim_type,ngrd  &
         ,c,b,flnm)
   ierr=RAMS_getvar('VP',idim_type,ngrd  &
         ,d,b,flnm)
   call RAMS_comp_speed(n1,n2,n3,c,d)
   ierr=RAMS_getvar('THETA',idim_type,ngrd  &
         ,d,b,flnm)
   ierr=RAMS_getvar('PI',idim_type,ngrd  &
         ,f,b,flnm)

!        Get topo
   ierr= RAMS_getvar('TOPT',idim_type,ngrd  &
         ,e,b,flnm)


   allocate (pv1(nnxp(ngrd),nnyp(ngrd),npatch) )
   allocate (pv2(nnxp(ngrd),nnyp(ngrd),npatch) )
   allocate (pv3(nnxp(ngrd),nnyp(ngrd),npatch) )
   allocate (pv4(nnxp(ngrd),nnyp(ngrd),npatch) )

!           Get ustar
   ierr = RAMS_getvar('USTAR',idim_type,ngrd,pv1,b,flnm)
        
!           Get net roughness
   ierr = RAMS_getvar('NET_Z0',idim_type,ngrd   &
        ,pv2,b,flnm)
        
!           Get patch canopy temperature
   ierr = RAMS_getvar('CAN_TEMP',idim_type,ngrd   &
        ,pv3,b,flnm)
        
!           Get % coverage
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,pv4,b,flnm)

   call RAMS_reduced_wind(nnxp(ngrd),nnyp(ngrd)  &
      ,nnzp(ngrd),npatch  &
      ,a,c,pv1,10.,ztn(2,ngrd)  &
      ,pv2,pv4,pv3,d,f,e  &
      ,zmn(nnzp(1)-1,1))

   deallocate (pv1,pv2,pv3,pv4)


   cdname='speed - 10m AGL;'
   cdunits='m/s;'

elseif(cvar(1:lv).eq.'direction') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_dir(n1,n2,n3,a,c,ngrd)
   cdname='direction;'
   cdunits='deg;'

elseif(cvar(1:lv).eq.'relvortx') then
   ivar_type=3
   ierr= RAMS_getvar('VP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('WP',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_relvortx(n1,n2,n3,a,c,b,d,ngrd)
   cdname='x-vorticity;'
   cdunits='rad/s;'

elseif(cvar(1:lv).eq.'relvorty') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('WP',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_relvorty(n1,n2,n3,a,c,b,d,ngrd)
   cdname='y-vorticity;'
   cdunits='rad/s;'

elseif(cvar(1:lv).eq.'relvortz') then
   ivar_type=3 
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_relvortz(n1,n2,n3,a,c,b,d,ngrd)
   cdname='relative z-vorticity;'
   cdunits='rad/s;'

elseif(cvar(1:lv).eq.'absvortz') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_totvortz(n1,n2,n3,a,c,b,d,ngrd)
   cdname='absolute z-vorticity;'
   cdunits='rad/s;'

elseif(cvar(1:lv).eq.'potvortz') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_totvortz(n1,n2,n3,a,c,b,d,ngrd)
   call RAMS_comp_dn0(n1,n2,n3,e,b,c,d,ngrd)

   ierr= RAMS_getvar('THETA',idim_type,ngrd,b,e,flnm)
   call RAMS_comp_potvortz(n1,n2,n3,a,b,c,e,d,ngrd)
   cdname='potential z-vorticity;'
   cdunits='rad/s;'

elseif(cvar(1:lv).eq.'horiz_div') then
   ivar_type=3
   ierr= RAMS_getvar('WP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_horizdiv(n1,n2,n3,a)
   cdname='horizontal divergence;'
   cdunits='/s;'

! 3D THERMODYNAMIC PROPERTIES OF AIR

elseif(cvar(1:lv).eq.'pi') then
   ivar_type=3
   ierr= RAMS_getvar('PI',idim_type,ngrd,a,b,flnm)
   cdname='Exner function;'
   cdunits='J/(kg K);'

elseif(cvar(1:lv).eq.'press') then
   ivar_type=3
   ierr= RAMS_getvar('PI',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_press(n1,n2,n3,a)
   cdname='pressure;'
   cdunits='mb;'

elseif(cvar(1:lv).eq.'theta') then
   ivar_type=3
   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)
   cdname='potential temp;'
   cdunits='K;'

elseif(cvar(1:lv).eq.'dn0') then
   ivar_type=3
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   print *,'Got TOPT...'
   call RAMS_comp_dn0(n1,n2,n3,c,b,a,e,ngrd)
   cdname='ref density;'

   cdunits='kg/m3;'

elseif(cvar(1:lv).eq.'pi0') then
   ivar_type=3
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,a,b,c,e,ngrd)
   cdname='ref Exner func;'
   cdunits='J/(kg K);'

elseif(cvar(1:lv).eq.'th0') then
   ivar_type=3
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,a,c,e,ngrd)
   cdname='reference virtual potential temp;'
   cdunits='K;'

elseif(cvar(1:lv).eq.'pert_pressure') then
   ivar_type=3
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,c,a,b,e,ngrd)
   ierr= RAMS_getvar('PI',idim_type,ngrd,a,b,flnm)
   if (ierr.eq.0) call RAMS_comp_ppress(n1,n2,n3,a,c)
   cdname='pert pressure;'
   cdunits='mb;'

elseif(cvar(1:lv).eq.'tempk') then
   ivar_type=3
   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,a,c)
   cdname='temperature;'
   cdunits='K;'

elseif(cvar(1:lv).eq.'tempc') then
   ivar_type=3
   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,a,c)
   call RAMS_comp_tempC(n1,n2,n3,a)
   cdname='temperature;'
   cdunits='C;'

elseif(cvar(1:lv).eq.'tempf') then
   ivar_type=3
   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,a,c)
   call RAMS_comp_tempF(n1,n2,n3,a)
   cdname='temperature;'
   cdunits='F;'

elseif(cvar(1:lv).eq.'theta_e') then
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)

   call RAMS_comp_thete(n1,n2,n3,a,c,d)
   cdname='equiv pot temp;'
   cdunits='K;'

elseif(cvar(1:lv).eq.'theta_v') then
   ivar_type=3
   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('RV',idim_type,ngrd,c,b,flnm)

   call RAMS_comp_thetv(n1,n2,n3,a,c)
   cdname='virtual pot temp;'
   cdunits='K;'

! 3D MOISTURE MASS MIXING RATIOS AND HUMIDITY

elseif(cvar(1:lv).eq.'vapor') then
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='vapor mix ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'cloud') then
   ivar_type=3
   ierr= RAMS_getvar('RCP',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='cloud mix ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'rain') then
   ivar_type=3
   ierr= RAMS_getvar('RRP',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='rain mix ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'pristine') then
   ivar_type=3
   ierr= RAMS_getvar('RPP',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='pristine mix ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'snow') then
   ivar_type=3
   ierr= RAMS_getvar('RSP',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='snow mix ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'aggregates') then
   ivar_type=3
   ierr= RAMS_getvar('RAP',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='aggregate mix ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'graupel') then
   ivar_type=3
   ierr= RAMS_getvar('RGP',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='graupel mix ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'hail') then
   ivar_type=3
   ierr= RAMS_getvar('RHP',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='hail mix ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'liquid') then
   ivar_type=3
   call RAMS_comp_zero(n1,n2,n3,a)
   ierr= RAMS_getvar('RCP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RRP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)

   ierr= RAMS_getvar('RGP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) then
      ierr= RAMS_getvar('Q6',idim_type,ngrd,d,b,flnm)
      if(ierr.eq.0) then
         call RAMS_comp_fracliq(n1,n2,n3,d)
         call RAMS_comp_mult(n1,n2,n3,c,d)
      endif
      call RAMS_comp_accum(n1,n2,n3,a,c)
   endif

   ierr= RAMS_getvar('RHP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) then
      ierr= RAMS_getvar('Q7',idim_type,ngrd,d,b,flnm)
      if(ierr.eq.0) then
         call RAMS_comp_fracliq(n1,n2,n3,d)
         call RAMS_comp_mult(n1,n2,n3,c,d)
      endif
      call RAMS_comp_accum(n1,n2,n3,a,c)
    endif

   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='liquid mix ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'ice') then
   ivar_type=3
   call RAMS_comp_zero(n1,n2,n3,a)
   ierr= RAMS_getvar('RPP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RSP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RAP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)

   ierr= RAMS_getvar('RGP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) then
      ierr= RAMS_getvar('Q6',idim_type,ngrd,d,b,flnm)
      if(ierr.eq.0) then
         call RAMS_comp_fracice(n1,n2,n3,d)
         call RAMS_comp_mult(n1,n2,n3,c,d)
      endif
      call RAMS_comp_accum(n1,n2,n3,a,c)
   endif

   ierr= RAMS_getvar('RHP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) then
      ierr= RAMS_getvar('Q7',idim_type,ngrd,d,b,flnm)
      if(ierr.eq.0) then
         call RAMS_comp_fracliq(n1,n2,n3,d)
         call RAMS_comp_mult(n1,n2,n3,c,d)
      endif
      call RAMS_comp_accum(n1,n2,n3,a,c)
   endif

   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='ice mix ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'total_cond') then
   ivar_type=3
   call RAMS_comp_zero(n1,n2,n3,a)
   ierr= RAMS_getvar('RCP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RRP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RPP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RSP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RAP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RGP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RHP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)

   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='cloud mix ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'r_total') then
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('RCP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RRP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RPP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RSP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RAP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RGP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RHP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)

   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='total mix ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'rtotal_orig') then
   ivar_type=3
   ierr= RAMS_getvar('RTP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='orig rtotal;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'dewptk') then
   ivar_type=3
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)

   call RAMS_comp_dewK(n1,n2,n3,a,c,d)
   call RAMS_comp_tempK(n1,n2,n3,a,c)
   cdname='dewpoint temp;'
   cdunits='K;'

elseif(cvar(1:lv).eq.'dewptf') then
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)

   call RAMS_comp_dewK(n1,n2,n3,a,c,d)
   call RAMS_comp_tempF(n1,n2,n3,a)
   cdname='dewpoint temp;'
   cdunits='F;'

elseif(cvar(1:lv).eq.'dewptc') then
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)

   call RAMS_comp_dewK(n1,n2,n3,a,c,d)
   call RAMS_comp_tempC(n1,n2,n3,a)
   cdname='dewpoint temp;'
   cdunits='C;'

elseif(cvar(1:lv).eq.'relhum') then
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)

   call RAMS_comp_rh(n1,n2,n3,a,c,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='relative humidity;'
   cdunits='pct;'

elseif(cvar(1:lv).eq.'clear_frac') then
   ivar_type=2
   ierr= RAMS_getvar('RV',idim_type,ngrd,b,a,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,a,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,a,flnm)

   call RAMS_comp_rh(n1,n2,n3,b,c,d)
   call RAMS_comp_noneg(n1,n2,n3,b)
   
   call cldfraction(n1,n2,n3,a,c,b)
   
   cdname='clear sky;'
   cdunits='frac;'

! 3D HYDROMETEOR, CCN, CN, Dep N, AND NONHYGROSCOPIC AEROSOL NUMBER CONCEN

elseif(cvar(1:lv).eq.'cloud_concen_mg') then
   ivar_type=3
! variable 18 is iccp
   ierr= RAMS_getvar('CCP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='cloud concen;'
   cdunits='#/mg;'

elseif(cvar(1:lv).eq.'rain_concen_kg') then
   ivar_type=3
   ierr= RAMS_getvar('CRP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='rain concen;'
   cdunits='#/kg;'

elseif(cvar(1:lv).eq.'pris_concen_kg') then
   ivar_type=3
   ierr= RAMS_getvar('CPP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='pristine concen;'
   cdunits='#/kg;'

elseif(cvar(1:lv).eq.'snow_concen_kg') then
   ivar_type=3
   ierr= RAMS_getvar('CSP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='snow concen;'
   cdunits='#/kg;'

elseif(cvar(1:lv).eq.'agg_concen_kg') then
   ivar_type=3
   ierr= RAMS_getvar('CAP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='aggregate concen;'
   cdunits='#/kg;'

elseif(cvar(1:lv).eq.'graup_concen_kg') then
   ivar_type=3
   ierr= RAMS_getvar('CGP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='graupel concen;'
   cdunits='#/kg;'

elseif(cvar(1:lv).eq.'hail_concen_kg') then
   ivar_type=3
   ierr= RAMS_getvar('CHP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='hail concen;'
   cdunits='#/kg;'

elseif(cvar(1:lv).eq.'cloud_concen_cm3') then
   ivar_type=3
   ierr= RAMS_getvar('CCP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='cloud concen;'
   cdunits='#/cm3;'

elseif(cvar(1:lv).eq.'rain_concen_m3') then
   ivar_type=3
   ierr= RAMS_getvar('CRP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='rain concen;'
   cdunits='#/m3;'

elseif(cvar(1:lv).eq.'pris_concen_m3') then
   ivar_type=3
   ierr= RAMS_getvar('CPP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='pristine concen;'
   cdunits='#/m3;'

elseif(cvar(1:lv).eq.'snow_concen_m3') then
   ivar_type=3
   ierr= RAMS_getvar('CSP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='snow concen;'
   cdunits='#/m3;'

elseif(cvar(1:lv).eq.'agg_concen_m3') then
   ivar_type=3
   ierr= RAMS_getvar('CAP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='aggregates concen;'
   cdunits='#/m3;'

elseif(cvar(1:lv).eq.'graup_concen_m3') then
   ivar_type=3
   ierr= RAMS_getvar('CGP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='graupel concen;'
   cdunits='#/m3;'

elseif(cvar(1:lv).eq.'hail_concen_m3') then
   ivar_type=3
   ierr= RAMS_getvar('CHP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='hail concen;'
   cdunits='#/m3;'

elseif(cvar(1:lv).eq.'ccn_concen') then
   ivar_type=3
   ierr= RAMS_getvar('CCCNP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)
   cdname='ccn1 concen;'
   cdunits='#/mg;'

elseif(cvar(1:lv).eq.'ifn_conc') then
   ivar_type=3
   ierr= RAMS_getvar('CIFNP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='CN mix ratio;'
   cdunits='#/kg;'

! 3D HYDROMETEOR DIAMETERS

elseif(cvar(1:lv).eq.'cloud_diam') then
   ivar_type=3
   ierr= RAMS_getvar('RCP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CCP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_hydrodiam(n1,n2,n3,a,c,cfmas(1),pwmas(1))
   call RAMS_comp_mults(n1,n2,n3,a,1.e6)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='cloud diam;'
   cdunits='microns;'

elseif(cvar(1:lv).eq.'rain_diam') then
   ivar_type=3
   ierr= RAMS_getvar('RRP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CRP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_hydrodiam(n1,n2,n3,a,c,cfmas(2),pwmas(2))
   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='rain diam;'
   cdunits='mm;'

elseif(cvar(1:lv).eq.'pris_diam') then
   ivar_type=3
   ierr= RAMS_getvar('RPP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CPP',idim_type,ngrd,c,b,flnm)
! more general case: write habit to anal file for cfmas & pwmas index
   call RAMS_comp_hydrodiam(n1,n2,n3,a,c,cfmas(3),pwmas(3))
   call RAMS_comp_mults(n1,n2,n3,a,1.e6)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='pristine diam;'
   cdunits='microns;'

elseif(cvar(1:lv).eq.'snow_diam') then
   ivar_type=3
   ierr= RAMS_getvar('RSP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CSP',idim_type,ngrd,c,b,flnm)
! more general case: write habit to anal file for cfmas & pwmas index
   call RAMS_comp_hydrodiam(n1,n2,n3,a,c,cfmas(4),pwmas(4))
   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='snow diam;'
   cdunits='mm;'

elseif(cvar(1:lv).eq.'agg_diam') then
   ivar_type=3
   ierr= RAMS_getvar('RAP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CAP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_hydrodiam(n1,n2,n3,a,c,cfmas(5),pwmas(5))
   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='aggregates diam;'
   cdunits='mm;'

elseif(cvar(1:lv).eq.'graup_diam') then
   ivar_type=3
   ierr= RAMS_getvar('RGP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CGP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_hydrodiam(n1,n2,n3,a,c,cfmas(6),pwmas(6))
   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='graupel diam;'
   cdunits='mm;'

elseif(cvar(1:lv).eq.'hail_diam') then
   ivar_type=3
   ierr= RAMS_getvar('RHP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CHP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_hydrodiam(n1,n2,n3,a,c,cfmas(7),pwmas(7))
   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='hail diam;'
   cdunits='mm;'

! 3D HYDROMETEOR TEMPERATURE, THERMAL ENERGY, LIQUID WATER FRACTION

elseif(cvar(1:lv).eq.'q2') then
   ivar_type=3
   ierr= RAMS_getvar('Q2',idim_type,ngrd,a,b,flnm)
   cdname='q2;'
   cdunits='J/kg;'

elseif(cvar(1:lv).eq.'q6') then
   ivar_type=3
   ierr= RAMS_getvar('Q6',idim_type,ngrd,a,b,flnm)
   cdname='q6;'
   cdunits='J/kg;'

elseif(cvar(1:lv).eq.'q7') then
   ivar_type=3
   ierr= RAMS_getvar('Q7',idim_type,ngrd,a,b,flnm)
   cdname='q7;'
   cdunits='J/kg;'

elseif(cvar(1:lv).eq.'rain_temp') then
   ivar_type=3
   ierr= RAMS_getvar('Q2',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_raintemp(n1,n2,n3,a)
   cdname='rain temperature;'
   cdunits='K;'

elseif(cvar(1:lv).eq.'graup_temp') then
   ivar_type=3
   ierr= RAMS_getvar('Q6',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_qtcpcp(n1,n2,n3,a)
   cdname='graupel temperature;'
   cdunits='C;'

elseif(cvar(1:lv).eq.'hail_temp') then
   ivar_type=3
   ierr= RAMS_getvar('Q7',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_qtcpcp(n1,n2,n3,a)
   cdname='hail temperature;'
   cdunits='C;'

elseif(cvar(1:lv).eq.'rain_air_tempdif') then
   ivar_type=3
   ierr= RAMS_getvar('Q2',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_raintemp(n1,n2,n3,a)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,d,c)
   call RAMS_comp_tempC(n1,n2,n3,d)
   call RAMS_comp_subt(n1,n2,n3,a,d)
   cdname='rain-air temp;'
   cdunits='K;'

elseif(cvar(1:lv).eq.'graup_air_tempdf') then
   ivar_type=3
   ierr= RAMS_getvar('Q6',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_qtcpcp(n1,n2,n3,a)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,d,c)
   call RAMS_comp_tempC(n1,n2,n3,d)
   call RAMS_comp_subt(n1,n2,n3,a,d)
   cdname='graupel-air temp;'
   cdunits='K;'

elseif(cvar(1:lv).eq.'hail_air_tempdif') then
   ivar_type=3
   ierr= RAMS_getvar('Q7',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_qtcpcp(n1,n2,n3,a)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,d,c)
   call RAMS_comp_tempC(n1,n2,n3,d)
   call RAMS_comp_subt(n1,n2,n3,a,d)
   cdname='hail-air temp;'
   cdunits='K;'

elseif(cvar(1:lv).eq.'graup_fracliq') then
   ivar_type=3
   ierr= RAMS_getvar('Q6',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_fracliq(n1,n2,n3,a)
   cdname='graupel liq frac;'
   cdunits=' ;'

elseif(cvar(1:lv).eq.'hail_fracliq') then
   ivar_type=3
   ierr= RAMS_getvar('Q7',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_fracliq(n1,n2,n3,a)
   cdname='hail liq frac;'
   cdunits=' ;'

! JCL(3/14/03) new variables for RAMS<=>STILT
!              code taken from Saulo's 'ramspost4.3_b.f90'
elseif(cvar(1:lv).eq.'afxu') then
   ivar_type=3
   ierr= RAMS_getvar('AFXU',idim_type,ngrd,a,b,flnm)
   cdname='adv u flux'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'afxub') then
   ivar_type=3
   ierr= RAMS_getvar('AFXUB',idim_type,ngrd,a,b,flnm)
   cdname='averaged adv u flux'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'afxv') then
   ivar_type=3
   ierr= RAMS_getvar('AFXV',idim_type,ngrd,a,b,flnm)
   cdname='adv v flux'
   cdunits='kg/m^2s'
   
elseif(cvar(1:lv).eq.'afxvb') then
   ivar_type=3
   ierr= RAMS_getvar('AFXVB',idim_type,ngrd,a,b,flnm)
   cdname='averaged adv v flux'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'afxw') then
   ivar_type=3
   ierr= RAMS_getvar('AFXW',idim_type,ngrd,a,b,flnm)
   cdname='adv w flux'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'afxwb') then
   ivar_type=3
   ierr= RAMS_getvar('AFXWB',idim_type,ngrd,a,b,flnm)
   cdname='averaged adv W flux'
   cdunits='kg/m^2s'

!elseif(cvar(1:lv).eq.'cfxup1') then
!   ivar_type=3
!   ierr= RAMS_getvar('CFXUP1',idim_type,ngrd,a,b,flnm)
!   cdname='conv up flux deep'
!   cdunits='kg/m^2s'

!elseif(cvar(1:lv).eq.'cfxup2') then
!   ivar_type=3
!   ierr= RAMS_getvar('CFXUP2',idim_type,ngrd,a,b,flnm)
!   cdname='conv up flux shallow'
!   cdunits='kg/m^2s'

!elseif(cvar(1:lv).eq.'cfxdn1') then
!   ivar_type=3
!   ierr= RAMS_getvar('CFXDN1',idim_type,ngrd,a,b,flnm)
!   cdname='conv down flux deep'
!   cdunits='kg/m^2s'

!elseif(cvar(1:lv).eq.'dfxup1') then
!   ivar_type=3
!   ierr= RAMS_getvar('DFXUP1',idim_type,ngrd,a,b,flnm)
!   cdname='deep conv flx up->env '
!   cdunits='kg/m^2s'

!elseif(cvar(1:lv).eq.'efxup1') then
!   ivar_type=3
!   ierr= RAMS_getvar('EFXUP1',idim_type,ngrd,a,b,flnm)
!   cdname='deep conv flx env->up '
!   cdunits='kg/m^2s'

!elseif(cvar(1:lv).eq.'dfxdn1') then
!   ivar_type=3
!   ierr= RAMS_getvar('DFXDN1',idim_type,ngrd,a,b,flnm)
!   cdname='deep conv flx down->env '
!   cdunits='kg/m^2s'

!elseif(cvar(1:lv).eq.'efxdn1') then
!   ivar_type=3
!   ierr= RAMS_getvar('EFXDN1',idim_type,ngrd,a,b,flnm)
!   cdname='deep conv flx env->down'
!   cdunits='kg/m^2s'

!elseif(cvar(1:lv).eq.'dfxup2') then
!   ivar_type=3
!   ierr= RAMS_getvar('DFXUP2',idim_type,ngrd,a,b,flnm)
!   cdname='shallow conv flx up->env '
!   cdunits='kg/m^2s'

!elseif(cvar(1:lv).eq.'efxup2') then
!   ivar_type=3
!   ierr= RAMS_getvar('EFXUP2',idim_type,ngrd,a,b,flnm)
!   cdname='shallow conv flx env -> up'
!   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'cfxup1') then
   ivar_type=3
   ierr= RAMS_getvar('CFXUP',idim_type,ngrd,o,b,flnm)
   call RAMS_comp_onecloud(n1,n2,n3,1,a,o)
   cdname='conv up flux deep'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'cfxup2') then
   ivar_type=3
   ierr= RAMS_getvar('CFXUP',idim_type,ngrd,o,b,flnm)
   call RAMS_comp_onecloud(n1,n2,n3,2,a,o)
   cdname='conv up flux shallow'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'cfxdn1') then
   ivar_type=3
   ierr= RAMS_getvar('CFXDN',idim_type,ngrd,o,b,flnm)
   call RAMS_comp_onecloud(n1,n2,n3,1,a,o)
   cdname='conv down flux deep'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'dfxup1') then
   ivar_type=3
   ierr= RAMS_getvar('DFXUP',idim_type,ngrd,o,b,flnm)
   call RAMS_comp_onecloud(n1,n2,n3,1,a,o)
   cdname='deep conv flx up->env '
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'efxup1') then
   ivar_type=3
   ierr= RAMS_getvar('EFXUP',idim_type,ngrd,o,b,flnm)
   call RAMS_comp_onecloud(n1,n2,n3,1,a,o)
   cdname='deep conv flx env->up '
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'dfxdn1') then
   ivar_type=3
   ierr= RAMS_getvar('DFXDN',idim_type,ngrd,o,b,flnm)
   call RAMS_comp_onecloud(n1,n2,n3,1,a,o)
   cdname='deep conv flx down->env '
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'efxdn1') then
   ivar_type=3
   ierr= RAMS_getvar('EFXDN',idim_type,ngrd,o,b,flnm)
   call RAMS_comp_onecloud(n1,n2,n3,1,a,o)
   cdname='deep conv flx env->down'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'dfxup2') then
   ivar_type=3
   ierr= RAMS_getvar('DFXUP',idim_type,ngrd,o,b,flnm)
   call RAMS_comp_onecloud(n1,n2,n3,2,a,o)
   cdname='shallow conv flx up->env '
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'efxup2') then
   ivar_type=3
   ierr= RAMS_getvar('EFXUP',idim_type,ngrd,o,b,flnm)
   call RAMS_comp_onecloud(n1,n2,n3,2,a,o)
   cdname='shallow conv flx env -> up'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'sigw') then
   ivar_type=3
   ierr= RAMS_getvar('SIGW',idim_type,ngrd,a,b,flnm)
   cdname=' sigma W'
   cdunits=''

elseif(cvar(1:lv).eq.'sigwb') then
   ivar_type=3
   ierr= RAMS_getvar('SIGWB',idim_type,ngrd,a,b,flnm)
   cdname='averaged sigma W'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'tlb') then
   ivar_type=3
   ierr= RAMS_getvar('TLB',idim_type,ngrd,a,b,flnm)
   cdname='averaged Lagr timescale'
   cdunits='s'

elseif(cvar(1:lv).eq.'tl') then
   ivar_type=3
   ierr= RAMS_getvar('TL',idim_type,ngrd,a,b,flnm)
   cdname='Lagr timescale'
   cdunits='s'


! 3D MISCELLANEOUS FIELDS

elseif(cvar(1:lv).eq.'geo') then
   ivar_type=3
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_z(n1,n2,n3,a,c,ngrd)
   cdname='geopotential height;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'tke') then
   ivar_type=3
   ierr= RAMS_getvar('TKEP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='turb kinetic energy;'
   cdunits='m2/s2;'

elseif(cvar(1:lv).eq.'pbl_ht') then
   ivar_type=2
   ierr=RAMS_getvar('TKEP',idim_type,ngrd,a,b,flnm)
   ierr=RAMS_getvar('TOPT',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_pbl(n1,n2,n3,a,c,ngrd)
   cdname='PBL height;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'scalar1_mixrat') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP1',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='scalar1 mixing ratio;'
   cdunits='units/kg;'

elseif(cvar(1:lv).eq.'scalar2_mixrat') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP2',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='scalar2 mixing ratio;'
   cdunits='units/kg;'

!     ; etc. for scalars

elseif(cvar(1:lv).eq.'cuparm_thetasrc') then
   ivar_type=3
   ierr= RAMS_getvar('THSRC',idim_type,ngrd,a,b,flnm)
   cdname='conv heat rate;'
   cdunits='K/s;'

elseif(cvar(1:lv).eq.'cuparm_rtsrc') then
   ivar_type=3
   ierr= RAMS_getvar('RTSRC',idim_type,ngrd,a,b,flnm)
   cdname='conv moist rate;'
   cdunits='kg/kg/s;'

elseif(cvar(1:lv).eq.'rad_thetasrc') then
   ivar_type=3
   ierr= RAMS_getvar('FTHRD',idim_type,ngrd,a,b,flnm)
   cdname='rad heat rate;'
   cdunits='K/s;'

elseif(cvar(1:lv).eq.'khh') then
   ivar_type=3
   ierr= RAMS_getvar('HKH',idim_type,ngrd,a,b,flnm)
   cdname='horiz diffusion coeff;'
   cdunits='m2/s;'

elseif(cvar(1:lv).eq.'khv') then
   ivar_type=3
   ierr= RAMS_getvar('VKH',idim_type,ngrd,a,b,flnm)
   cdname='vert diffusion coeff;'
   cdunits='m2/s;'

! 2D SURFACE PRECIPITATION

!      elseif(cvar(1:lv).eq.'accpc') then
!         ivar_type=2
!         ierr= RAMS_getvar('ACCPC',idim_type,ngrd,a,b,flnm)
!         cdname='accum fog precip;'
!         cdunits='kg/m2;'

elseif(cvar(1:lv).eq.'accpr') then
   ivar_type=2
   ierr= RAMS_getvar('ACCPR',idim_type,ngrd,a,b,flnm)
   cdname='accum rain;'
   cdunits='kg/m2;'

elseif(cvar(1:lv).eq.'accpp') then
   ivar_type=2
   ierr= RAMS_getvar('ACCPP',idim_type,ngrd,a,b,flnm)
   cdname='accum pristine;'
   cdunits='kg/m2;'

elseif(cvar(1:lv).eq.'accps') then
   ivar_type=2
   ierr= RAMS_getvar('ACCPS',idim_type,ngrd,a,b,flnm)
   cdname='accum snow;'
   cdunits='kg/m2;'

elseif(cvar(1:lv).eq.'accpa') then
   ivar_type=2
   ierr= RAMS_getvar('ACCPA',idim_type,ngrd,a,b,flnm)
   cdname='accum aggregates;'
   cdunits='kg/m2;'

elseif(cvar(1:lv).eq.'accpg') then
   ivar_type=2
   ierr= RAMS_getvar('ACCPG',idim_type,ngrd,a,b,flnm)
   cdname='accum graupel;'
   cdunits='kg/m2;'

elseif(cvar(1:lv).eq.'accph') then
   ivar_type=2
   ierr= RAMS_getvar('ACCPH',idim_type,ngrd,a,b,flnm)
   cdname='accum hail;'
   cdunits='kg/m2;'

elseif(cvar(1:lv).eq.'totpcp' .or. cvar(1:lv).eq.'totpcp_in' .or.  &
       cvar(1:lv).eq.'precip' .or. cvar(1:lv).eq.'precip_in') then
   ivar_type=2
   call RAMS_comp_zero(n1,n2,1,a)
   ierr= RAMS_getvar('ACCPR',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   ierr= RAMS_getvar('ACCPP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   ierr= RAMS_getvar('ACCPS',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   ierr= RAMS_getvar('ACCPA',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   ierr= RAMS_getvar('ACCPG',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   ierr= RAMS_getvar('ACCPH',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)

   if (cvar(1:lv).eq.'precip'.or.cvar(1:lv).eq.'precip_in') then
      ierr= RAMS_getvar('ACONPR',idim_type,ngrd,c,b,flnm)
      if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
      cdname='total accum precip;'
   else
      cdname='total resolved precip;'
   endif

   if(cvar(1:lv).eq.'totpcp'.or.cvar(1:lv).eq.'precip') then
      cdunits='mm liq;'
   else
      call RAMS_comp_mults(n1,n2,n3,a,.03937)
      cdunits='in liq;'
   endif
   call RAMS_comp_noneg(n1,n2,1,a)

elseif(cvar(1:lv).eq.'pcprr') then
   ivar_type=2
   ierr= RAMS_getvar('PCPRR',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,3600.)
   cdname='rain precip rate;'
   cdunits='mm/hr liq equiv;'

elseif(cvar(1:lv).eq.'pcprp') then
   ivar_type=2
   ierr= RAMS_getvar('PCPRP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,3600.)
   cdname='pristine precip rate;'
   cdunits='mm/hr liq equiv;'

elseif(cvar(1:lv).eq.'psprs') then
   ivar_type=2
   ierr= RAMS_getvar('PCPRS',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,3600.)
   cdname='snow precip rate;'
   cdunits='mm/hr liq equiv;'

elseif(cvar(1:lv).eq.'pcpra') then
   ivar_type=2
   ierr= RAMS_getvar('PCPRA',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,3600.)
   cdname='aggregates precip rate;'
   cdunits='mm/hr liq equiv;'

elseif(cvar(1:lv).eq.'pcprg') then
   ivar_type=2
   ierr= RAMS_getvar('PCPRG',idim_type,ngrd,a,b,flnm)
   cdname='graupel precip rate;'
   cdunits='mm/hr liq equiv;'

elseif(cvar(1:lv).eq.'pcprh') then
   ivar_type=2
   ierr= RAMS_getvar('PCPRH',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,3600.)
   cdname='hail precip rate;'
   cdunits='mm/hr liq equiv;'

elseif(cvar(1:lv).eq.'pcpg') then
   ivar_type=2
   ierr= RAMS_getvar('PCPG',idim_type,ngrd,a,b,flnm)
   cdname='pcpg;'
   cdunits='kg/m2;'

elseif(cvar(1:lv).eq.'qpcpg') then
   ivar_type=2
   ierr= RAMS_getvar('QPCPG',idim_type,ngrd,a,b,flnm)
   cdname='qpcpg;'
   cdunits='J/m2;'

elseif(cvar(1:lv).eq.'dpcpg') then
   ivar_type=2
   ierr= RAMS_getvar('DPCPG',idim_type,ngrd,a,b,flnm)
   cdname='dpdpg;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'pcprate'.or.cvar(1:lv).eq.'pcprate_in'.or.  &
       cvar(1:lv).eq.'precipr'.or.cvar(1:lv).eq.'precipr_in') then
   ivar_type=2
   call RAMS_comp_zero(n1,n2,1,a)
   ierr= RAMS_getvar('PCPRR',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   ierr= RAMS_getvar('PCPRP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   ierr= RAMS_getvar('PCPRS',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   ierr= RAMS_getvar('PCPRA',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   ierr= RAMS_getvar('PCPRG',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   ierr= RAMS_getvar('PCPRH',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   call RAMS_comp_noneg(n1,n2,1,a)

   if (cvar(1:lv).eq.'precipr'.or.cvar(1:lv).eq.'precipr_in') then
      ierr= RAMS_getvar('CONPRR',idim_type,ngrd,c,b,flnm)
      if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
      cdname='total precip rate;'
   else
      cdname='resolved precip rate;'
   endif

   if(cvar(1:lv).eq.'pcprate'.or.cvar(1:lv).eq.'precipr') then
      call RAMS_comp_mults(n1,n2,1,a,3600.)
      cdunits='mm/hr;'
   elseif(cvar(1:lv).eq.'pcprate_in'.or.cvar(1:lv).eq.'precipr_in') then
      call RAMS_comp_mults(n1,n2,1,a,141.732)
      cdunits='in/hr;'
   endif

elseif(cvar(1:lv).eq.'conpcp') then
   ivar_type=2
   ierr= RAMS_getvar('CONPRR',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,3600.)
   call RAMS_comp_noneg(n1,n2,1,a)
   cdname='convective pcp rate;'
   cdunits='mm/hr;'

elseif(cvar(1:lv).eq.'acccon') then
   ivar_type=2
   ierr= RAMS_getvar('ACONPR',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,1,a)
   cdname='accum convective pcp;'
   cdunits='mm;'

! Vertically-integrated atmospheric moisture

elseif(cvar(1:lv).eq.'vertint_rt' .or. cvar(1:lv).eq.'vertint_cond') then
   ivar_type=2

   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,c,b,d,e,ngrd)

   if (cvar(1:lv).eq.'vertint_rt') then
      ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
      cdname='vertint total water;'
   else
      call RAMS_comp_zero(n1,n2,n3,a)
      cdname='vertint condensate;'
   endif

   ierr= RAMS_getvar('RCP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RRP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RPP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RSP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RAP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RGP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RHP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)

   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd)

   cdunits='mm;'


! 2D SURFACE HEAT, MOISTURE, MOMENTUM AND RADIATIVE FLUXES

elseif(cvar(1:lv).eq.'tfz') then
   ivar_type=2
   ierr= RAMS_getvar('TFZ',idim_type,ngrd,a,b,flnm)
   cdname='tfz;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'qfz') then
   ivar_type=2
   ierr= RAMS_getvar('QFZ',idim_type,ngrd,a,b,flnm)
   cdname='qfz;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'uw') then
   ivar_type=2
   ierr= RAMS_getvar('UW',idim_type,ngrd,a,b,flnm)
   cdname='uw;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'vw') then
   ivar_type=2
   ierr= RAMS_getvar('VW',idim_type,ngrd,a,b,flnm)
   cdname='vw;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'wfz') then
   ivar_type=2
   ierr= RAMS_getvar('WFZ',idim_type,ngrd,a,b,flnm)
   cdname='wfz;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'sens_flux') then
   ivar_type=2
   ierr= RAMS_getvar('TFZ',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
   call RAMS_comp_mults(n1,n2,1,a,1004.)
   cdname='sfc sens heat flx;'
   cdunits='W/m2;'

elseif(cvar(1:lv).eq.'lat_flux') then
   ivar_type=2
   ierr= RAMS_getvar('QFZ',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
   call RAMS_comp_mults(n1,n2,1,a,2.5e6)
   cdname='sfc lat heat flx;'
   cdunits='W/m2;'

elseif(cvar(1:lv).eq.'etrans') then
   ivar_type=2
   ierr= RAMS_getvar('QFZ',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
!                 Divide by water density to get depth and 
!                   convert units from m/s to mm/hour (3600./1000.)
   call RAMS_comp_mults(n1,n2,1,a,3.6)
   cdname='evapo-transpiration;'
   cdunits='mm/hour;'

elseif(cvar(1:lv).eq.'etrans_in') then
   ivar_type=2
   ierr= RAMS_getvar('QFZ',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
!                 Divide by water density to get depth and 
!                   convert units from m/s to in/hour (39.37 * 3600./1000.)
   call RAMS_comp_mults(n1,n2,n3,a,141.732)
   cdname='evapo-transpiration;'
   cdunits='in/hour;'

elseif(cvar(1:lv).eq.'umom_flx') then
   ivar_type=2
   ierr= RAMS_getvar('UW',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
   cdname='sfc u-momentum flx;'
   cdunits='Pa;'

elseif(cvar(1:lv).eq.'vmom_flx') then
   ivar_type=2
   ierr= RAMS_getvar('VW',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
   cdname='sfc v-momentum flx;'
   cdunits='Pa;'

elseif(cvar(1:lv).eq.'wmom_flx') then
   ivar_type=2
   ierr= RAMS_getvar('WFZ',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
   cdname='sfc w-momentum flx;'
   cdunits='Pa;'

elseif(cvar(1:lv).eq.'bowen') then
   ivar_type=2
   ierr= RAMS_getvar('TFZ',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('QFZ',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_bowen(n1,n2,1,a,c)
   cdname='bowen ratio;'
   cdunits=' ;'

elseif(cvar(1:lv).eq.'rshort') then
   ivar_type=2
   ierr= RAMS_getvar('RSHORT',idim_type,ngrd,a,b,flnm)
   cdname='rshort;'
   cdunits='W/m2;'

elseif(cvar(1:lv).eq.'rlong') then
   ivar_type=2
   ierr= RAMS_getvar('RLONG',idim_type,ngrd,a,b,flnm)
   cdname='rlong;'
   cdunits='W/m2;'

elseif(cvar(1:lv).eq.'rlongup') then
   ivar_type=2
   ierr= RAMS_getvar('RLONGUP',idim_type,ngrd,a,b,flnm)
   cdname='rlongup;'
   cdunits='W/m2;'

elseif(cvar(1:lv).eq.'albedt') then
   ivar_type=2
   ierr= RAMS_getvar('ALBEDT',idim_type,ngrd,a,b,flnm)
   cdname='albedt;'
   cdunits=' ;'

! 2D TOPOGRAPHY AND GEOGRAPHIC VALUES

elseif(cvar(1:lv).eq.'topo') then
   ivar_type=2
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,a,b,flnm)
   cdname='topo;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'lat') then
   ivar_type=2
   ierr= RAMS_getvar('GLAT',idim_type,ngrd,a,b,flnm)
   cdname='latitude;'
   cdunits='deg;'

elseif(cvar(1:lv).eq.'lon') then
   ivar_type=2
   ierr= RAMS_getvar('GLON',idim_type,ngrd,a,b,flnm)
   cdname='longitude;'
   cdunits='deg;'

! 2D MISCELLANEOUS FIELDS
elseif(cvar(1:lv).eq.'pblhgt') then
   ivar_type=2
   ierr= RAMS_getvar('PBLHGT',idim_type,ngrd,a,b,flnm)
   cdname='PBL Height;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'gpp') then
   ivar_type=2
   ierr= RAMS_getvar('GPP',idim_type,ngrd,a,b,flnm)
   cdname='Gross primary production;'
   cdunits='umolC/m2/s;'

elseif(cvar(1:lv).eq.'plresp') then
   ivar_type=2
   ierr= RAMS_getvar('PLRESP',idim_type,ngrd,a,b,flnm)
   cdname='Plant respiration;'
   cdunits='umol/m2/s;'

elseif(cvar(1:lv).eq.'hetresp') then
   ivar_type=2
   ierr= RAMS_getvar('RESPHET',idim_type,ngrd,a,b,flnm)
   cdname='Heterotrophic respiration;'
   cdunits='umol/m2/s;'

elseif(cvar(1:lv).eq.'sea_press') then
   ivar_type=2
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_z(n1,n2,n3,c,a,ngrd)

   ierr= RAMS_getvar('PI',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)

   call RAMS_comp_slpress(n1,n2,n3,a,d,c,a)
   cdname='sea level pressure;'
   cdunits='mb;'

elseif(cvar(1:lv).eq.'sfc_div') then
   ivar_type=2
   ierr= RAMS_getvar('WP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_sfcdiv(n1,n2,n3,a,ngrd)
   cdname='surface divergence;'
   cdunits='1/s;'

! Special use of sst: acquired for patch #1 even where no water exists

elseif(cvar(1:lv).eq.'sst') then
   ivar_type=2

   ierr = RAMS_getvar('SOIL_Q',idim_type,ngrd   &
        ,c,b,flnm)
   kp = nzg
   call rams_fill_sst(n1,n2,nzg*npatch,kp,a,c)

   call RAMS_comp_tempC(n1,n2,1,a)
   cdname='water temperature;'
   cdunits='C;'


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! LEAF2 variables section

! If want a horiz plot, specify a string like 'tgpatch'; it will
!   return i,j,ip array.
! Specify a new ivar_type, not corresponding to anal file var type.  With
!   horiz plot, get back into iplt.  If have this var type, don't slice.
! Need replacement for rams3to2d because windowing is done in there.
! Replacement would window but not slice.
! Then, if want xz (vert cross section) have name like tgpatch_vert.
! This would return entire 4d array from hvlib.f.
! Then we have to slice and window with yet another replacement to rams3to2d.

! nkk is the record number, where n is the LEAF field number (1, 2, 3, or 4)
! and kk is the k level.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


elseif(cvar(1:lv).eq.'patch_area') then

   ivar_type = 7
   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   cdname='patch fractional area;'
   cdunits=';'

elseif(cvar(1:lv).eq.'soil_z0_p' .or. cvar(1:lv).eq.'soil_z0_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('SOIL_Z0',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'soil_z0_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil roughness;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'veg_class_p' .or. cvar(1:lv).eq.'veg_class_bp') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('VEG_CLASS',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_comp_vegclass(irecsize,1,1,a(irecind))

   if (cvar(1:lv).eq.'veg_class_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_bigpatch(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='vegetation class;'
   cdunits='#;'

elseif(cvar(1:lv).eq.'qveg_class_p' .or. cvar(1:lv).eq.'qveg_class_bp') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('DATQ_CLASS',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_comp_vegclass(irecsize,1,1,a(irecind))

   if (cvar(1:lv).eq.'qveg_class_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_bigpatch(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='q vegetation class;'
   cdunits='#;'

elseif(cvar(1:lv).eq.'veg_fracarea_p' .or. cvar(1:lv).eq.'veg_fracarea_ps')then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('VEG_FRAC',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'veg_fracarea_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='vegetation frac area;'
   cdunits=';'

elseif(cvar(1:lv).eq.'land') then

   ivar_type = 2
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_1minus(nnxp(ngrd),nnyp(ngrd),1,a)
   cdname='land frac area;'
   cdunits=';'

elseif(cvar(1:lv).eq.'veg_lai_p' .or. cvar(1:lv).eq.'veg_lai_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('VEG_LAI',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'veg_lai_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='leaf area index;'
   cdunits=';'

elseif(cvar(1:lv).eq.'net_z0_p' .or. cvar(1:lv).eq.'net_z0_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('NET_Z0',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'net_z0_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='net roughness;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'veg_z0_p' .or. cvar(1:lv).eq.'veg_z0_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('VEG_Z0',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'veg_z0_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='vegetation roughness;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'veg_disp_p' .or. cvar(1:lv).eq.'veg_disp_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('VEG_DISP',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'veg_disp_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='vegetation displacement height;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'patch_wetind') then

   ivar_type = 7
   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('WET_INDEX',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   cdname='patch wetness index;'
   cdunits=';'

elseif(cvar(1:lv).eq.'snowlevels') then

   ivar_type = 7
   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('KSNOW',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   cdname='number of snow levels;'
   cdunits='#;'

elseif(cvar(1:lv).eq.'grnd_mixrat_p' .or. cvar(1:lv).eq.'grnd_mixrat_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('SFC_RS',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_comp_mults(n1,n2,npatch,a(irecind),1.e3)

   if(cvar(1:lv).eq.'grnd_mixrat_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='ground mixing ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'soil_mixrat_p' .or. cvar(1:lv).eq.'soil_mixrat_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('SOIL_RS',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_comp_mults(n1,n2,npatch,a(irecind),1.e3)

   if(cvar(1:lv).eq.'soil_mixrat_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil mixing ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'veg_moist_p' .or. cvar(1:lv).eq.'veg_moist_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('VEG_MOIST',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'veg_moist_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

    cdname='vegetation moisture;'
   cdunits='kg/m2;'

elseif(cvar(1:lv).eq.'canopy_mixrat_p' .or. cvar(1:lv).eq.'canopy_mixrat_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('CAN_RV',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_comp_mults(n1,n2,npatch,a(irecind),1.e3)

   if(cvar(1:lv).eq.'canopy_mixrat_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='canopy mixing ratio;'
   cdunits='g/kg;'

elseif(cvar(1:lv).eq.'veg_temp_p' .or. cvar(1:lv).eq.'veg_temp_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('VEG_TEMP',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_comp_tempC(n1,n2,npatch,a(irecind))

   if(cvar(1:lv).eq.'veg_temp_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='vegetation temperature;'
   cdunits='C;'

elseif(cvar(1:lv).eq.'canopy_temp_p' .or. cvar(1:lv).eq.'canopy_temp_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('CAN_TEMP',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_comp_tempC(n1,n2,npatch,a(irecind))

   if(cvar(1:lv).eq.'canopy_temp_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='canopy temperature;'
   cdunits='C;'

elseif(cvar(1:lv).eq.'ustar_p' .or. cvar(1:lv).eq.'ustar_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('USTAR',idim_type,ngrd,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'ustar_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='ustar;'
   cdunits='m/s;'

elseif(cvar(1:lv).eq.'tstar_p' .or. cvar(1:lv).eq.'tstar_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('TSTAR',idim_type,ngrd,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'tstar_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='tstar;'
   cdunits='K;'

elseif(cvar(1:lv).eq.'rstar_p' .or. cvar(1:lv).eq.'rstar_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('RSTAR',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'rstar_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='rstar;'
   cdunits='kg/kg;'

elseif(cvar(1:lv).eq.'sens_heat_flux_p' .or.  &
       cvar(1:lv).eq.'sens_heat_flux_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('USTAR',idim_type,ngrd,a(irecind),b,flnm)
   ierr = RAMS_getvar('TSTAR',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_mult(n1,n2,npatch,a(irecind),c)

   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_multap(n1,n2,1,npatch,a(irecind),d)
   call RAMS_comp_mults(n1,n2,npatch,a(irecind),-1004.)

   if(cvar(1:lv).eq.'sens_heat_flux_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='sfc sens heat flx;'
   cdunits='W/m2;'

elseif(cvar(1:lv).eq.'lat_heat_flux_p' .or.  &
       cvar(1:lv).eq.'lat_heat_flux_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('USTAR',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   ierr = RAMS_getvar('RSTAR',idim_type,ngrd   &
        ,c,b,flnm)
   call RAMS_comp_mult(n1,n2,npatch,a(irecind),c)

   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_multap(n1,n2,1,npatch,a(irecind),d)
   call RAMS_comp_mults(n1,n2,npatch,a(irecind),-2.5e6)

   if(cvar(1:lv).eq.'lat_heat_flux_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='sfc lat heat flx;'
   cdunits='W/m2;'

elseif(cvar(1:lv).eq.'snow_depth_p' .or. cvar(1:lv).eq.'snow_depth_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('SNOW_DEPTH',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_sum_snowlayers(nnxp(ngrd)*nnyp(ngrd),nzs,npatch,a(irecind))

   if(cvar(1:lv).eq.'snow_depth_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='snow depth;'
   cdunits='m;'

elseif(cvar(1:lv).eq.'snowcover_p' .or. cvar(1:lv).eq.'snowcover_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('SNOW_MOIST',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_sum_snowlayers(nnxp(ngrd)*nnyp(ngrd),nzs,npatch,a(irecind))

   if(cvar(1:lv).eq.'snow_depth_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='snowcover;'
   cdunits='kg/m2;'

elseif(cvar(1:lv).eq.'sltex_p' .or. cvar(1:lv).eq.'sltex_bp') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   irecind = irecind + irecsize
   ierr = RAMS_getvar('SOIL_TEXT',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if (cvar(1:lv).eq.'sltex_p') then
      ivar_type = 8
   else
      ivar_type = 3
      call RAMS_comp_bigpatch(nnxp(ngrd),nnyp(ngrd),nzg,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil textural class;'
   cdunits='#;'

elseif(cvar(1:lv).eq.'soilq_p' .or. cvar(1:lv).eq.'soilq_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   irecind = irecind + irecsize
   ierr = RAMS_getvar('SOIL_Q',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if (cvar(1:lv).eq.'soilq_p') then
      ivar_type = 8
   else
      ivar_type = 3
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),nzg,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil q;'
   cdunits='J/m3;'

elseif(cvar(1:lv).eq.'soil_temp_p' .or. cvar(1:lv).eq.'soil_temp_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   irecind = irecind + irecsize
   ierr = RAMS_getvar('SOIL_Q',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   ierr = RAMS_getvar('SOIL_MOIST',idim_type,ngrd   &
        ,c,b,flnm)
   ierr = RAMS_getvar('SOIL_TEXT',idim_type,ngrd   &
        ,d,b,flnm)
   call RAMS_comp_copysst(n1,n2,nzg,a(irecind))

   irecsizep = nnxp(ngrd) * nnyp(ngrd) * nzg
   call RAMS_comp_qwtc(n1,n2,nzg*(npatch-1),a(irecind+irecsizep)  &
      ,c(1+irecsizep),d(1+irecsizep))


   if (cvar(1:lv).eq.'soil_temp_p') then
      ivar_type = 8
   else
      ivar_type = 3
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),nzg,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil/sea temp;'
   cdunits='C;'

elseif(cvar(1:lv).eq.'5050_temp_ps' .or. cvar(1:lv).eq.'5050_tempf_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('CAN_TEMP',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   ivar_type = 2
   call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
      ,a(irecind),a(1),b)

   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,d,c)
   call RAMS_comp_5050(n1,n2,n3,a,d)

   if(cvar(1:lv).eq.'5050_temp_ps') then
      call RAMS_comp_tempC(n1,n2,n3,a)
      cdname='5050 tempC;'
      cdunits='C;'
   else
      call RAMS_comp_tempF(n1,n2,n3,a)
      cdname='5050 tempF;'
      cdunits='F;'
   endif


elseif(cvar(1:lv).eq.'soil_moist_p' .or. cvar(1:lv).eq.'soil_moist_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   irecind = irecind + irecsize
   ierr = RAMS_getvar('SOIL_MOIST',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if (cvar(1:lv).eq.'soil_moist_p') then
      ivar_type = 8
   else
      ivar_type = 3
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),nzg,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil moisture;'
   cdunits='m3/m3;'

elseif(cvar(1:lv).eq.'soil_moistf_p' .or. cvar(1:lv).eq.'soil_moistf_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   irecind = irecind + irecsize
   ierr = RAMS_getvar('SOIL_MOIST',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   ierr = RAMS_getvar('SOIL_TEXT',idim_type,ngrd   &
        ,c,b,flnm)
   call rams_comp_slmstf(irecsize,1,1,a(irecind),c)

   if (cvar(1:lv).eq.'soil_moistf_p') then
      ivar_type = 8
   else
      ivar_type = 3
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),nzg,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil moisture frac;'
   cdunits='m3/m3;'

elseif(cvar(1:lv).eq.'leaf2_moisture') then

! These values should somehow be scaled across soil, snow, vegetation, and canopy air
! using calls to rams_comp_snownorm, rams_comp_vegnorm, and rams_comp_cannorm,
! which are not yet completed.

   ivar_type = 10

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   irecsize = nnxp(ngrd) * nnyp(ngrd) * nzg * npatch
   ierr = RAMS_getvar('SOIL_MOIST',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   ierr = RAMS_getvar('SOIL_TEXT',idim_type,ngrd   &
        ,c,b,flnm)
   call rams_comp_slmstf(irecsize,1,1,a(irecind),c)

   irecind = irecind + irecsize
   irecsize = nnxp(ngrd) * nnyp(ngrd) * nzs * npatch
   ierr = RAMS_getvar('SNOW_MOIST',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call rams_comp_snownorm(irecsize,1,1,a(irecind),c)

   irecind = irecind + irecsize
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('VEG_MOIST',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call rams_comp_vegnorm(irecsize,1,1,a(irecind),c)

   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   irecind = irecind + irecsize
   ierr = RAMS_getvar('CAN_RV',idim_type,ngrd   &
        ,c,b,flnm)
   call rams_comp_cannorm(irecsize,1,1,a(irecind),c)

   ! also get pcpg, and vapor for k=2

   cdname='leaf2 moisture frac;'
   cdunits='m3/m3;'

elseif(cvar(1:lv).eq.'leaf2_temp') then

   ! similar to leaf2_moisture


elseif(cvar(1:lv).eq.'ctprof') then
   ivar_type=2

!   Determine RH so need species AND 0.99 saturated for cloud.
!   ierr=RAMS_getvar(20,ind,nind,3,ngrd,a,b,flnm)
!   ierr=RAMS_getvar(6,ind,nind,3,ngrd,c,b,flnm)
!   ierr=RAMS_getvar(19,ind,nind,3,ngrd,d,b,flnm)
!   call RAMS_comp_rh(n1,n2,n3,a,c,d)
!   call RAMS_comp_noneg(n1,n2,n3,a)
!   call ae1(n1*n2*n3,d,a)

!        Now accumulate the uphysics species.
!   call RAMS_comp_zero(n1,n2,n3,a)
!   call RAMS_comp_zero(n1,n2,n3,c)
!   ierr=RAMS_getvar(61,ind,nind,3,ngrd,a,b,flnm)
!   ierr=RAMS_getvar(9,ind,nind,3,ngrd,c,b,flnm)
!   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
!   ierr=RAMS_getvar(10,ind,nind,3,ngrd,c,b,flnm)
!   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
!   ierr=RAMS_getvar(11,ind,nind,3,ngrd,c,b,flnm)
!   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
!   ierr=RAMS_getvar(12,ind,nind,3,ngrd,c,b,flnm)
!   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
!   ierr=RAMS_getvar(13,ind,nind,3,ngrd,c,b,flnm)
!   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)

!   call RAMS_comp_ctprof(n1,n2,n3,a,d,ngrd)
!   ierr_getvar=0

!   cdname='cloud top height;'
!   cdunits='m;'



!=====================================================================
! THE HYPACT VARIABLES MUST BE CHANGED BECAUSE RAMS_GETVAR NOW USES
! CHARACTER STRINGS

! HYPACT variables

elseif(cvar(1:lv2).eq.'part_lag') then
   ! this is done in HYP_fill_fld

elseif(cvar(1:lv2).eq.'conc_lag') then
   read(cvar(lv2+2:lv),*) ispec
   write(cspec,'(i3.3)') ispec
   ivar_type=3
   ierr=RAMS_getvar('CONC_LAG_I'//cspec,idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call read_SPEC(ispec,-1,flnm,cdname,cdunits)
   cdname='Lag- '//cdname(1:lastchar(cdname))

elseif(cvar(1:lv2).eq.'conc_lag_avg') then
   read(cvar(lv2+2:lv),*) ispec
   write(cspec,'(i3.3)') ispec
   ivar_type=3
   ierr=RAMS_getvar('CONC_LAG_A'//cspec,idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call read_SPEC(ispec,-1,flnm,cdname,cdunits)
   cdname='Avg Lag- '//cdname(1:lastchar(cdname))

elseif(cvar(1:lv2).eq.'conc_eul') then
   read(cvar(lv2+2:lv),*) ispec
   write(cspec,'(i3.3)') ispec
   ivar_type=3
   ierr=RAMS_getvar('CONC_EUL_I'//cspec,idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call read_SPEC(ispec,-1,flnm,cdname,cdunits)
   cdname='Eul- '//cdname(1:lastchar(cdname))

elseif(cvar(1:lv2).eq.'conc_eul_avg') then
   read(cvar(lv2+2:lv),*) ispec
   write(cspec,'(i3.3)') ispec
   ivar_type=3
   ierr=RAMS_getvar('CONC_EUL_A'//cspec,idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call read_SPEC(ispec,-1,flnm,cdname,cdunits)
   cdname='Avg Eul- '//cdname(1:lastchar(cdname))

elseif(cvar(1:lv2).eq.'conc_hyb') then
   read(cvar(lv2+2:lv),*) ispec
   write(cspec,'(i3.3)') ispec
   ivar_type=3
   ierr=RAMS_getvar('CONC_LAG_I'//cspec,idim_type,ngrd,a,b,flnm)
   ierr=RAMS_getvar('CONC_EUL_I'//cspec,idim_type,ngrd,c   &
       ,b,flnm)
   call RAMS_comp_accum(n1,n2,n3,a,c)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call read_SPEC(ispec,-1,flnm,cdname,cdunits)
   cdname='Hyb- '//cdname(1:lastchar(cdname))

elseif(cvar(1:lv2).eq.'conc_hyb_avg') then
   read(cvar(lv2+2:lv),*) ispec
   write(cspec,'(i3.3)') ispec
   ivar_type=3
   ierr=RAMS_getvar('CONC_LAG_A'//cspec,idim_type,ngrd,a,b,flnm)
   ierr=RAMS_getvar('CONC_EUL_A'//cspec,idim_type,ngrd,c   &
       ,b,flnm)
   call RAMS_comp_accum(n1,n2,n3,a,c)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call read_SPEC(ispec,-1,flnm,cdname,cdunits)
   cdname='Avg Hyb- '//cdname(1:lastchar(cdname))

!=====================================================================

else

   print*,'Variable name not found in hvlib.f - ',cvar(1:lv)
   ivar_type=0

endif

if(ierr_getvar.eq.1.or.ifound.eq.0) ivar_type=0

return
end

!*******************************************************************************

! Use this to indicate whether the computed variable exist here - if
! missing the plot will be skipped.

subroutine RAMS_varinfo(num,ival)
implicit none
integer :: num,ival

integer :: ierr_getvar,ifound,ivar_type
common /getvar/ierr_getvar,ifound,ivar_type

if(num.eq.1) then
   ival=ivar_type
endif

return
end

!***************************************************************************

subroutine read_SPEC(ispec,isrc,flnm,cdname,cdunits)

! Routine opens the SPEC file for HYPACT runs to get the variable name and
! units

character*(*) flnm,cdname,cdunits
character sflnm*80,line*256,tokens(20)*32

cdname='none;'
cdunits='none;'

lp=index(flnm(lastslash(flnm):),'-')
sflnm=flnm(1:lp+lastslash(flnm)-1)//'spec.txt'

open(8,file=sflnm,status='old')
read(8,'(a)') line
call parse(line,tokens,ntok)
read(tokens(1),*) iver
read(tokens(2),*) nsources
do nsrc=1,nsources
   read(8,'(a)') line
   call parse(line,tokens,ntok)
   read(tokens(1),*) isrcf
   read(tokens(8),*) ispecf
   if(isrc.eq.isrcf.or.isrc.eq.-1.or.isrc.eq.0) then
      if(isrc.eq.0) then
         cdunits='all sources;'
      elseif(isrc.eq.-1) then
         cdunits=tokens(10)(1:lastchar(tokens(10)))//'/m:S:3:N:;'
      else
         cdunits=tokens(3)(1:lastchar(tokens(3)))//';'
      endif
      if(ispec.eq.ispecf.or.ispec.eq.0) then
         if(ispec.eq.0) then
            cdname='all species;'
         else
            cdname=tokens(9)(2:lastchar(tokens(9))-1)//';'
         endif
         goto 2
      endif
   endif
enddo
2 continue
close(8)

!print*,cdname
!print*,cdunits

return
end

