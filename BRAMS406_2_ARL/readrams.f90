!---------------------------------------------------------------
! University of Houston
! Air Quality Modeling and Monitoring Center
! 218 Old Science Building
! Houston, TX 77204-5048
! USA
!
! email: sbkim@math.uh.edu (Dr. Seung-Bum Kim)
!        dwbyun@math.uh.edu (Dr. Daewon W. Byun)
!----------------------------------------------------------------

subroutine readrams(nx,ny,nz,nzm1,lfirst,ng,flnm)
!----------------------------------------------------------------
! Name:     readrams
! Revised:  06 Aug 2002  Created for RAMS2ARL (S.-B. Kim)
! Note:     this version is F90 for use with RAMS v4.3
!
!---------------------------------------------------------------
!
include 'param.inc' 
include 'fields.inc' 
logical lfirst
!JCL(03/17/04): change length of filenames to allow for long names
!character(len=80) :: flnm
character(len=200) :: flnm
character(len=32) :: cdname,cdunits
integer, parameter :: nind=300

real, allocatable :: au(:),av(:),aw(:),atempk(:),apress(:),      &
     avapor(:),adn0(:),atke(:),api0(:),app(:),        &
     atheta(:),                           &  !/3d
     auw(:),avw(:),arlong(:),arshort(:),           &
     austar(:),atstar(:),arstar(:),             &
!JCL(3/14/03)additional variables
!DMM(1/21/05) added asigwb & atlb
     aafxu(:),aafxv(:),aafxw(:),acfxup1(:),acfxup2(:),acfxdn1(:),&
     adfxup1(:),adfxup2(:),aefxup1(:),aefxup2(:),adfxdn1(:),     &
     aefxdn1(:),atl(:),atlb(:),asigw(:),asigwb(:),        &   
     atopo(:),alat(:),alon(:),aprecip(:),       &        !/2d
     asoil_moist(:), &               !JCL(3/14/03)additional variables
     agpp(:),aplresp(:),ahetresp(:),apblhgt(:)

real, allocatable :: ascratch(:)
integer, parameter :: mnxyz=mnx*mny*mnz
!
!--------------------------------------------------------------------
! Allocate necessary variables
!--------------------------------------------------------------------
allocate ( au(mnxyz) )
allocate ( av(mnxyz) )
allocate ( aw(mnxyz) )
allocate ( atempk(mnxyz) )
allocate ( apress(mnxyz) )
allocate ( avapor(mnxyz) )
allocate ( adn0(mnxyz) )
allocate ( atke(mnxyz) )
allocate ( api0(mnxyz) )
allocate ( app(mnxyz) )
allocate ( atheta(mnxyz) )
allocate ( auw(mnxyz) )
allocate ( avw(mnxyz) )
allocate ( arlong(mnxyz) )
allocate ( arshort(mnxyz) )
allocate ( austar(mnxyz) )
allocate ( atstar(mnxyz) )
allocate ( arstar(mnxyz) )
allocate ( atopo(mnxyz) )
allocate ( alat(mnxyz) )
allocate ( alon(mnxyz) )
allocate ( aprecip(mnxyz) )
![MLO(3/1/09) more variables
allocate ( apblhgt(mnxyz) )
allocate ( agpp(mnxyz) )
allocate ( aplresp(mnxyz) )
allocate ( ahetresp(mnxyz) )
!MLO]
allocate ( ascratch(mnxyz) )
!JCL(3/14/03)additional variables  
allocate ( aafxu(mnxyz) )
allocate ( aafxv(mnxyz) )
allocate ( aafxw(mnxyz) )
allocate ( acfxup1(mnxyz) )
allocate ( acfxup2(mnxyz) )
allocate ( acfxdn1(mnxyz) )
allocate ( adfxup1(mnxyz) )
allocate ( adfxup2(mnxyz) )
allocate ( aefxup1(mnxyz) )
allocate ( aefxup2(mnxyz) )
allocate ( adfxdn1(mnxyz) )
allocate ( aefxdn1(mnxyz) )
allocate ( asoil_moist(mnxyz) )
allocate ( atl(mnxyz) )
allocate ( asigw(mnxyz) )
!DMM(1/21/05) added asigwb & atlb
allocate ( asigwb(mnxyz) )
allocate ( atlb(mnxyz) )

!--------------------------------------------------------------------
!################## 3D FIELDS
!--->U: x-direction wind components [m/s]
call RAMS_varlib('u',nx,ny,nz,ng,AU,ascratch,&
      flnm,cdname,cdunits,nind) 
!--->V: y-direction wind components [m/s]
call RAMS_varlib('v',nx,ny,nz,ng,AV,ascratch,&
      flnm,cdname,cdunits,nind) 
!--->W: z-direction wind components [m/s]
call RAMS_varlib('w',nx,ny,nz,ng,AW,ascratch,&
      flnm,cdname,cdunits,nind) 
 
!--->TEMPK: temperature [K]
call RAMS_varlib('tempk',nx,ny,nz,ng,ATEMPK,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->PRESS: pressure [mb]
call RAMS_varlib('press',nx,ny,nz,ng,APRESS,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->VAPOR: water vapor mixing ratio [g/kg]
call RAMS_varlib('vapor',nx,ny,nz,ng,AVAPOR,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->DN0: reference state density [kg/m3]
call RAMS_varlib('dn0',nx,ny,nz,ng,ADN0,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->TKE: turbulent kinetic energy [m**2/s**2]
call RAMS_varlib('tke',nx,ny,nz,ng,ATKE,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->PI0: reference Exner function [J/(kgK)]
call RAMS_varlib('pi0',nx,ny,nz,ng,API0,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->PERT_PRESSURE: perturbation pressure [mb]
call RAMS_varlib('pert_pressure',nx,ny,nz,ng,APP,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->THETA: potential temperature [K]
call RAMS_varlib('theta',nx,ny,nz,ng,ATHETA,ascratch,&
      flnm,cdname,cdunits,nind) 

!JCL(3/14/03) new variables

!JCL '*b' stands for TIMESTEP-AVERAGED mass fluxes
call RAMS_varlib('afxub',nx,ny,nz,ng,AAFXU,ascratch,&
      flnm,cdname,cdunits,nind) 

call RAMS_varlib('afxvb',nx,ny,nz,ng,AAFXV,ascratch,&
      flnm,cdname,cdunits,nind) 

call RAMS_varlib('afxwb',nx,ny,nz,ng,AAFXW,ascratch,&
      flnm,cdname,cdunits,nind) 

call RAMS_varlib('tl',nx,ny,nz,ng,ATL,ascratch,&
      flnm,cdname,cdunits,nind) 

!DMM(1/21/05) Added atlb
call RAMS_varlib('tlb',nx,ny,nz,ng,ATLB,ascratch,&
      flnm,cdname,cdunits,nind)
      
call RAMS_varlib('sigw',nx,ny,nz,ng,ASIGW,ascratch,&
      flnm,cdname,cdunits,nind) 
!DMM(1/21/05) Added sigwb
call RAMS_varlib('sigwb',nx,ny,nz,ng,ASIGWB,ascratch,&
      flnm,cdname,cdunits,nind)
      
call RAMS_varlib('cfxup1',nx,ny,nz,ng,ACFXUP1,ascratch,&
      flnm,cdname,cdunits,nind) 

call RAMS_varlib('cfxup2',nx,ny,nz,ng,ACFXUP2,ascratch,&
      flnm,cdname,cdunits,nind) 

call RAMS_varlib('cfxdn1',nx,ny,nz,ng,ACFXDN1,ascratch,&
      flnm,cdname,cdunits,nind) 

call RAMS_varlib('dfxup1',nx,ny,nz,ng,ADFXUP1,ascratch,&
      flnm,cdname,cdunits,nind) 

call RAMS_varlib('dfxup2',nx,ny,nz,ng,ADFXUP2,ascratch,&
      flnm,cdname,cdunits,nind) 

call RAMS_varlib('efxup1',nx,ny,nz,ng,AEFXUP1,ascratch,&
      flnm,cdname,cdunits,nind) 

call RAMS_varlib('efxup2',nx,ny,nz,ng,AEFXUP2,ascratch,&
      flnm,cdname,cdunits,nind) 

call RAMS_varlib('dfxdn1',nx,ny,nz,ng,ADFXDN1,ascratch,&
      flnm,cdname,cdunits,nind) 

call RAMS_varlib('efxdn1',nx,ny,nz,ng,AEFXDN1,ascratch,&
      flnm,cdname,cdunits,nind) 

!################## 2D FIELDS

!--->UW [m2/s2]
call RAMS_varlib('uw',nx,ny,1,ng,AUW,ascratch,&
      flnm,cdname,cdunits,nind) 
 
!--->VW [m2/s2]
call RAMS_varlib('vw',nx,ny,1,ng,AVW,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->RSHORT [W/m2]
call RAMS_varlib('rshort',nx,ny,1,ng,ARSHORT,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->RLONG [W/m2]
call RAMS_varlib('rlong',nx,ny,1,ng,ARLONG,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->PRECIP (mm) --> converts to [m] & designate one of TPP1, TPP3....
call RAMS_varlib('precip',nx,ny,1,ng,APRECIP,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->USTAR_PS [m/s]
call RAMS_varlib('ustar_ps',nx,ny,1,ng,AUSTAR,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->TSTAR_PS [K]
call RAMS_varlib('tstar_ps',nx,ny,1,ng,ATSTAR,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->RSTAR_PS [kg/kg]
call RAMS_varlib('rstar_ps',nx,ny,1,ng,ARSTAR,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->TOPO [m]      
call RAMS_varlib('topo',nx,ny,1,ng,ATOPO,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->LAT [deg]
call RAMS_varlib('lat',nx,ny,1,ng,ALAT,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->LON [deg]
call RAMS_varlib('lon',nx,ny,1,ng,ALON,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->LON [deg]
call RAMS_varlib('pblhgt',nx,ny,1,ng,APBLHGT,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->LON [deg]
call RAMS_varlib('gpp',nx,ny,1,ng,AGPP,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->LON [deg]
call RAMS_varlib('plresp',nx,ny,1,ng,APLRESP,ascratch,&
      flnm,cdname,cdunits,nind) 

!--->LON [deg]
call RAMS_varlib('hetresp',nx,ny,1,ng,AHETRESP,ascratch,&
      flnm,cdname,cdunits,nind) 

!JCL(3/14/03) new variables
!call RAMS_varlib('soil_moist',nx,ny,1,ng,ASOIL_MOIST,ascratch,&
!      flnm,cdname,cdunits,nind) 

!-----------------------------------------------------------------
!-----Load individual arrays from scratch vector a()
!
! --- 3D variables
do k = 1,nz 
  do j = 1,ny 
  do i = 1,nx 
    n3d = i + (j-1)*nx + (k-1)*nx*ny
!JCL(3/14/03) use MASS FLUXES instead of velocities
!    u(k,i,j) = au(n3d) 
!    v(k,i,j) = av(n3d) 
!    w(k,i,j) = aw(n3d) 
    u(k,i,j) = aafxu(n3d) 
    v(k,i,j) = aafxv(n3d) 
    w(k,i,j) = aafxw(n3d) 
    t(k,i,j) = atempk(n3d) 
    p(k,i,j) = apress(n3d) 
    rv(k,i,j) = avapor(n3d)/1.e3  ! [g/kg] to [kg/kg] 
    dn0(k,i,j) = adn0(n3d) 
    tke(k,i,j) = atke(n3d) 
    pi0(k,i,j) = api0(n3d) 
    pp(k,i,j) = app(n3d) 
    theta(k,i,j) = atheta(n3d) 
!JCL(3/14/03) new variables
    tlgr(k,i,j) = atl(n3d)
    sigw(k,i,j) = asigw(n3d)
!DMM(1/21/05) added sigwb and tlgrb
    sigwb(k,i,j) = asigwb(n3d)
    tlgrb(k,i,j) = atlb(n3d)
    cfu1(k,i,j) = acfxup1(n3d)
    cfu2(k,i,j) = acfxup2(n3d)
    cfd1(k,i,j) = acfxdn1(n3d)
    dfu1(k,i,j) = adfxup1(n3d)
    dfu2(k,i,j) = adfxup2(n3d)
    efu1(k,i,j) = aefxup1(n3d)
    efu2(k,i,j) = aefxup2(n3d)
    dfd1(k,i,j) = adfxdn1(n3d)
    efd1(k,i,j) = aefxdn1(n3d)
  enddo 
  enddo 
enddo

!JCL(3/14/03) write out variables
!write(*,*)'cfu1:'
!do k = 1,nz 
!  do j = 1,ny 
!  do i = 1,nx 
  !do j = 30,30
  !do i = 20,20
!  write(*,*)i,j,k,cfu1(k,i,j),cfu2(k,i,j),cfd1(k,i,j),dfu1(k,i,j),&
!      dfu2(k,i,j),efu1(k,i,j),efu2(k,i,j),dfd1(k,i,j),efd1(k,i,j)
!  enddo 
!  enddo 
!enddo

! --- 2D variables
do k = 1,1 
  do j = 1,ny 
  do i = 1,nx 
    n3d = i + (j-1)*nx + (k-1)*nx*ny
    umof(i,j)=auw(n3d)
    vmof(i,j)=avw(n3d)
    dlwf(i,j)=arlong(n3d)
    dswf(i,j)=arshort(n3d)
    tpp1(i,j)=aprecip(n3d)/100. ! [mm] to [m]
    ustr(i,j)=austar(n3d)
    tstr(i,j)=atstar(n3d)
    qstr(i,j)=arstar(n3d)
    shgt(i,j)=atopo(n3d)
    glat(i,j)=alat(n3d)
    glon(i,j)=alon(n3d)
    pblhgt(i,j)=apblhgt(n3d)
    gpp(i,j)=agpp(n3d)
    plresp(i,j)=aplresp(n3d)
    hetresp(i,j)=ahetresp(n3d)
  enddo 
  enddo 
enddo
!
! Deallocate necessary variables
!
deallocate (au,av,aw,atempk,apress)
deallocate (avapor,adn0,atke,api0,app)  
deallocate (atheta)
deallocate (auw,avw,arlong,arshort)
deallocate (austar,atstar,arstar)
deallocate (atopo,alat,alon,aprecip) 
deallocate (ascratch)

!JCL(3/14/03) deallocate new variables
deallocate (aafxu,aafxv,aafxw)
deallocate (acfxup1,acfxup2,acfxdn1,adfxup1,adfxup2)
deallocate (aefxup1,aefxup2,adfxdn1,aefxdn1)
deallocate (asoil_moist)
deallocate (atl)
deallocate (asigw)
!DMM(1/21/05) deallocate asigwb and tlb
deallocate (asigwb)
deallocate (atlb)
deallocate (apblhgt,agpp,aplresp,ahetresp)
!
return
end
