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

program brams406_arl
!----------------------------------------------------------------
! Name:     rams_arl 
! Purpose:  read RAMS output (analysis files) and converts
!           it to ARL packed format data.
! Revised:  06 Aug 2002  Created for RAMS2ARL (S.-B. Kim)
! Note:     this version is F90 for use with RAMS v4.3
!
!---------------------------------------------------------------
!
use an_header
include 'param.inc'
include 'fields.inc'

! RAMS for input
integer(8) :: sdate,edate !12 digit interger required
integer :: stime,etime,smin,emin 
!    USER specified # of points in z-dir and boundary points to skip
integer :: user_kz, user_bksp !
!JCL(2/18/04): change length of filenames to allow for long names
!character(len=80) :: fname, afilout
character(len=200) :: fname, afilout
logical lfirst
data zero /0./
data lfirst/.true./
integer :: imm, jdate
real, dimension(80) :: zt
!JCL(3/27/03): add variable that stores altitudes of FLUX (not scalar) levels
real, dimension(80) :: zm
integer :: mygrdn

! RAMS & ARL interface for date and hour information
integer :: iyear1,imonth1,idate1

! ARL
integer, dimension(80) :: jrecno
integer :: iupst, LL, kini
integer :: nlbcskp
!JCL(3/27/03): need FRACTION of hour when timestep is smaller than 1 hour
!integer :: tint, delhr
integer :: tint
real :: delhr
character :: cnstn
character(len=90) :: datnam
character(len=80) :: cfilnm,cfgnam  
character(len=50) :: header
character(len=4), dimension(navrb) :: ftype(navrb)
character(len=4) :: varb
logical lfrst 

integer :: iyr,im,iday,ihr,imin,itimeh 

integer :: imnt(12),grdn
real :: ainit(500),xpl1(20),ypl1(20)
real, allocatable :: rvar(:,:)
character*1, allocatable :: ivari(:)

!--->ncons=#constants in header, n1dx=#1d x arrays in header
data ncons,n1dx,n1dy,n1dz/navrb,1,1,1/

!---> when adding variables cfgset varbs nwfo,narl,n3dv mst also be set!
data (ftype(II),II=1,nwfo)/'RH2M'/
data (ftype(II),II=nwfo+1,navrb)/'SHGT','HPBL','GPP0','PLRE','HTRE',&
      'TPP1','PRSS',&
      'T02M','USTR','TSTR','QSTR','UMOF','VMOF',&
      'DSWF','DLWF',&
      'UWND','VWND','WWND','SPHU','TEMP','PRES','TKEN',&
      'CFU1','CFU2','CFD1','DFU1','DFU2','EFU1','EFU2',& 
      'DFD1','EFD1','TLGR','SIGW'/
! JCL(3/14/03) extra variables from coupling between RAMS<=>STILT

data imnt/31,28,31,30,31,30,31,31,30,31,30,31/
data lfrst/.TRUE./


! (DMM 5/28/04) Original authors did not declare gravitation acceleration,g,
! in their "rams_arl.f90". They sent out a correction, which I added
! here. G is only used in the calculation of WWND, the vertical wind 

real G
data G /9.80/


!
!-----Get user-supplied inputs
!
read(*,'(20x,i10)') ingrid
write(*,'(a,t20,i10)') 'RAMS Grid number',ingrid
read(*,'(20x,i10)') user_kz
write(*,'(a,t20,i10)') 'Number of vertical layers',user_kz
read(*,'(20x,i10)') user_bksp
write(*,'(a,t20,i10)') 'Skipped boundary grid points',user_bksp

read(*,'(20x,i12,1x,i12)') sdate, edate

! For ARLWRT  
!iyear1=sdate/1000000
!imonth1=(mod(sdate,1000000))/10000
!idate1=(mod(sdate,10000))/100
!Changed to include minutes YYYYMMDDHHmm 7-Apr-2004 dmm
iyear1=sdate/100000000
imonth1=(mod(sdate,100000000))/1000000
idate1=(mod(sdate,1000000))/10000

! For RAMS
!stime = mod(sdate,100)
!etime = mod(edate,100)
smin = mod(sdate,100)
emin = mod(edate,100)

sdate = sdate/100
edate = edate/100
!End addition of code for minutes 

stime = mod(sdate,100)
etime = mod(edate,100)

sdate = sdate/100
edate = edate/100

call juldate(sdate)
call juldate(edate)
!JCL 'sdate' is in yyjjj format, where jjj is Julian day; 'stime' is in hh format
write(*,'(a,t20,2(i10.5,i10,i10))') &
     'Start/end dates',sdate,stime,smin,edate,etime,emin

! output file name
read(*,'(20x,a)') afilout

!-----Setup variables to be read from raw RAMS files

read(*,'(20x,a)') fname
open(20,file=fname)
write(*,*)'Opened RAMS header file: ',fname

read(20,*) nvbtab
if (allocated(anal_table)) deallocate(anal_table)
  allocate (anal_table(nvbtab))
  do nv = 1,nvbtab
     read(20,*) anal_table(nv)%string, &
                anal_table(nv)%npointer, &
                anal_table(nv)%idim_type, &
                anal_table(nv)%ngrid, &
                anal_table(nv)%nvalues
   enddo
!  call commio('ANAL','READ',20,ingrid,nx,ny,nz,deltax,xrm,yrm, &
!              xrt,yrt,plon,plat,zt)
   call commio('ANAL','READ',20,ingrid,nx,ny,nz,deltax,xrm,yrm, &
               xrt,yrt,plon,plat,zt,zm)
close(20)
!
!-----RAMS grid to be processed
!
x0rams = xrm(1)/1000.
y0rams = yrm(1)/1000.
xframs = xrm(nx-1)/1000.
yframs = yrm(ny-1)/1000.
deltax = deltax/1000.
do i = 1,nx
   xrt(i) = xrt(i)/1000.
enddo
do j = 1,ny
   yrt(j) = yrt(j)/1000.
enddo
nzm1 = nz - 1

write(*,*)
write(*,*)'Grid parameters for the input RAMS domain'
write(*,'(a,2i10)')  '          NEST ID:',ingrid
write(*,'(a,3i10)')  '         NX,NY,NZ:',nx,ny,nz
write(*,'(a,f10.0)') '               DX:',deltax
write(*,'(a,2f10.3)')'     Pole lon/lat:',plon,plat
write(*,'(a,2f10.1)')'    SW x/y corner:',x0rams,y0rams
write(*,'(a,2f10.1)')'    NE x/y corner:',xframs,yframs
write(*,'(a,2f10.1)')'  SW thermo point:',xrt(2),yrt(2)
write(*,'(a,2f10.1)')'  NE thermo point:',xrt(nx-1),yrt(ny-1)
if (nx.gt.mnx .or. ny.gt.mny .or. nz.gt.mnz) then
  write(*,*)'RAMS dimensions too large for arrays'
  write(*,*)'Increase array dimensions in param.inc and recompile'
  stop
endif
!
!-----Loop over number of input RAMS files
!
read(*,'(20x,i10)') tint
!JCL(03/27/03): changed 'delhr' to a REAL
!delhr=tint/3600
delhr=tint/3600.0
read(*,'(20x,i10)') nfiles

do nf = 1,nfiles
! This part should be changed to assign the filename automatically 
  read(*,'(20x,a)') fname
  write(*,*)
  write(*,*)'Input RAMS filename: ',fname
!
!-----Calculate date/time
!
  write(*,*)'stime,nf=',stime,nf
  imm = stime*60 + smin + (nf - 1)*delhr*60
  jdate = sdate
  do while (imm.gt.(23*60+59))
     imm = imm - 24*60
     jdate = jdate + 1
  enddo ! imm
  write(*,'(a,t20,i10.5,i10,/)')'RAMS date/hr:',jdate,imm
!
!-----Read raw RAMS data
!
  call readrams(nx,ny,nz,nzm1,lfirst,ingrid,fname)
!
!-----Compute some met. fields needed for ARL packed data or convert
!     to ARL index/units convention
!
  call comp_ARL(nz,nx,ny,zt)
 
!********************************************************************
!-----Write the RAMS data to ARL packed file
!********************************************************************
!
! For quick look plotting u,v,w,theta written out on zstar levels
! in scaled 4 byte integer at every model level 

  iupst=51 ! UNIT NUMBER OF OUTPUT FILE
  iohdr=61 ! UNIT NUMBER OF FTP FILE
  ng=ingrid
  iupst=iupst+ingrid-1
  iohdr=iohdr+ingrid-1
  nxy=nx*ny
  kqz=USER_KZ 
  if(nz.le.kqz) kqz=nz

!--->Reduce grid dimensions to omit boundary points 
  nlbcskp=USER_BKSP 
  n1=nx-2*nlbcskp
  n2=ny-2*nlbcskp
! CHG(10/02/03) use 16 bits (2 bytes)
!  n12=n1*n2
  n12=n1*n2*2
  n3=kqz-1  
  lrec=n12+50

!--->Compute the Date and Time information needed for PAKREC subroutine
  if(lfrst) then
    im=imonth1
    iday=idate1
    iyr=iyear1-1900 ! ARL-PACKED 2digit year and Y2K fix
    if(iyr.ge.100)iyr=iyr-100
    if(mod(iyr,4).EQ.0) imnt(2)=29 ! Leap year fix
!JCL(03/27/03):first store in temporary variable that can accept REAL vars
!   ihr = stime + (nf - 1)*delhr
!CHG(09/05/03): round off issue
!    hrtmp = stime + (nf - 1)*delhr
    hrtmp = stime + (nf - 1)*delhr + 0.001
    write(*,*)'hrtmp (a)=',hrtmp
    ihr=int(hrtmp)
  else
!JCL(03/27/03):change updating of 'ihr' b/c 'delhr' can be less than 1.0
!   ihr=ihr+delhr
    hrtmp=hrtmp+delhr
    write(*,*)'hrtmp (b)=',hrtmp
    ihr=int(hrtmp)
!    ihr = stime + (nf - 1)*delhr
  endif
  itimeh = (nf-1)*delhr ! forecast hour
!JCL(03/27/03) need to have minute information!
!  imin=0
  imin=mod((nf-1)*tint/60,60)

!--->Reset IM,IDAY,IHR,IYR if the next day
  write(*,*)'ihr=',ihr,iday
  if (ihr .ge. 24) then
110 ihr = ihr - 24
!JCL(03/27/03):change updating of 'ihr' b/c 'delhr' can be less than 1.0
  hrtmp=real(ihr)
  write(*,*)'after subtracting 24 (1):  ihr=',ihr,iday,hrtmp
    iday=iday+1
    if (imnt(im).lt.iday) then
       im=im+1 
       iday=1 
    endif 
  write(*,*)'after subtracting 24 (2):  ihr=',ihr,iday,hrtmp
    if(ihr .ge. 24) goto 110
  endif 
  if (im.gt.12) then 
     im=1 
     iyr=iyr+1
  endif 
!--->Set ARL Grid number igrdn1=15
  mygrdn=igrdn1+(ingrid-1)*10
 
!--->OUTPUT INITIAL HEADER FIELDS
  if(lfrst) then
    cfilnm=afilout 
    do ib=1,80 
      if(cfilnm(ib:ib).eq.' ') goto 998
    enddo 
998 ie=ib-1      
!    Create ARL filenames and open direct access output files
    cnstn=char(ingrid+48)
    datnam=cfilnm(1:ie)//'_'// cnstn //  '.bin'

!     Intialization of some parameters
    jrecno(Ingrid)=0
    igrd=0
    k=0
    ij=1
    ntot=ncons+n1dx*nx+n1dy*ny+n1dz*nz
    val1=GLAT(1,1)
    val2=GLON(1,1)
    val3=GLAT(nx,ny)
    val4=GLON(nx,ny) 
!--->Compute radius for READY plots
    radrdy=abs(val3-val1)
    radrdy=min(radrdy,abs(val4-val2))
    radrdy=radrdy*.5

!--->With DMAP routines XPL1 and YPL1 become the synch points of each grid
!--->Which can be defined anywhere on that grid
!--->For this case we will choose the first point of the READY subrgrid
!--->Where the boundary points are removed
    xpl1(ingrid)=nlbcskp+1 ! X starting grid number
    ypl1(ingrid)=nlbcskp+1 ! Y starting grid number
    synclat=GLAT(nint(xpl1(ingrid)),nint(ypl1(ingrid)))
    synclon=GLON(nint(xpl1(ingrid)),nint(ypl1(ingrid)))
    nexp=nx-nlbcskp ! X ending grid number
    neyp=ny-nlbcskp ! Y ending grid number
    rnelat=GLAT(nexp,neyp)
    rnelon=GLON(nexp,neyp)

!chg(09/12/03) write lat lon to file
!    open(210,file='latlonraw.txt')
!    do iii=1,nx
!      do jjj=1,ny
!        write(210,*),iii,jjj,GLON(iii,jjj),GLAT(iii,jjj)
!      end do
!    end do

!   Reset for subgrid which removes boundary points (3/98)
    xpl1(ingrid)=xpl1(ingrid)-nlbcskp ! go back to 1
    ypl1(ingrid)=ypl1(ingrid)-nlbcskp ! go back to 1

!--->Using new ARL Packing routine with index record 
    dxkm=deltax
    cfgnam='RAMSDATA'//CNSTN//'.CFG'

!--->KREC is the starting record number to write output
    krec=1
    print *, 'outside=',mygrdn
    call cfgset(plat,plon,dxkm,xpl1(ingrid),ypl1(ingrid), &
!JCL(3/27/03): pass on altitudes of FLUX LEVELS, instead of scalar levels
!      grdn,n1,n2,nz,kqz,zt,cfgnam,ftype,iohdr, &
       mygrdn,n1,n2,nz,kqz,zm,cfgnam,ftype,iohdr, &
       synclat,synclon,radrdy,navrb)

!JCL 'pakset' is opening the *.CFG file and reading config info
    print *,'AFTER CFGSET'
!--->Pakset will alter values of nx,ny,nz so input temp values
    call pakset(iupst,cfgnam,krec,n1,n2,n3)
    print *,'AFTER PAKSET'

!--->Open ARL packed data file after PAKSET call
    open(unit=iupst,file=datnam,form='unformatted', &
         access='direct',recl=lrec)

    lfrst=.FALSE. !!!! NGRIDS ?????
     
  endif ! lfrst

!
  allocate ( rvar(n1,n2))
  allocate ( ivari(n12))

!--->KINI is the time period init. flag for the PAK routines
  kini=0
!--->At surface LL = 1
  LL=1

! sbkim
  nsxg=nlbcskp+1
  nexg=nx-nlbcskp
  nsyg=nlbcskp+1
  neyg=ny-nlbcskp
  
!--->Create and Output WFO products
  if (NWFO .gt. 0) then

!---> RH2M: Relative Humidity at 2m in %
    igrd=0
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,RH2M)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass RH2M'

  endif ! NWFO

!==================STD RAMS VARIABLES=========================
!--------- 2D sfc varaibles

!--->SHGT: TOPOGRAPHY IN M   
  jrecno(ingrid)=jrecno(ingrid)+1
  ng=ingrid
  igrd=nwfo+1 ! Reset label pointer after number of wfo grids
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,SHGT)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass SHGT'

!---> HPBL: PBL Height in m
  jrecno(ingrid)=jrecno(ingrid)+1
  igrd=igrd+1
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,PBLHGT)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass HPBL'

!--->GPP0: Gross primary production umol/m2/s
  jrecno(ingrid)=jrecno(ingrid)+1
  igrd=igrd+1
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,GPP)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass GPP0'

!--->PLRP: Plant respiration umol/m2/s
  jrecno(ingrid)=jrecno(ingrid)+1
  igrd=igrd+1
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,PLRESP)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass PLRE'

!--->HTRP: Plant respiration umol/m2/s
  jrecno(ingrid)=jrecno(ingrid)+1
  igrd=igrd+1
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,HETRESP)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass HTRE'

!--->TPP1: HRLY ACCUMULATED PRECIP IN M 
  jrecno(ingrid)=jrecno(ingrid)+1
  igrd=igrd+1
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,TPP1)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass TPP1'

!--->PRSS: sfc pressure in mb
  jrecno(ingrid)=jrecno(ingrid)+1
  igrd=igrd+1
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,PRSS)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass PRSS'

!--->T02M: temperature at 2m in Kelvin 
  jrecno(ingrid)=jrecno(ingrid)+1
  igrd=igrd+1
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,T02M)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass T02M'

!--->USTR: ustar in meters/sec
  jrecno(ingrid)=jrecno(ingrid)+1
  igrd=igrd+1
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,USTR)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass USTR'

!--->TSTR: theta star in Kelvin
  jrecno(ingrid)=jrecno(ingrid)+1
  igrd=igrd+1
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,TSTR)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass TSTR'

!--->QSTR: qstar
  jrecno(ingrid)=jrecno(ingrid)+1
  igrd=igrd+1
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,QSTR)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass QSTR'

!--->UMOF: U-Momentum flux in N/m2 (Pa)
  jrecno(ingrid)=jrecno(ingrid)+1
  igrd=igrd+1
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,UMOF)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass UMOF'

!--->VMOF: V-Momentum flux in N/m2 (Pa)
  jrecno(ingrid)=jrecno(ingrid)+1
  igrd=igrd+1
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,VMOF)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass VMOF'

!--->DSWF: Downward Shortwave flux in W/m2 
  jrecno(ingrid)=jrecno(ingrid)+1
  igrd=igrd+1
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,DSWF)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass DSWF'

!--->DLWF: Downward Longwave flux in W/m2 
  jrecno(ingrid)=jrecno(ingrid)+1
  igrd=igrd+1
  call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,DLWF)
  call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
       iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass DLWF'

!--------- 3D FIELDS
  igrd2d=igrd
  LL=1
  do k=2,kqz
    igrd=igrd2d     
    LL=LL+1

!--->UWND: U IN M/S
!      Output from half level 2 since half level 1 is below ground
!      interpoluted to T points 
    do j=1,ny
    do i=1,nx
      im1=i-1
      if(im1.le.0)im1=1
!JCL(3/19/03) switch off interpolation
!      val1=U(K,IM1,J)
!      val2=U(K,I,J)
!      UWND(I,J)=(val1+val2)*.5
!      WRITE(*,*)'UWND:',I,J,K,U(K,I,J)
       UWND(I,J)=U(K,I,J)
!       IF(j.eq.19.and.i.eq.34)WRITE(*,*)'uwnd',I,J,K,U(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,UWND)    
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass UWND', k

!--->VWND: V IN M/S
    do j=1,ny
    jm1=j-1
    if(jm1.le.0) jm1=1
    do i=1,nx
!JCL(3/19/03) switch off interpolation
!      val1=V(K,I,JM1)
!      val2=V(K,I,J)
!      VWND(I,J)=(val1+val2)*.5
       VWND(I,J)=V(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,VWND)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass VWND', k

!--->WWND: WP IN MB/S
    do j=1,ny
    do i=1,nx
!JCL(3/19/03) switch off conversions
!      val1=W(K,I,J) ! W in m/s
!      val2=DN0(K,I,J) ! Rho in kg/m3
!      G in m/s, Omega in Pa/s
!      0.01*Omega=Omega in mb/s (1mb=100Pa)
!      WWND(I,J)=-val1*val2*G*0.01
      WWND(I,J)=W(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,WWND)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass WWND', k

 

!--->SPHU: water vapor mixing ratio in kg/kg (kg/m3)
    do j=1,ny
    do I=1,nx
      SPHU(I,J)=RV(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,SPHU)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass SPHU', k

!--->TEMP: temperature in Kelvin
    do J=1,ny
    do I=1,nx
      TEMP(I,J)=T(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,TEMP)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass TEMP', k

!--->PRES: pressure in mb 
    do J=1,ny
    do I=1,nx
!      WRITE(*,*)'PRES:',I,J,K,P(K,I,J)
      PRES(I,J)=P(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,PRES)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass PRES', k

!--->TKEN: TKE (m**2/s**2) if used.
    do J=1,ny
    do I=1,nx
      TKEN(I,J)=TKE(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,TKEN)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass TKEN', k

!******************************
!JCL(3/19/03) add additional 3D vars for RAMS<=>STILT 
!   NOTE:  the order has to follow the order set in 'ftype'
!--->CFU1: 
    do J=1,ny
    do I=1,nx
      CFU1N(I,J)=CFU1(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,CFU1N)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass CFU1', k

!--->CFU2: 
    do J=1,ny
    do I=1,nx
      CFU2N(I,J)=CFU2(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,CFU2N)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass CFU2', k

!--->CFD1: 
    do J=1,ny
    do I=1,nx
      CFD1N(I,J)=CFD1(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,CFD1N)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass CFD1', k

!--->DFU1: 
    do J=1,ny
    do I=1,nx
      DFU1N(I,J)=DFU1(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,DFU1N)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass DFU1', k

!--->DFU2: 
    do J=1,ny
    do I=1,nx
      DFU2N(I,J)=DFU2(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,DFU2N)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass DFU2', k

!--->EFU1: 
    do J=1,ny
    do I=1,nx
      EFU1N(I,J)=EFU1(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,EFU1N)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass EFU1', k

!--->EFU2: 
    do J=1,ny
    do I=1,nx
      EFU2N(I,J)=EFU2(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,EFU2N)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass EFU2', k

!--->DFD1: 
    do J=1,ny
    do I=1,nx
      DFD1N(I,J)=DFD1(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,DFD1N)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass DFD1', k

!--->EFD1: 
    do J=1,ny
    do I=1,nx
      EFD1N(I,J)=EFD1(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,EFD1N)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
    print *,'pass EFD1', k

!--->TLGR: Lagrangian timescale
    do J=1,ny
    do I=1,nx
!      TLGRN(I,J)=TLGR(K,I,J)
!DMM (1/21/05) Quick Fix--forcing SIGWN to get output from sigwb (time averaged
!                         sigw, rather than instantaneous)
       TLGRN(I,J)=TLGRB(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,TLGRN)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
!    print *,'pass TLGR', k
     print *,'pass TLGR(b)', k

!--->SIGW: stdev of vertical velocity
    do J=1,ny
    do I=1,nx
!      SIGWN(I,J)=SIGW(K,I,J)
!DMM (1/21/05) Quick Fix--forcing SIGWN to get output from sigwb (time averaged
!                         sigw, rather than instantaneous)
	SIGWN(I,J)=SIGWB(K,I,J)
    enddo
    enddo
    jrecno(ingrid)=jrecno(ingrid)+1
    igrd=igrd+1
    call adim2(mnx,mny,n1,n2,nsxg,nexg,nsyg,neyg,RVAR,SIGWN)
    call pakrec(iupst,rvar,ivari,n1,n2,n12,ftype(igrd), &
         iyr,im,iday,ihr,imin,itimeh,LL,kini)
!    print *,'pass SIGW', k
    print *,'pass SIGW(b)', k
!JCL(3/19/03) add additional 3D vars for RAMS<=>STILT 
!******************************

!
  enddo ! k vertical grid loop index
 
    deallocate (rvar)
    deallocate (ivari)
!--->Write ARL packed index record to output file
  call pakndx(iupst)
  print 220,GRDN,NGRID,IM,IDAY,IYR,IHR,IMIN  
220 FORMAT (' WROTE ARL FILE ',I3,' NGRID=',I3, &
       ' DATE ',I2,'/',I2,'/',I4,I4,':',I2,' UTC')

!
 if (jdate.eq.edate .and. imm.eq.(etime*60+emin)) goto 999
enddo ! nf
999  continue
!
stop
end program brams406_arl
