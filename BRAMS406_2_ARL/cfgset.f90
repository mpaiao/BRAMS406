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

subroutine cfgset(polelat,polelon,dxkm,xtan,ytan,igrdn, &
  n1,n2,nzp,kqz,zout,cfgnam,ftype,kunt,slat,slon,rad,navrb)
!----------------------------------------------------------------
! Name:     cfgset
! Purpose:  This routine creates the RAMSDATA.CFG file needed 
!           for packing  
! Revised:  Mar 1995  Created (Jeff McQueen)
!           Aug 2002  Converted to f90 format (S.-B. Kim)
! Note:     this version is F90 for use with RAMS v4.3
!
!---------------------------------------------------------------
!
real :: zout(80)
integer :: igrdn
character(len=2) :: cdx
character(len=4) :: ftype(navrb),fder(3)
character(len=80) :: cfgnam,cfgweb
data ivcord,orien,coneang/3,0.,90./
data nskip/1/
 
! JCL(3/14/03) extra variables from coupling between RAMS<=>STILT
!data zsfc,nv3d/0,7/
data zsfc,nv3d/0,18/
!---> Cfg parameters for READY web page
data nder,fder/3,'WVCT','FLAG','WSPD'/

close (kunt)
open(kunt,file=cfgnam)
nasfc=navrb-nv3d
 
!---> Create Standard ARLPACKED config file 
   
print *,' OPENING CFG FILE : ',cfgnam

!JCL
!WRITE(*,*)'nasfc,navrb,nv3d:',nasfc,navrb,nv3d

write(kunt,100)
print *, 'inside=',igrdn
write(kunt,101) igrdn
write(kunt,102) ivcord

!---> Tangent lat,lon
write(kunt,103) polelat
write(kunt,104) polelon

!---> Reference lat,lon
write(kunt,105) polelat
write(kunt,106) polelon
write(kunt,107) dxkm   
write(kunt,108) orien
write(kunt,109) coneang

!---> Sync X,Y, LAT,LON
write(kunt,110) xtan
write(kunt,111) ytan

!jtm  4-99: Sync x,y is determined anywhere on the grid 
!jtm        instead of polelat,lon

write(kunt,112) slat
write(kunt,113) slon
write(kunt,*)'SPECIAL:           '

write(kunt,115) n1
write(kunt,116) n2

nlevels=(kqz-1)/nskip+1
LL=1
write(kunt,117) nlevels
write(kunt,118) LL,zsfc,nasfc,(ftype(i),i=1,nasfc)
!    : (ftype(i),i=8,nasfc)

i3dst=nasfc+1
do k=2,nlevels,nskip
  LL=LL+1
  write(kunt,118) LL,zout(k),nv3d,(ftype(i),i=i3dst,navrb)
enddo
close(kunt)

100  format('MODEL TYPE:' ,9X,'RAMS')
101  format('GRID NUMB: ' ,9X,I4)
102  format('VERT COORD:' ,9X,I4)
103  format('POLE LAT:  ' ,9X,F10.2)
104  format('POLE LON:  ' ,9X,F10.2)
105  format('REF  LAT:  ' ,9X,F10.2)
106  format('REF  LON:  ' ,9X,F10.2)
107  format('REF GRID:  ' ,9X,F10.2)
108  format('ORIENTATION' ,9X,F10.2)
109  format('CONE ANGLE:' ,9X,F10.2)
110  format('SYNC X:    ' ,9X,F10.2)
111  format('SYNC Y:    ' ,9X,F10.2)
112  format('SYNC LAT:  ' ,9X,F10.2)
113  format('SYNC LON:  ' ,9X,F10.2)
115  format('NUMB X:    ' ,9X,I4)
116  format('NUMB Y:    ' ,9X,I4)
117  format('NUMB LEVELS' ,9X,I4)
! CHG(09/09/03) change format to keep 1 decimal for heights
!118  format('LEVEL',I3,12X,F6.0,I3,40(1X,A4))
118  format('LEVEL',I3,12X,F7.1,I3,40(1X,A4))


return
end
