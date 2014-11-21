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

subroutine juldate(idate)
!----------------------------------------------------------------
! Name:     juldate
! Purpose:  converts date from calendar (YYYYMMDD) format 
!            to Julian (YYJJJ) format
! Revised:  06 Aug 2002  Created for RAMS2ARL (S.-B. Kim)
! Note:     this version is F90 for use with RAMS v4.3
!
!---------------------------------------------------------------
!
integer, dimension(12) :: nday
data nday/31,28,31,30,31,30,31,31,30,31,30,31/
!
!-----Entry point
!
idate = mod(idate,1000000)
iyear = idate/10000
imonth = (idate - iyear*10000)/100
iday = idate - iyear*10000 - imonth*100
!
nday(2) = 28
if (mod(iyear,4).eq.0) nday(2) = 29
  mday = 0
  do n = 1,imonth-1
    mday = mday + nday(n)
  enddo ! n
  jday = mday + iday
  idate = iyear*1000 + jday
!
return
end
