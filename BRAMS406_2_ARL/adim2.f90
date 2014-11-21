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

subroutine adim2(mx,my,nx,ny,nsx,nex,nsy,ney,a,b)
!----------------------------------------------------------------
! Name:     adim2
! Purpose:  To save 'b' parameter to 'a' from nsx to nex 
!            and from nsy to ney
! Revised:  06 Aug 2002  Created for RAMS2ARL (S.-B. Kim)
! Note:     this version is F90 for use with RAMS v4.3
!
!---------------------------------------------------------------
!
integer, intent(in) :: mx,my,nx,ny,nsx,nex,nsy,ney
real, dimension (nx,ny) :: a
real, dimension (mx,my) :: b
!

do i=1,nx 
do j=1,ny 
  a(i,j)=b(nsx+(i-1),nsy+(j-1))
enddo
enddo

!
return
end
