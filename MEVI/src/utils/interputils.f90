!==========================================================================================!
!==========================================================================================!
!  MEVI. interputils.f90                                                                   !
!                                                                                          !
!     This file contains a set of useful procedures to interpolate variables both in the   !
! horizontal and in the vertical. They are built to be "I/O blind" , which means that they !
! should work for any given format, as long as it has the necessary information.           !
!==========================================================================================!
!==========================================================================================!
subroutine interp_prevert(nxp,nyp,nzp,lev,pres,invar,outvar)
   use mod_ioopts, only : missflg_real ! ! intent(in)
   implicit none
   !----- Arguments. ----------------------------------------------------------------------!
   integer                     , intent(in)  :: nxp    ! # of points in X direction
   integer                     , intent(in)  :: nyp    ! # of points in Y direction
   integer                     , intent(in)  :: nzp    ! # of points in Z direction
   real, dimension(        nzp), intent(in)  :: lev    ! Fixed grid levels 
   real, dimension(nxp,nyp,nzp), intent(in)  :: pres   ! Pressures
   real, dimension(nxp,nyp,nzp), intent(in)  :: invar  ! Variable to be interpolated
   real, dimension(nxp,nyp,nzp), intent(out) :: outvar ! Interpolated variable
   !----- Local variables. ----------------------------------------------------------------!
   integer                                   :: x      ! Counter
   integer                                   :: y      ! Counter
   integer                                   :: z      ! Counter
   integer                                   :: zbot   ! Bottom of defined layer
   integer                                   :: ztop   ! Top of defined layer
   integer                                   :: zdn    ! Levels used at interpolation.
   integer                                   :: zup    ! Levels used at interpolation.
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !   First we will fill the output var with missing flags, so values below surface or    !
   ! above the top will remain like that.                                                  !
   !---------------------------------------------------------------------------------------!
   outvar = missflg_real
   do x = 1,nxp
      do y=1,nyp
         zbot = maxloc(lev(:)-pres(x,y,  1),dim=1,mask=lev(:) < pres(x,y,1))
         ztop = minloc(lev(:)-pres(x,y,nzp),dim=1,mask=lev(:) > pres(x,y,nzp))
         do z=zbot,ztop
            zdn = minloc(abs(pres(x,y,:)-lev(z)),dim=1,mask=pres(x,y,:) > lev(  z))
            zup = zdn + 1
            outvar(x,y,z) = invar(x,y,zdn) + (invar(x,y,zup)-invar(x,y,zdn))               &
                          * log(lev(z)/pres(x,y,zdn))/log(pres(x,y,zup)/pres(x,y,zdn))
         end do
      end do
   end do
   !---------------------------------------------------------------------------------------!

   return
end subroutine interp_prevert
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!     This subroutine is necessary for Vis-5D output. Vis5d requires the fastest varying   !
! dimension to be y from north to south. Then the 2nd dimension longitude from west to     !
! east, and the 3rd dimension from bottom to top, if it is a 3d variable.                  !
!------------------------------------------------------------------------------------------!
subroutine swaplatv5d(nxp,nyp,nzp,swapy,swapz,invar,outvar)
   implicit none
   !----- Arguments. ----------------------------------------------------------------------!
   integer                     , intent(in)   :: nxp
   integer                     , intent(in)   :: nyp
   integer                     , intent(in)   :: nzp
   logical                     , intent(in)   :: swapy
   logical                     , intent(in)   :: swapz
   real, dimension(nxp,nyp,nzp), intent(in)   :: invar
   real, dimension(nyp,nxp,nzp), intent(out)  :: outvar
   !----- Local variables. ----------------------------------------------------------------!
   integer                                    :: x
   integer                                    :: y
   integer                                    :: z
   integer                                    :: yout
   integer                                    :: zout
   !---------------------------------------------------------------------------------------!
  
   do x=1,nxp
      do y=1,nyp
         if(swapy) then
            yout = nyp - y + 1
         else
            yout = y
         end if
         do z=1,nzp
            if (swapz) then
               zout = nzp - z + 1
            else
               zout = z
            end if
            outvar(yout,x,zout) = invar(x,y,z)
         end do
      end do
   end do

   return   
end subroutine swaplatv5d
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!     This subroutine can be used to reverse latitude and vertical coordinates without     !
! permuting the dimensions of longitude and latitude.                                      !
!------------------------------------------------------------------------------------------!
subroutine swaplatlev(nxp,nyp,nzp,swapy,swapz,invar,outvar)
   implicit none
   !----- Arguments. ----------------------------------------------------------------------!
   integer                     , intent(in)   :: nxp
   integer                     , intent(in)   :: nyp
   integer                     , intent(in)   :: nzp
   logical                     , intent(in)   :: swapy
   logical                     , intent(in)   :: swapz
   real, dimension(nxp,nyp,nzp), intent(in)   :: invar
   real, dimension(nxp,nyp,nzp), intent(out)  :: outvar
   !----- Local variables. ----------------------------------------------------------------!
   integer                                    :: x
   integer                                    :: y
   integer                                    :: z
   integer                                    :: yout
   integer                                    :: zout
   !---------------------------------------------------------------------------------------!
  
   do x=1,nxp
      do y=1,nyp
         if(swapy) then
            yout = nyp - y + 1
         else
            yout = y
         end if
         do z=1,nzp
            if (swapz) then
               zout = nzp - z + 1
            else
               zout = z
            end if
            outvar(x,yout,zout) = invar(x,y,z)
         end do
      end do
   end do

   return   
end subroutine swaplatlev
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!     This sub-routine copies one column of longitudes from a three-dimensional array,     !
! given the latitude row and the vertical layer.                                           !
!------------------------------------------------------------------------------------------! 
subroutine xyz_2_x(nxp,nyp,nzp,y,z,invar,outvar)
   implicit none
   !----- Arguments. ----------------------------------------------------------------------!
   integer                     , intent(in)   :: nxp
   integer                     , intent(in)   :: nyp
   integer                     , intent(in)   :: nzp
   integer                     , intent(in)   :: y
   integer                     , intent(in)   :: z
   real, dimension(nxp,nyp,nzp), intent(in)   :: invar
   real, dimension(nxp)        , intent(out)  :: outvar
   !----- Local variables. ----------------------------------------------------------------!
   integer                                    :: x
   !---------------------------------------------------------------------------------------!
   
   do x=1,nxp
      outvar(x) = invar(x,y,z)
   end do
   
   return
end subroutine xyz_2_x
!==========================================================================================!
!==========================================================================================!
