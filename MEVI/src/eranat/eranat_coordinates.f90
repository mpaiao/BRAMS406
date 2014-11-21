subroutine eranat_coordinates()

   use an_header      , only : nfiles, info_table
   use mod_grid       , only : grid_g
   use mod_model      , only : ngrids,nnxp,nnyp,nnzp,xtn,ytn,ztn
   implicit none
   !----- Local variables. ----------------------------------------------------------------!
   integer       :: ifm
   integer       :: nf
   integer       :: x
   integer       :: y
   integer       :: z
   !---------------------------------------------------------------------------------------!
   
   gridloop: do ifm=1,ngrids
      !----- Skip the grid in case it is not always there ---------------------------------!
      do nf=1,nfiles
         if (.not. info_table(nf)%avail_grid(ifm)) cycle gridloop
      end do
      !------------------------------------------------------------------------------------!
      !   Longitude and latitude should never change (MEVI doesn't support moving grids).  !
      ! So we will read only the first file and the first time to get longitude and        !
      ! latitude.                                                                          !
      !------------------------------------------------------------------------------------!
      do x=1,nnxp(ifm)
         do y=1,nnyp(ifm)
            grid_g(ifm)%lon(x,y) = xtn(x,ifm)
            grid_g(ifm)%lat(x,y) = ytn(y,ifm)
         end do
      end do
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !    ZTN was already filled with all levels needed at ecmwfcio.F90, here it's just a !
      ! matter of copying it to the grid structure...                                      !
      !------------------------------------------------------------------------------------!
      do z=1,nnzp(ifm)
         grid_g(ifm)%lev(z) = ztn(z,ifm)
      end do
      !------------------------------------------------------------------------------------!
   end do gridloop

   return
end subroutine eranat_coordinates
!==========================================================================================!
!==========================================================================================!
