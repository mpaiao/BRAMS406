subroutine wrf_coordinates()

#if USE_NCDF
   use netcdf
   use an_header      , only : nfiles, info_table
   use mod_grid       , only : grid_g
   use mod_model      , only : ngrids,nnxp,nnyp,nnzp,xtn,ytn,ztn,if_adap,ihtran
   use mod_netcdf     , only : ncid
   use mod_scratch    , only : scratch
   use mod_ncdf_globio, only : ncio_0dvar,ncio_2dvar,ncdf_load_err
   use mod_ioopts     , only : time_address,outtimes,nouttimes,mevi_notfound,mevi_absent
   implicit none

   integer :: ierr,tadd
   integer :: ifm,nf,nt,dimsiz,k,x,y
   real    :: psfcmax,ptopmin,ptopnow
   
   gridloop: do ifm=1,ngrids
      !----- Skipping the grid in case it is not always there -----------------------------!
      do nf=1,nfiles
         if (.not. info_table(nf)%avail_grid(ifm)) cycle gridloop
      end do
      !------------------------------------------------------------------------------------!
      !   Longitude and latitude should never change (MEVI doesn't support moving grids).  !
      ! So we will read only the first file and the first time to get longitude and        !
      ! latitude.                                                                          !
      !------------------------------------------------------------------------------------!
      ierr = nf90_open(info_table(1)%filename,NF90_NOWRITE,ncid)
      if (ierr /= NF90_NOERR) then
         call ncdf_load_err(ierr)
         call fatal_error ('Error opening the file '//trim(info_table(nf)%filename)//'...' &
                          ,'wrf_coordinates','wrf_coordinates.F90'                   ) 
      end if

      select case (ihtran)
      case (-1)
         do y=1,nnyp(ifm)
            do x=1,nnxp(ifm)
               grid_g(ifm)%lon(x,y) = xtn(x,ifm)
               grid_g(ifm)%lat(x,y) = ytn(y,ifm)
            end do
         end do
      case default
         ierr = ncio_2dvar('XLONG',.true.,1,nnxp(ifm),nnyp(ifm),grid_g(ifm)%lon)
         ierr = ncio_2dvar('XLAT' ,.true.,1,nnxp(ifm),nnyp(ifm),grid_g(ifm)%lat)
      end select

      ierr = nf90_close(ncid)
      if (ierr /= NF90_NOERR) then
         call ncdf_load_err(ierr)
         call fatal_error ('Error closing the file '//trim(info_table(nf)%filename)//'...' &
                          ,'wrf_coordinates','wrf_coordinates.F90'                   ) 
      end if

      select case (if_adap)
      case (-1,0)
         do k=1,nnzp(ifm)
            grid_g(ifm)%lev(k) = ztn(k,ifm)
         end do
      case (2)
         !---------------------------------------------------------------------------------!
         !   Looping over all files, and getting the minimum top pressure and the maximum  !
         ! surface pressure. The sigma-p coordinate varies in time and space, so to have   !
         ! an interpolation that will capture the full domain and yet it is fixed, we      !
         ! capture the maximum surface pressure and the minimum top pressure. Then we in-  !
         ! terpolate the fields to this grid. If the user specified the levels they want,  !
         ! then we will use that instead.                                                  !
         !---------------------------------------------------------------------------------!
         psfcmax = -huge(1.)
         ptopmin =  huge(1.)
         dimsiz  =  nnxp(ifm)*nnyp(ifm)
         do nt=1,nouttimes
            ftloop: do nf=1,nfiles
               tadd = time_address(nf,nt,ifm) 
               if (tadd == mevi_notfound .or. tadd == mevi_absent) then
                  cycle ftloop
               else
                  write (unit=*,fmt='(a,1x,2a)')                                           &
                                  '       [*] Loading time :',outtimes(nt)%gradsstamp,'...'
               end if
               ierr = nf90_open(info_table(nf)%filename,NF90_NOWRITE,ncid)
               if (ierr /= NF90_NOERR) then
                  call ncdf_load_err(ierr)
                  call fatal_error (                                                       &
                      'Error opening the file '//trim(info_table(nf)%filename)//'...'      &
                     ,'wrf_coordinates','wrf_coordinates.F90')
               end if

               ierr    = ncio_0dvar('P_TOP',.true.,tadd,ptopnow)
               ptopmin = min(ptopmin,ptopnow)

               ierr    = ncio_2dvar('PSFC',.true.,tadd,nnxp(ifm),nnyp(ifm)                 &
                                  ,scratch%tmparr21(1:dimsiz))
               psfcmax = max(psfcmax,maxval(scratch%tmparr21(1:dimsiz),dim=1))


               ierr = nf90_close(ncid)
               if (ierr /= NF90_NOERR)  then
                  call ncdf_load_err(ierr)
                  call fatal_error (                                                       &
                      'Error closing the file '//trim(info_table(nf)%filename)//'...'      &
                     ,'wrf_coordinates','wrf_coordinates.F90')
               end if
            end do ftloop
         end do
         if (psfcmax == 0.) then
            psfcmax = 101325.
            if_adap = 4
         end if
         !----- Now we translate the sigma-p into pressure levels. ------------------------!
         do k=1,nnzp(ifm)
            grid_g(ifm)%lev(k) = ptopmin + ztn(k,ifm) * (psfcmax-ptopmin)
         end do
      end select


   end do gridloop

#else
   implicit none
   call fatal_error ('Your compilation doesn''t support NetCDF so you can''t use it...'    &
                     'wrf_coordinates','wrf_coordinates.F90')
   return
#endif

end subroutine wrf_coordinates
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine load_wrf_pressure(nf,nt,ifm)
#if USE_NCDF
   use mod_netcdf,only : ncid
   use mod_ncdf_globio, only : ncio_0dvar, ncio_2dvar
   use netcdf
#endif

   use mod_grid, only : grid_g
   use an_header, only : info_table
   use mod_model, only : nnxp,nnyp,nnzp,ztn
   implicit none
   integer, intent(in) :: nf,nt,ifm

#if USE_NCDF
   integer :: ierr,x,y,z
   !---------------------------------------------------------------------------------------!
   !   Here I will be inefficent, I will keep opening and closing the files. This way we   !
   ! the wrapper can be simpler, and more flexible for other I/O formats.                  !
   !---------------------------------------------------------------------------------------!
   ierr    = nf90_open(info_table(nf)%filename,NF90_NOWRITE,ncid)
   !----- Getting the pressure at the top -------------------------------------------------!
   ierr    = ncio_0dvar('P_TOP',.true.,nt,grid_g(ifm)%top)
   !----- Getting the surface pressure ----------------------------------------------------!
   ierr    = ncio_2dvar('PSFC',.true.,nt,nnxp(ifm),nnyp(ifm),grid_g(ifm)%surf)
   !----- Finding the current profile, which will be stored at ptn variable ---------------!
   do x = 1,nnxp(ifm)
      do y= 1, nnyp(ifm)
         do z=1, nnzp(ifm)
            grid_g(ifm)%pres(x,y,z) = grid_g(ifm)%top                                      &
                                    + ztn(z,ifm) * (grid_g(ifm)%surf(x,y)- grid_g(ifm)%top)
         end do
      end do
   end do
   !----- Closing the file ----------------------------------------------------------------!
   ierr    = nf90_close(ncid)
   
#else
   call fatal_error ('Your compilation doesn''t support NetCDF so you can''t use it...'    &
                     'wrf_coordinates','wrf_coordinates.F90')
#endif

   return
end subroutine load_wrf_pressure
!==========================================================================================!
!==========================================================================================!
