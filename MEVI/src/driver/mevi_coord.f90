!==========================================================================================!
!==========================================================================================!
!  MEVI. Subroutine coordinate_driver                                                      !
!                                                                                          !
!     This is going to download the grid coordinate and populate the grid_g structure, so  !
! the output driver will have the information needed to perform any interpolation that may !
! be necessary. This depends on the input, which means that the main subroutine here will  !
! just allocate the grid structure then call the appropriate subroutine to deal with the   !
! specifics of each input format.                                                          !
!==========================================================================================!
!==========================================================================================!
subroutine coordinate_driver(intype)
   use mod_model  , only : ngrids       & ! intent(in)
                         , nnxp         & ! intent(in)
                         , nnyp         & ! intent(in)
                         , nnzp         ! ! intent(in)
   use mod_grid   , only : grid_g       & ! intent(in)
                         , alloc_grid   & ! sub-routine
                         , nullify_grid & ! sub-routine
                         , zero_grid    ! ! sub-routine
   use an_header  , only : info_table   & ! intent(in)
                         , nfiles       ! ! intent(in)
   use mod_maxdims, only : maxstr       ! ! intent(in)
   implicit none
   !----- Arguments. ----------------------------------------------------------------------!
   character(len=maxstr), intent(in) :: intype
   !----- Local variables. ----------------------------------------------------------------!
   integer                           :: ifm
   integer                           :: nf
   !---------------------------------------------------------------------------------------!


   !---------------------------------------------------------------------------------------!
   !    First thing here is to allocate the grid structure. Then we will loop through the  !
   ! grid and allocate the structure elements. We will do it only for grids that exist in  !
   ! every file (so to convert WRF files from nested grids you will probably need to run   !
   ! MEVI for each grid separately).                                                       !
   !---------------------------------------------------------------------------------------!
   allocate (grid_g(ngrids))

   gridloop: do ifm=1,ngrids
      fileloop: do nf=1,nfiles
        if (.not. info_table(nf)%avail_grid(ifm)) cycle gridloop
      end do fileloop
      !------------------------------------------------------------------------------------!
      !    Here we have three steps for safe allocation. First nullify, then allocate, and !
      ! finally we initialise the structure.                                               !
      !------------------------------------------------------------------------------------!
      call nullify_grid(grid_g(ifm))
      call alloc_grid(grid_g(ifm),nnxp(ifm),nnyp(ifm),nnzp(ifm))
      call zero_grid(grid_g(ifm))

   end do gridloop

   !---------------------------------------------------------------------------------------!
   !    Now that the structure is available, we will choose the appropriate one to load    !
   ! the data.                                                                             !
   !---------------------------------------------------------------------------------------!
   select case (trim(intype))
   case ('rams','brams')
      call fatal_error('Sorry'//trim(intype)//'input is not available yet...'              &
                      ,'coordinate_driver','mevi_coord.f90')

   case ('wrf','zwrf')
      call wrf_coordinates()

   case ('ecmwf','ncep')
      call ecmwf_coordinates()

   case ('eranat')
      call eranat_coordinates()

   case ('grads')
      call fatal_error('Sorry,'//trim(intype)//'input is not available yet...'             &
                      ,'coordinate_driver','mevi_coord.f90')

   case ('grib')
      call fatal_error('Sorry,'//trim(intype)//'input is not available yet...'             &
                      ,'coordinate_driver','mevi_coord.f90')


   case ('hdf5')
      call fatal_error('Sorry,'//trim(intype)//'input is not available yet...'             &
                      ,'coordinate_driver','mevi_coord.f90')

   end select


   !----- Adjust the area to be cropped. --------------------------------------------------!
   call mevi_cropper(intype)

   return
end subroutine coordinate_driver
!==========================================================================================!
!==========================================================================================!





!==========================================================================================!
!==========================================================================================!
!     This subroutine will make sure that the variables that control the model sub-domain  !
! are right.  This should be called after the grid coordinates are defined, so longitude   !
! and latitude variables are consistent.                                                   !
!------------------------------------------------------------------------------------------!
subroutine mevi_cropper(intype)
   use mod_model      , only : ngrids  & ! intent(in)
                             , nnxp    & ! intent(in)
                             , nnyp    & ! intent(in)
                             , nnzp    & ! intent(in)
                             , xtn     & ! intent(in)
                             , ytn     & ! intent(in)
                             , ztn     ! ! intent(in)
   use mod_ioopts     , only : lonw    & ! intent(in)
                             , lone    & ! intent(in)
                             , lats    & ! intent(in)
                             , latn    & ! intent(in)
                             , levb    & ! intent(in)
                             , levt    & ! intent(in)
                             , i1st    & ! intent(in)
                             , ilast   & ! intent(in)
                             , j1st    & ! intent(in)
                             , jlast   & ! intent(in)
                             , k1st    & ! intent(in)
                             , klast   & ! intent(in)
                             , x_360   & ! intent(in)
                             , x_cyc   & ! intent(in)
                             , y_n2s   & ! intent(in)
                             , z_t2b   ! ! intent(in)
   use mod_maxdims    , only : maxstr  ! ! intent(in)
   implicit none
   !----- Arguments. ----------------------------------------------------------------------!
   character(len=maxstr), intent(in) :: intype
   !----- Local variables. ----------------------------------------------------------------!
   integer                           :: ifm
   integer                           :: x
   integer                           :: y
   integer                           :: z
   integer                           :: itry
   integer                           :: jtry
   integer                           :: ktry
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !     Find out whether the longitude goes from -180 to 180 or from 0 to 360.  In case   !
   ! we can't determine (e.g. regional run over India), then the default is that the range !
   ! is between -180 and 180.                                                              !
   !---------------------------------------------------------------------------------------!
   select case (trim(intype))
   case ('rams','brams')
      x_360 = .false.
   case default
      if (maxval(xtn(:,1)) > 180. .and. minval(xtn(:,1)) < 0.) then
         write (unit=*,fmt='(a)')           '----------------------------------------------'
         write (unit=*,fmt='(a)')           ' Longitude range makes no sense... '
         write (unit=*,fmt='(a)')           ' It should range either between -180 and 180'
         write (unit=*,fmt='(a)')           '    or between 0 and 360.'
         write (unit=*,fmt='(a,1x,es12.5)') ' - Minimum longitude:' ,minval(xtn(:,1))
         write (unit=*,fmt='(a,1x,es12.5)') ' - Maximum longitude:' ,minval(xtn(:,1))
         write (unit=*,fmt='(a)')           '----------------------------------------------'
         call fatal_error('Longitude input is wrong!','mevi_cropper','mevi_coord.f90')
      elseif (maxval(xtn(:,1)) > 180.) then
         x_360 = .true.
      else
         x_360 = .false.
      end if
   end select
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !     Find out whether the latitude is swapped (i.e. latitude goes from north to        !
   ! south).                                                                               !
   !---------------------------------------------------------------------------------------!
   y_n2s = ytn(1,1) > ytn(2,1)
   !---------------------------------------------------------------------------------------!


   !---------------------------------------------------------------------------------------!
   !     Now we find out whether the level is going from top to bottom.  This requires us  !
   ! to check the vertical coordinates because pressure decreases with height.             !
   !---------------------------------------------------------------------------------------!
   select case (trim(intype))
   case ('ecmwf','eranat')
      z_t2b   = .true.
      levt(:) = levt(:) * 100.
      levb(:) = levb(:) * 100.
   case ('wrf')
      z_t2b   = .false.
      levt(:) = levt(:) * 100.
      levb(:) = levb(:) * 100.
   case default
      z_t2b = .false.
   end select
   !---------------------------------------------------------------------------------------!


   !---------------------------------------------------------------------------------------!
   !     Now we go and make sure that the "level" and grid coordinates are consistent and  !
   ! match.                                                                                !
   !---------------------------------------------------------------------------------------!
   do ifm=1,ngrids
      !------------------------------------------------------------------------------------! 
      !     Find the first and the last longitude to seek.  Here we should proceed         !
      ! differently depending on whether the longitude goes from 0. to 360. or from -180.  !
      ! to 180.                                                                            !
      !------------------------------------------------------------------------------------!
      if (x_360) then
         !----- Make longitude bounds between 0. and 360. ---------------------------------!
         if (lonw(ifm) <    0.) lonw(ifm) = lonw(ifm) + 360.
         if (lone(ifm) <    0.) lone(ifm) = lone(ifm) + 360.
      else
         !----- Make longitude bounds between 0. and 360. ---------------------------------!
         if (lonw(ifm) >= 180.) lonw(ifm) = lonw(ifm) - 360.
         if (lone(ifm) >= 180.) lone(ifm) = lone(ifm) - 360.
      end if

      !------------------------------------------------------------------------------------!
      !     Find the westernmost point based on lonw.                                      !
      !------------------------------------------------------------------------------------!
      itry = 1
      westloop: do x=1,nnxp(ifm)-1
         if (lonw(ifm) >= xtn(x,ifm) .and. lonw(ifm) < xtn(x+1,ifm)) then
            itry = x
            exit westloop
         end if
      end do westloop
      i1st(ifm) = min(nnxp(ifm),max(i1st(ifm),itry))
      lonw(ifm) = xtn(i1st(ifm),ifm)
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !     Find the easternmost point based on lone.                                      !
      !------------------------------------------------------------------------------------!
      itry = nnxp(ifm)
      eastloop: do x=nnxp(ifm),2,-1
         if (lone(ifm) > xtn(x-1,ifm) .and. lone(ifm) <= xtn(x,ifm)) then
            itry = x
            exit eastloop
         end if
      end do eastloop
      ilast(ifm) = max(1,min(ilast(ifm),itry))
      lone(ifm)  = xtn(ilast(ifm),ifm)
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !     Check whether this domain crosses the end of a global longitude block (e.g.,   !
      ! it crosses the date line in a domain between -180 and 180.)                        !
      !------------------------------------------------------------------------------------!
      x_cyc(ifm) = i1st(ifm) > ilast(ifm)
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !     Latitude edges, here we must check whether the data are oriented north to      !
      ! south or from south to north before doing the actual check.                        !
      !------------------------------------------------------------------------------------!
      if (y_n2s) then
         !---------------------------------------------------------------------------------!
         !     Data are aligned from North to South.                                       !
         !---------------------------------------------------------------------------------!
         !----- Find the northernmost point. ----------------------------------------------!
         jtry = 1
         northloop1: do y=1,nnyp(ifm)-1
            if (latn(ifm) <= ytn(y,ifm) .and. latn(ifm) > ytn(y+1,ifm)) then
               jtry = y
               exit northloop1
            end if
         end do northloop1
         j1st(ifm) = min(nnyp(ifm),max(j1st(ifm),jtry))
         latn(ifm) = ytn(j1st(ifm),ifm)

         !----- Find the southernmost point. ----------------------------------------------!
         jtry = nnyp(ifm)
         southloop1: do y=nnyp(ifm),2,-1
            if (lats(ifm) < ytn(y-1,ifm) .and. lats(ifm) >= ytn(y,ifm)) then
               jtry=y
               exit southloop1
            end if
         end do southloop1
         jlast(ifm) = max(1,min(jlast(ifm),jtry))
         lats(ifm)  = ytn(jlast(ifm),ifm)

      else
         !---------------------------------------------------------------------------------!
         !     Data are aligned from South to North.                                       !
         !---------------------------------------------------------------------------------!
         !----- Find the southernmost point. ----------------------------------------------!
         jtry = 1
         southloop2: do y=1,nnyp(ifm)-1
            if (lats(ifm) >= ytn(y,ifm) .and. lats(ifm) < ytn(y+1,ifm)) then
               jtry = y
               exit southloop2
            end if
         end do southloop2
         j1st(ifm) = min(nnyp(ifm),max(j1st(ifm),jtry))
         lats(ifm) = ytn(j1st(ifm),ifm)
         !---------------------------------------------------------------------------------!



         !---------------------------------------------------------------------------------!
         !     Find the northernmost point based on latn.                                  !
         !---------------------------------------------------------------------------------!
         jtry = nnyp(ifm)
         northloop2: do y=nnyp(ifm),2,-1
            if (latn(ifm) > ytn(y-1,ifm) .and. latn(ifm) <= ytn(y,ifm)) then
               jtry = y
               exit northloop2
            end if
         end do northloop2
         jlast(ifm) = max(1,min(jlast(ifm),jtry))
         latn(ifm)  = ytn(jlast(ifm),ifm)
         !---------------------------------------------------------------------------------!
      end if
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !     Vertical edges, here we must check whether the data are oriented top to bottom !
      ! or bottom to top before doing the actual check.                                    !
      !------------------------------------------------------------------------------------!
      if (z_t2b) then
         !---------------------------------------------------------------------------------!
         !     Data are aligned from top to bottom.                                        !
         !---------------------------------------------------------------------------------!
         !----- Find the topmost point. ---------------------------------------------------!
         ktry = 1
         toploop1: do z=1,nnzp(ifm)-1
            if (levt(ifm) <= ztn(z,ifm) .and. levt(ifm) > ztn(z+1,ifm)) then
               ktry = z
               exit toploop1
            end if
         end do toploop1
         k1st(ifm) = min(nnzp(ifm),max(k1st(ifm),ktry))
         levt(ifm) = ztn(k1st(ifm),ifm)

         !----- Find the southermost point. -----------------------------------------------!
         ktry = nnzp(ifm)
         bottomloop1: do z=nnzp(ifm),2,-1
            if (levb(ifm) < ztn(z-1,ifm) .and. levb(ifm) >= ztn(z,ifm)) then
               ktry = z
               exit bottomloop1
            end if
         end do bottomloop1
         klast(ifm) = max(1,min(klast(ifm),ktry))
         levb(ifm)  = ztn(klast(ifm),ifm)

      else
         !---------------------------------------------------------------------------------!
         !     Data are aligned from bottom to top.                                        !
         !---------------------------------------------------------------------------------!
         !----- Find the bottommost point. ------------------------------------------------!
         ktry = 1
         bottomloop2: do z=1,nnzp(ifm)-1
            if (levb(ifm) >= ztn(z,ifm) .and. levb(ifm) < ztn(z+1,ifm)) then
               ktry = z
               exit bottomloop2
            end if
         end do bottomloop2
         k1st(ifm) = min(nnzp(ifm),max(k1st(ifm),ktry))
         levb(ifm) = ztn(k1st(ifm),ifm)
         !---------------------------------------------------------------------------------!



         !---------------------------------------------------------------------------------!
         !     Find the topmost point based on levt.                                       !
         !---------------------------------------------------------------------------------!
         ktry = nnzp(ifm)
         toploop2: do z=nnzp(ifm),2,-1
            if (levt(ifm) > ztn(z-1,ifm) .and. levt(ifm) <= ztn(z,ifm)) then
               ktry = z
               exit toploop2
            end if
         end do toploop2
         klast(ifm) = max(1,min(klast(ifm),ktry))
         levt(ifm)  = ztn(klast(ifm),ifm)
         !---------------------------------------------------------------------------------!
      end if
      !------------------------------------------------------------------------------------!

      write(unit=*,fmt='(a)'                       ) ' '
      write(unit=*,fmt='(64a)'                     ) ('-',z=1,64)
      write(unit=*,fmt='(a,1x,i4,a)'               ) ' Crop info for grid',ifm,':'
      write(unit=*,fmt='(a,1x,a)'                  ) ' Input format           :'           &
                                                                              ,trim(intype)
      write(unit=*,fmt='(a,1x,l1)'                 ) ' Longitude from 0 to 360:',x_360
      write(unit=*,fmt='(a,1x,l1)'                 ) ' Cyclic longitude       :',x_cyc(ifm)
      write(unit=*,fmt='(a,1x,l1)'                 ) ' North to south         :',y_n2s
      write(unit=*,fmt='(a,1x,l1)'                 ) ' Top to bottom          :',z_t2b
      write(unit=*,fmt='(64a)'                     ) ('-',z=1,64)
      write(unit=*,fmt='(4(a,1x),a)'               ) '    VARIABLE','    MIN.GRID'         &
                                                    ,'    MAX.GRID','   MIN.VALUE'         &
                                                    ,'   MAX.VALUE'
      write(unit=*,fmt='(64a)'                     ) ('-',z=1,64)
      write(unit=*,fmt='(a,2(1x,i12),2(1x,es12.5))') '   LONGITUDE',i1st(ifm),ilast(ifm)   &
                                                                   ,lonw(ifm),lone(ifm)
      write(unit=*,fmt='(a,2(1x,i12),2(1x,es12.5))') '    LATITUDE',j1st(ifm),jlast(ifm)   &
                                                                   ,lats(ifm),latn(ifm)
      write(unit=*,fmt='(a,2(1x,i12),2(1x,es12.5))') '    VERTICAL',k1st(ifm),klast(ifm)   &
                                                                   ,levb(ifm),levt(ifm)
      write(unit=*,fmt='(64a)'                     ) ('-',z=1,64)
      write(unit=*,fmt='(a)'                       ) ' '

      if (x_cyc(ifm)) then
         call fatal_error('MEVI is currently unable to do cyclic domains...'               &
                         ,'mevi_cropper','mevi_coord.f90')
      end if

   end do

   return
end subroutine mevi_cropper
!==========================================================================================!
!==========================================================================================!
