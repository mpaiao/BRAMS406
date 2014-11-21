!==========================================================================================!
!==========================================================================================!
!  MEVI. Subroutine mevi_driver                                                            !
!                                                                                          !
!     This is the main MEVI driver, this is actually a wrapper for the basic steps of data !
! conversion, namely the data reading, the necessary calculation, such as horizontal and/or!
! vertical interpolation and diagnostic value calculation and writing the output. The      !
! decision on how to do each step is made upon checking the namelist input.                !
!==========================================================================================!
!==========================================================================================!
subroutine mevi_driver()

   use mod_ioopts, only:        &
           intype               & ! intent(in) - Input type
          ,inpref               & ! intent(in) - Input prefix
          ,outtype              & ! intent(in) - Output type
          ,outpref              ! ! intent(in) - Output prefix

   use mod_model , only:        &
           vfmfrmt              ! ! intent(in) - Header format 

   implicit none


   !---------------------------------------------------------------------------------------!
   ! STEP 1. Getting the variable table. This depends on whether we are using RAMS, WRF,   !
   !         GrADS, generic netcdf, or generic hdf5.                                       !
   !---------------------------------------------------------------------------------------!
   select case (trim(intype))
   case ('rams','brams')
      !----- Case a. RAMS. Check whether the data is in vfm or hdf5 and load the table ----!
      if (vfmfrmt) then
         call rams_fill_infotable_vfm(inpref)
      else
         write (unit=*,fmt='(3(a,1x))')                                                    &
               'Sorry,',trim(intype),'with hdf5 is not available yet...'
         return
      end if

   case ('wrf')
      call wrf_fill_infotable(inpref)

   case ('zwrf')
      call zwrf_fill_infotable(inpref)

   case ('ecmwf')
      call ecmwf_fill_infotable(inpref)

   case ('eranat')
      call eranat_fill_infotable(inpref)

   case ('ncep')
      call ncep_fill_infotable(inpref)

   case ('grads')
      write (unit=*,fmt='(3(a,1x))') 'Sorry,',trim(intype),'input is not available yet...'
      return
   case ('grib')
      write (unit=*,fmt='(3(a,1x))') 'Sorry,',trim(intype),'input is not available yet...'
      return
   case ('hdf5')
      write (unit=*,fmt='(3(a,1x))') 'Sorry,',trim(intype),'input is not available yet...'
      return
   case default
      call fatal_error('Invalid intype '//trim(intype)//'!!! Can''t move on.'              &
                      ,'mevi_driver','mevi_driver.f90')
   end select
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !  STEP 2. Mapping the input variables.                                                 !
   !---------------------------------------------------------------------------------------!
   call map_input_vars()
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !  STEP 3. Get the native coordinate information and store at grid_g structure.         !
   !---------------------------------------------------------------------------------------!
   call coordinate_driver(intype)
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !  STEP 4. Loading the data into the scratch arrays and writing into the desired output !
   !---------------------------------------------------------------------------------------!
   select case (trim(outtype))
   case ('vis5d')
      call v5d_output(intype,outpref)
   case ('ralph2')
      call ralph2_output(intype,outpref)
   case ('netcdf')
      write (unit=*,fmt='(3(a,1x))') 'Sorry,',trim(outtype),'output is not available yet...'
   case ('hdf5')
      write (unit=*,fmt='(3(a,1x))') 'Sorry,',trim(outtype),'output is not available yet...'
   case ('grads')
      write (unit=*,fmt='(3(a,1x))') 'Sorry,',trim(outtype),'output is not available yet...'
   case ('arl')
      write (unit=*,fmt='(3(a,1x))') 'Sorry,',trim(outtype),'output is not available yet...'
   end select
   !---------------------------------------------------------------------------------------!

   return
end subroutine mevi_driver
!==========================================================================================!
!==========================================================================================!
