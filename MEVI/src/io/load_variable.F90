!==========================================================================================!
!==========================================================================================!
!  MEVI. load_variable.F90                                                                 !
!                                                                                          !
!     This subroutine load the variable based on the input type and variable type. The     !
! variable will be stored at scratch%outarr?d, where ? is the dimension (excluding time).  !
!==========================================================================================!
!==========================================================================================!
subroutine load_variable(intype,nf,nt,vadd,nxpo,nypo,offx,offy,stridex,stridey)

   use mod_maxdims, only : maxstr
   implicit none

   character(len=maxstr), intent(in) :: intype
   integer              , intent(in) :: nf,nt,vadd
   integer              , intent(in) :: nxpo,nypo
   integer              , intent(in) :: offx,offy
   integer              , intent(in) :: stridex,stridey

   !---------------------------------------------------------------------------------------!
   !    We will choose the appropriate subroutine to load the data.                        !
   !---------------------------------------------------------------------------------------!
   select case (trim(intype))
   case ('rams','brams')
      call fatal_error('Sorry'//trim(intype)//'input is not available yet...'              &
                      ,'load_variable','load_variable.F90')

   case ('wrf','zwrf')
      call wrf_loadvar(nf,nt,vadd,nxpo,nypo,offx,offy,stridex,stridey)

   case ('ecmwf','ncep')
      call ecmwf_loadvar(nf,nt,vadd,nxpo,nypo,offx,offy,stridex,stridey)

   case ('eranat')
      call eranat_loadvar(nf,vadd,nxpo,nypo,offx,offy,stridex,stridey)

   case ('grads')
      call fatal_error('Sorry'//trim(intype)//'input is not available yet...'              &
                      ,'load_variable','load_variable.F90')

   case ('grib')
      call fatal_error('Sorry'//trim(intype)//'input is not available yet...'              &
                      ,'load_variable','load_variable.F90')

   case ('netcdf')
      call fatal_error('Sorry'//trim(intype)//'input is not available yet...'              &
                      ,'load_variable','load_variable.F90')

   case ('hdf5')
      call fatal_error('Sorry'//trim(intype)//'input is not available yet...'              &
                      ,'load_variable','load_variable.F90')

   end select

end subroutine load_variable
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine wrf_loadvar(nf,nt,vadd,nxpo,nypo,offx,offy,stridex,stridey)

#if USE_NCDF
   use mod_netcdf     , only : ncid
   use netcdf
   use mod_ncdf_globio, only : ncio_2dvar, ncio_3dvar, ncio_4dvar
#endif

   use an_header  , only : info_table
   use mod_scratch, only : scratch
   use mod_ioopts , only : mevi_libvar
   use mod_maxdims, only : maxstr
   implicit none
   !----- Input variables -----------------------------------------------------------------!
   integer, intent(in) :: nf,nt,vadd,nxpo,nypo,offx,offy,stridex,stridey
   !----- Local variables -----------------------------------------------------------------!
   character(len=maxstr) :: vname
   integer               :: ierr,dim3,dim4,off1,off2,off3

   !----- Initialising offsets, if the variable is staggered, the offset changes... -------!
   off1 = offx
   off2 = offy
   off3 = 0

#if USE_NCDF
   !----- Opening the file ----------------------------------------------------------------!
   ierr = nf90_open(info_table(nf)%filename,NF90_NOWRITE,ncid)
   
   !----- Retrieving the variable depending on its dimensions -----------------------------!
   if (vadd /= mevi_libvar) then 
      vname = info_table(nf)%varname(vadd)
      !----- Here we decide which function we should use to download the data -------------!
      select case (info_table(nf)%idim_type(vadd))
      case (22)
         if (info_table(nf)%stagger(1,vadd)) off1=off1+1
         if (info_table(nf)%stagger(2,vadd)) off2=off2+1
         ierr = ncio_2dvar(vname,.true.,nt,nxpo,nypo,scratch%outarr2d,offin1=off1          &
                         ,offin2=off2,strin1=stridex,strin2=stridey)
      case (33)
         if (info_table(nf)%stagger(1,vadd)) off1=off1+1
         if (info_table(nf)%stagger(2,vadd)) off2=off2+1
         if (info_table(nf)%stagger(3,vadd)) off3=off3+1
         dim3 = info_table(nf)%dims(3,vadd)
         ierr = ncio_3dvar(vname,.true.,nt,nxpo,nypo,dim3,scratch%outarr3d,offin1=off1     &
                         ,offin2=off2,offin3=off3,strin1=stridex,strin2=stridey)
      case (34,35,36,37)
         if (info_table(nf)%stagger(1,vadd)) off1=off1+1
         if (info_table(nf)%stagger(2,vadd)) off2=off2+1
         dim3 = info_table(nf)%dims(3,vadd)
         ierr = ncio_3dvar(vname,.true.,nt,nxpo,nypo,dim3,scratch%outarr3d,offin1=off1     &
                         ,offin2=off2,strin1=stridex,strin2=stridey)
      case (46)
         if (info_table(nf)%stagger(1,vadd)) off1=off1+1
         if (info_table(nf)%stagger(2,vadd)) off2=off2+1
         if (info_table(nf)%stagger(3,vadd)) off3=off3+1
         dim3 = info_table(nf)%dims(3,vadd)
         dim4 = info_table(nf)%dims(4,vadd)
         ierr = ncio_4dvar(vname,.true.,nt,nxpo,nypo,dim3,dim4,scratch%outarr4d            &
                          ,offin1=off1,offin2=off2,offin3=off3,strin1=stridex              &
                          ,strin2=stridey)
      case (47,48)
         if (info_table(nf)%stagger(1,vadd)) off1=off1+1
         if (info_table(nf)%stagger(2,vadd)) off2=off2+1
         dim3 = info_table(nf)%dims(3,vadd)
         dim4 = info_table(nf)%dims(4,vadd)
         ierr = ncio_4dvar(vname,.true.,nt,nxpo,nypo,dim3,dim4,scratch%outarr4d            &
                         ,offin1=off1,offin2=off2,strin1=stridex,strin2=stridey)
      end select

   else
      call fatal_error ('Sorry, WRF library is not ready yet...'                           &
                       ,'wrf_loadvar','load_variable.F90')
      ! In the future library variables will be downloaded here... 
      !   call wrf_varlib()
   end if

   !----- closing the file ----------------------------------------------------------------!
   ierr = nf90_close(ncid)

#else
   call fatal_error ('Your compilation doesn''t support NetCDF so you can''t use it...'    &
                     'wrf_loadvar','load_variable.F90')

#endif
   return
end subroutine wrf_loadvar
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine ecmwf_loadvar(nf,nt,vadd,nxpo,nypo,offx,offy,stridex,stridey)

#if USE_NCDF
   use mod_netcdf     , only : ncid
   use netcdf
   use mod_ncdf_globio, only : ncio_2dvar, ncio_3dvar, ncio_2dshort,ncio_3dshort
#endif

   use an_header  , only : info_table
   use mod_scratch, only : scratch
   use mod_ioopts , only : mevi_libvar
   use mod_maxdims, only : maxstr
   implicit none
   !----- Input variables -----------------------------------------------------------------!
   integer, intent(in) :: nf,nt,vadd,nxpo,nypo,offx,offy,stridex,stridey
   !----- Local variables -----------------------------------------------------------------!
   character(len=maxstr) :: vname
   integer               :: ierr,dim3,dim4,off1,off2,off3

   !----- Initialising offsets, if the variable is staggered, the offset changes... -------!
   off1 = offx
   off2 = offy
   off3 = 0

#if USE_NCDF
   !----- Opening the file ----------------------------------------------------------------!
   ierr = nf90_open(info_table(nf)%filename,NF90_NOWRITE,ncid)
   
   !----- Retrieving the variable depending on its dimensions -----------------------------!
   if (vadd /= mevi_libvar) then 
      vname = info_table(nf)%varname(vadd)
      !----- Here we decide which function we should use to download the data -------------!
      select case (info_table(nf)%idim_type(vadd))
      case (22)
         if (info_table(nf)%stagger(1,vadd)) off1=off1+1
         if (info_table(nf)%stagger(2,vadd)) off2=off2+1
         ierr = ncio_2dvar(vname,.true.,nt,nxpo,nypo,scratch%outarr2d,offin1=off1          &
                            ,offin2=off2,strin1=stridex,strin2=stridey)
      case (33)
         if (info_table(nf)%stagger(1,vadd)) off1=off1+1
         if (info_table(nf)%stagger(2,vadd)) off2=off2+1
         if (info_table(nf)%stagger(3,vadd)) off3=off3+1
         dim3 = info_table(nf)%dims(3,vadd)
         ierr = ncio_3dvar(vname,.true.,nt,nxpo,nypo,dim3,scratch%outarr3d,offin1=off1     &
                         ,offin2=off2,offin3=off3,strin1=stridex,strin2=stridey)
      case (62)
         if (info_table(nf)%stagger(1,vadd)) off1=off1+1
         if (info_table(nf)%stagger(2,vadd)) off2=off2+1
         ierr = ncio_2dshort(vname,.true.,nt,nxpo,nypo,scratch%outarr2d,offin1=off1        &
                            ,offin2=off2,strin1=stridex,strin2=stridey)
      case (63)
         if (info_table(nf)%stagger(1,vadd)) off1=off1+1
         if (info_table(nf)%stagger(2,vadd)) off2=off2+1
         if (info_table(nf)%stagger(3,vadd)) off3=off3+1
         dim3 = info_table(nf)%dims(3,vadd)
         ierr = ncio_3dshort(vname,.true.,nt,nxpo,nypo,dim3,scratch%outarr3d,offin1=off1   &
                         ,offin2=off2,offin3=off3,strin1=stridex,strin2=stridey)
      end select

   else
      call fatal_error ('Sorry, ECMWF library is not ready yet...'                         &
                       ,'ecmwf_loadvar','load_variable.F90')
      ! In the future library variables will be downloaded here... 
      !   call wrf_varlib()
   end if

   !----- closing the file ----------------------------------------------------------------!
   ierr = nf90_close(ncid)

#else
   call fatal_error ('Your compilation doesn''t support NetCDF so you can''t use it...'    &
                     'ecmwf_loadvar','load_variable.F90')

#endif
   return
end subroutine ecmwf_loadvar
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine eranat_loadvar(nf,vadd,nxpo,nypo,offx,offy,stridex,stridey)

#if USE_NCDF
   use mod_netcdf     , only : ncid
   use netcdf
   use mod_ncdf_globio, only : ncio_2dvarnotime   & ! function
                             , ncio_3dvarnotime   ! ! function
#endif

   use an_header      , only : info_table         ! ! intent(in)
   use mod_scratch    , only : scratch            ! ! structure, intent(inout)
   use mod_ioopts     , only : mevi_libvar        ! ! subroutine
   use mod_maxdims    , only : maxstr             ! ! intent(in)
   implicit none
   !----- Input variables -----------------------------------------------------------------!
   integer              , intent(in) :: nf
   integer              , intent(in) :: vadd
   integer              , intent(in) :: nxpo
   integer              , intent(in) :: nypo
   integer              , intent(in) :: offx
   integer              , intent(in) :: offy
   integer              , intent(in) :: stridex
   integer              , intent(in) :: stridey
   !----- Local variables -----------------------------------------------------------------!
   character(len=maxstr)             :: vname
   integer                           :: ierr
   integer                           :: dim3
   integer                           :: dim4
   integer                           :: off1
   integer                           :: off2
   integer                           :: off3
   !---------------------------------------------------------------------------------------!


   !---------------------------------------------------------------------------------------!
   !     Initialise offsets with default values.  In case the variable is staggered, then  !
   ! we m offset changes...                                                                !
   !---------------------------------------------------------------------------------------!
   off1 = offx
   off2 = offy
   off3 = 0
   !---------------------------------------------------------------------------------------!



#if USE_NCDF
   !----- Open the file. ------------------------------------------------------------------!
   ierr = nf90_open(info_table(nf)%filename,NF90_NOWRITE,ncid)
   

   !----- Retrieve the variable depending on its dimensions -------------------------------!
   if (vadd /= mevi_libvar) then 
      vname = info_table(nf)%varname(vadd)
      !----- Here we decide which function we should use to download the data. ------------!
      select case (info_table(nf)%idim_type(vadd))
      case (22)
         if (info_table(nf)%stagger(1,vadd)) off1=off1+1
         if (info_table(nf)%stagger(2,vadd)) off2=off2+1
         ierr = ncio_2dvarnotime(vname,.true.,nxpo,nypo,scratch%outarr2d,offin1=off1       &
                                ,offin2=off2,strin1=stridex,strin2=stridey)
      case (33)
         if (info_table(nf)%stagger(1,vadd)) off1=off1+1
         if (info_table(nf)%stagger(2,vadd)) off2=off2+1
         if (info_table(nf)%stagger(3,vadd)) off3=off3+1
         dim3 = info_table(nf)%dims(3,vadd)
         ierr = ncio_3dvarnotime(vname,.true.,nxpo,nypo,dim3,scratch%outarr3d,offin1=off1  &
                                ,offin2=off2,offin3=off3,strin1=stridex,strin2=stridey)
      end select

   else
      call fatal_error ('Sorry, ECMWF interim library is not ready yet...'                 &
                       ,'ecmwf_loadvar','load_variable.F90')
      !----- In the future library variables will be downloaded here...  ------------------!
      !   call ernat_varlib()
   end if

   !----- Close the file. -----------------------------------------------------------------!
   ierr = nf90_close(ncid)

#else
   call fatal_error ('Your compilation doesn''t support NetCDF so you can''t use it...'    &
                     'eranat_loadvar','load_variable.F90')

#endif
   return
end subroutine eranat_loadvar
!==========================================================================================!
!==========================================================================================!
