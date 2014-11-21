!==========================================================================================!
!==========================================================================================!
! MEVI. wcio.F90  This is the equivalent of rcio for WRF, so it reads the global           !
!                 attributes and the dimensions. It will read and store only the variables !
!                 that are needed for conversion into the output formats.                  !
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine commio_wrf(ngrid,ntimes)
   use mod_model      , only : ngrids,nzg,nzs,nclouds,nwave,naddsc,npatch,polelon,polelat  &
                              ,l2ndlat,centlon,centlat,nnxp,nnyp,nnzp,zero_time,ihtran     &
                              ,deltaxn,deltayn,slz,ztn,zmn,dztn,dzmn,if_adap,zero_time     &
                              ,this_time,xtn,ytn
   use mod_scratch    , only : scratch, alloc_scratch
   use mod_maxdims    , only : maxtimes
#if USE_NCDF
   use netcdf
   use mod_ioopts     , only : missflg_int
   use mod_netcdf     , only : idtimes,idnnxp,idnnyp,idnnzp,idnzg,idnpatch,idnclouds       &
                              ,idnnxpst,idnnypst,idnnzpst,idtimelen
   use mod_ncdf_globio, only : ncio_glo,ncio_glo_sca, ncio_dim, ncio_1dvar,ncio_wrf_time   &
                              ,ncdf_load_err,ncio_2dvar
#endif

   implicit none
   integer          , intent(out)      :: ngrid   ! Grid ID regarding this variable.
   integer          , intent(out)      :: ntimes  ! Grid ID regarding this variable.
   !----- External functions for time handling --------------------------------------------!
   integer          , external         :: julday,julday1583,v5d_datestamp,v5d_timestamp
   character(len=3) , external         :: monchar
   real             , external         :: day_fraction
   character(len=17), external         :: grads_dtstamp
   character(len=19), external         :: mevi_dtstamp

   !----- Local variables -----------------------------------------------------------------!
   integer                             :: curryear,currdoy,it,s,t
   real   , dimension(maxtimes)        :: gmttime
   integer, dimension(maxtimes)        :: years,doys
   integer                             :: timelength
!------ Giving the preprocessor the option to entirely skip this routine ------------------!
#if USE_NCDF

   integer                      :: ierr
   character(len=NF90_MAX_NAME) :: dumchar
   integer                      :: mapproj
   integer                      :: dummyint



   ierr   = ncio_glo_sca ('GRID_ID', .true. , intout = ngrid)
   ngrids = ngrid !---- This is because we will read just this grid now -------------------!


   !----- Retrieving the main dimensions --------------------------------------------------!
   ierr   = ncio_dim   ('Time'                  , .true.  ,ntimes         ,idtimes   )
   ierr   = ncio_dim   ('DateStrLen'            , .true.  ,timelength     ,idtimelen )
   ierr   = ncio_dim   ('west_east'             , .true.  ,nnxp(ngrid)    ,idnnxp    )
   ierr   = ncio_dim   ('south_north'           , .true.  ,nnyp(ngrid)    ,idnnyp    )
   ierr   = ncio_dim   ('bottom_top'            , .true.  ,nnzp(ngrid)    ,idnnzp    )
   !---------------------------------------------------------------------------------------!



   !----- Retrieving the grid information -------------------------------------------------!
   ierr   = ncio_glo_sca ('MAP_PROJ', .true. , intout = mapproj )
   select case (mapproj)
   case (0) !---- Lat-Lon -----------------------------------------------------------------!
      ihtran = 0

      ierr = ncio_glo_sca('POLE_LON'              , .false., realout = polelon )
      if (ierr /= NF90_NOERR) then
         ierr   = ncio_2dvar ('XLONG'  ,.true. , 1, nnxp(ngrid),1,xtn(:,ngrid))
         polelon = xtn(1,ngrid)
      end if
      ierr = ncio_glo_sca('POLE_LAT'              , .false., realout = polelat )
      if (ierr /= NF90_NOERR) then
         ierr   = ncio_2dvar ('XLAT'   ,.true. , 1, 1,nnyp(ngrid),ytn(:,ngrid))
         polelat = ytn(1,ngrid)
      end if
   case (2) !----- Polar-stereographic ----------------------------------------------------!
      ihtran = 1
      ierr = ncio_glo_sca('STAND_LON'             , .true. , realout = polelon )
      ierr = ncio_glo_sca('TRUELAT1'              , .true. , realout = polelat )
      l2ndlat = 90. ! Any junk is fine, it won't be used
   case (6) !----- Global lon-lat (think it's unecessary to split between 0 and 6...) -----!
      ihtran = 0
      polelon = 0.
      polelat = 90.
      l2ndlat = 90.
   case (1) !----- Lambert conformal ------------------------------------------------------!
      ihtran = 2
      ierr = ncio_glo_sca('STAND_LON'             , .true. , realout = polelon )
      ierr = ncio_glo_sca('TRUELAT1'              , .true. , realout = polelat )
      ierr = ncio_glo_sca('TRUELAT2'              , .true. , realout = l2ndlat )
   case (3) !----- Not implemented yet... -------------------------------------------------!
      call fatal_error('Sorry, this map projection is not supported yet...'                &
                      ,'commio_wrf','wcio.F90')
   case default 
      call fatal_error('Invalid map projection...','commio_wrf','wcio.F90')
   end select 
   !---------------------------------------------------------------------------------------!



   !------ Retrieving the other global attributes -----------------------------------------!
   ierr   = ncio_glo   ('DX'                    , .true. ,realout = deltaxn(ngrid) )
   ierr   = ncio_glo   ('DY'                    , .true. ,realout = deltayn(ngrid) )
   ierr   = ncio_glo   ('CEN_LON'               , .true. ,realout = centlon(ngrid) )
   ierr   = ncio_glo   ('CEN_LAT'               , .true. ,realout = centlat(ngrid) )
   !---------------------------------------------------------------------------------------!


   !----- Retrieving the times ------------------------------------------------------------!
   ierr   = ncio_wrf_time  ('Times'                 , .true. ,ntimes,timelength,this_time)


   !----- Retrieving the initial time. If not available use the first time ----------------!
   ierr   = ncio_glo   ('SIMULATION_START_DATE' , .true. , charout = dumchar  )
   read(dumchar,fmt='(i4.4,5(1x,i2.2))')  zero_time%year, zero_time%month, zero_time%day   &
                                        , zero_time%hour, zero_time%minu , zero_time%seco

   zero_time%doy        = julday (zero_time%month,zero_time%day,zero_time%year)
   zero_time%mmm        = monchar(zero_time%month)
   zero_time%fracday    = day_fraction(zero_time%hour,zero_time%minu,zero_time%seco)
   zero_time%yyyyddd    = v5d_datestamp(zero_time%year,zero_time%doy)
   zero_time%hhmmss     = v5d_timestamp(zero_time%hour,zero_time%minu,zero_time%seco)
   zero_time%gradsstamp = grads_dtstamp(zero_time%year,zero_time%mmm,zero_time%day         &
                                       ,zero_time%hour,zero_time%minu)
   zero_time%timestr    = mevi_dtstamp(zero_time%year,zero_time%month,zero_time%day        &
                                      ,zero_time%hour,zero_time%minu,zero_time%seco)
   zero_time%elapsed    = dble(julday1583(zero_time%month,zero_time%day,zero_time%year))   &
                        + dble(zero_time%fracday)


   !---------------------------------------------------------------------------------------!
   !    The next three variables may not be in the header. If that's the case, set them to !
   ! one.                                                                                  !
   !---------------------------------------------------------------------------------------!
   ierr   = ncio_dim   ('soil_layers_stag'      , .false.  ,nzg            ,idnzg    )
   if (ierr /= NF90_NOERR) then
      nzg   = 1
      idnzg = -1 ! Nowhere
   end if
   ierr   = ncio_dim   ('land_cat_stag'         , .false. ,npatch         ,idnpatch  )
   if (ierr /= NF90_NOERR) then
      npatch  = 1
      idnpatch = -1 ! Nowhere
   end if
   ierr   = ncio_dim   ('ensemble_stag'         , .false. ,nclouds        ,idnclouds  )
   if (ierr /= NF90_NOERR) then
      nclouds  = 1
      idnpatch = -1 ! Nowhere
   end if


   !----- We only need the ID for staggered variables -------------------------------------!
   ierr   = ncio_dim   ('west_east_stag'        , .true. ,dummyint       ,idnnxpst   )
   ierr   = ncio_dim   ('south_north_stag'      , .true. ,dummyint       ,idnnypst   )
   ierr   = ncio_dim   ('bottom_top_stag'       , .true. ,dummyint       ,idnnzpst   )
   !----- WRF doesn't have temporary water/snow, or wavelength, assigning ones. -----------!
   nzs     = 1
   nwave   = 1

   !---------------------------------------------------------------------------------------!
   !    Now that I have the grid and dimension information loaded, I will allocate the     !
   ! scratch structure                                                                     !
   !---------------------------------------------------------------------------------------!
   call alloc_scratch(ngrid,ngrid)

   !----- WRF vertical coordinate is the terrain-following pressure coordinate ------------!
   if_adap = 2
   !---------------------------------------------------------------------------------------!
   do it=1,ntimes
      ierr   = ncio_1dvar ('ZS'    ,.true. , it, nzg           ,scratch%tmparr11   )
      ierr   = ncio_1dvar ('ZNU'   ,.true. , it, nnzp(ngrid)   ,ztn(:,ngrid)       )
      ierr   = ncio_1dvar ('ZNW'   ,.true. , it, nnzp(ngrid)+1 ,zmn(:,ngrid)       )
      !------------------------------------------------------------------------------------!
      !  The next variables may not be there.                                              !
      !------------------------------------------------------------------------------------!
      ierr   = ncio_1dvar ('DN'    ,.false. , it, nnzp(ngrid)   ,dztn(:,ngrid)     )
      if (ierr /= NF90_NOERR) dztn = 0.
      ierr   = ncio_1dvar ('DNW'   ,.false. , it, nnzp(ngrid)   ,dzmn(:,ngrid)     )
      if (ierr /= NF90_NOERR) dzmn = 0.

      !------------------------------------------------------------------------------------!
      !     WRF soil is defined differently than RAMS, which we will use as standard.      !
      ! Levels should be negative, going from the deepest layer to the shallowest.         !
      !------------------------------------------------------------------------------------!
      do s=nzg,1,-1
         slz(s)=-scratch%tmparr11(nzg-s+1)
      end do
   end do



!------------------------------------------------------------------------------------------!
!     If the user disabled netCDF, then this routine should never be called. If that       !
! happens, crash it!                                                                       !
!------------------------------------------------------------------------------------------!
#else

   ngrid  = missflg_int
   ntimes = missflg_int
   call fatal_error('NetCDF libraries are required to use netCDF files...'                 &
                   ,'commio_wrf','wcio.F90')
#endif

   return
end subroutine commio_wrf
!==========================================================================================!
!==========================================================================================!
