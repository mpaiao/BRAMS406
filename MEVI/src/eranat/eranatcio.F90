!==========================================================================================!
!==========================================================================================!
! MEVI. eranatcio.F90  This is the equivalent of rcio for ECMWF with full resolution       !
!                     (native coordinates, the files that we can only get from CISL        !
!                     server), so it reads the global attributes and the dimensions.  It   !
!                     will read and store only the variables that are needed for           !
!                     conversion into the output formats.                                  !
!==========================================================================================!
!==========================================================================================!
subroutine commio_eranat(ngrid,ntimes)
   use mod_model      , only : ngrids           & ! intent(inout)
                             , nzg              & ! intent(inout)
                             , nzs              & ! intent(inout)
                             , nclouds          & ! intent(inout)
                             , nwave            & ! intent(inout)
                             , naddsc           & ! intent(inout)
                             , npatch           & ! intent(inout)
                             , polelon          & ! intent(out)
                             , polelat          & ! intent(out)
                             , l2ndlat          & ! intent(inout)
                             , centlon          & ! intent(inout)
                             , centlat          & ! intent(inout)
                             , nnxp             & ! intent(inout)
                             , nnyp             & ! intent(inout)
                             , nnzp             & ! intent(inout)
                             , ihtran           & ! intent(out)
                             , deltaxn          & ! intent(inout)
                             , deltayn          & ! intent(inout)
                             , slz              & ! intent(inout)
                             , ztn              & ! intent(inout)
                             , zmn              & ! intent(inout)
                             , dztn             & ! intent(inout)
                             , dzmn             & ! intent(inout)
                             , if_adap          & ! intent(inout)
                             , zero_time        & ! intent(inout)
                             , this_time        & ! intent(inout)
                             , xtn              & ! intent(inout)
                             , ytn              & ! intent(inout)
                             , atn              ! ! intent(inout)
   use mod_scratch    , only : scratch          & ! intent(inout)
                             , alloc_scratch    ! ! sub-routine
   use mod_maxdims    , only : maxtimes         & ! intent(in)
                             , maxgrds          ! ! intent(in)
#if USE_NCDF
   use netcdf
   use mod_ioopts     , only : missflg_int      ! ! intent(inout)
   use mod_netcdf     , only : idtimes          & ! intent(inout)
                             , idnnxp           & ! intent(inout)
                             , idnnyp           & ! intent(inout)
                             , idnnzp           & ! intent(inout)
                             , idnzg            & ! intent(inout)
                             , idnpatch         & ! intent(inout)
                             , idnclouds        & ! intent(inout)
                             , idnnxpst         & ! intent(inout)
                             , idnnypst         & ! intent(inout)
                             , idnnzpst         & ! intent(inout)
                             , idtimelen        ! ! intent(inout)
   use mod_ncdf_globio, only : ncio_glo         & ! subroutine
                             , ncio_glo_sca     & ! subroutine
                             , ncio_dim         & ! subroutine
                             , ncio_1dnotime    & ! subroutine
                             , ncio_eranat_time ! ! subroutine
   use rconstants     , only : spcon            & ! intent(in)
                             , pio180           ! ! intent(in)
#endif


   implicit none
   integer          , intent(out)      :: ngrid   ! Grid ID regarding this variable.
   integer          , intent(out)      :: ntimes  ! Grid ID regarding this variable.
   !----- External functions for time handling --------------------------------------------!
   integer          , external         :: julday
   integer          , external         :: julday1583
   integer          , external         :: v5d_datestamp
   integer          , external         :: v5d_timestamp
   character(len=3) , external         :: monchar
   real             , external         :: day_fraction
   character(len=17), external         :: grads_dtstamp
   character(len=19), external         :: mevi_dtstamp

   !----- Local variables -----------------------------------------------------------------!
   integer                             :: curryear
   integer                             :: currdoy
   integer                             :: it
   integer                             :: s
   integer                             :: t
   integer                             :: x
   integer                             :: y
   integer                             :: yrev
   integer                             :: z
   integer                             :: zrev
   real   , dimension(maxtimes)        :: gmttime
   integer, dimension(maxtimes)        :: years
   integer, dimension(maxtimes)        :: doys
   integer                             :: timelength
   real                                :: glon
   real                                :: glat
   real                                :: glev
   !------ Give the preprocessor the option to entirely skip this routine. ----------------!
#if USE_NCDF
   integer                             :: ierr
   character(len=NF90_MAX_NAME)        :: dumchar
   integer                             :: mapproj
   integer                             :: dummyint

   integer, dimension(maxgrds), save   :: nnzpmax = 0
   !---------------------------------------------------------------------------------------!



   !----- Native ECMWF will have always a single grid and a single time. ------------------!
   ngrid  = 1
   ngrids = 1
   ntimes = 1

   !----- Get the dimensions --------------------------------------------------------------!
   ierr   = ncio_dim   ('g4_lon_2'              , .true.  ,nnxp(ngrid)    ,idnnxp    )
   ierr   = ncio_dim   ('g4_lat_1'              , .true.  ,nnyp(ngrid)    ,idnnyp    )
   ierr   = ncio_dim   ('lv_ISBL0'              , .true.  ,nnzp(ngrid)    ,idnnzp    )

   !----- Other dimensions will be set to 1, hardly should they be different --------------!
   nzg     = 1
   nzs     = 1
   npatch  = 1
   nclouds = 1
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !    Now that I have the grid and dimension information loaded, I will allocate the     !
   ! scratch structure                                                                     !
   !---------------------------------------------------------------------------------------!
   call alloc_scratch(ngrid,ngrid)
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !    ECMWF will always have a Gaussian lon/lat grid that is almost regular.             !
   !---------------------------------------------------------------------------------------!
   ihtran  = 0
   !----- ECMWF will always have a regular pressure coordinate. ---------------------------!
   if_adap = 4
   !----- Get longitude, latitude and levels. ---------------------------------------------!
   ierr   = ncio_1dnotime('g4_lon_2'     ,.true.,nnxp(ngrid), xtn(1:nnxp(ngrid),ngrid))
   ierr   = ncio_1dnotime('g4_lat_1'     ,.true.,nnyp(ngrid), ytn(1:nnyp(ngrid),ngrid))
   ierr   = ncio_1dnotime('lv_ISBL0'     ,.true.,nnzp(ngrid), ztn(1:nnzp(ngrid),ngrid))

   !----- Convert vertical levels to Pascals. ---------------------------------------------!
   ztn(1:nnzp(ngrid),ngrid) = ztn(1:nnzp(ngrid),ngrid) * 100.

   polelon = minval(xtn(1:nnxp(ngrid),ngrid),dim=1)
   polelat = maxval(ytn(1:nnyp(ngrid),ngrid),dim=1)
   l2ndlat = polelat

   !----- Dump anything on centlon and centlat --------------------------------------------!
   centlon(ngrid) = (xtn(nnxp(ngrid)/2,ngrid))
   centlat(ngrid) = (ytn(nnyp(ngrid)/2,ngrid))
   !---------------------------------------------------------------------------------------!

   !----- Find some delta-x and delta-y in metres -----------------------------------------! 
   deltaxn(ngrid) = (xtn(nnxp(ngrid),ngrid) - xtn(1,ngrid)) * spcon                        &
                  * cos(centlat(ngrid)*pio180) / real(nnxp(ngrid))
   deltayn(ngrid) = (ytn(1,ngrid) - ytn(nnyp(ngrid),ngrid)) * spcon / real(nnyp(ngrid))
   !---------------------------------------------------------------------------------------!

   !----- Retrieving all times ------------------------------------------------------------!
   ierr   = ncio_eranat_time('T_GDS4_ISBL', .true., ntimes,this_time)
   !---------------------------------------------------------------------------------------!

#else
   ngrid  = missflg_int
   ntimes = missflg_int
   call fatal_error ('You can''t run ECMWF without compiling with netcdf!'                 &
                    ,'commio_eranat','eranatcio.F90')
#endif


   return
end subroutine commio_eranat
!==========================================================================================!
!==========================================================================================!

