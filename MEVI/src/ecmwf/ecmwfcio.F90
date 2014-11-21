!==========================================================================================!
!==========================================================================================!
! MEVI. ecmwfcio.F90  This is the equivalent of rcio for ECMWF, so it reads the global     !
!                     attributes and the dimensions. It will read and store only the       !
!                     variables that are needed for conversion into the output formats.    !
!==========================================================================================!
!==========================================================================================!
subroutine commio_ecmwf(ngrid,ntimes)
   use mod_model      , only : ngrids,nzg,nzs,nclouds,nwave,naddsc,npatch,polelon,polelat  &
                              ,l2ndlat,centlon,centlat,nnxp,nnyp,nnzp,zero_time,ihtran     &
                              ,deltaxn,deltayn,slz,ztn,zmn,dztn,dzmn,if_adap,zero_time     &
                              ,this_time,xtn,ytn
   use mod_scratch    , only : scratch, alloc_scratch
   use mod_maxdims    , only : maxtimes,maxgrds
#if USE_NCDF
   use netcdf
   use mod_ioopts     , only : missflg_int
   use mod_netcdf     , only : idtimes,idnnxp,idnnyp,idnnzp,idnzg,idnpatch,idnclouds       &
                              ,idnnxpst,idnnypst,idnnzpst,idtimelen
   use mod_ncdf_globio, only : ncio_glo,ncio_glo_sca, ncio_dim, ncio_1dnotime              &
                              ,ncio_ecmwf_time
   use rconstants     , only : spcon
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
   integer                             :: curryear,currdoy,it,s,t,z
   real   , dimension(maxtimes)        :: gmttime
   integer, dimension(maxtimes)        :: years,doys
   integer                             :: timelength
!------ Giving the preprocessor the option to entirely skip this routine ------------------!
#if USE_NCDF

   integer                      :: ierr
   character(len=NF90_MAX_NAME) :: dumchar
   integer                      :: mapproj
   integer                      :: dummyint

   logical                           :: gotz
   integer, dimension(maxgrds), save :: nnzpmax = 0

   !----- ECMWF will have always a single grid --------------------------------------------!
   ngrid  = 1
   ngrids = 1
   
   !----- Getting the dimensions ----------------------------------------------------------!
   ierr   = ncio_dim   ('time'                  , .true.  ,ntimes         ,idtimes   )
   ierr   = ncio_dim   ('longitude'             , .true.  ,nnxp(ngrid)    ,idnnxp    )
   ierr   = ncio_dim   ('latitude'              , .true.  ,nnyp(ngrid)    ,idnnyp    )

   !---------------------------------------------------------------------------------------!
   !    Not always will the ECMWF file have levelist. If the file has only sfc variables,  !
   ! they won't be present, in which case we will assign 1                                 !
   !---------------------------------------------------------------------------------------!
   ierr   = ncio_dim   ('levelist'              , .false. ,dummyint       ,idnnzp    )
   if (ierr == NF90_NOERR) then
      if (nnzpmax(ngrid) < dummyint) then
         gotz           = .true.
         nnzp(ngrid)    = dummyint
         nnzpmax(ngrid) = dummyint 
      else
         gotz           = .false.
      end if
   else 
       gotz           = .false.
   end if
   !----- Other dimensions will be set to 1, hardly should they be different --------------!
   nzg     = 1
   nzs     = 1
   npatch  = 1
   nclouds = 1

   !---------------------------------------------------------------------------------------!
   !    Now that I have the grid and dimension information loaded, I will allocate the     !
   ! scratch structure                                                                     !
   !---------------------------------------------------------------------------------------!
   call alloc_scratch(ngrid,ngrid)

   !----- ECMWF will always have a regular lon/lat grid -----------------------------------!
   ihtran = 0
   !----- ECMWF will always have a regular pressure coordinate ----------------------------!
   if_adap = 4
   !----- Getting longitude, latitude and levels ------------------------------------------!
   ierr   = ncio_1dnotime('longitude'     ,.true.,nnxp(ngrid), xtn(1:nnxp(ngrid),ngrid))
   ierr   = ncio_1dnotime('latitude'      ,.true.,nnyp(ngrid), ytn(1:nnyp(ngrid),ngrid))

   !----- Retrieving the levels only when nnzp is greater than one ------------------------!
   if (gotz) then
       ierr   = ncio_1dnotime('levelist',.true.,nnzp(ngrid),ztn(1:nnzp(ngrid),ngrid))
       !----- Converting the levels to Pa -------------------------------------------------!
       ztn(1:nnzp(ngrid),ngrid) =ztn(1:nnzp(ngrid),ngrid) * 100.
   end if
   
   polelon = minval(xtn(1:nnxp(ngrid),ngrid),dim=1)
   polelat = maxval(ytn(1:nnyp(ngrid),ngrid),dim=1)
   l2ndlat = polelat
   
   !----- Finding delta-x and delta-y in metres -------------------------------------------! 
   deltaxn(ngrid) = (xtn(2,ngrid)-xtn(1,ngrid))*spcon
   deltayn(ngrid) = (ytn(2,ngrid)-ytn(1,ngrid))*spcon
   !----- Dumping anything on centlon and centlat -----------------------------------------!
   centlon(ngrid) = (xtn(nnxp(ngrid)/2,ngrid))
   centlat(ngrid) = (ytn(nnyp(ngrid)/2,ngrid))
   !----- Retrieving all times ------------------------------------------------------------!
   ierr   = ncio_ecmwf_time('time', .true., ntimes,this_time)

#else
   ngrid  = missflg_int
   ntimes = missflg_int
   call fatal_error ('You can''t run ECMWF without compiling with netcdf!'                 &
                    ,'commio_ecmwf','ecmwfcio.F90')
#endif


   return
end subroutine commio_ecmwf

