!==========================================================================================!
!==========================================================================================!
! MEVI. wcio.F90  This is the equivalent of rcio for WRF, so it reads the global           !
!                 attributes and the dimensions. It will read and store only the variables !
!                 that are needed for conversion into the output formats.                  !
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine commio_zwrf(ngrid,ntimes)
   use mod_model      , only : ngrids,nzg,nzs,nclouds,nwave,naddsc,npatch,polelon,polelat  &
                              ,l2ndlat,centlon,centlat,nnxp,nnyp,nnzp,zero_time,ihtran     &
                              ,deltaxn,deltayn,slz,ztn,zmn,dztn,dzmn,if_adap,zero_time     &
                              ,this_time,xtn,ytn
   use mod_scratch    , only : scratch, alloc_scratch
   use mod_maxdims    , only : maxtimes
   use rconstants     , only : day_sec
#if USE_NCDF
   use netcdf
   use mod_ioopts     , only : missflg_int
   use mod_netcdf     , only : idtimes,idnnxp,idnnyp,idnnzp,idnzg,idnpatch,idnclouds       &
                              ,idnnxpst,idnnypst,idnnzpst,idtimelen
   use mod_ncdf_globio, only : ncio_glo,ncio_glo_sca, ncio_dim, ncio_1dvar,ncio_0dvar      &
                              ,ncdf_load_err,ncio_2dvar,ncio_1dnotime
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
   integer                      :: dummyint,tott
   real(kind=8)                 :: dtnow
   real   , save                :: deltat
   integer, save                :: ntottimes=0
   logical, save                :: first_time = .true.,second_time = .false.

   !----- Setting zero_time to be an arbitrary initial time (Jan 1, 2001, 00GMT) ----------!
   if (first_time) then
      zero_time%year       = 2001
      zero_time%month      =    1
      zero_time%day        =    1
      zero_time%hour       =    0
      zero_time%minu       =    0
      zero_time%seco       =    0
      zero_time%doy        = julday (zero_time%month,zero_time%day,zero_time%year)
      zero_time%mmm        = monchar(zero_time%month)
      zero_time%fracday    = day_fraction(zero_time%hour,zero_time%minu,zero_time%seco)
      zero_time%yyyyddd    = v5d_datestamp(zero_time%year,zero_time%doy)
      zero_time%hhmmss     = v5d_timestamp(zero_time%hour,zero_time%minu,zero_time%seco)
      zero_time%gradsstamp = grads_dtstamp(zero_time%year,zero_time%mmm,zero_time%day      &
                                          ,zero_time%hour,zero_time%minu)
      zero_time%timestr    = mevi_dtstamp(zero_time%year,zero_time%month,zero_time%day     &
                                         ,zero_time%hour,zero_time%minu,zero_time%seco)
      zero_time%elapsed    = dble(julday1583(zero_time%month,zero_time%day                 &
                                            ,zero_time%year))+dble(zero_time%fracday)
      !first_time = .false. - This will be turned off at the end of this subroutine
   end if


   ngrid  = 1
   ngrids = ngrid 

   !----- Retrieving the main dimensions --------------------------------------------------!
   ierr   = ncio_dim   ('time'                  , .true.  ,ntimes         ,idtimes   )
   ierr   = ncio_dim   ('x'                     , .true.  ,nnxp(ngrid)    ,idnnxp    )
   ierr   = ncio_dim   ('y'                     , .true.  ,nnyp(ngrid)    ,idnnyp    )
   ierr   = ncio_dim   ('z'                     , .true.  ,nnzp(ngrid)    ,idnnzp    )
   !---------------------------------------------------------------------------------------!



   !----- Assigning default grid dimensions. It is theoretical, so use normal values ------!
   ihtran         = -1
   polelon        =  0.
   polelat        =  0.
   l2ndlat        =  0.
   centlon(ngrid) =  0.
   centlat(ngrid) =  0.
   nzg            =  1  ; idnzg     = -1
   npatch         =  1  ; idnpatch  = -1
   nclouds        =  1  ; idnclouds = -1
   nzs            =  1
   nwave          =  1
   slz(1)         = -0.1

   !----- Setting staggered ID's to -1, meaning "nowhere"... ------------------------------!
   idnnxpst       = -1
   idnnypst       = -1
   idnnzpst       = -1

   !----- ZWRF vertical coordinate is the terrain-following height coordinate -------------!
   if_adap = -1
   !---------------------------------------------------------------------------------------!
   if (first_time) then
      ierr = ncio_1dnotime ('x'    ,.true.  ,nnxp(ngrid)       ,xtn(:,ngrid)       )
      ierr = ncio_1dnotime ('y'    ,.true.  ,nnyp(ngrid)       ,ytn(:,ngrid)       )
      ierr = ncio_1dnotime ('z'    ,.true.  ,nnzp(ngrid)       ,ztn(:,ngrid)       )
      deltaxn(ngrid) = xtn(2,ngrid)-xtn(1,ngrid)
      deltayn(ngrid) = ytn(2,ngrid)-ytn(1,ngrid)
   end if
   !---------------------------------------------------------------------------------------!


   !----- Retrieving the times ------------------------------------------------------------!
   timeloop: do it=1,ntimes
      tott = ntottimes+it

      if (first_time) then
         second_time   = .true.
         this_time(it) = zero_time
         first_time    = .false.
         cycle timeloop
      elseif (second_time) then
         ierr =ncio_0dvar('time',.true.,it,deltat)
         !----- If the netcdf doesn't have time information, use arbitrary unit -----------!
         if (deltat == 0.) deltat = 5.
         second_time = .false.
      end if
      dtnow = dble(tott-1)*dble(deltat)
      call date_add_to(zero_time%year,zero_time%month,zero_time%day,zero_time%hhmmss       &
                      ,dtnow,'s',this_time(it)%year                                        &
                      ,this_time(it)%month,this_time(it)%day,this_time(it)%hhmmss)
      this_time(it)%hour       = int(this_time(it)%hhmmss/10000)
      this_time(it)%minu       = mod(int(this_time(it)%hhmmss/100),100)
      this_time(it)%seco       = mod(this_time(it)%hhmmss,100)
      this_time(it)%doy        = julday (this_time(it)%month,this_time(it)%day             &
                                        ,this_time(it)%year)
      this_time(it)%mmm        = monchar(this_time(it)%month)
      this_time(it)%fracday    = day_fraction(this_time(it)%hour,this_time(it)%minu        &
                                             ,this_time(it)%seco)
      this_time(it)%yyyyddd    = v5d_datestamp(this_time(it)%year,this_time(it)%doy)
      this_time(it)%hhmmss     = v5d_timestamp(this_time(it)%hour,this_time(it)%minu       &
                                              ,this_time(it)%seco)
      this_time(it)%gradsstamp = grads_dtstamp(this_time(it)%year,this_time(it)%mmm        &
                                              ,this_time(it)%day,this_time(it)%hour        &
                                              ,this_time(it)%minu)
      this_time(it)%timestr    = mevi_dtstamp(this_time(it)%year,this_time(it)%month       &
                                             ,this_time(it)%day ,this_time(it)%hour        &
                                             ,this_time(it)%minu,this_time(it)%seco)
      this_time(it)%elapsed    = dble(julday1583(this_time(it)%month,this_time(it)%day     &
                                     ,this_time(it)%year))+dble(this_time(it)%fracday)
   end do timeloop
   ntottimes=ntottimes + ntimes

   !---------------------------------------------------------------------------------------!
   !    Now that I have the grid and dimension information loaded, I will allocate the     !
   ! scratch structure                                                                     !
   !---------------------------------------------------------------------------------------!
   call alloc_scratch(ngrid,ngrid)


!------------------------------------------------------------------------------------------!
!     If the user disabled netCDF, then this routine should never be called. If that       !
! happens, crash it!                                                                       !
!------------------------------------------------------------------------------------------!
#else

   ngrid  = missflg_int
   ntimes = missflg_int
   call fatal_error('NetCDF libraries are required to use netCDF files...'                 &
                   ,'commio_zwrf','zwrfcio.F90')
#endif

   return
end subroutine commio_zwrf
!==========================================================================================!
!==========================================================================================!
