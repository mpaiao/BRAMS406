!==========================================================================================!
!==========================================================================================!
!  MEVI. map_input_vars.f90                                                                !
!                                                                                          !
!     This subroutine will check every variable at the input and find in the variable      !
! table. Variables with index equals to mevi_libvar are composed variables, which means    !
! that they need to be preprocessed (this is currently disabled, will be available soon).  !
! Variables with index equals to mevi_absent are absent, and therefore will be ignored     !
! after a huge banner is displayed. Also in the future development side is the ability to  !
! handle variables that are present part of the time (currently variables must be present  !
! in all files).
!     Variables that are in the var list are simply read and dumped into the scratch       !
! variable.                                                                                !
!==========================================================================================!
!==========================================================================================!
subroutine map_input_vars()
   use mod_ioopts , only : intype        & ! intent(in)
                         , outtype       & ! intent(in)
                         , nvars         & ! intent(in)
                         , vars          & ! intent(in)
                         , var_address   & ! intent(in)
                         , mevi_absent   & ! intent(in)
                         , mevi_libvar   & ! intent(in)
                         , mevi_partly   & ! intent(in)
                         , mevi_notfound & ! intent(in)
                         , mevi_badtype  & ! intent(in)
                         , v5d_ok_types  & ! intent(in)
                         , time_address  & ! intent(in)
                         , outtimes      & ! intent(in)
                         , nouttimes     & ! intent(in)
                         , ttta          & ! intent(in)
                         , tttz          ! ! intent(in)
   use mod_model  , only : ngrids
   use an_header  , only : info_table, nfiles
   use mod_maxdims, only : maxstr,maxvarin,maxtimes
   implicit none
   !----- Local variables. ----------------------------------------------------------------!
   integer                            :: invvar     ! Counter
   integer                            :: nmlvar     ! Counter
   integer                            :: nmltime    ! Time countes for "namelist"
   integer                            :: invtime    ! Time countes for inventory
   integer                            :: newnvars   !
   integer                            :: newntimes  !
   integer                            :: nf         ! File counter
   integer                            :: ifm        ! Grid counter
   integer                            :: vadd       ! Aux. alias variable
   integer                            :: avant      ! Aux. alias variable
   character(len=maxstr)              :: currvar    ! Just to make it case insensitive
   character(len=maxstr)              :: currnmlv   ! Just to make it case insensitive
   integer, allocatable, dimension(:) :: countvar
   integer, allocatable, dimension(:) :: counttime
   integer, allocatable, dimension(:) :: countsince
   integer                            :: maxcvar
   integer                            :: maxctime
   integer                            :: ta
   integer                            :: tz
   integer                            :: longtime


   !----- Initialise some variables. ------------------------------------------------------!
   var_address (:,:,:) = mevi_notfound
   time_address(:,:,:) = mevi_notfound
   !---------------------------------------------------------------------------------------!


   !----- Allocate the variable count -----------------------------------------------------!
   allocate(countvar(nvars),counttime(nouttimes),countsince(nouttimes))
   !---------------------------------------------------------------------------------------!


   !---------------------------------------------------------------------------------------!
   !    Loop through the variables. For each grid, each variable must be present every     !
   ! time, otherwise it will be flagged as missing. We currently three possible distribu-  !
   ! tions:                                                                                !
   ! 1. RAMS style : various grids, one file (header) per time, variables and grids pre-   !
   !                 sent every file, no time overlapping between files.                   !
   ! 2. WRF style  : each file contains all variables and various times for a single grid. !
   !                 No time overlapping between files, multiple grids will be handled by  !
   !                 independent sets of files.                                            !
   ! 3. ECMWF style: one grid, each file containing some variables for all times. No var-  !
   !                 iable overlapping between files.                                      !
   !---------------------------------------------------------------------------------------!
   gridloop: do ifm=1,ngrids
      countvar  = 0
      counttime = 0
      maxcvar   = 0
      maxctime  = 0


      !------------------------------------------------------------------------------------!
      !   Here we loop through all times checking where each time is.  We also sum the     !
      ! occurence by using the number of variables, so in the end each time should have    !
      ! the same count. If not, we will eliminate some times                               !
      !------------------------------------------------------------------------------------!
      ta =max(1,ttta(ifm))
      tz =min(nouttimes,tttz(ifm))
      nmltimeloop: do nmltime=ta,tz
         fileloop_t: do nf=1,nfiles
            if (.not. info_table(nf)%avail_grid(ifm)) cycle fileloop_t
            invtimeloop:  do invtime=1, info_table(nf)%ntimes
               if (outtimes(nmltime)%elapsed == info_table(nf)%file_time(invtime)%elapsed) &
               then
                  time_address(nf,nmltime,ifm) = invtime
                  counttime(nmltime) = counttime(nmltime) + info_table(nf)%nvars
                  maxctime           = max(maxctime,counttime(nmltime))
                  cycle fileloop_t
               end if
            end do invtimeloop
         end do fileloop_t
      end do nmltimeloop
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !     Missing times aren't accepted up to this version.  So in case there are miss-  !
      ! ing times, I will reduce the time range to between the longest segment within the  !
      ! input time range that has no "gaps".                                               !
      !------------------------------------------------------------------------------------!
      !----- Find the longest stretch of time that is available. --------------------------!
      countsince = 0
      if (counttime(1) == maxctime) countsince(1) = 1
      do nmltime=2,nouttimes
         if (counttime(nmltime) == maxctime) then
            countsince(nmltime) = countsince(nmltime-1) + 1
         end if
      end do
      !----- Find the first and last time. ------------------------------------------------!
      tz = maxloc(countsince,dim=1)
      ta = tz-countsince(tz)+1
      newntimes = countsince(tz)
      if (newntimes == 0) then
         do nmltime=1,nouttimes
            write (unit=*,fmt='(3(a,1x,i5,1x))') 'IT=        ',nmltime                     &
                                                ,'COUNTTIME =',counttime(nmltime)          &
                                                ,'COUNTSINCE=',countsince(nmltime)
         end do
         call fatal_error('No time period is available with all variables...'              &
                         ,'map_input_vars','map_input_vars.f90')
      end if

      !----- Throw away other data --------------------------------------------------------!
      time_address(1:nfiles,1:newntimes,ifm) = time_address(1:nfiles,ta:tz,ifm)
      time_address(1:nfiles,newntimes+1:nouttimes,ifm) = mevi_notfound
      nouttimes = newntimes
      where (time_address(1:nfiles,1:nouttimes,ifm) == mevi_notfound)
         time_address(1:nfiles,1:nouttimes,ifm) = mevi_absent
      end where

      !------------------------------------------------------------------------------------!
      !    Here we loop through all variables checking where each variable is available,   !
      ! and we sum the occurences using ntimes, so in the end each variable should appear  !
      ! the same number of times, otherwise it will be removed.                            !
      !------------------------------------------------------------------------------------!
      countvar(nmlvar) = 0
      nmlvarloop: do nmlvar=1,nvars
         fileloop_v: do nf=1,nfiles
            if (.not. info_table(nf)%avail_grid(ifm)) cycle fileloop_v
            invvarloop: do invvar = 1,info_table(nf)%nvars
               !----- Make it case insensitive --------------------------------------------!
               currvar = info_table(nf)%varname(invvar)
               call tolower(currvar)

               !----- Copy the namelist variable into a scratch. --------------------------!
               select case(intype)
               case ('eranat')
                  currnmlv = trim(vars(nmlvar))//'_gds4_isbl'
               case default
                  currnmlv = trim(vars(nmlvar))
               end select
 
               if (trim(currnmlv) == trim(currvar)) then

                  var_address(nf,nmlvar,ifm) = invvar
                  !------------------------------------------------------------------------!
                  !   Check how many times are available in this file and that were not    !
                  ! discarded.                                                             !
                  !------------------------------------------------------------------------!
                  do nmltime=1,nouttimes
                     if (time_address(nf,nmltime,ifm) /= mevi_absent) then
                        countvar(nmlvar) = countvar(nmlvar) + 1
                     end if
                  end do
                  !------------------------------------------------------------------------!
                  maxcvar = max(maxcvar,countvar(nmlvar))

                  cycle fileloop_v
               end if
            end do invvarloop
            !------------------------------------------------------------------------------!
            !    Here we should have a second loop to check whether the variable is defin- !
            ! ed in the library.                                                           !
            !------------------------------------------------------------------------------!
            !select case (intype)
            !case ('rams','brams')
            !   call check_rams_library(nmlvar)
            !case ('wrf')
            !   call check_wrf_library(nmlvar)
            !end select
            !------------------------------------------------------------------------------!

            if (var_address(nf,nmlvar,ifm) < 0 .and.                                       &
                var_address(nf,nmlvar,ifm) /= mevi_libvar)                                 &
                var_address(nf,nmlvar,ifm) = mevi_absent

         end do fileloop_v
      end do nmlvarloop

      !------------------------------------------------------------------------------------!
      !    Flag variables that are not available all times or cannot be converted for      !
      ! elimination.                                                                       !
      !------------------------------------------------------------------------------------!
      elimloop: do nmlvar=1,nvars
         if (countvar(nmlvar) < maxcvar) then
            write (unit=*,fmt='(a)') '----------------------------------------------------'
            write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING! WARNING!    '
            write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING! WARNING!    '
            write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING! WARNING!    '
            write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING! WARNING!    '
            write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING! WARNING!    '
            write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING! WARNING!    '
            write (unit=*,fmt='(a)') '----------------------------------------------------'
            write (unit=*,fmt='(3(a,1x))') '    Variable',trim(vars(nmlvar)),'is not'//    &
                                           ' always present so it will be ignored...'
            write (unit=*,fmt='(a)') '----------------------------------------------------'
            write (unit=*,fmt='(a)') ' '
            var_address(1:nfiles,nmlvar,1:ngrids) = mevi_partly
            cycle elimloop
         elseif (all(var_address(1:nfiles,nmlvar,ifm) == mevi_absent)) then
            write (unit=*,fmt='(a)') '----------------------------------------------------'
            write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING! WARNING!    '
            write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING! WARNING!    '
            write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING! WARNING!    '
            write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING! WARNING!    '
            write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING! WARNING!    '
            write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING! WARNING!    '
            write (unit=*,fmt='(a)') '----------------------------------------------------'
            write (unit=*,fmt='(3(a,1x))') '    Variable',trim(vars(nmlvar)),'wasn''t'//  &
                                           ' found so it will be ignored...'
            write (unit=*,fmt='(a)') '----------------------------------------------------'
            write (unit=*,fmt='(a)') ' '
            var_address(1:nfiles,nmlvar,1:ngrids) = mevi_notfound
            cycle elimloop
         end if

         !------ Verify whether the variable can be converted -----------------------------!
         select case (trim(outtype))
         case ('vis5d')
            do nf=1,nfiles
               vadd = var_address(nf,nmlvar,ifm)
               if (vadd > 0 .or. vadd == mevi_libvar) then
                  if (.not. any(v5d_ok_types == info_table(nf)%idim_type(vadd))) then
                     write (unit=*,fmt='(a)') '--------------------------------------------' 
                     write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING!     '
                     write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING!     '
                     write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING!     '
                     write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING!     '
                     write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING!     '
                     write (unit=*,fmt='(a)') '    WARNING! WARNING! WARNING! WARNING!     '
                     write (unit=*,fmt='(a)') '--------------------------------------------'
                     write (unit=*,fmt='(3(a,1x))') '    Variable',trim(vars(nmlvar))      &
                                                   ,'cannot be used in Vis5D, ignoring it!'
                     write (unit=*,fmt='(a)') '--------------------------------------------'
                     write (unit=*,fmt='(a)') ' '
                     var_address(nf,nmlvar,ifm) = mevi_badtype  
                  end if                               
               end if
            end do
         end select
      end do elimloop

      newnvars=0
      !------------------------------------------------------------------------------------!
      !     For now I am removing all variables that won't be converted. In the future we  !
      ! may need to rethink this as some outputs may be fine with partial availability or  !
      ! other exceptions.                                                                  !
      !------------------------------------------------------------------------------------!
      do nmlvar=1,nvars
         if (any(var_address(1:nfiles,nmlvar,ifm) == mevi_partly   ) .or.                  &
             any(var_address(1:nfiles,nmlvar,ifm) == mevi_badtype ) .or.                   &
             any(var_address(1:nfiles,nmlvar,ifm) == mevi_notfound)) then
           do avant=nmlvar,nvars-1
              var_address(1:nfiles,avant,1:ngrids) = var_address(1:nfiles,avant+1,1:ngrids)
           end do
         else 
           newnvars = newnvars + 1
         end if
      end do
      nvars = newnvars
      var_address(1:nfiles,nvars+1:maxvarin,1:ngrids) = mevi_notfound


      
      !----- Printing the variable table --------------------------------------------------!
      write (unit=*,fmt='(80a1)'   ) ('-',nf=1,80)
      write (unit=*,fmt='(a,1x,i5)') 'Variable table: nvars=',nvars
      do nmlvar=1,nvars
         write (unit=*,fmt='(2(1x,a,1x,i5),1x,a,1x,a10,1x,a,1x,200(1x,i5))')               &
            'Grid:',ifm,'nmlvar=',nmlvar,'name=',trim(vars(nmlvar))                        &
           ,'address=',(var_address(nf,nmlvar,ifm),nf=1,nfiles)
      end do
      write (unit=*,fmt='(80a1)') ('-',nf=1,80)
      write (unit=*,fmt='(a)'   )  ' '
      !------------------------------------------------------------------------------------!

      !----- Printing the time table ------------------------------------------------------!
      write (unit=*,fmt='(80a1)'   ) ('-',nf=1,80)
      write (unit=*,fmt='(a,1x,i5)') 'Time table: ntimes=',nouttimes
      do nmltime=1,nouttimes
         write (unit=*,fmt='(a,1x,i5,3(1x,a),1x,es14.7,1x,a,200(1x,i5))')                  &
           'Grid:',ifm,'time=',outtimes(nmltime)%timestr                                   &
                      ,'elapsed=',outtimes(nmltime)%elapsed                                &
                      ,'address=',(time_address(nf,nmltime,ifm),nf=1,nfiles)
      end do
      write (unit=*,fmt='(80a1)') ('-',nf=1,80)
      write (unit=*,fmt='(a)'   )  ' '
      !------------------------------------------------------------------------------------!

   end do gridloop
   deallocate (countvar,counttime)
   return
end subroutine map_input_vars
