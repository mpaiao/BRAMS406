!==========================================================================================!
!==========================================================================================!
!  MEVI. v5d_output.F90                                                                    !
!                                                                                          !
!     This subroutine will loop through the variables, check the ones the user wants in    !
! the output, and dump into the vis5d file. Note that this subrotine will be ignored if    !
! the user didn't include the "includes" and libraries from Vis5D or Vis5D+.               !
!==========================================================================================!
!==========================================================================================!
subroutine v5d_output(intype,outpref)

#if USE_V5DP
   use mod_v5d   , only : v5dcreate                    & ! intent(inout)
                        , v5dcreatesimple              & ! intent(inout)
                        , v5dwrite                     & ! intent(inout)
                        , v5dmcfile                    & ! intent(inout)
                        , v5dclose                     & ! intent(inout)
                        , v5d_maxvars    => maxvars    & ! intent(inout)
                        , v5d_maxtimes   => maxtimes   & ! intent(inout)
                        , v5d_maxlevels  => maxlevels  & ! intent(inout)
                        , v5d_maxrows    => maxrows    & ! intent(inout)
                        , v5d_maxcolumns => maxcolumns & ! intent(inout)
                        , maxproj                      & ! intent(inout)
                        , v5d_varlen                   & ! intent(inout)
                        , v5d_unitlen                  & ! intent(inout)
                        , missing                      & ! intent(inout)
                        , imissing                     ! ! intent(inout)
#endif
   use mod_grid   , only : grid_g                      & ! structure
                         , grid_o                      & ! structure
                         , alloc_grid                  & ! sub-routine
                         , nullify_grid                & ! sub-routine
                         , zero_grid                   & ! sub-routine
                         , copy_grid                   ! ! sub-routine
   use mod_ioopts , only : var_address                 & ! intent(inout)
                         , mevi_absent                 & ! intent(in)
                         , mevi_badtype                & ! intent(in)
                         , mevi_partly                 & ! intent(in)
                         , mevi_libvar                 & ! intent(in)
                         , nvars                       & ! intent(in)
                         , vars                        & ! intent(in)
                         , missflg_real                & ! intent(in)
                         , outtimes                    & ! intent(in)
                         , nouttimes                   & ! intent(in)
                         , time_address                & ! intent(in)
                         , i1st                        & ! intent(in)
                         , ilast                       & ! intent(in)
                         , j1st                        & ! intent(in)
                         , jlast                       & ! intent(in)
                         , k1st                        & ! intent(in)
                         , klast                       & ! intent(in)
                         , ttta                        & ! intent(in)
                         , tttz                        & ! intent(in)
                         , strinx                      & ! intent(in)
                         , striny                      & ! intent(in)
                         , strinz                      & ! intent(in)
                         , y_n2s                       & ! intent(in)
                         , z_t2b                       ! ! intent(in)
   use mod_maxdims, only : maxstr                      ! ! intent(in)
   use mod_model  , only : ngrids                      & ! intent(in)
                         , ihtran                      & ! intent(in)
                         , if_adap                     & ! intent(in)
                         , polelon                     & ! intent(in)
                         , polelat                     & ! intent(in)
                         , l2ndlat                     & ! intent(in)
                         , centlon                     & ! intent(in)
                         , centlat                     & ! intent(in)
                         , deltaxn                     & ! intent(in)
                         , deltayn                     & ! intent(in)
                         , nnxp                        & ! intent(in)
                         , nnyp                        & ! intent(in)
                         , nnzp                        ! ! intent(in)
   use an_header  , only : nfiles                      & ! intent(in)
                         , info_table                  ! ! intent(in)
   use rconstants , only : spcon                       ! ! intent(in)
   use mod_scratch, only : scratch                     ! ! structure, intent(inout)
   implicit none
   !----- Arguments. ----------------------------------------------------------------------!
   character(len=maxstr), intent(in) :: intype
   character(len=maxstr), intent(in) :: outpref

#if USE_V5DP
   !---------------------------------------------------------------------------------------!
   !  Vis5D variables, based on foo2_to_v5d.f.                                             !
   !---------------------------------------------------------------------------------------!
   character(len=maxstr)                                :: outname
   integer                                              :: numtimes
   integer                                              :: numvars
   integer                                              :: nxpo,nypo,dimsiz
   integer                   , dimension(v5d_maxvars)   :: nzpo
   character(len=v5d_varlen) , dimension(v5d_maxvars)   :: varname
   character(len=v5d_unitlen), dimension(v5d_maxvars)   :: varunit
   integer                   , dimension(v5d_maxtimes)  :: times
   integer                   , dimension(v5d_maxtimes)  :: dates
   integer                                              :: compressmode
   integer                                              :: projection
   real                      , dimension(maxproj)       :: proj_args
   integer                                              :: vertical
   real                      , dimension(v5d_maxlevels) :: vert_args
   !---------------------------------------------------------------------------------------!
   ! Other variables                                                                       !
   !---------------------------------------------------------------------------------------!
   character(len=14), parameter                         :: frmt='(a,1x,i5,1x,a)'
   real                                                 :: lonavg,latavg,levavg
   integer                                              :: ierr,ifm,nf,nt,nv,k,kout
   integer                                              :: vadd,tadd
   integer                                              :: stridex,stridey,stridez
   integer                                              :: offx,offy,offz
   integer                                              :: ia,iz,ja,jz,ka,kz
   logical                                              :: swapy,swapz
   !---------------------------------------------------------------------------------------!


   !---------------------------------------------------------------------------------------!
   !   We first allocate the output grid structure                                         !
   !---------------------------------------------------------------------------------------!
   allocate (grid_o(ngrids))


   !---------------------------------------------------------------------------------------!
   !   This big loop is to cover all grids. One vis5d file will be created for each grid.  !
   !---------------------------------------------------------------------------------------!
   gridloop: do ifm = 1, ngrids

      !------------------------------------------------------------------------------------!
      ! 1. Attach the suffix to the output                                                 !
      !------------------------------------------------------------------------------------!
      write(outname,fmt='(4a,i2.2,a)') &
         trim(outpref),'-N-',trim(info_table(1)%file_time(1)%timestr),'_g',ifm,'.v5d'
      write(unit=*,fmt=*) trim(outname)
      
      !------------------------------------------------------------------------------------!
      ! 2. Initialise the variable information.                                            !
      !------------------------------------------------------------------------------------!
      numtimes = nouttimes

      !------------------------------------------------------------------------------------!
      ! 3. Vis-5D has a maximum number of rows and columns, and often the input data will  !
      !    have more data than the maximum that vis5d can handle. Therefore we need to     !
      !    check this here. We first apply all the offsets and strides provided by the     !
      !    user, because it may be enough to bring it within the maximum size. In case it  !
      !    doesn't, the run must stop.                                                     !
      !------------------------------------------------------------------------------------!
      !----- Checking the rows first ------------------------------------------------------!
      offy    = max(j1st(ifm),1)-1
      stridey = max(striny(ifm),1)
      nypo    = 1 + (min(jlast(ifm),nnyp(ifm)) - offy - 1)/stridey
      
      !----- Check whether this can be handled by Vis-5D ----------------------------------!
      if (nypo > v5d_maxrows) then
          !--------------------------------------------------------------------------------!
          !     Compute suggestions, save on X, which is fine here because the run is      !
          ! about to stop with no output.                                                  !
          !--------------------------------------------------------------------------------!
          stridex = (min(jlast(ifm),nnyp(ifm)) - offy - 1)/(v5d_maxrows-1)
          nxpo    = stridey*v5d_maxrows
          nxpo    = nnyp(ifm) - nxpo
          write (unit=*,fmt='(a)')       '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write (unit=*,fmt='(a)')       '! Your subdomain can''t be written to Vis-5D'
          write (unit=*,fmt='(a,1x,i6)') '!   - Vis-5D limit on # of rows:',v5d_maxrows
          write (unit=*,fmt='(a,1x,i6)') '!   - # of rows based on your namelist: ',nypo
          write (unit=*,fmt='(a)')       '! '
          write (unit=*,fmt=frmt )       '! Here are some suggestions for grid: ',ifm,':'
          write (unit=*,fmt=frmt )       '!  - Increase striny to ',stridex,' or more;'
          write (unit=*,fmt=frmt )       '!  - Crop a total of ',nxpo,' points from Y.'
          write (unit=*,fmt='(a)')       '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          call fatal_error('Number of rows exceeded maximum for Vis-5D!'                   &
                          ,'v5d_output','v5d_output.F90')
      end if

      !----- Now I check the columns ------------------------------------------------------!
      offx    = max(i1st(ifm),1)-1
      stridex = max(strinx(ifm),1)
      nxpo    = 1 + (min(ilast(ifm),nnxp(ifm)) - offx - 1)/stridex
      
      !----- Check whether this can be handled by Vis-5D ----------------------------------!
      if (nxpo > v5d_maxcolumns) then
          !--------------------------------------------------------------------------------!
          !     Compute suggestions, save on Y, which is fine here because the run is      !
          ! about to stop with no output.                                                  !
          !--------------------------------------------------------------------------------!
          stridey = (min(ilast(ifm),nnxp(ifm)) - offx - 1)/(v5d_maxcolumns-1)
          nypo    = stridex*v5d_maxcolumns
          nypo    = nnxp(ifm) - nypo
          write (unit=*,fmt='(a)')       '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write (unit=*,fmt='(a)')       '! Your subdomain can''t be written to Vis-5D'
          write (unit=*,fmt='(a,1x,i6)') '!   - Vis-5D limit on # of columns:'             &
                                                                            ,v5d_maxcolumns
          write (unit=*,fmt='(a,1x,i6)') '!   - # of columns based on your namelist: ',nxpo
          write (unit=*,fmt='(a)')       '! '
          write (unit=*,fmt=frmt )       '! Here are some suggestions for grid ',ifm,':'
          write (unit=*,fmt=frmt )       '!  - Increase strinx to ',stridey,' or more;'
          write (unit=*,fmt=frmt )       '!  - Crop a total of ',nypo,' points from X.'
          write (unit=*,fmt='(a)')       '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          call fatal_error('Number of columns exceeded maximum for Vis-5D!'                &
                          ,'v5d_output','v5d_output.F90')
      end if
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!
      ! 4. Initialise output grid                                                          !
      !------------------------------------------------------------------------------------!
      call nullify_grid(grid_o(ifm))
      call alloc_grid(grid_o(ifm),nxpo,nypo,nnzp(ifm))
      call zero_grid(grid_o(ifm))



      !------------------------------------------------------------------------------------!
      ! 5. For Vis-5D we must have the variable present all times, so all the dimension    !
      !    check is done using the first file that has the variable.                       !
      !------------------------------------------------------------------------------------!
      numvars      = nvars
      vdimloop: do nv = 1,nvars
        fdimloop:do nf = 1, nfiles
           vadd = var_address(nf,nv,ifm)
           
           !----- Loop until I reach a file in which the variable is present. -------------!
           if (vadd == mevi_absent) cycle fdimloop
           
           !----- Determining the # of vertical levels accordingly to the variable type ---!
           select case (info_table(nf)%idim_type(vadd))
           case (22,62)
              nzpo(nv) = 1
              !----- Getting the variable name, constraining by v5d_varlength -------------!
              varname(nv) = info_table(nf)%varname(vadd)(1:min(v5d_varlen,maxstr))
           case (33,63)
              nzpo(nv) = info_table(nf)%dims(3,vadd)
           end select
            !----- Getting the variable name, constraining by v5d_varlength ---------------!
            varname(nv) = info_table(nf)%varname(vadd)(1:min(v5d_varlen,maxstr))
            exit fdimloop !---- Found the variable, move to the next. ---------------------!
         end do fdimloop
      end do vdimloop

      !------------------------------------------------------------------------------------!
      ! 6. Initialise the time information.                                                !
      !------------------------------------------------------------------------------------!
      do nt=1,nouttimes
         dates(nt) = outtimes(nt)%yyyyddd
         times(nt) = outtimes(nt)%hhmmss
      end do

      !------------------------------------------------------------------------------------!
      ! 7. Set compression mode (not sure, used the same as REVU).                         !
      !------------------------------------------------------------------------------------!
      compressmode = 1

      !------------------------------------------------------------------------------------!
      ! 8. Set the projection information. This depends on the input data.                 !
      !------------------------------------------------------------------------------------!
      select case (ihtran)
      case (-1) !----- Evenly spaced, arbitrary units -------------------------------------!
         projection   = 0
         proj_args(1) = polelat
         proj_args(2) = -polelon
         proj_args(3) = abs(stridey*deltayn(ifm))
         proj_args(4) = abs(stridex*deltaxn(ifm))
      case (0) !----- Evenly spaced Lon-lat (cylindrical equidistant) ---------------------!
         projection   = 1
         proj_args(1) =  polelat
         proj_args(2) = -polelon !---- Vis5D uses positive for west longitude (!!!) -------!
         proj_args(3) = abs(stridey*deltayn(ifm)/spcon)
         proj_args(4) = abs(stridex*deltaxn(ifm)/spcon)
      case (1) !----- Polar-stereographic -------------------------------------------------!
         projection = 3
         proj_args(1) =  polelat
         proj_args(2) = -polelon !---- Vis5D uses positive for west longitude (!!!) -------!
         proj_args(3) = nnyp(ifm)/2 ! THIS IS WRONG! JUST TO COMPILE FOR NOW...
         proj_args(4) = nnxp(ifm)/2 ! THIS IS WRONG TOO...
         proj_args(5) = sqrt((stridex*deltaxn(ifm))**2                                     &
                            +(stridey*deltayn(ifm))**2) * 0.001 ! In km.
      case (2) !----- Lambert-conformal ---------------------------------------------------!
         projection = 2
         proj_args(1) = polelat
         proj_args(2) = l2ndlat
         proj_args(3) = nnyp(ifm)/2 ! THIS IS WRONG! JUST TO COMPILE AND TEST...
         proj_args(3) = nnxp(ifm)/2 ! THIS IS WRONG! JUST TO COMPILE AND TEST...
         proj_args(4) = -polelon !---- Vis5D uses positive for west longitude (!!!) -------!
         proj_args(6) = sqrt((stridex*deltaxn(ifm))**2                                     &
                            +(stridey*deltayn(ifm))**2)/1000. ! In km.
      end select

      !------------------------------------------------------------------------------------!
      ! 9. Set the vertical coordinates.                                                   !
      !------------------------------------------------------------------------------------!
      select case (if_adap)
      case (-1,0,1,3) ! Height-based, need to convert the height to kilometres
                      ! -1 is the same as 0, except that it won't convert the height.
        vertical = 2
        swapz = grid_g(ifm)%lev(2) < grid_g(ifm)%lev(1)
        
        
        do k=1,nnzp(ifm)
           if (swapz) then
              kout = nnzp(ifm) - k +1
           else
              kout = k
           end if
           select case(if_adap)
           case (-1)
              vert_args(kout) = grid_g(ifm)%lev(k)
           case default
              vert_args(kout) = 0.001 * grid_g(ifm)%lev(k)
           end select
        end do
      case (2,4) ! Pressure-based, need to convert the pressure to hPa.
        vertical = 3
        swapz = grid_g(ifm)%lev(2) > grid_g(ifm)%lev(1)
        do k=1,nnzp(ifm)
           if (swapz) then
              kout = nnzp(ifm) - k +1
           else
              kout = k
           end if
           vert_args(kout) = 0.01 * grid_g(ifm)%lev(k)
        end do
      end select
      !------------------------------------------------------------------------------------!
      ! 10. Check whether latitude is from south to north or from north to south in the    !
      !     input file.                                                                    !
      !------------------------------------------------------------------------------------!
      swapy= grid_g(ifm)%lat(1,1) <= grid_g(ifm)%lat(1,2)

      !------------------------------------------------------------------------------------!
      ! 11. Open the Vis5d file:                                                            !
      !------------------------------------------------------------------------------------!
      ierr = v5dcreate(outname,numtimes,numvars,nypo,nxpo,nzpo,varname,times,dates         &
                      ,compressmode,projection,proj_args,vertical,vert_args)
      !------------------------------------------------------------------------------------!
      ! 12. Loop over the time and variables, and convert them depending on the input      !
      !     format. For sigma-p coordinate, we read the surface and top pressure every     !
      !      timestep. Other coordinates, such as sigma-z or adaptive were already taken   !
      !      care.                                                                         !
      !------------------------------------------------------------------------------------!
      write (unit=*,fmt='(a,1x,i5,a)') ' [+] Getting data for grid: ',ifm,'...'
      nf = 1
      timeloop: do nt=1,nouttimes
         
         write (unit=*,fmt='(a,1x,2a)')                                                    &
            '   [-] Time: ',trim(outtimes(nt)%timestr),'...'

         varloop: do nv=1,nvars

            write (unit=*,fmt='(a,1x,2a)') '     [#] Variable: ',trim(vars(nv)),'...'
            fileloop: do 
               vadd = var_address(nf,nv,ifm)
               tadd = time_address(nf,nt,ifm)

               if (tadd == mevi_absent .or. vadd == mevi_absent) then
                  nf = mod(nf,nfiles)+1
                  cycle fileloop
               end if
               write (unit=*,fmt='(a,1x,2a)')                                              &
                   '       [~] File: ',trim(info_table(nf)%filename),'...'


               !---------------------------------------------------------------------------!
               ! a. This will load the variable and put at the appropiate scratch array.   !
               !    The variable will be almost ready to be put at the vis-5d file.        !
               !---------------------------------------------------------------------------!
               call load_variable(intype,nf,tadd,vadd,nxpo,nypo,offx,offy,stridex,stridey)
               
               !---------------------------------------------------------------------------!
               ! b. Final touches: vis5d requires the latitude to be from north to south,  !
               !    so we need to swap the latitude. Also, for WRF we need to interpolate  !
               !    to the static grid so it will be with actual pressure. Once the swap-  !
               !    ping is done, write the variable...                                    !
               !---------------------------------------------------------------------------!
               dimsiz = nxpo*nypo*nzpo(nv)
               select case (info_table(nf)%idim_type(vadd))
               case (22,62)
                  scratch%tmparr21(1:dimsiz) = scratch%outarr2d(1:dimsiz)
                  !---- The swapped version will be at 3D, even though this is a 2D... ----!
                  call swaplatv5d(nxpo,nypo,nzpo(nv),swapy,swapz                           &
                                 ,scratch%tmparr21(1:dimsiz),scratch%outarr2d(1:dimsiz))
                  !---- Switch undefined value by Vis-5D default --------------------------!
                  where (scratch%outarr2d(1:dimsiz) == missflg_real)
                      scratch%outarr2d(1:dimsiz) = missing
                  end where
                  write (unit=*,fmt='(2(a,1x),2a,2(1x,es14.7))')                           &
                     '         [|] Outputting :',info_table(nf)%varname(vadd),'...'        &
                             ,' range=',minval(scratch%outarr2d(1:dimsiz))                 &
                                       ,maxval(scratch%outarr2d(1:dimsiz))

                  !----- Write at the Vis-5D file -----------------------------------------!
                  ierr = v5dwrite(nt,nv,scratch%outarr2d(1:dimsiz))


               case (33,63)
                  scratch%tmparr31(1:dimsiz) = scratch%outarr3d(1:dimsiz)
                  !----- If this is a WRF run we need to interpolate in the vertical... ---!
                  if (if_adap == 2) then
                     !----- Load pressure -------------------------------------------------!
                     call load_wrf_pressure(nf,tadd,ifm)
                     write (unit=*,fmt='(a)')  &
                         '         [.] Interpolating in the vertical...'

                     !----- Copying original grid to output, respecting offset/stride -----!
                     call copy_grid(grid_g(ifm),nnxp(ifm),nnyp(ifm),nnzp(ifm),grid_o(ifm)  &
                                   ,nxpo,nypo,nnzp(ifm),offx,offy,0,stridex,stridey,1)

                     call interp_prevert(nxpo,nypo,nzpo(nv)                                &
                                        ,grid_o(ifm)%lev,grid_o(ifm)%pres                  &
                                        ,scratch%tmparr31(1:dimsiz)                        &
                                        ,scratch%tmparr36(1:dimsiz))
                     call swaplatv5d(nxpo,nypo,nzpo(nv),swapy,swapz                        &
                                    ,scratch%tmparr36(1:dimsiz),scratch%outarr3d(1:dimsiz))
                  else
                     call swaplatv5d(nxpo,nypo,nzpo(nv),swapy,swapz                        &
                                    ,scratch%tmparr31(1:dimsiz),scratch%outarr3d(1:dimsiz))
                  end if

                  !---- Switching undefined value by Vis-5D default -----------------------!
                  where (scratch%outarr3d(1:dimsiz) == missflg_real)
                      scratch%outarr3d(1:dimsiz) = missing
                  end where
                  write (unit=*,fmt='(2(a,1x),2a,2(1x,es14.7))')                           &
                     '         [|] Outputting :',trim(info_table(nf)%varname(vadd)),'...'  &
                             ,' range=',minval(scratch%outarr3d(1:dimsiz))                 &
                                       ,maxval(scratch%outarr3d(1:dimsiz))
                  !----- Writing at the Vis-5D file ---------------------------------------!
                  ierr = v5dwrite(nt,nv,scratch%outarr3d(1:dimsiz))
               end select
               exit fileloop
            end do fileloop
         end do varloop
      end do timeloop


      ierr = v5dclose()
   end do gridloop
#endif


   return

end subroutine v5d_output
!==========================================================================================!
!==========================================================================================!
