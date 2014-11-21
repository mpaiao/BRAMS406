!==========================================================================================!
!==========================================================================================!
!  MEVI. ralph2_output.f90                                                                 !
!                                                                                          !
!     This subroutine will loop through the variables, standardise the output, if needed,  !
! and write the RALPH2 files. The  variables that go to a ralph2 output are predefined,    !
! since this is for RAMS isentropic analysis only, so the input must contain all variables !
! needed.                                                                                  !
!==========================================================================================!
!==========================================================================================!
subroutine ralph2_output(intype,outpref)

   use mod_grid   , only : grid_g        & ! structure, intent(in)
                         , grid_o        & ! structure
                         , alloc_grid    & ! sub-routine
                         , nullify_grid  & ! sub-routine
                         , zero_grid     & ! sub-routine
                         , copy_grid     ! ! sub-routine
   use mod_ioopts , only : var_address   & ! intent(inout)
                         , mevi_absent   & ! intent(inout)
                         , mevi_badtype  & ! intent(inout)
                         , mevi_partly   & ! intent(inout)
                         , mevi_libvar   & ! intent(inout)
                         , nvars         & ! intent(inout)
                         , vars          & ! intent(inout)
                         , missflg_real  & ! intent(inout)
                         , outtimes      & ! intent(inout)
                         , nouttimes     & ! intent(inout)
                         , time_address  & ! intent(inout)
                         , lone          & ! intent(inout)
                         , lonw          & ! intent(inout)
                         , lats          & ! intent(inout)
                         , latn          & ! intent(inout)
                         , levb          & ! intent(inout)
                         , levt          & ! intent(inout)
                         , i1st          & ! intent(inout)
                         , ilast         & ! intent(inout)
                         , j1st          & ! intent(inout)
                         , jlast         & ! intent(inout)
                         , k1st          & ! intent(inout)
                         , klast         & ! intent(inout)
                         , ttta          & ! intent(in)
                         , tttz          & ! intent(in)
                         , strinx        & ! intent(in)
                         , striny        & ! intent(in)
                         , strinz        & ! intent(in)
                         , x_360         & ! intent(in)
                         , x_cyc         & ! intent(in)
                         , y_n2s         & ! intent(in)
                         , z_t2b         ! ! intent(in)
   use mod_maxdims, only : maxstr        & ! intent(in)
                         , nzpmax        ! ! intent(in)
   use mod_model  , only : ngrids        & ! intent(in)
                         , ihtran        & ! intent(in)
                         , if_adap       & ! intent(in)
                         , polelon       & ! intent(in)
                         , polelat       & ! intent(in)
                         , l2ndlat       & ! intent(in)
                         , centlon       & ! intent(in)
                         , centlat       & ! intent(in)
                         , deltaxn       & ! intent(in)
                         , deltayn       & ! intent(in)
                         , nnxp          & ! intent(in)
                         , nnyp          & ! intent(in)
                         , nnzp          ! ! intent(in)
   use an_header  , only : nfiles        & ! intent(in)
                         , info_table    ! ! intent(inout)
   use rconstants , only : spcon         & ! intent(in)
                         , grav          ! ! intent(in)
   use mod_scratch, only : scratch       ! ! intent(in)

   implicit none
   !----- Arguments. ----------------------------------------------------------------------!
   character(len=maxstr)                     , intent(in) :: intype
   character(len=maxstr)                     , intent(in) :: outpref
   !----- Local variables. ----------------------------------------------------------------!
   character(len=maxstr)                                  :: outname
   character(len=maxstr)                                  :: formlev
   character(len=maxstr)                                  :: formlon
   integer              , dimension(nzpmax)               :: levout
   real                                                   :: lonavg
   real                                                   :: latavg
   real                                                   :: levavg
   real                                                   :: dlon
   real                                                   :: dlat
   integer                                                :: ierr
   integer                                                :: ifm
   integer                                                :: nf
   integer                                                :: nt
   integer                                                :: nv
   integer                                                :: nxpo
   integer                                                :: nypo
   integer                                                :: nzpo
   integer                                                :: dimsiz
   integer                                                :: x
   integer                                                :: y
   integer                                                :: z
   integer                                                :: zrev
   integer                                                :: k
   integer                                                :: kout
   integer                                                :: vadd
   integer                                                :: tadd
   integer                                                :: stridex
   integer                                                :: stridey
   integer                                                :: stridez
   integer                                                :: offx
   integer                                                :: offy
   integer                                                :: offz
   !----- Local constants. ----------------------------------------------------------------!
   character(len=14)                         , parameter  :: frmt='(a,1x,i5,1x,a)'
   !---------------------------------------------------------------------------------------!


   !---------------------------------------------------------------------------------------!
   !   We first allocate the output grid structure                                         !
   !---------------------------------------------------------------------------------------!
   allocate (grid_o(ngrids))
   !---------------------------------------------------------------------------------------!


   !---------------------------------------------------------------------------------------!
   !   This big loop is to cover all grids.  Each RALPH2 file will have a single grid and  !
   ! a single time, with the full domain.                                                  !
   !---------------------------------------------------------------------------------------!
   write(unit=*,fmt='(80a)') ('-',nt=1,80)
   gridloop: do ifm = 1, ngrids
      write (unit=*,fmt='(a,1x,i5,a)') ' [+] Getting data for grid: ',ifm,'...'


      !----- Find the longitude-related dimensions. ---------------------------------------!
      offx    = max(i1st(ifm),1)-1
      stridex = max(strinx(ifm),1)
      nxpo    = 1 + (min(ilast(ifm),nnxp(ifm)) - offx - 1)/stridex
      dlon    = (lone(ifm)-lonw(ifm)) / (nxpo-1)
      !------------------------------------------------------------------------------------!



      !----- Find the latitude-related dimensions -----------------------------------------!
      offy    = max(j1st(ifm),1)-1
      stridey = max(striny(ifm),1)
      nypo    = 1 + (min(jlast(ifm),nnyp(ifm)) - offy - 1)/stridey
      dlat    = (latn(ifm)-lats(ifm)) / (nypo-1)
      !------------------------------------------------------------------------------------!



      !----- Find the latitude-related dimensions -----------------------------------------!
      offz    = max(k1st(ifm),1)-1
      stridez = max(strinz(ifm),1)
      nzpo    = 1 + (min(klast(ifm),nnzp(ifm)) - offz - 1)/stridez
      !------------------------------------------------------------------------------------!


      !----- Swap vertical levels if needed be, and convert it to hPa. --------------------!
      if (z_t2b) then
         zrev = k1st(ifm)
         do z = nzpo,1,-1
            levout(z) = nint(grid_g(ifm)%lev(zrev) * 0.01)
            zrev = zrev + stridez
         end do
      else
      end if
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!
      !    Initialise output grid                                                          !
      !------------------------------------------------------------------------------------!
      call nullify_grid(grid_o(ifm))
      call alloc_grid(grid_o(ifm),nxpo,nypo,nzpo)
      call zero_grid(grid_o(ifm))
      !------------------------------------------------------------------------------------!


      !----- Find the formats for the levels and the longitude. ---------------------------!
      write(formlev,fmt='(a,i3.3,a)') '(',nzpo,'(1x,i4))'
      write(formlon,fmt='(a,i3.3,a)') '(',nxpo,'(1x,f11.5))'
      !------------------------------------------------------------------------------------!




      !------------------------------------------------------------------------------------!
      !  Big loop for time.                                                                !
      !------------------------------------------------------------------------------------!
      nf = 1
      timeloop: do nt=1,nouttimes
         write (unit=*,fmt='(a,1x,2a)')                                                    &
            '   [-] Time: ',trim(outtimes(nt)%timestr),'...'

         write(outname,fmt='(3a,i2.2,a,i4.4,2(a,i2.2),a,2i2.2)')  trim(outpref),'-'        &
                                                                 ,'g',ifm,'-'              &
                                                                 ,outtimes(nt)%year,'-'    &
                                                                 ,outtimes(nt)%month,'-'   &
                                                                 ,outtimes(nt)%day,'-'     &
                                                                 ,outtimes(nt)%hour        &
                                                                 ,outtimes(nt)%minu
         write(unit=*,fmt='(a,1x,a)') '     [#] Creating file:',trim(outname)

         !----- Open the file and start the new section. ----------------------------------!
         open(unit=16,file=trim(outname),status='replace',action='write')

         !----- First line, date and dataset dimensions -----------------------------------!
         write(unit=16,fmt='(i4,3(1x,i2.2),3(1x,i3),4(1x,f9.4))') outtimes(nt)%year        &
                                                                 ,outtimes(nt)%month       &
                                                                 ,outtimes(nt)%day         &
                                                                 ,outtimes(nt)%hour        &
                                                                 ,nzpo,nxpo,nypo           &
                                                                 ,lonw(ifm),lats(ifm)      &
                                                                 ,dlon,dlat
         !----- Second line, vertical levels ----------------------------------------------!
         write(unit=16,fmt=formlev) (levout(z),z=1,nzpo)


         !----- Load variables. -----------------------------------------------------------!
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
               !    This will load the variable and put at the appropiate scratch array.   !
               ! The variable will be almost ready to be put at the RALPH2 file.           !
               !---------------------------------------------------------------------------!
               call load_variable(intype,nf,tadd,vadd,nxpo,nypo,offx,offy,stridex,stridey)

               dimsiz = nxpo*nypo*nzpo
               select case (info_table(nf)%idim_type(vadd))
               case (33)
                  scratch%tmparr36(1:dimsiz) = scratch%outarr3d(1:dimsiz)
                  !----- If this is a WRF run we need to interpolate in the vertical... ---!
                  if (if_adap == 2) then
                     !----- Load pressure -------------------------------------------------!
                     call load_wrf_pressure(nf,tadd,ifm)
                     write (unit=*,fmt='(a)')                                              &
                                           '         [.] Interpolating in the vertical...'

                     !----- Copying original grid to output, respecting offset/stride -----!
                     call copy_grid(grid_g(ifm),nnxp(ifm),nnyp(ifm),nnzp(ifm),grid_o(ifm)  &
                                   ,nxpo,nypo,nzpo,offx,offy,offz,stridex,stridey,stridez)

                     call interp_prevert(nxpo,nypo,nzpo,grid_o(ifm)%lev,grid_o(ifm)%pres   &
                                        ,scratch%tmparr36(1:dimsiz)                        &
                                        ,scratch%outarr3d(1:dimsiz))
                  end if
                  !------------------------------------------------------------------------!



                  !------------------------------------------------------------------------!
                  !    Swap the output so it always has latitude going from south to north !
                  ! and vertical level going from bottom to top.                           !
                  !------------------------------------------------------------------------!
                  call swaplatlev(nxpo,nypo,nzpo,y_n2s,z_t2b,scratch%outarr3d(1:dimsiz)    &
                                 ,scratch%tmparr36(1:dimsiz))
                  !------------------------------------------------------------------------!


                  !------------------------------------------------------------------------!
                  !     Ralph2 output is done level by level, which means that we must     !
                  ! store all variables in temporary arrays before we create the output.   !
                  !------------------------------------------------------------------------!
                  select case(nv)
                  case (1)
                     scratch%tmparr31(1:dimsiz) = scratch%tmparr36(1:dimsiz)
                  case (2)
                     scratch%tmparr32(1:dimsiz) = scratch%tmparr36(1:dimsiz)
                  case (3)
                     scratch%tmparr33(1:dimsiz) = scratch%tmparr36(1:dimsiz)
                  case (4)
                     scratch%tmparr34(1:dimsiz) = scratch%tmparr36(1:dimsiz) / grav
                  case (5)
                     scratch%tmparr35(1:dimsiz) = scratch%tmparr36(1:dimsiz) * 0.01
                     !----- Make sure we have no negative variables or super-saturation. --!
                     where (scratch%tmparr35 < 0.01)
                        scratch%tmparr35 = 0.01
                     end where
                     where (scratch%tmparr35 > 1.00)
                        scratch%tmparr35 = 1.00
                     end where
                     !---------------------------------------------------------------------!
                  case default
                     write (unit=*,fmt='(a,1x,i5)') ' NVARS =',nvars
                     call fatal_error('RALPH2 output cannot output more than 5 variables!' &
                                     ,'ralph2_output','ralph2_output.f90')
                  end select
               case default
                  write (unit=*,fmt='(a,1x,i6)') ' NF        =',nf
                  write (unit=*,fmt='(a,1x,a) ') ' VARNAME   ='                            &
                                                ,trim(info_table(nf)%varname(vadd))
                  write (unit=*,fmt='(a,1x,i6)') ' IDIM_TYPE ='                            &
                                                ,info_table(nf)%idim_type(vadd)
                  call fatal_error('Invalid variable type in RALPH2 output'                &
                                  ,'ralph2_output','ralph2_output.f90')
               end select
               exit fileloop
            end do fileloop
         end do varloop
         !---------------------------------------------------------------------------------!


         !---------------------------------------------------------------------------------!
         !      Copy the variables to temporary 2-D matrices and write on the output file. !
         !---------------------------------------------------------------------------------!
         do z=1,nzpo
            do y=1,nypo
               call xyz_2_x(nxpo,nypo,nzpo,y,z,scratch%tmparr31,scratch%tmparr16)
               write(unit=16,fmt=formlon) (scratch%tmparr16(x),x=1,nxpo)
            end do
            do y=1,nypo
               call xyz_2_x(nxpo,nypo,nzpo,y,z,scratch%tmparr32,scratch%tmparr16)
               write(unit=16,fmt=formlon) (scratch%tmparr16(x),x=1,nxpo)
            end do
            do y=1,nypo
               call xyz_2_x(nxpo,nypo,nzpo,y,z,scratch%tmparr33,scratch%tmparr16)
               write(unit=16,fmt=formlon) (scratch%tmparr16(x),x=1,nxpo)
            end do
            do y=1,nypo
               call xyz_2_x(nxpo,nypo,nzpo,y,z,scratch%tmparr34,scratch%tmparr16)
               write(unit=16,fmt=formlon) (scratch%tmparr16(x),x=1,nxpo)
            end do
            do y=1,nypo
               call xyz_2_x(nxpo,nypo,nzpo,y,z,scratch%tmparr35,scratch%tmparr16)
               write(unit=16,fmt=formlon) (scratch%tmparr16(x),x=1,nxpo)
            end do
         end do
         !----- Close file. ---------------------------------------------------------------!
         close(unit=16,status='keep')
         !---------------------------------------------------------------------------------!
      end do timeloop
      !------------------------------------------------------------------------------------!
   end do gridloop
   write(unit=*,fmt='(80a)') ('-',nt=1,80)
   write(unit=*,fmt='(a)'  ) ' '
   !---------------------------------------------------------------------------------------!

   return
end subroutine ralph2_output
!==========================================================================================!
!==========================================================================================!
