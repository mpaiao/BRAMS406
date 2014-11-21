!==========================================================================================!
!==========================================================================================!
!  MEVI. Subroutine zwrf_fill_infotable                                                    !
!                                                                                          !
!     This subroutine will loop through the simple WRF files, using z coordinates and no   !
! grid information, and fill the information table with the information retrieved from the !
! netcdf files. Here we will follow RAMS notation as sort of standard. After the WRF vari- !
! ables are read, they will be inline with RAMS to ease the output step. Note that this    !
! subroutine will do something only if netcdf libraries are loaded, otherwise this sub-    !
! routine will be a dummy one.                                                             !
!==========================================================================================!
!==========================================================================================!
subroutine zwrf_fill_infotable(flnm)
   !---------------------------------------------------------------------------------------!
   !    This subroutine reads the RAMS/BRAMS header files, seeking information on avail-   !
   ! able variables, and their properties such as dimensions, position, etc.               !
   !---------------------------------------------------------------------------------------!
   use mod_maxdims    , only : maxstr,maxfiles,maxrank
   use mod_model      , only : if_adap,ngrids,nzg,nzs,nclouds,nwave,naddsc,npatch,polelon  &
                              ,polelat,centlon,centlat,nnxp,nnyp,nnzp,this_time,zero_time
   use an_header      , only : nfiles, info_table, alloc_anheader, nullify_anheader
   use mod_ioopts     , only : outtimes,nouttimes
#if USE_NCDF
   use mod_netcdf     , only : ncid,ndimensions,nvariables,nglobals,unlimiteddimid         &
                              ,xtype,timeid,dummy_vname,ndims,dimids,natts,dimglobal       &
                              ,globalid
   use mod_ncdf_globio, only : ncdf_load_err
   use netcdf
#endif
   implicit none

   character(len=maxstr) , intent(in)          :: flnm        ! File prefix

#if USE_NCDF
   character(len=maxstr)                       :: flnm_full   ! Scratch w/ prefix & suffix
   character(len=maxstr) , dimension(maxfiles) :: fnames      ! File name list
   integer                                     :: na,nd,nf    ! Various counters
   integer                                     :: nv, nt,tcnt ! Various counters
   integer               , pointer             :: ifm         ! Shortcut to current grid
   integer                                     :: nvbtab      ! Scratch for var count
   integer                                     :: itrim       ! String length
   logical                                     :: readlist    ! Flag for file list reading
   integer                                     :: ierr        ! Error flag.
   integer                                     :: ngrid       ! Error flag.
   integer                                     :: ntimes      ! Shortcut for # of times/file

   !----- Deallocating table if necessary, it will be allocated soon. ---------------------!
   if(allocated(info_table)) deallocate(info_table)

   !----- Initialising the number of times ------------------------------------------------!
   nouttimes = 0

   !---------------------------------------------------------------------------------------!
   !     Finding out how many data files I have:                                           !
   !---------------------------------------------------------------------------------------!
   itrim = len_trim(flnm)
   select case (flnm(itrim-11:itrim))
   case ('filelist.inp')
      !----- The input is in a file, so I will read this file -----------------------------!
      flnm_full=trim(flnm)
      readlist=.true.
   case default
      !----- Use internal procedures to determine the files -------------------------------!
      flnm_full=trim(flnm)//'_ctl*'
      readlist=.false.
   end select
   call filelist_f(fnames,flnm_full,nfiles,readlist)

   !----- Now I will allocate the variable table accordingly ------------------------------!
   allocate (info_table(nfiles))

   !----- Loop through the files and store the variable info ------------------------------!
   fileloop: do nf=1,nfiles
      write (unit=*,fmt='(a,1x,2a)') ' [+] Opening file :',trim(fnames(nf)),'...'

      !----- Opening the file -------------------------------------------------------------!
      ierr = nf90_open(fnames(nf),NF90_NOWRITE,ncid)
      if (ierr /= NF90_NOERR) then
         call ncdf_load_err(ierr)
         call fatal_error ('Error opening the file '//trim(fnames(nf))//'...'              &
                          ,'wrf_fill_infotable','wrf_fill_infotable.F90'                   ) 
      end if


      !----- Reading the header and extracting the file main dimensions -------------------!
      ierr = nf90_inquire(ncid,ndimensions,nvariables,nglobals,unlimiteddimid)
      info_table(nf)%filename = fnames(nf)
      info_table(nf)%nvars    = nvariables
      
      !----- Getting some global arguments ------------------------------------------------!
      write (unit=*,fmt='(a)') '     - Loading global attributes and dimensions...'
      call commio_zwrf(ngrid,ntimes)
      info_table(nf)%ngrids    = ngrids
      info_table(nf)%ntimes    = ntimes
      info_table(nf)%init_time = zero_time
      
      !----- Initialising the analysis structure for this file ----------------------------!
      call nullify_anheader(info_table(nf))
      call alloc_anheader(info_table(nf))
      info_table(nf)%avail_grid(:)       = .false.
      info_table(nf)%avail_grid(ngrid)   = .true.
      info_table(nf)%file_time(1:ntimes) = this_time(1:ntimes)

      !----- Adding the new times into the outtimes array ---------------------------------!
      addtimeloop: do nt=1,ntimes
         !----- I will only add times that were not there before --------------------------!
         if (nouttimes > 0) then
            if (any(outtimes(1:nouttimes)%elapsed == this_time(nt)%elapsed)) then
               do tcnt=1,nouttimes
                  write (unit=*,fmt='(3(a,1x,i5,1x),2(a,1x,es12.5,1x),a,1x,l1)')           &
                     'TCNT=',tcnt,'NOUTTIMES=',nouttimes,'NT=',nt                          &
                    ,'OUTTIMES(TCNT)=',outtimes(tcnt)%elapsed                              &
                    ,'THIS_TIME     =',this_time(nt)%elapsed                               &
                    ,'ALL_THE_SAME  =',outtimes(tcnt)%elapsed == this_time(nt)%elapsed
               end do
               cycle addtimeloop
            end if
         end if
         nouttimes=nouttimes+1
         outtimes(nouttimes) = this_time(nt)
      end do addtimeloop
      print *, 'NOUTTIMES=',nouttimes
      call sort_time(nouttimes,outtimes(1:nouttimes))

      !----- Getting the variable information----------------------------------------------!
      write (unit=*,fmt='(a)') ' '
      write (unit=*,fmt='(a)') '-----------------------------------------------------------'
      write (unit=*,fmt='(a)') ' - Loading variable table...'

      do nv=1,nvariables
         call wrf_load_var_table(nv,ngrid                                                  &
                             ,info_table(nf)%varname(nv)  , info_table(nf)%npointer(nv)    &
                             ,info_table(nf)%idim_type(nv), info_table(nf)%ngrid(nv)       &
                             ,info_table(nf)%nvalues(nv)  , info_table(nf)%rank(nv)        &
                             ,info_table(nf)%dims(:,nv)   , info_table(nf)%stagger(:,nv)   )
         write (unit=*,fmt='(a,1x,a,1x,2(a,1x,i6,1x))') &
            '    [|] Retrieving:',info_table(nf)%varname(nv)                               &
           , 'idim_type=',info_table(nf)%idim_type(nv),'rank=',info_table(nf)%rank(nv)
      end do
      
      !----- Closing the file. It will be opened again later when we will read the data ---!
      ierr = nf90_close(ncid)
      if (ierr /= NF90_NOERR) then
         call ncdf_load_err(ierr)
         call fatal_error ('Error closing the file '//trim(fnames(nf))//'...'              &
                          ,'wrf_fill_infotable','wrf_fill_infotable.F90'                   ) 
      end if

   end do fileloop
#endif

   return
end subroutine zwrf_fill_infotable
!==========================================================================================!
!==========================================================================================!
