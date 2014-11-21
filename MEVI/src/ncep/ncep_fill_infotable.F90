!==========================================================================================!
!==========================================================================================!
!  MEVI. Subroutine ncep_fill_infotable                                                    !
!                                                                                          !
!     This subroutine will loop through the NCEP reanalysis files, and fill the informa-   !
! tion table with the information retrieved from the netcdf files. Here we will follow     !
! RAMS notation as sort of standard. After the NCEP variables are read, they will be in-   !
! line with RAMS to ease the output step. Note that this subroutine will do something only !
! if netcdf libraries are loaded, otherwise this subroutine will be a dummy one.           !
!==========================================================================================!
!==========================================================================================!
subroutine ncep_fill_infotable(flnm)
   use mod_maxdims    , only : maxstr,maxfiles,maxrank
   use mod_model      , only : if_adap,ngrids,nzg,nzs,nclouds,nwave,naddsc,npatch,polelon  &
                              ,polelat,centlon,centlat,nnxp,nnyp,nnzp,this_time,zero_time  &
                              ,ztn
   use an_header      , only : nfiles, info_table, alloc_anheader, nullify_anheader
   use mod_ioopts     , only : outtimes,nouttimes,missflg_char

#if USE_NCDF
   use mod_netcdf     , only : ncid,ndimensions,nvariables,nglobals,unlimiteddimid         &
                              ,xtype,timeid,dummy_vname,ndims,dimids,natts,dimglobal       &
                              ,globalid
   use netcdf
   use mod_ncdf_globio, only : ncdf_load_err
#endif
   implicit none

   character(len=maxstr) , intent(in)          :: flnm        ! File prefix

#if USE_NCDF
   character(len=maxstr)                       :: flnm_full   ! Scratch w/ prefix & suffix
   character(len=maxstr) , dimension(maxfiles) :: fnames      ! File name list
   integer                                     :: na,nd,nf    ! Various counters
   integer                                     :: nv,nt,tcnt  ! Various counters
   integer               , pointer             :: ifm         ! Shortcut to current grid
   integer                                     :: nvbtab      ! Scratch for var count
   integer                                     :: itrim       ! String length
   logical                                     :: readlist    ! Flag for file list reading
   integer                                     :: ierr        ! Error flag.
   integer                                     :: ngrid       ! Error flag.
   integer                                     :: ntimes      ! Shortcut for # of times/file
   integer                                     :: avant       ! Cleaning handle
   integer                                     :: oldnfiles   ! # of files after cleaning
   !----- Deallocating table if necessary, it will be allocated soon. ---------------------!
   if(allocated(info_table)) deallocate(info_table)

   !----- Initialising the number of times and vertical levels ----------------------------!
   nouttimes = 0
   nnzp      = 0

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
      flnm_full=trim(flnm)//'/*nc'
      readlist=.false.
   end select

   !---------------------------------------------------------------------------------------!
   !    In case the user didn't provide the filelist.inp, clean files other than .nc       !
   !---------------------------------------------------------------------------------------!
   call filelist_f(fnames,flnm_full,nfiles,readlist)
   oldnfiles = nfiles
   nf = 1
   do while (nf <= nfiles)
      itrim = len_trim(fnames(nf))
      if (fnames(nf)(itrim-2:itrim) == '.nc' .or. fnames(nf)(itrim-2:itrim) == '.NC') then
         nf = nf + 1
      else
         do avant=nf,nfiles-1
            fnames(avant) = fnames(avant+1)     
         end do
         nfiles = nfiles - 1
      end if
   end do
   fnames(nfiles+1:oldnfiles) = missflg_char


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
                          ,'ncep_fill_infotable','ncep_fill_infotable.F90'                   ) 
      end if


      !----- Reading the header and extracting the file main dimensions -------------------!
      ierr = nf90_inquire(ncid,ndimensions,nvariables,nglobals,unlimiteddimid)
      info_table(nf)%filename = fnames(nf)
      info_table(nf)%nvars    = nvariables
      
      !----- Getting some global arguments ------------------------------------------------!
      write (unit=*,fmt='(a)') '     - Loading global attributes and dimensions...'
      call commio_ncep(ngrid,ntimes)
      info_table(nf)%ngrids    = ngrids
      info_table(nf)%ntimes    = ntimes
      info_table(nf)%init_time = this_time(1)

      !----- Initialising the analysis structure for this file ----------------------------!
      call nullify_anheader(info_table(nf))
      call alloc_anheader(info_table(nf))
      info_table(nf)%avail_grid(:)       = .false.
      info_table(nf)%avail_grid(ngrid)   = .true.
      info_table(nf)%file_time(1:ntimes) = this_time(1:ntimes)


      !----- Adding the new times into the outtimes array ---------------------------------!
      tcnt=0
      addtimeloop: do nt=1,ntimes
         !----- I will only add times that were not there before --------------------------!
         if (nouttimes > 0) then
           if (any(outtimes(1:nouttimes)%elapsed == this_time(nt)%elapsed))                &
              cycle addtimeloop
         end if
         tcnt=tcnt+1
         outtimes(nouttimes+tcnt) = this_time(nt)
      end do addtimeloop
      !----- Updating time and sorting them up --------------------------------------------!
      nouttimes = nouttimes + tcnt
      call sort_time(nouttimes,outtimes(1:nouttimes))

      !----- Getting the variable information----------------------------------------------!
      write (unit=*,fmt='(a)') ' '
      write (unit=*,fmt='(a)') '-----------------------------------------------------------'
      write (unit=*,fmt='(a)') ' - Loading variable table...'

      !------------------------------------------------------------------------------------!
      !     Even though we are downloading NCEP, the way to build the table for NCEP is    !
      ! the same as for ECMWF, so "borrow" the ECMWF subroutine...                         !
      !------------------------------------------------------------------------------------!
      do nv=1,nvariables
         call ecmwf_load_var_table(nv,ngrid                                                &
                             ,info_table(nf)%varname(nv)  , info_table(nf)%npointer(nv)    &
                             ,info_table(nf)%idim_type(nv), info_table(nf)%ngrid(nv)       &
                             ,info_table(nf)%nvalues(nv)  , info_table(nf)%rank(nv)        &
                             ,info_table(nf)%dims(:,nv)   , info_table(nf)%stagger(:,nv)   )
         write (unit=*,fmt='(a,1x,a,1x,2(a,1x,i6,1x))') &
            '         [|] Retrieving:',info_table(nf)%varname(nv)                          &
           , 'idim_type=',info_table(nf)%idim_type(nv),'rank=',info_table(nf)%rank(nv)
      end do
      
      !----- Closing the file. It will be opened again later when we will read the data ---!
      ierr = nf90_close(ncid)
      if (ierr /= NF90_NOERR) then
         call ncdf_load_err(ierr)
         call fatal_error ('Error closing the file '//trim(fnames(nf))//'...'              &
                          ,'ncep_fill_infotable','ncep_fill_infotable.F90'                 ) 
      end if
   end do fileloop
   !---------------------------------------------------------------------------------------!
   !     Here we need to fix the number of vertical levels. In case all that was provided  !
   ! was surface variables, nnzp by 1 and set up any junk for ztn(1,:).                    !
   !---------------------------------------------------------------------------------------!
   where (nnzp(1:ngrids) == 0)
      nnzp(1:ngrids)  = 1
      ztn(1,1:ngrids) = 1.
   end where
   
   return
#else
   call fatal_error('You can''t use ncep input without compiling with netcdf libraries...' &
                   ,'ncep_fill_infotable','ncep_fill_infotable.F90')
#endif

   return
end subroutine ncep_fill_infotable
!==========================================================================================!
!==========================================================================================!
