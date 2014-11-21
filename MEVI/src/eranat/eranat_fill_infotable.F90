!==========================================================================================!
!==========================================================================================!
!  MEVI. Subroutine eranat_fill_infotable                                                  !
!                                                                                          !
!     This subroutine will loop through the ECMWF reanalysis files, and fill the informa-  !
! tion table with the information retrieved from the netcdf files. Here we will follow     !
! RAMS notation as sort of standard. After the ECMWF variables are read, they will be in-  !
! line with RAMS to ease the output step. Note that this subroutine will do something only !
! if netcdf libraries are loaded, otherwise this subroutine will be a dummy one.           !
!==========================================================================================!
!==========================================================================================!
subroutine eranat_fill_infotable(flnm)
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
   use mod_ncdf_globio, only : ncdf_load_err
   use netcdf
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
                          ,'eranat_fill_infotable','eranat_fill_infotable.F90'             )
      end if


      !----- Reading the header and extracting the file main dimensions -------------------!
      ierr = nf90_inquire(ncid,ndimensions,nvariables,nglobals,unlimiteddimid)
      info_table(nf)%filename = fnames(nf)
      info_table(nf)%nvars    = nvariables
      
      !----- Getting some global arguments ------------------------------------------------!
      write (unit=*,fmt='(a)') '     - Loading global attributes and dimensions...'
      call commio_eranat(ngrid,ntimes)
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

      do nv=1,nvariables
         call eranat_load_var_table(nv,ngrid                                               &
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
                          ,'eranat_fill_infotable','eranat_fill_infotable.F90'             ) 
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
   call fatal_error('You can''t use ECMWF input without linking netcdf libraries...'       &
                   ,'eranat_fill_infotable','eranat_fill_infotable.F90')
#endif

   return
end subroutine eranat_fill_infotable
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine eranat_load_var_table(nv,current_grid,varname,npointer,idim_type,ngrid,nvalues  &
                                ,rank,dims,stagger)
   use mod_maxdims, only: maxrank,maxstr
#if USE_NCDF
   use netcdf
   use mod_netcdf     , only : ncid,nvariables,varid,xtype,dummy_vname,ndims,dimids,natts  &
                              ,idnnxp,idnnyp,idnnzp ,idtimes,idnpatch,idnclouds,idnnxpst   &
                              ,idnnypst,idnnzpst,idnzg
   use mod_ioopts     , only : missflg_int,missflg_real,missflg_char
   use mod_model      , only : nnxp,nnyp,nnzp,nwave,nclouds,npatch,nzg,nzs
   use mod_ncdf_globio, only : ncdf_load_err
#endif
   implicit none

   integer                    , intent(in)  :: nv, current_grid
   character(len=*)           , intent(out) :: varname
   integer                    , intent(out) :: npointer
   integer                    , intent(out) :: idim_type
   integer                    , intent(out) :: ngrid
   integer                    , intent(out) :: nvalues
   integer                    , intent(out) :: rank
   integer, dimension(maxrank), intent(out) :: dims
   logical, dimension(maxrank), intent(out) :: stagger
   !----- Local variables -----------------------------------------------------------------!
   integer                                  :: ierr
   character(len=maxstr)                    :: memorder
   character(len=maxstr)                    :: stagdim
   character(len=maxstr)                    :: vareranat
   !---------------------------------------------------------------------------------------!
   !   
#if USE_NCDF
   ierr = nf90_inquire_variable(ncid,nv,varname,xtype,ndims,dimids,natts)
   
   !----- Crash in case something went wrong ----------------------------------------------!
   if (ierr /= NF90_NOERR) then
      call ncdf_load_err(ierr)
      call fatal_error (                                                                   &
          'Unable to get variable information for variable '//trim(varname)//'...'         &
         ,'eranat_load_var_table','eranat_fill_infotable.F90')
   end if

   !---- The following variables are just for BRAMS files, fill it with anything ----------! 
   npointer  = missflg_int
   nvalues   = missflg_int
   ngrid     = current_grid
   
   !---- Initialising dims and stagger with "default" values ------------------------------!
   stagger   = .false.
   dims      = 1
   idim_type = 99

   !---------------------------------------------------------------------------------------!
   !     Now I decide which category the variable falls in. Currently only real vectors    !
   ! and arrays and all scalars are stored with special dimension flag. Otherwise, we save !
   ! the variable with the default 99 category, which currently means "ignore me".         !
   !     Rank is the rank of each array (0 being a scalar, 1 a 1-D vector, 2 a 2-D, and so !
   ! on). The value assigned to idim_type is standardised for all models and the look-up   !
   ! table is available at ${MEVI_ROOT}/doc/dimension_table.txt.                           !
   !---------------------------------------------------------------------------------------!
   !----- Integer, only  --------------------------------------------!
   if (xtype == NF90_INT .and. ndims == 1) then
      idim_type = 80
      rank      = 1
   !----- Real, scalar, vectors and higher-rank arrays are considered, check everything ---!
   elseif (xtype == NF90_FLOAT) then
      select case(ndims)
      !------------------------------------------------------------------------------------!
      !   1D fields.  Reals, only 1D vectors are considered.                               !
      !------------------------------------------------------------------------------------!
      case(1)
         rank      = 1
         if (dimids(1) == idnnzp) then
            idim_type = 13
            dims(1) = nnzp(ngrid)
         elseif (dimids(1) == idnnxp) then
            dims(1) = nnxp(ngrid)
         elseif (dimids(1) == idnnyp) then
            dims(1) = nnyp(ngrid)
         end if

      !------------------------------------------------------------------------------------!
      !   2D fields. Currently only XY variables are known...                              !
      !------------------------------------------------------------------------------------!
      case (2)
         rank = 2
         !----- Currently the only 2D dimension is X and Y. -------------------------------!
         idim_type = 22

         !----- X dimension ---------------------------------------------------------------!
         if (dimids(1) == idnnxp) then
            dims(1) = nnxp(ngrid)
         else 
            write (unit=*,fmt='(3(a,1x,i5,1x))')                                           &
               'Dimids(1)=',dimids(1),'IDNNXP=',idnnxp,'IDNNXPST=',idnnxpst
            call fatal_error ('X Dimension is wrong for '//trim(varname)//'!!!'            &
                             ,'eranat_load_var_table','eranat_fill_infotable.F90')
         end if

         !----- Y dimension ---------------------------------------------------------------!
         if (dimids(2) == idnnyp) then
            dims(2) = nnyp(ngrid)
         else 
            call fatal_error ('Y Dimension is wrong for '//trim(varname)//'!!!'            &
                             ,'eranat_load_var_table','eranat_fill_infotable.F90')
         end if


      !------------------------------------------------------------------------------------!
      !   3D fields. There are some possible idim_type, but all of them  have X and Y,     !
      !              what changes is the third dimension.                                  !
      !------------------------------------------------------------------------------------!
      case (3)
         rank = 3
         !----- X dimension ---------------------------------------------------------------!
         if (dimids(1) == idnnxp) then
            dims(1) = nnxp(ngrid)
         else 
            call fatal_error ('X Dimension is wrong for '//trim(varname)//'!!!'            &
                             ,'eranat_load_var_table','eranat_fill_infotable.F90')
         end if
         !----- Y dimension ---------------------------------------------------------------!
         if (dimids(2) == idnnyp) then
            dims(2) = nnyp(ngrid)
         else 
            call fatal_error ('Y Dimension is wrong for '//trim(varname)//'!!!'            &
                             ,'eranat_load_var_table','eranat_fill_infotable.F90')
         end if

         !----- 3rd dimension -------------------------------------------------------------!
         if (dimids(3) == idnnzp) then
            idim_type = 33
            dims(3) = nnzp(ngrid)
         else 
            call fatal_error ('Y Dimension is wrong for '//trim(varname)//'!!!'            &
                             ,'eranat_load_var_table','eranat_fill_infotable.F90')
         end if
      end select
   end if
#else
    varname   = missflg_char
    npointer  = missflg_int
    idim_type = missflg_int
    ngrid     = missflg_int
    nvalues   = missflg_int
    rank      = missflg_int
    dims      = missflg_int
    stagger   = .false.
#endif


   return
end subroutine eranat_load_var_table
