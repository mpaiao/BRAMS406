!==========================================================================================!
!==========================================================================================!
! Copyright (C) 1991-2004  ; All Rights Reserved ; ATMET, LLC                              !
!                                                                                          !
! This file is free software; you can redistribute it and/or modify it under the           !
! terms of the GNU General Public License as published by the Free Software                !
! Foundation; either version 2 of the License, or (at your option) any later version.      !
!                                                                                          !
! This software is distributed in the hope that it will be useful, but WITHOUT ANY         !
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A          !
! PARTICULAR PURPOSE.  See the GNU General Public License for more details.                !
!                                                                                          !
! You should have received a copy of the GNU General Public License along with this        !
! program; if not, write to the Free Software Foundation, Inc.,                            !
! 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.                                 !
!==========================================================================================!
!==========================================================================================!
subroutine rams_fill_infotable_vfm(flnm)
   !---------------------------------------------------------------------------------------!
   !    This subroutine reads the RAMS/BRAMS header files, seeking information on avail-   !
   ! able variables, and their properties such as dimensions, position, etc.               !
   !---------------------------------------------------------------------------------------!
   use mod_maxdims , only : maxstr,maxfiles,maxrank
   use mod_model   , only : if_adap,ngrids,nzg,nzs,nclouds,nwave,naddsc,npatch,polelon     &
                           ,polelat,centlon,centlat,nnxp,nnyp,nnzp,this_time,zero_time
   use an_header   , only : nfiles, info_table, alloc_anheader, nullify_anheader
   use mod_ioopts  , only : outtimes,nouttimes
   implicit none

   character(len=maxstr) , intent(in)          :: flnm        ! File prefix

   character(len=maxstr)                       :: flnm_full   ! Scratch w/ prefix & suffix
   character(len=maxstr) , dimension(maxfiles) :: fnames      ! File name list
   integer                                     :: nv, nf      ! Variable and file counter
   integer               , pointer             :: ifm         ! Shortcut to current grid
   integer                                     :: nvbtab      ! Scratch for var count
   integer                                     :: itrim       ! String length
   integer                                     :: input_dtype ! Var type as is from RAMS
   logical                                     :: readlist    ! Flag for file list reading

   !----- Deallocating table if necessary, it will be allocated soon. ---------------------!
   if(allocated(info_table)) deallocate(info_table)


   !---------------------------------------------------------------------------------------!
   !     Finding out how many times with data I have:                                      !
   !---------------------------------------------------------------------------------------!
   itrim = len_trim(flnm)
   select case (flnm(itrim-11:itrim))
   case ('filelist.inp')
      !----- The input is in a file, so I will read this file -----------------------------!
      flnm_full=trim(flnm)
      readlist=.true.
   case default
      !----- Appending the extension if needed --------------------------------------------!
      flnm_full=trim(flnm)//'*-head.txt'
      readlist=.false.
   end select
   call filelist_f(fnames,flnm_full,nfiles,readlist)

   !----- Now I will allocate the variable table accordingly ------------------------------!
   allocate (info_table(nfiles))

   !----- Loop through the files and store the variable info ------------------------------!
   fileloop: do nf=1,nfiles
      write (unit=*,fmt='(a,1x,2a)') ' [+] Opening file :',trim(fnames(nf)),'...'
      !----- Reading the header and extracting the variable table info --------------------!
      open(unit=60,file=trim(fnames(nf)),status='old')
      read(unit=60,fmt=*) nvbtab
      
      !----- Filling values that do not depend on the variables ---------------------------!
      info_table(nf)%filename = fnames(nf)
      info_table(nf)%nvars    = nvbtab
      info_table(nf)%ntimes   = 1          ! Only one time per file on BRAMS

      !----- Initialising the analysis structure for this time ----------------------------!
      call nullify_anheader(info_table(nf))
      call alloc_anheader(info_table(nf))
      
      !----- Getting the header information -----------------------------------------------!
      do nv=1,nvbtab
         read(unit=60,fmt=*)   info_table(nf)%varname  (nv)  &
                              ,info_table(nf)%npointer (nv)  &
                              ,input_dtype                   &
                              ,info_table(nf)%ngrid    (nv)  &
                              ,info_table(nf)%nvalues  (nv)

         !---------------------------------------------------------------------------------!
         !    Assigning the variable table for MEVI. This is changed so we can accomodate  !
         ! the dimensions of other met drivers. The table with the dimension description   !
         ! is available at ${MEVI_ROOT}/doc/dimension_table.txt.                           ! 
         !---------------------------------------------------------------------------------!
         select case(input_dtype)
         case(2)
            info_table(nf)%idim_type(nv) = 22
         case(3)
            info_table(nf)%idim_type(nv) = 33
         case(4)
            info_table(nf)%idim_type(nv) = 47
         case(5)
            info_table(nf)%idim_type(nv) = 48
         case(6)
            info_table(nf)%idim_type(nv) = 37
         case(7)
            info_table(nf)%idim_type(nv) = 35
         case(8)
            info_table(nf)%idim_type(nv) = 46
         case(9)
            info_table(nf)%idim_type(nv) = 36
         end select
         write (unit=*,fmt='(a,1x,i5,1x,a16,1x,i5)')                                       &
               '     - Retrieving var:',nv,trim(info_table(nf)%varname(nv))                &
                                       ,info_table(nf)%idim_type(nv)

      end do
      
      !----- Getting the dimension properties from the header. ----------------------------!
      write (unit=*,fmt='(a)') '     - Loading header...'
      call commio_light('ANAL','READ',60)
      
      info_table(nf)%init_time    = zero_time
      info_table(nf)%file_time(1) = this_time(1)
      
      !----- Adding the new time into the times array -------------------------------------!
      if (nouttimes == 0) then
        nouttimes = 1
        outtimes(1)  = this_time(1)
      elseif (.not. any(outtimes(1:nouttimes)%elapsed == this_time(1)%elapsed)) then
         nouttimes = nouttimes + 1
         outtimes(nouttimes) = this_time(1)
         call sort_time(nouttimes,outtimes(1:nouttimes))
      end if
      
      
      
      do nv=1,nvbtab
         ifm => info_table(nf)%ngrid(nv)
         
         !----- Although some variables are actually staggered we set up all to false -----!
         info_table(nf)%stagger(:,nv) = .false.

         !----- Filling the variable rank and dimensions depending on the variable type ---!
         select case (info_table(nf)%idim_type(nv))
         case (22) !----- 2-D variables (nxp,nyp) -----------------------------------------!
            info_table(nf)%rank(nv)           = 2
            info_table(nf)%dims(1,nv)         = nnxp(ifm)
            info_table(nf)%dims(2,nv)         = nnyp(ifm)
            info_table(nf)%dims(3:maxrank,nv) = 1

         case (33) !----- 3-D variables (nxp,nyp,nzp), ------------------------------------!
            info_table(nf)%rank(nv)           = 3
            info_table(nf)%dims(1,nv)         = nnxp(ifm)
            info_table(nf)%dims(2,nv)         = nnyp(ifm)
            info_table(nf)%dims(3,nv)         = nnzp(ifm)
            info_table(nf)%dims(4:maxrank,nv) = 1

         case (47) !----- 4-D variables (nxp,nyp,nzg,npatch) ------------------------------!
            info_table(nf)%rank(nv)           = 4
            info_table(nf)%dims(1,nv)         = nnxp(ifm)
            info_table(nf)%dims(2,nv)         = nnyp(ifm)
            info_table(nf)%dims(3,nv)         = nzg
            info_table(nf)%dims(4,nv)         = npatch
            info_table(nf)%dims(5:maxrank,nv) = 1

         case (48) !----- 4-D variables (nxp,nyp,nzs,npatch) ------------------------------!
            info_table(nf)%rank(nv)           = 4
            info_table(nf)%dims(1,nv)         = nnxp(ifm)
            info_table(nf)%dims(2,nv)         = nnyp(ifm)
            info_table(nf)%dims(3,nv)         = nzs
            info_table(nf)%dims(4,nv)         = npatch
            info_table(nf)%dims(5:maxrank,nv) = 1

         case (37) !----- 3-D variables (nxp,nyp,npatch) ----------------------------------!
            info_table(nf)%rank(nv)           = 3
            info_table(nf)%dims(1,nv)         = nnxp(ifm)
            info_table(nf)%dims(2,nv)         = nnyp(ifm)
            info_table(nf)%dims(3,nv)         = npatch
            info_table(nf)%dims(4:maxrank,nv) = 1

         case (35) !----- 3-D variables (nxp,nyp,nwave) -----------------------------------!
            info_table(nf)%rank(nv)           = 3
            info_table(nf)%dims(1,nv)         = nnxp(ifm)
            info_table(nf)%dims(2,nv)         = nnyp(ifm)
            info_table(nf)%dims(3,nv)         = nwave
            info_table(nf)%dims(4:maxrank,nv) = 1

         case (46) !----- 3-D variables (nxp,nyp,nzp,nclouds) -----------------------------!
            info_table(nf)%rank(nv)           = 4
            info_table(nf)%dims(1,nv)         = nnxp(ifm)
            info_table(nf)%dims(2,nv)         = nnyp(ifm)
            info_table(nf)%dims(3,nv)         = nnzp(ifm)
            info_table(nf)%dims(4,nv)         = nclouds
            info_table(nf)%dims(5:maxrank,nv) = 1

         case (36) !----- 3-D variables (nxp,nyp,nclouds) ---------------------------------!
            info_table(nf)%rank(nv)           = 3
            info_table(nf)%dims(1,nv)         = nnxp(ifm)
            info_table(nf)%dims(2,nv)         = nnyp(ifm)
            info_table(nf)%dims(3,nv)         = nclouds
            info_table(nf)%dims(4:maxrank,nv) = 1

         end select

      end do
      
      close(unit=10,status='keep')
   end do fileloop

   return
end subroutine rams_fill_infotable_vfm
!==========================================================================================!
!==========================================================================================!
