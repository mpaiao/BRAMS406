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






!==========================================================================================!
!==========================================================================================!
!     This function is based on the RAMS filelist, with the difference that this version   !
! also allows the user to pre-define the list of files they want (in a file called         !
! filelist.inp), so MEVI can also be used in less standard names.                          !
!------------------------------------------------------------------------------------------!
subroutine filelist_f (fnames,file_prefix,nfile,readlist)
 
   use mod_maxdims, only : maxstr, maxfiles

   implicit none
   integer                       , intent(out)    :: nfile
   character(len=*), dimension(*), intent(out)    :: fnames
   character(len=maxstr)         , intent(inout)  :: file_prefix
   logical                       , intent(in)     :: readlist

   character(len=maxstr)                          :: file,command,cdir
   integer                                        :: iflag,iprelen,nc,nf,iun,lndir

   character(len=maxfiles*maxstr)                 :: filelist
   character(len=maxstr)                          :: dir
   character(len=maxstr)                          :: prefix
   character(len=maxstr)                          :: myform
   integer, dimension(maxfiles)                   :: indices
   integer                                        :: n,i,j,ierr
   logical                                        :: foundbslash

   nfile = 0
   write(myform,fmt='(a,i3.3,a)') '(a',maxstr,')'
   
   !----- If the user provided the list of files, read the list ---------------------------!
   isitalist: if (readlist) then
      open (unit=66,file=trim(file_prefix),status='old',action='read',iostat=ierr)
      if (ierr /= 0) call fatal_error('Filelist '//trim(file_prefix)//' not found!'        &
                                     ,'wrf_fill_infotable','wrf_fill_infotable.F90'        )
      nf=0
      readlistloop: do
         nf = nf + 1
         read (unit=66,fmt=trim(myform),iostat=ierr) fnames(nf)
         if (ierr == 0) then
            write (unit=*,fmt='(a,1x,i5,1x,a)') '[-] File #: ',nf,trim(fnames(nf))
         else 
            exit readlistloop
         end if
      end do readlistloop
      close (unit=66,status='keep')



   !----- Otherwise, determine the files based on the prefix ------------------------------!
   else ! if (.not. readlist) then
      write (unit=*,fmt='(a,1x,a)') ' [-] filelist_f: Checking prefix: ',trim(file_prefix)

      iprelen = len_trim(file_prefix)
      if(iprelen == 0) iprelen=len(file_prefix)
         
!----- Use preprocessor tool. Windows require different way to list... --------------------!
#if defined (PC_NT1)
      !------------------------------------------------------------------------------------!
      ! First change all "/" to "\" so same namelist can be used for Unix/Linux/Windows    !
      !------------------------------------------------------------------------------------!
      do nc=1,iprelen
         if(file_prefix(nc:nc) == '/') file_prefix(nc:nc)='\'
      enddo
      command=  &
        'dir /b '//file_prefix(1:len_trim(file_prefix))//' >c:\temp\MEVI_filelist'
      call system(trim(command))
   
      !----- Open the directory list ------------------------------------------------------!
      iun=66
      open(unit=iun,file='c:\temp\MEVI_filelist',status='old',iostat=ierr)
      if (ierr /= 0) call fatal_error('filelist_f: Error opening temporary MEVI_filelist'  &
                                     ,'filelist_f','filelist.F90')
   
      !----- Read through the files. Windows doesn't put directory names on "dir", so... --!
      foundbslash=.false.
      nameloop: do nc=len_trim(file_prefix),1,-1
         if(file_prefix(nc:nc).eq.'\') then
            lndir=nc
            cdir=file_prefix(1:lndir)
            foundbslash = .true.
            exit nameloop
         endif
      end do nameloop
      if (.not. foundbslash) then
        lndir=2
        cdir='.\'
      end if
   
      readloop: do
         read(iun,'(a128)',iostat=ierr) file
         if (ierr /= 0) exit readloop
         fnames(nf) = cdir(1:lndir)//file
      end do readloop
      close(iun)

      command= 'del c:\temp\RAMS_filelist'
      call system(command)
         
#else

      !----- Appending char(0), so C can handle the characters properly. ------------------!
      prefix = file_prefix(1:iprelen) // char(0)

      !------------------------------------------------------------------------------------!
      !    The following subroutine uses intrinsic C functions to return an vector of file !
      ! names that match the search prefix strings. The strings contained in the string    !
      ! vector are indexed by the integer array.                                           !
      !------------------------------------------------------------------------------------!
      call filelist_c(n,indices,prefix,filelist)

      do nf=1,n

         fnames(nf) = trim(filelist(indices(nf):indices(nf+1)-1))
         write (unit=*,fmt='(a,1x,i5,1x,a)') '   [-] File #: ',nf,trim(fnames(nf))
      end do

#endif
   
   end if isitalist
   
   nfile=nf-1

   if (nfile == 0) then
      print *, 'No INPUT files for prefix:',file_prefix
      call fatal_error('filelist_f: No files found...','filelist_f','filelist.F90')
   end if

   return
end subroutine filelist_f
