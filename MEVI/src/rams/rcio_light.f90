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
subroutine commio_light(cfile,io,iun)
!------------------------------------------------------------------------------------------!
!   This is a light version of RAMS subroutine commio, we read only the variables that are !
! needed for grid definition and dimensions. For back compability dimension variables that !
! were introduced on later revisions will be assigned only if the variable is present,     !
! otherwise they will be assigned the default value, 1.                                    !
!------------------------------------------------------------------------------------------!
   use mod_model, only : if_adap,ngrids,nzg,nzs,nclouds,naddsc,npatch,nwave,ihtran,polelon &
                       ,polelat,centlon,centlat,deltaxn,deltayn,nnxp,nnyp,nnzp,zero_time   &
                       ,this_time,xtn,xmn,ytn,ymn,ztn,zmn,dztn,dzmn,slz,l2ndlat
   use mod_maxdims, only: maxstr

   
   implicit none
   !---------------------------------------------------------------------------------------!
   !    Variable declaration                                                               !
   !---------------------------------------------------------------------------------------!
   !------ Arguments ----------------------------------------------------------------------!
   integer         , intent(in) :: iun         ! File unit
   character(len=*), intent(in) :: io          ! Flag for reading/writing header
   character(len=*), intent(in) :: cfile       ! Current file 
   !------ Local variables ----------------------------------------------------------------!
   integer                      :: irw         ! Flag for reading/writing
   integer                      :: ie          ! Error flag.
   integer                      :: itimea      ! Initial time (HHMM)
   integer                      :: analtime100 ! Analysis time
   real(kind=8)                 :: etime       ! Elapsed time
   character(len=2)             :: cng         ! Grid number in character
   integer                      :: ng          ! Grid counter
   !------ Functions ----------------------------------------------------------------------!
   integer         , external   :: cio_i,cio_f,cio_i_sca,cio_f_sca,cio_f8_sca
   !----- External functions for time handling --------------------------------------------!
   integer          , external         :: julday,julday1583,v5d_datestamp,v5d_timestamp
   character(len=3) , external         :: monchar
   real             , external         :: day_fraction
   character(len=17), external         :: grads_dtstamp
   character(len=19), external         :: mevi_dtstamp
   !---------------------------------------------------------------------------------------!

   select case (trim(io))
   case ('READ','read')
      irw = 1
   case ('WRITE','write')
      irw = 2
   end select

   !----- Retrieving data from header: ----------------------------------------------------!
   ie=cio_i_sca (iun,irw,'if_adap',if_adap         ,.true.        )
   ie=cio_i_sca (iun,irw,'ngrids' ,ngrids          ,.true.        )
   ie=cio_i_sca (iun,irw,'ihtran' ,ihtran          ,.true.        )
   ie=cio_i_sca (iun,irw,'nzg'    ,nzg             ,.true.        )
   ie=cio_i_sca (iun,irw,'nzs'    ,nzs             ,.true.        )

   ie=cio_i_sca (iun,irw,'naddsc' ,naddsc          ,.true.        )

   ie=cio_f_sca (iun,irw,'polelon',polelon         ,.true.        )
   ie=cio_f_sca (iun,irw,'polelat',polelat         ,.true.        )
   l2ndlat = polelat !----- This is for Lambert-conformal projection, unused for BRAMS ----!

   ie=cio_i     (iun,irw,'nnxp'   ,nnxp            ,.true. ,ngrids)
   ie=cio_i     (iun,irw,'nnyp'   ,nnyp            ,.true. ,ngrids)
   ie=cio_i     (iun,irw,'nnzp'   ,nnzp            ,.true. ,ngrids)

   ie=cio_f     (iun,irw,'centlon',centlon         ,.true. ,ngrids)
   ie=cio_f     (iun,irw,'centlat',centlat         ,.true. ,ngrids)
   ie=cio_f     (iun,irw,'deltaxn',deltaxn         ,.true. ,ngrids)
   ie=cio_f     (iun,irw,'deltayn',deltayn         ,.true. ,ngrids)

   !----- These two variables may not be in the header. If they are not, assign 1. --------!
   ie=cio_i_sca (iun,irw,'nclouds',nclouds         ,.false.       )
   if (ie /= 0) nclouds = 1
   ie=cio_i_sca (iun,irw,'nwave'  ,nwave           ,.false.       )
   if (ie /= 0) nwave = 1

   !---------------------------------------------------------------------------------------!
   !    Filling the domain coordinates                                                     !
   !---------------------------------------------------------------------------------------!
   do ng=1,ngrids
      write(cng,'(i2.2)') ng
      ie=cio_f  (iun,irw,'xmn'//cng   ,xmn   (:,ng),.true.,nnxp(ng))
      ie=cio_f  (iun,irw,'xtn'//cng   ,xtn   (:,ng),.true.,nnxp(ng))
      ie=cio_f  (iun,irw,'ymn'//cng   ,ymn   (:,ng),.true.,nnyp(ng))
      ie=cio_f  (iun,irw,'ytn'//cng   ,ytn   (:,ng),.true.,nnyp(ng))
      ie=cio_f  (iun,irw,'zmn'//cng   ,zmn   (:,ng),.true.,nnzp(ng))
      ie=cio_f  (iun,irw,'ztn'//cng   ,ztn   (:,ng),.true.,nnzp(ng))
      ie=cio_f  (iun,irw,'dzmn'//cng  ,dzmn  (:,ng),.true.,nnzp(ng))
      ie=cio_f  (iun,irw,'dztn'//cng  ,dztn  (:,ng),.true.,nnzp(ng))
   enddo
   ie = cio_f   (iun,irw,'slz'        ,slz  (1:nzg),.true.,nzg     )

   !---------------------------------------------------------------------------------------!
   !    Filling the initial time information                                               !
   !---------------------------------------------------------------------------------------!
   !------ Reading what I can read from header --------------------------------------------!
   ie=cio_i_sca (iun,irw,'iyear1' ,zero_time%year  ,.true.        )
   ie=cio_i_sca (iun,irw,'imonth1',zero_time%month ,.true.        )
   ie=cio_i_sca (iun,irw,'idate1' ,zero_time%day   ,.true.        )
   ie=cio_i_sca (iun,irw,'itime1' ,itimea          ,.true.        )
   !------ Completing the initial time structure information ------------------------------!
   zero_time%hour    = itimea/100
   zero_time%minu    = mod(itimea,100)
   zero_time%seco    = 0
   zero_time%doy     = julday (zero_time%month,zero_time%day,zero_time%year)
   zero_time%mmm     = monchar(zero_time%month)
   zero_time%fracday = day_fraction(zero_time%hour,zero_time%minu,zero_time%seco)
   zero_time%yyyyddd    = v5d_datestamp(zero_time%year,zero_time%doy)
   zero_time%hhmmss     = v5d_timestamp(zero_time%hour,zero_time%minu,zero_time%seco)
   zero_time%gradsstamp = grads_dtstamp(zero_time%year,zero_time%mmm,zero_time%day         &
                                       ,zero_time%hour,zero_time%minu)
   zero_time%timestr    = mevi_dtstamp(zero_time%year,zero_time%month,zero_time%day        &
                                      ,zero_time%hour,zero_time%minu,zero_time%seco)
   zero_time%elapsed    = dble(julday1583(zero_time%month,zero_time%day,zero_time%year))   &
                        + dble(zero_time%fracday)

   !---------------------------------------------------------------------------------------!
   !    Filling the analysis time information                                              !
   !---------------------------------------------------------------------------------------!
   !------ Reading the elapsed time -------------------------------------------------------!
   ie=cio_f8_sca(iun,irw,'time'   ,etime           ,.true.        )
   !------ Finding the analysis time ------------------------------------------------------!
   call date_add_to (zero_time%year,zero_time%month,zero_time%day,100*itimea  &
                    ,etime,'s',this_time(1)%year,this_time(1)%month,this_time(1)%day       &
                    ,analtime100)
   !---------------------------------------------------------------------------------------!
   this_time(1)%hour    = analtime100/10000
   this_time(1)%minu    = mod(analtime100/100,100)
   this_time(1)%seco    = mod(analtime100,100)
   this_time(1)%doy     = julday (this_time(1)%month,this_time(1)%day,this_time(1)%year)
   this_time(1)%mmm     = monchar(this_time(1)%month)
   this_time(1)%fracday = day_fraction(this_time(1)%hour,this_time(1)%minu                 &
                                      ,this_time(1)%seco)
   this_time(1)%yyyyddd    = v5d_datestamp(this_time(1)%year,this_time(1)%doy)
   this_time(1)%hhmmss     = v5d_timestamp(this_time(1)%hour,this_time(1)%minu             &
                                          ,this_time(1)%seco)
   this_time(1)%gradsstamp = grads_dtstamp(this_time(1)%year,this_time(1)%mmm              &
                                          ,this_time(1)%day,this_time(1)%hour              &
                                          ,this_time(1)%minu)
   this_time(1)%timestr    = mevi_dtstamp(this_time(1)%year,this_time(1)%month             &
                                         ,this_time(1)%day,this_time(1)%hour               &
                                         ,this_time(1)%minu,this_time(1)%seco)
   this_time(1)%elapsed    = dble(julday1583(this_time(1)%month,this_time(1)%day           &
                                            ,this_time(1)%year))                           &
                           + dble(this_time(1)%fracday)
   return
end subroutine commio_light
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!    This function reads integer data from the header, and returns an error message.       !
!------------------------------------------------------------------------------------------!
integer function cio_i(iun,irw,cstr,ia,verbose,n)
   use mod_maxdims, only : maxstr
   implicit none
   integer, intent(in)                  :: iun,irw,n
   logical              , intent(in)    :: verbose
   integer, dimension(*), intent(inout) :: ia
   character(len=*)                     :: cstr
   character(len=maxstr)                :: string
   integer                              :: nn,i

   select case (irw)
   !----- Reading the data ----------------------------------------------------------------!
   case (1)
      call cio_pos_file (iun,cstr,verbose,cio_i)
      if(cio_i == 1) return
      read(unit=iun,fmt=*) nn
      read(unit=iun,fmt=*) (ia(i),i=1,nn)

   !----- Writing the data ----------------------------------------------------------------!
   case (2)
      write(unit=iun,fmt='(2a)') '__',cstr
      write(unit=iun,fmt=*)      n
      write(unit=iun,fmt='(i6)') (ia(i),i=1,n)
      cio_i=0

   end select

   return
end function cio_i
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!    This function reads real data from the header, and returns an error message.          !
!------------------------------------------------------------------------------------------!
integer function cio_f(iun,irw,cstr,ia,verbose,n)
   use mod_maxdims, only : maxstr
   implicit none
   integer, intent(in)               :: iun,irw,n
   logical              , intent(in) :: verbose
   real, dimension(*), intent(inout) :: ia
   character(len=*)                  :: cstr
   character(len=maxstr)             :: string
   integer                           :: nn,i

   select case (irw)
   !----- Reading the data ----------------------------------------------------------------!
   case (1)
      call cio_pos_file (iun,cstr,verbose,cio_f)
      if(cio_f == 1) return
      read(unit=iun,fmt=*) nn
      read(unit=iun,fmt=*) (ia(i),i=1,nn)

   !----- Writing the data ----------------------------------------------------------------!
   case (2)
      write(unit=iun,fmt='(2a)')     '__',cstr
      write(unit=iun,fmt=*)          n
      write(unit=iun,fmt='(es16.8)') (ia(i),i=1,n)
      cio_f=0

   end select

   return
end function cio_f
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!    This function reads dble precision data from the header, returning an error message.  !
!------------------------------------------------------------------------------------------!
integer function cio_f8(iun,irw,cstr,ia,verbose,n)
   use mod_maxdims, only : maxstr
   implicit none
   integer                   , intent(in)    :: iun,irw,n
   logical              , intent(in)         :: verbose
   real(kind=8), dimension(*), intent(inout) :: ia
   character(len=*)                          :: cstr
   character(len=maxstr)                     :: string
   integer                                   :: nn,i

   select case (irw)
   !----- Reading the data ----------------------------------------------------------------!
   case (1)
      call cio_pos_file (iun,cstr,verbose,cio_f8)
      if(cio_f8 == 1) return
      read(unit=iun,fmt=*) nn
      read(unit=iun,fmt=*) (ia(i),i=1,nn)

   !----- Writing the data ----------------------------------------------------------------!
   case (2)
      write(unit=iun,fmt='(2a)')     '__',cstr
      write(unit=iun,fmt=*)           n
      write(unit=iun,fmt='(es24.12)') (ia(i),i=1,n)
      cio_f8=0

   end select

   return
end function cio_f8
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!    This function reads character data from the header, and returns an error message.     !
!------------------------------------------------------------------------------------------!
integer function cio_c(iun,irw,cstr,ia,verbose,n)
   use mod_maxdims, only : maxstr
   implicit none
   integer, intent(in)                           :: iun,irw,n
   logical              , intent(in)             :: verbose
   character(len=*), dimension(*), intent(inout) :: ia
   character(len=*)                              :: cstr
   character(len=maxstr)                         :: string
   integer                                       :: nn,i

   select case (irw)
   !----- Reading the data ----------------------------------------------------------------!
   case (1)
      call cio_pos_file (iun,cstr,verbose,cio_c)
      if(cio_c == 1) return
      read(unit=iun,fmt=*) nn
      read(unit=iun,fmt=*) (ia(i),i=1,nn)

   !----- Writing the data ----------------------------------------------------------------!
   case (2)
      write(unit=iun,fmt='(2a)') '__',cstr
      write(unit=iun,fmt=*)      n
      write(unit=iun,fmt='(a)')  (ia(i),i=1,n)
      cio_c=0

   end select

   return
end function cio_c
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!    This function reads integer data from the header, and returns an error message.       !
!------------------------------------------------------------------------------------------!
integer function cio_i_sca(iun,irw,cstr,ia,verbose)
   use mod_maxdims, only : maxstr
   implicit none
   integer              , intent(in)    :: iun,irw
   logical              , intent(in)    :: verbose
   integer              , intent(inout) :: ia
   character(len=*)                     :: cstr
   character(len=maxstr)                :: string
   integer                              :: nn

   select case (irw)
   !----- Reading the data ----------------------------------------------------------------!
   case (1)
      call cio_pos_file (iun,cstr,verbose,cio_i_sca)
      if(cio_i_sca == 1) return
      read(unit=iun,fmt=*) nn
      if (nn /= 1) call fatal_error(                                                       &
                   'Variable '//trim(cstr)//' should be scalar but I found a vector!!!'    &
                  ,'cio_i_sca','rcio_light.f90')
      read(unit=iun,fmt=*) ia

   !----- Writing the data ----------------------------------------------------------------!
   case (2)
      write(unit=iun,fmt='(2a)') '__',cstr
      write(unit=iun,fmt=*)      1
      write(unit=iun,fmt='(i6)') ia
      cio_i_sca=0

   end select

   return
end function cio_i_sca
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!    This function reads real data from the header, and returns an error message.          !
!------------------------------------------------------------------------------------------!
integer function cio_f_sca(iun,irw,cstr,ia,verbose)
   use mod_maxdims, only : maxstr
   implicit none
   integer              , intent(in)    :: iun,irw
   logical              , intent(in)    :: verbose
   real                 , intent(inout) :: ia
   character(len=*)                     :: cstr
   character(len=maxstr)                :: string
   integer                              :: nn

   select case (irw)
   !----- Reading the data ----------------------------------------------------------------!
   case (1)
      call cio_pos_file (iun,cstr,verbose,cio_f_sca)
      if(cio_f_sca == 1) return
      read(unit=iun,fmt=*) nn
      if (nn /= 1) call fatal_error(                                                       &
                   'Variable '//trim(cstr)//' should be scalar but I found a vector!!!'    &
                  ,'cio_i_sca','rcio_light.f90')
      read(unit=iun,fmt=*) ia

   !----- Writing the data ----------------------------------------------------------------!
   case (2)
      write(unit=iun,fmt='(2a)')     '__',cstr
      write(unit=iun,fmt=*)          1
      write(unit=iun,fmt='(es16.8)') ia
      cio_f_sca=0

   end select

   return
end function cio_f_sca
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!    This function reads dble precision data from the header, returning an error message.  !
!------------------------------------------------------------------------------------------!
integer function cio_f8_sca(iun,irw,cstr,ia,verbose)
   use mod_maxdims, only : maxstr
   implicit none
   integer              , intent(in)    :: iun,irw
   logical              , intent(in)    :: verbose
   real(kind=8)         , intent(inout) :: ia
   character(len=*)                     :: cstr
   character(len=maxstr)                :: string
   integer                              :: nn

   select case (irw)
   !----- Reading the data ----------------------------------------------------------------!
   case (1)
      call cio_pos_file (iun,cstr,verbose,cio_f8_sca)
      if(cio_f8_sca == 1) return
      read(unit=iun,fmt=*) nn
      if (nn /= 1) call fatal_error(                                                       &
                   'Variable '//trim(cstr)//' should be scalar but I found a vector!!!'    &
                  ,'cio_i_sca','rcio_light.f90')
      read(unit=iun,fmt=*) ia

   !----- Writing the data ----------------------------------------------------------------!
   case (2)
      write(unit=iun,fmt='(2a)')     '__',cstr
      write(unit=iun,fmt=*)          1
      write(unit=iun,fmt='(es24.16)') ia
      cio_f8_sca=0

   end select

   return
end function cio_f8_sca
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!    This function reads real data from the header, and returns an error message.          !
!------------------------------------------------------------------------------------------!
integer function cio_c_sca(iun,irw,cstr,ia,verbose)
   use mod_maxdims, only : maxstr
   implicit none
   integer              , intent(in)    :: iun,irw
   logical              , intent(in)    :: verbose
   character(len=*)     , intent(inout) :: ia
   character(len=*)                     :: cstr
   character(len=maxstr)                :: string
   integer                              :: nn

   select case (irw)
   !----- Reading the data ----------------------------------------------------------------!
   case (1)
      call cio_pos_file (iun,cstr,verbose,cio_c_sca)
      if(cio_c_sca == 1) return
      read(unit=iun,fmt=*) nn
      if (nn /= 1) call fatal_error(                                                       &
                   'Variable '//trim(cstr)//' should be scalar but I found a vector!!!'    &
                  ,'cio_i_sca','rcio_light.f90')
      read(unit=iun,fmt=*) ia

   !----- Writing the data ----------------------------------------------------------------!
   case (2)
      write(unit=iun,fmt='(2a)')     '__',cstr
      write(unit=iun,fmt=*)          1
      write(unit=iun,fmt='(es16.8)') ia
      cio_c_sca=0

   end select

   return
end function cio_c_sca
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!   This subroutine leaves the header in a position ready to read the data.                !
!------------------------------------------------------------------------------------------!
subroutine cio_pos_file(iun,cstr,verbose,ierr)
   use mod_maxdims, only : maxstr
   implicit none
   integer              , intent(in)   :: iun
   logical              , intent(in)   :: verbose
   integer              , intent(out)  :: ierr
   character(len=*)     , intent(in)   :: cstr
   character(len=maxstr)               :: line,csearch

   integer :: nc,iend

   logical :: secondtime

   secondtime=.false.
   readloop: do 
      read(unit=iun,fmt='(a)',iostat=ierr) line
      if (ierr /= 0 .and. secondtime) then
         !----- Reached the EOF again without finding anything before, giving up ----------!
         ierr=1
         if (verbose) write (unit=*,fmt=*) '---- Name NOT found on header file:',cstr
         rewind(iun)
         return

      elseif (ierr /= 0) then 
         !----- Reached the EOF, rewind the header, it may be before... -------------------!
         secondtime = .true. 
         rewind(unit=iun)
         cycle readloop

      else
         !----- Normal line, now I must check whether this is what I was looking for ------!
         csearch = '__'//trim(cstr)
         nc=index(line,trim(csearch))
         if (nc == 1) then
            !----- Found it, now I can leave this routine ---------------------------------!
            ierr = 0
            return
         end if
      end if
   end do readloop

   return
end subroutine cio_pos_file
!==========================================================================================!
!==========================================================================================!
