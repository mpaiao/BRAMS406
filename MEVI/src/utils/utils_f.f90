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
real function walltime(wstart)
  implicit none
  real :: wstart
  integer :: ii,ir

  call system_clock(count=ii,count_rate=ir)
  walltime=float(ii)/float(ir) - wstart
  return
end function walltime
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
real function cputime(w1)
  implicit none
  real :: w1
  real :: cc,fsecs
  real, external :: walltime

  call timing(2,cc)
  cputime=cc
  fsecs=72559200.
  w1=walltime(fsecs)
  return
end function cputime
!==========================================================================================!
!==========================================================================================!
