!------------------------------------------------------------------------------------------!
! Subroutine gettime                                                                       !
!                                                                                          !
!    This subroutine converts the time written in the ctl into "normal" time               !
!------------------------------------------------------------------------------------------!
subroutine gettime(time0,year,month,day,hour,minu)
!------------------------------------------------------------------------------------------!
! Variable declaration section                                                             !
!------------------------------------------------------------------------------------------!
  implicit none
!----- Parameters -------------------------------------------------------------------------!
  character(len=3), dimension(12), parameter :: mmm=(/'jan','feb','mar','apr','may','jun', &
                                                      'jul','aug','sep','oct','nov','dec'/)
!----- Input variables --------------------------------------------------------------------!
  character(len=*), intent(in) :: time0
!----- Output variables -------------------------------------------------------------------!
  integer, intent(out) :: year,month,day,hour,minu
!----- Other variables --------------------------------------------------------------------!
  integer  :: colon,z,etime,emon,m
!------------------------------------------------------------------------------------------!

!----- Finding some important positions ---------------------------------------------------!
  etime=len_trim(time0)
  z=index(time0,'z',.true.)
  colon=index(time0,':',.true.)
!----- If there is no :, so it's a round hour, I then set min to zero ---------------------!
  if (colon == 0) then
    minu=0
    read(time0(1:z-1),fmt=*) hour
  else
    read(time0(1:colon-1),fmt=*) hour
    read(time0(colon+1:z-1),fmt=*) minu
  end if
!----- Determining the month --------------------------------------------------------------!
  monthloop: do m=1,12
    emon=index(time0,mmm(m),.true.)
    if (emon /= 0) then
      month=m
      exit monthloop
    end if
  end do monthloop
!----- Determining the day and year -------------------------------------------------------!
  read(time0(z+1:emon-1),fmt=*) day
  read(time0(emon+3:etime),fmt=*) year
  return
end subroutine gettime
