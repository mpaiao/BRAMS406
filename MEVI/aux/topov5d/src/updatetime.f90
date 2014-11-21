!------------------------------------------------------------------------------------------!
! Subroutine that updates time                                                             !
!------------------------------------------------------------------------------------------!
recursive subroutine updatetime(dt,dtunit,year,month,day,hour,minu)
!------------------------------------------------------------------------------------------!
! Variable declaration section                                                             !
!------------------------------------------------------------------------------------------!
  implicit none
!----- Input variables --------------------------------------------------------------------!
  integer          , intent(in)    :: dt
  character(len=2) , intent(in)    :: dtunit
!----- Other external variables -----------------------------------------------------------!
  integer          , intent(inout) :: year,month,day,hour,minu
!----- Other variables --------------------------------------------------------------------!
  integer, dimension(12) :: daymax=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  logical                :: isleap
  integer                :: doy,effdelta,lastday
!------------------------------------------------------------------------------------------!

!----- First changing daymax of February for leap years -----------------------------------!
  if (isleap(year)) daymax(2) = 29
!----- Finding the day of year (sometimes wrongly called julian day) ----------------------!
  doy=day+sum(daymax(1:month-1))

  select case(dtunit)
  case('yr')
    year=year+dt
    return !----- Don't go to the end to avoid problems with the day of year --------------!
  case('mo')
    if (int(dt/12) > 0) call updatetime(int(dt/12),'yr',year,month,day,hour,minu)
!----- Now that all years were conveniently added I add month, updating the year if needed-!
    month=month+mod(dt,12)
    if (month > 12) then
      month=mod(month,12)
      year=year+1
    end if
    return !----- Don't go to the end to avoid problems with the day of year --------------!
!----- Day is the most complicated one because it depends on leap years -------------------!
  case('dy')
    if (isleap(year)) then
      lastday=366
    else
      lastday=365
    end if
    effdelta=dt
    do while (mod(effdelta,lastday) > int(effdelta/lastday))
      year=year+1
!----- If it is before February 28, the last year is what matters -------------------------!
      if (doy <= 59) then
        effdelta=effdelta-lastday
        if (isleap(year)) then
          lastday=366
        else
          lastday=365
        end if
!----- If it is after February 29, the new year is what matters ---------------------------!
      else
        if (isleap(year)) then
          lastday=366
        else
          lastday=365
        end if
        effdelta=effdelta-lastday
      end if
    end do
    doy=doy+effdelta
    if (doy > lastday) then
      year=year+1
      doy=mod(doy,lastday)
    end if
  case('hr')
!----- If hr > 24, then I update in days, then I update the hours less than 1 day ---------!
    if (int(dt/24) > 0) call updatetime(int(dt/24),'dy',year,month,day,hour,minu)
    hour=hour+mod(dt,24)
    if (hour >= 24) then
      hour=mod(hour,24)
      doy=doy+1
      if (isleap(year)) then
        lastday=366
      else
        lastday=365
      end if
      if (doy > lastday) then
        doy=mod(doy,lastday)
        year=year+1
      end if
    end if
  case('mn')
    if (int(dt/60) > 0) call updatetime(int(dt/60),'hr',year,month,day,hour,minu)
    minu=minu+ mod(dt,60)
    if (minu >= 60) then
      minu=mod(minu,60)
      hour=hour+1
      if (hour > 24) then
        hour=mod(hour,24)
        doy=doy+1
        if (isleap(year)) then
          lastday=366
        else
          lastday=365
        end if
        if (doy > lastday) then
          doy=mod(doy,lastday)
          year=year+1
        end if
      end if
    end if
  case default
    write(unit=*,fmt=*) 'The time units ',dtunit,' is invalid!!! Allowed values are:'
    write(unit=*,fmt=*) '   ===> yr: year'
    write(unit=*,fmt=*) '   ===> mo: month'
    write(unit=*,fmt=*) '   ===> dy: day'
    write(unit=*,fmt=*) '   ===> hr: hour'
    write(unit=*,fmt=*) '   ===> mn: minute'
    stop 'Execution aborted...' 
  end select
!----- Return date to month and day corresponding to the day of year ----------------------!
  call doytodaymon(doy,year,day,month)
  return
end subroutine updatetime
!------------------------------------------------------------------------------------------!





!------------------------------------------------------------------------------------------!
! Function that checks whether the year is leap or not                                     !
!------------------------------------------------------------------------------------------!
logical function isleap(year)
!------------------------------------------------------------------------------------------!
! Variable declaration section                                                             !
!------------------------------------------------------------------------------------------!
  implicit none
!----- Input variables --------------------------------------------------------------------!
  integer, intent(in) :: year
!------------------------------------------------------------------------------------------!
  isleap=(mod(year,400) == 0 .or. (mod(year,4) == 0 .and. mod(year,100) /= 0))
  return
end function isleap
!------------------------------------------------------------------------------------------!





!------------------------------------------------------------------------------------------!
! Subroutine that gives the day and month for a given day of year and year                 !
!------------------------------------------------------------------------------------------!
subroutine doytodaymon(doy,year,day,month)
!------------------------------------------------------------------------------------------!
! Variable declaration section                                                             !
!------------------------------------------------------------------------------------------!
  implicit none
!----- Input variables --------------------------------------------------------------------!
  integer, intent(in)  :: doy,year
!----- Output variables -------------------------------------------------------------------!
  integer, intent(out) :: day, month
!----- Internal variables -----------------------------------------------------------------!
  logical                  :: isleap
  integer, dimension(0:12) :: maxdoy
!------------------------------------------------------------------------------------------!
!----- Assigning initial values for maxday and maxdoy -------------------------------------!
  maxdoy=(/0,31,59,90,120,151,181,212,243,273,304,334,365/)
!----- Adjusting the minimum and maximum day of year considering leap years ---------------!
  if (isleap(year)) then
    maxdoy(2:12)=maxdoy(2:12)+1
  end if
!----- Loop that will stop when day is within the bounds of that month --------------------!
  month=1
  do while (doy > maxdoy(month))
    month=month+1
  end do
  day=doy-maxdoy(month-1)
  if (month > 12) then
    write(unit=*,fmt=*) 'In subroutine doytodaymon: Invalid day of year, ',doy,'!!!'
    stop 'Execution aborted!!!'
  end if
  return
end subroutine doytodaymon
!------------------------------------------------------------------------------------------!
