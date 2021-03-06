!==========================================================================================!
!==========================================================================================!
!   MEVI. dateutils.f90 library. These subroutines were adapted from ED, which were in     !
!         turn adapted from RAMS... They use January 1, 1583, 00 GMT as the time origin    !
!         and expects elapsed time to be in double precision.                              !
!==========================================================================================!
!==========================================================================================!
subroutine date_abs_secs2 (year1,month1,date1,hour1,seconds)
   use rconstants, only : day_sec,hr_sec,min_sec
   implicit none
   real(kind=8) :: seconds

   ! compute number of seconds past 1 January 1900 12:00 am

   real(kind=8) :: s1,s2,s3,s4
   integer :: year1,month1,date1,hour1,iy,ndays
   integer :: elapdays
   integer, external  :: julday
   logical, external  :: isleap
   integer, parameter :: firstyear=1583

   !---------------------------------------------------------------------------------------!
   ! Counting the # of leap days between the reference and current year.                   !
   !---------------------------------------------------------------------------------------!
   elapdays = 0
   if (firstyear < year1) then
      do iy=firstyear,year1-1
         if (isleap(iy)) then
           elapdays=elapdays + 366
         else
           elapdays=elapdays + 365
         end if
      end do
   elseif (firstyear > year1) then
      do iy=firstyear-1,year1,-1
         if (isleap(iy)) then
           elapdays=elapdays - 366
         else
           elapdays=elapdays - 365
         end if
      end do
      elapdays = elapdays - 1
   end if
   
   ndays = elapdays + julday(month1,date1,year1)
   s1= dble(ndays) * day_sec
   s2= dble(hour1/10000) * hr_sec
   s3= dble(mod(hour1,10000)/100)*min_sec
   s4= dble(mod(hour1,100))
   seconds= s1+s2+s3+s4

   return
end subroutine date_abs_secs2
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine date_2_seconds (iyearc,imonthc,idatec,itimec, &
    iyeara,imontha,idatea,itimea,total_seconds)
   implicit none

   ! Output model time = total_seconds
   real(kind=8) :: total_seconds,secs_a,secs_c

   ! Input the model initialization time
   ! NOT ITIMEA IS A SIX DIGIT INTEGER HHMMSS
   integer :: iyeara,imontha,idatea,itimea

   ! Input also the current integer times, and day-seconds
   integer :: iyearc,imonthc,idatec,itimec
   !real :: secondsc
   !integer :: itimec

   !itimec =  10000*int(secondsc/3600) + &     !hours
   !     100*int(secondsc/60) + &              !minutes
   !     mod(int(secondsc),60)                 !seconds

   call date_abs_secs2(iyearc,imonthc,idatec,itimec,secs_c)
   call date_abs_secs2(iyeara,imontha,idatea,itimea,secs_a)
   total_seconds = secs_c - secs_a

return
end subroutine date_2_seconds
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine date_secs_ymdt (seconds,iyear1,imonth1,idate1,ihour1)
   use rconstants, only : day_sec,hr_sec,min_sec
   implicit none
   real(kind=8) :: seconds,s1
   integer :: iyear1,imonth1,idate1,ihour1

   integer,parameter :: firstyear=1583
   logical, external :: isleap

   ! compute real time given number of seconds past 1 January 1583 12:00 am  

   integer :: ny,nyr,ileap,nm,nd,ihr,imn,isc

   integer :: mondays(12)
   data mondays/31,28,31,30,31,30,31,31,30,31,30,31/

   ! Get what year it is
   s1=seconds
   do ny=0,10000
      ileap=0
      if(isleap(firstyear+ny)) ileap=1
      s1=s1-(365.+ileap)* day_sec
      if(s1 < 0.) then
         nyr=ny
         s1=s1+(365.+ileap)* day_sec
         exit
      endif
   enddo
   iyear1=firstyear+nyr

   ! s1 is now number of secs into the year
   !   Get month
   do nm=1,12
      ileap=0
      if(isleap(firstyear+ny) .and. nm == 2) ileap=1
      s1=s1-(mondays(nm)+ileap)* day_sec
      if(s1 < 0.) then
         s1=s1+(mondays(nm)+ileap)* day_sec
         exit
      endif
   enddo
   imonth1=nm

   ! s1 is now number of secs into the month
   !   Get date and time

   idate1=int(s1/day_sec)
   s1=s1-idate1*day_sec
   !---- Don't know if this is right. idate1 should not be +1. Although days start
   ! at idate1, idate =0 means that it is still running the last day of previous month,
   ! just that it is past midnight so the previous test gave a negative. 
   ! idate1=idate1+1 ! Since date starts at 1
   if (idate1 == 0) then
     imonth1=imonth1-1
     if (imonth1 == 0) then
       imonth1 = 12
       iyear1  = iyear1 - 1
     end if
     ileap =0
     if (isleap(iyear1) .and. imonth1 == 2) ileap=1
     idate1=mondays(imonth1)+ileap
   end if

   ihr=int(s1/hr_sec)
   s1=s1-ihr*hr_sec
   imn=int(s1/min_sec)
   s1=s1-imn*min_sec
   isc=s1
   ihour1=ihr*10000+imn*100+isc

   return
end subroutine date_secs_ymdt
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine date_add_to (inyear,inmonth,indate,inhour  &
                        ,tinc,tunits,outyear,outmonth,outdate,outhour)
   use rconstants, only : day_sec,hr_sec,min_sec
   implicit none

   integer inyear,inmonth,indate,inhour  &
          ,outyear,outmonth,outdate,outhour

   character(len=1) :: tunits

   ! adds/subtracts a time increment to a date and output new date
   ! -> uses hhmmss for hours, 4 digit year


   real(kind=8) :: tinc,ttinc,secs

   ! convert input time to seconds

   ttinc=tinc
   if(tunits.eq.'m') ttinc=tinc*min_sec
   if(tunits.eq.'h') ttinc=tinc*hr_sec
   if(tunits.eq.'d') ttinc=tinc*day_sec
   !print*,'inc:',tinc,tunits,ttinc


   call date_abs_secs2(inyear,inmonth,indate,inhour,secs)

   secs=secs+ttinc

   call date_secs_ymdt(secs,outyear,outmonth,outdate,outhour)

   !print*,'out stuff:',outyear,outmonth,outdate,outhour

   return
end subroutine date_add_to
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine date_unmake_big (inyear,inmonth,indate,inhour,outdate)
   implicit none
   integer :: inyear,inmonth,indate,inhour
   character(len=14) :: outdate

   read(outdate(1:4),10) inyear
   read(outdate(5:6),11) inmonth
   read(outdate(7:8),11) indate
   read(outdate(9:14),12) inhour
   10 format (i4)
   11 format (i2)
   12 format (i6)

   return
end subroutine date_unmake_big
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
integer function julday (imonth,iday,iyear)
   implicit none
   integer :: imonth,iday,iyear

   integer           :: febdays
   logical, external :: isleap

   ! compute the julian day from a normal date

   if (isleap(iyear)) then
      febdays=29
   else
      febdays=28
   end if
   
   julday= iday  &
         + min(1,max(0,imonth-1))*31  &
         + min(1,max(0,imonth-2))*febdays  &
         + min(1,max(0,imonth-3))*31  &
         + min(1,max(0,imonth-4))*30  &
         + min(1,max(0,imonth-5))*31  &
         + min(1,max(0,imonth-6))*30  &
         + min(1,max(0,imonth-7))*31  &
         + min(1,max(0,imonth-8))*31  &
         + min(1,max(0,imonth-9))*30  &
         + min(1,max(0,imonth-10))*31  &
         + min(1,max(0,imonth-11))*30  &
         + min(1,max(0,imonth-12))*31

   return
end function julday
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
integer function julday1583 (imonth,iday,iyear)
   implicit none
   integer :: imonth,iday,iyear

   integer :: i,imm,idd,jd
   
   integer           :: febdays
   logical, external :: isleap

   ! compute the julian day (from 1583) from a normal date w/4 digit yr
   ! 1583 is the first full year with Gregorian calendar, so that should cover
   ! most cases.

   julday1583=0
   if (iyear >= 1583) then
      do i=1583,iyear

         imm=12
         idd=31
         if(i==iyear)then
            imm=imonth
            idd=iday
         endif
         
         if (isleap(i)) then
            febdays=29
         else
            febdays=28
         end if

         jd= idd  &
            + min(1,max(0,imm-1))*31  &
            + min(1,max(0,imm-2))*febdays  &
            + min(1,max(0,imm-3))*31  &
            + min(1,max(0,imm-4))*30  &
            + min(1,max(0,imm-5))*31  &
            + min(1,max(0,imm-6))*30  &
            + min(1,max(0,imm-7))*31  &
            + min(1,max(0,imm-8))*31  &
            + min(1,max(0,imm-9))*30  &
            + min(1,max(0,imm-10))*31  &
            + min(1,max(0,imm-11))*30  &
            + min(1,max(0,imm-12))*31

         julday1583=julday1583+jd

      end do  
   else
      do i=1582,iyear,-1
         if (isleap(i)) then
            julday1583=julday1583-366
         else
            julday1583=julday1583-365
         end if
      end do

      imm=imonth
      idd=iday
      
      if (isleap(iyear)) then
         febdays=29
      else
         febdays=28
      end if
      
      jd= idd  &
         + min(1,max(0,imm-1))*31  &
         + min(1,max(0,imm-2))*febdays  &
         + min(1,max(0,imm-3))*31  &
         + min(1,max(0,imm-4))*30  &
         + min(1,max(0,imm-5))*31  &
         + min(1,max(0,imm-6))*30  &
         + min(1,max(0,imm-7))*31  &
         + min(1,max(0,imm-8))*31  &
         + min(1,max(0,imm-9))*30  &
         + min(1,max(0,imm-10))*31  &
         + min(1,max(0,imm-11))*30  &
         + min(1,max(0,imm-12))*31

      julday1583=julday1583+jd
   end if 

   return
end function julday1583
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
logical function isleap(year)
   !This function runs a check on whether the year is leap or not, based 
   ! on Gregorian calendar
   integer, intent(in) :: year
   isleap = (mod(year,400) == 0) .or.  &
            (mod(year,4) == 0 .and. mod(year,100) /= 0)

   return
end function isleap
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
character(len=3) function monchar(month)
   !----- This function simply gives the month in a 3-character letter --------------------!
   integer, intent(in) :: month
   character(len=3), dimension(12), parameter ::  m3letters=                               &
                 (/'jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'/)
   monchar=m3letters(month)
   return
end function monchar
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
real function day_fraction(hour,minu,seco)
   use rconstants, only: day_sec,hr_sec,min_sec
   !----- This function returns hour/minute/second as the fraction of a full day ----------!
   implicit none
   integer, intent(in) :: hour,minu,seco
   day_fraction = (real(seco)+min_sec*real(minu)+hr_sec*real(hour))/day_sec
end function day_fraction
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine doy_2_monday(doy,year,month,day,mmm)
   !----- This function returns the month and day based on day of year (doy) and year -----!
   implicit none
   integer         , intent(in)  :: doy,year
   integer         , intent(out) :: month,day
   character(len=3), intent(out) :: mmm

   logical, external    :: isleap

   real, dimension(12), parameter :: elap_regu=(/  0, 31, 59, 90,120,151 &
                                                ,181,212,243,273,304,334/)
   real, dimension(12), parameter :: elap_leap=(/  0, 31, 60, 91,121,152 &
                                                ,182,213,244,274,305,335/)
   character(len=3), external :: monchar

   if (isleap(year)) then
      month=minloc(doy-elap_leap,dim=1,mask=elap_leap < doy)
      day=doy-elap_leap(month)
   else
      month=minloc(doy-elap_regu,dim=1,mask=elap_regu < doy)
      day=doy-elap_regu(month)
   end if
   mmm=monchar(month)

   return
end subroutine doy_2_monday
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine gmt_2_hms(gmt,hour,minu,seco)
   !----- This function returns hour/minute/second as the fraction of a full day ----------!
   implicit none
   real,    intent(in)  :: gmt
   integer, intent(out) :: hour,minu,seco

   hour = int(gmt)
   minu = int(mod(gmt,1.) * 60)
   seco = int(mod(gmt,1.) * 3600) - 60.*minu

end subroutine gmt_2_hms
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
integer function v5d_datestamp(year,doy)
   !----- This function simply concatenates the year with the day of year for vis5d -------!
   implicit none
   integer, intent(in) :: year, doy
   v5d_datestamp = 1000 * year + doy
   return
end function v5d_datestamp
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
integer function v5d_timestamp(hour,minu,seco)
   !----- This function simply concatenates the hour, minute, and second for vis5d --------!
   implicit none
   integer, intent(in) :: hour,minu,seco

   v5d_timestamp = 10000*hour + 100 *minu + seco
   
   return
end function v5d_timestamp
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
character(len=17) function grads_dtstamp(year,mmm,day,hour,minu)
   !----- This function simply concatenates time info for GrADS time stamp ----------------!
   implicit none
   integer         , intent(in) :: year,day,hour,minu
   character(len=3), intent(in) :: mmm
   
   if (minu == 0) then
      write(grads_dtstamp,fmt='(i2.2,a1,i2.2,a3,i4.4)') &
         hour,'z',day,mmm,year
   else
      write(grads_dtstamp,fmt='(2(i2.2,a1),i2.2,a3,i4.4)') &
         hour,':',minu,'z',day,mmm,year
   end if
   
   return
end function grads_dtstamp
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
character(len=19) function mevi_dtstamp(year,month,day,hour,minu,seco)
   !----- This function simply concatenates time info for MEVI filename stamp -------------!
   implicit none
   integer, intent(in) :: year,month,day,hour,minu,seco

   write(mevi_dtstamp,fmt='(i4.4,3(a1,i2.2),2i2.2)') &
      year,'-',month,'-',day,'-',hour,minu,seco
   
   return
end function mevi_dtstamp
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine sort_time(nmytimes,mytimes)
   use mod_time, only : time_stt
   implicit none
   !----- Arguments -----------------------------------------------------------------------!
   integer                             , intent(in)   :: nmytimes
   type(time_stt), dimension(nmytimes), intent(inout) :: mytimes
   !----- Local variables -----------------------------------------------------------------!
   type(time_stt)                                     :: timeholder
   real(kind=8)                                       :: elapholder
   real(kind=8) , dimension(nmytimes)                 :: elapsed
   logical      , dimension(nmytimes)                 :: unlocked
   integer                                            :: n,imin
   
   unlocked = .true.
   elapsed  = mytimes(:)%elapsed

   mainloop: do n = 1, nmytimes
      imin          = minloc(elapsed,dim=1,mask=unlocked)
      
      !----- If imin == n, then I don't need to sort anything this time, move on... -------!
      if (imin == n) then
        unlocked(n)   = .false. 
        cycle mainloop
      end if
      timeholder    = mytimes(imin)
      elapholder    = elapsed(imin)

      mytimes(imin) = mytimes(n)
      elapsed(imin) = elapsed(n)

      mytimes(n)    = timeholder
      elapsed(n)    = elapholder

      unlocked(n)   = .false. 
   end do mainloop
   
   return
end subroutine sort_time
!==========================================================================================!
!==========================================================================================!
