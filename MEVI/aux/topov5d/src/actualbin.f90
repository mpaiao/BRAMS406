!------------------------------------------------------------------------------------------!
! Function actualbin                                                                       !
!                                                                                          !
!    This function writes the actual binary name to be read. If the ctl was a template,    !
! it will write the name accordingly, otherwise it will simply return the input bin file.  !
!------------------------------------------------------------------------------------------!
character(len=200) function actualbin(template,binway,year,month,day,hour,minu)
!----- Variable declaration ---------------------------------------------------------------!
  implicit none
!----- Input variables --------------------------------------------------------------------!
  character(len=*), intent(in) :: binway
  logical, intent(in)          :: template
  integer, intent(in)          :: year, month, day, hour, minu  
!----- Internal variables -----------------------------------------------------------------!
  integer                      :: radpos,endway,nextpos,ypos,mpos,dpos,hpos,npos
  character(len=3)             :: flag
  character(len=30)            :: fileformat 
!----- Parameters -------------------------------------------------------------------------!
  character(len=3), parameter, dimension(12) :: mmm=(/'jan','feb','mar','apr','may','jun', &
                                                      'jul','aug','sep','oct','nov','dec'/)
  character(len=3), parameter, dimension(12) :: eee=(/'JAN','FEB','MAR','APR','MAY','JUN', &
                                                      'JUL','AUG','SEP','OCT','NOV','DEC'/)
  integer, parameter                         :: huuuge=huge(year)
!------------------------------------------------------------------------------------------!

!----- First check whether we are dealing with templates or not ---------------------------!
  if (.not. template) then
    actualbin=binway
  else
!----- Initialize some constants ----------------------------------------------------------!
    endway=len_trim(binway)

!----- Finding the position of all elements -----------------------------------------------!
    ypos=index(binway,'%y',.true.)
    mpos=index(binway,'%m',.true.)
    dpos=index(binway,'%d',.true.)
    hpos=index(binway,'%h',.true.)
    npos=index(binway,'%n',.true.)
!----- Here I "discard" the ones not included in the template -----------------------------!
    if (ypos == 0) ypos=huuuge
    if (mpos == 0) mpos=huuuge
    if (dpos == 0) dpos=huuuge
    if (hpos == 0) hpos=huuuge
    if (npos == 0) npos=huuuge

!----- Here I will loop over the position of all the template signs until I am done -------!
    radpos=min(ypos,mpos,dpos,hpos,npos)-1
    actualbin=binway
    do while (ypos /= huuuge .or. mpos /= huuuge .or. dpos /= huuuge .or.                  &
              hpos /= huuuge .or. npos /= huuuge) 
      nextpos=min(ypos,mpos,dpos,hpos,npos)
!----- Here I determine which is going to be the next, the smaller positon... -------------!
      read(binway(nextpos:nextpos+2),fmt=*) flag
      select case (flag)
      case ('%y4','%Y4')
        write(actualbin,fmt=('(a,i4.4,a)')) actualbin(1:radpos),year,                      &
                                            binway(nextpos+3:endway)
        radpos=radpos+4
        ypos=huuuge
      case ('%y2','%Y2')
        write(actualbin,fmt=('(a,i2.2,a)')) actualbin(1:radpos),mod(year,100),             &
                                            binway(nextpos+3:endway)
        radpos=radpos+2
        ypos=huuuge
      case('%mc')
        write(actualbin,fmt=('(3a)')) actualbin(1:radpos),mmm(month),                      &
                                      binway(nextpos+3:endway)
        radpos=radpos+3
        mpos=huuuge
      case('%MC')
        write(actualbin,fmt=('(3a)')) actualbin(1:radpos),eee(month),                      &
                                      binway(nextpos+3:endway)
        radpos=radpos+3
        mpos=huuuge
      case('%m2','%M2')
        write(actualbin,fmt=('(a,i2.2,a)')) actualbin(1:radpos),month,                     &
                                            binway(nextpos+3:endway)
        radpos=radpos+2
        mpos=huuuge
      case('%m1','%M1')
        if (month < 10) then
          write(actualbin,fmt=('(a,i1,a)')) actualbin(1:radpos),month,                     &
                                            binway(nextpos+3:endway)
          radpos=radpos+1
          mpos=huuuge
        else
          write(actualbin,fmt=('(a,i2.2,a)')) actualbin(1:radpos),month,                   &
                                              binway(nextpos+3:endway)
          radpos=radpos+2
          mpos=huuuge
        end if 
      case('%d2','%D2')
        write(actualbin,fmt=('(a,i2.2,a)')) actualbin(1:radpos),day,                       &
                                            binway(nextpos+3:endway)
        radpos=radpos+2
        dpos=huuuge      
      case('%d1','%D1')
        if (day < 10) then
          write(actualbin,fmt=('(a,i1,a)')) actualbin(1:radpos),day,                       &
                                            binway(nextpos+3:endway)
          radpos=radpos+1
          dpos=huuuge
        else
          write(actualbin,fmt=('(a,i2.2,a)')) actualbin(1:radpos),day,                     &
                                              binway(nextpos+3:endway)
          radpos=radpos+2
          dpos=huuuge
        end if 
      case('%h2','%H2')
        write(actualbin,fmt=('(a,i2.2,a)')) actualbin(1:radpos),hour,                      &
                                            binway(nextpos+3:endway)
        radpos=radpos+2
        hpos=huuuge      
      case('%h1','%H1')
        if (hour < 10) then
          write(actualbin,fmt=('(a,i1,a)')) actualbin(1:radpos),hour,                      &
                                            binway(nextpos+3:endway)
          radpos=radpos+1
          hpos=huuuge
        else
          write(actualbin,fmt=('(a,i2.2,a)')) actualbin(1:radpos),hour,                    &
                                              binway(nextpos+3:endway)
          radpos=radpos+2
          hpos=huuuge
        end if
      case('%n2','%N2')
        write(actualbin,fmt=('(a,i2.2,a)')) actualbin(1:radpos),minu,                      &
                                            binway(nextpos+3:endway)
        radpos=radpos+2
        npos=huuuge
      case default
        write(unit=*,fmt='(3a)') 'Unknown format in the ctl template: ',flag,' !!!'
        stop 'Execution aborted...'
      end select      
    end do
  end if
  return
end function actualbin
