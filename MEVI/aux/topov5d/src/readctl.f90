!------------------------------------------------------------------------------------------!
! Subroutine readctl.f90                                                                   !
!                                                                                          !
! This subroutine reads the ctl and extracts all the information we need from it.          !
!------------------------------------------------------------------------------------------!
subroutine readctl(ctlfile,hugez,maxvar,binway,template,undefin,xmax,lon,dlon,ymax,lat,    &
                   dlat,zmax,level,tmax,year,month,day,hour,minu,dtime,dtunit,vmax,        &
                   varname,zvar)
!------------------------------------------------------------------------------------------!
! Variable declaration section                                                             !
!------------------------------------------------------------------------------------------!
  implicit none
!----- Input variables --------------------------------------------------------------------!
  character(len=*), intent(in) :: ctlfile
  integer,          intent(in) :: hugez, maxvar
!----- Output variables -------------------------------------------------------------------!
  character(len=*) ,                     intent(out) :: binway,dtunit
  character(len=*) , dimension(maxvar) , intent(out) :: varname
  integer          ,                     intent(out) :: xmax,ymax,zmax,tmax,vmax
  integer          ,                     intent(out) :: year,month,day,hour,minu,dtime
  integer          , dimension(maxvar) , intent(out) :: zvar
  logical          ,                     intent(out) :: template
  real             ,                     intent(out) :: undefin,lon,dlon,lat,dlat
  real             , dimension(hugez)  , intent(out) :: level
!----- Internal variables -----------------------------------------------------------------!
  integer           :: bctl,ectl,hbin,ebin, error,n,z0,dlev,extt
  character(len=30) :: what, auxi,time0,dtimefull
  character(len=30), dimension(6) :: ctloptions
!------------------------------------------------------------------------------------------!

!----- Initialize level, zvar and varname -------------------------------------------------!
  level=0.
  zvar=0
   

!----- Determine position of path and end of ctl ------------------------------------------!
  bctl=index(ctlfile,'/',.true.) 
  ectl=len_trim(ctlfile)

  open(unit=11,file=ctlfile,status='old',action='read',iostat=error)
  if (error /= 0) stop 'The ctl provided has problems. Does it exist?'
  do while(error==0)
!----- Read the first element of the line, then I decide what to do... --------------------!
    read(unit=11,fmt=*,iostat=error) what
    backspace(unit=11)
    call tolower(what,1)
!----- And here are the choices!!! Pick one that best fits! -------------------------------!
    select case (what)
!---- Picking up the binary files. Later I will figure whether it is a template... --------!
    case ('dset')
      read(unit=11,fmt='(a4,1x,a99)') auxi,binway
      hbin=index(binway,'^',.true.)
      ebin=len_trim(binway)
      binway=ctlfile(1:bctl)//binway(hbin+1:ebin)
!----- Discovering whether this ctl is template or not ------------------------------------!
    case ('options','OPTIONS')
      read(unit=11,fmt=*) auxi,(ctloptions(n),n=1,6)
      call tolower(ctloptions,6)
      template=any(ctloptions == 'template')
!----- Rewind the file until the line with options... -------------------------------------!
      auxi=''
      rewind(unit=11)
      do while (auxi /= 'options')
        read(unit=11,fmt=*) auxi
        call tolower(auxi,1)
      end do
!----- Reading the undefined flag ---------------------------------------------------------!
    case('undef')
      read(unit=11,fmt=*) auxi,undefin
!----- Reading the number of longitude points ---------------------------------------------!
    case ('xdef')
      read(unit=11,fmt=*) auxi,xmax,auxi,lon,dlon
!----- Reading the number of latitude points ----------------------------------------------!
    case ('ydef')
      read(unit=11,fmt=*) auxi,ymax,auxi,lat,dlat
!----- Reading the vertical levels --------------------------------------------------------!
    case ('zdef')
      read(unit=11,fmt=*) auxi,zmax,auxi
      backspace(unit=11)
      if (error /= 0) stop 'Your computer does not have enough memory!!!'
      call tolower(auxi,1)
      if (auxi == 'levels') then         
        read(unit=11,fmt=*) auxi,auxi,auxi,(level(n),n=1,zmax)
      else
        read(unit=11,fmt=*) auxi,auxi,auxi,z0,dlev
        do n=1,zmax
          level(n)=z0+(n-1)*dlev
        end do
      end if
!----- Read the time span -----------------------------------------------------------------!
    case ('tdef')
      read(unit=11,fmt=*) auxi,tmax,auxi,time0,dtimefull
      extt=len_trim(dtimefull)
      dtunit=dtimefull(extt-1:extt)
      read(dtimefull(1:extt-2),fmt=*) dtime
      call tolower(time0,1)
      call tolower(dtunit,1)
      call gettime(time0,year,month,day,hour,minu)
!----- Read the number of variables available ---------------------------------------------!
    case('vars')
      read(unit=11,fmt=*) auxi, vmax
      do n=1,vmax
        read(unit=11,fmt=*) varname(n),zvar(n)
      end do
      call tolower(varname(1:vmax),vmax)
!----- Read the endvars -------------------------------------------------------------------!
    case('endvars')
      error=-999 !Just to leave the ctl
!----- If it is an unimportant line, like title, or comment, just skip---------------------!
    case default
     read(unit=11,fmt=*,iostat=error)
    end select
  end do 

  close(unit=11,status='keep')
  return
end subroutine readctl
