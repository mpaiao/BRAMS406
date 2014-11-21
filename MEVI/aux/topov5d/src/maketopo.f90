program maketopo
!------------------------------------------------------------------------------------------!
! Variable declaration section                                                             !
!------------------------------------------------------------------------------------------!
  implicit none
!----- Parameters -------------------------------------------------------------------------!
  real, parameter                             :: undef=-999.0
  integer, parameter                          :: maxvar=300, hugez=99
  character(len=40), parameter                :: head=adjustl('TOPO2'//char(0))
!----- Namelist variables -----------------------------------------------------------------!
  integer             :: inpfrmt
  character(len=200)  :: ctlfile, v5dfile
  character(len=40)   :: topo,land
  character(len=10)   :: scale
  real                :: beta0,beta1,beta2
  integer             :: usethistime
!----- Other variables --------------------------------------------------------------------!
  character(len=200)                      :: binway,binary
  character(len=200) , external           :: actualbin
  character(len=15)                       :: time0
  character(len=2)                        :: dtunit
  character(len=30) , dimension(maxvar)   :: varname
  integer, external                       :: match
  integer                                 :: idtopo,idland
  integer                                 :: xmax,ymax,zmax,tmax,vmax,z,t,v, erad
  integer                                 :: year,month,day,hour,minu,error, dtime, posi=0
  integer           , dimension(maxvar)   :: zvar
  integer(kind=2), allocatable, dimension(:,:) :: outvar
  logical                                 :: template
  real                                    :: wlon,dlon,elon,slat,dlat,nlat
  real                                    :: undefin
  real              , dimension(hugez)    :: pres
  real, allocatable , dimension(:,:)      :: mytopo
  real, allocatable , dimension(:,:,:,:)  :: dataset
!----- Namelist ---------------------------------------------------------------------------!
  namelist /settings/inpfrmt,ctlfile,v5dfile,topo,land,usethistime,scale,beta0,beta1,beta2
!------------------------------------------------------------------------------------------!
  write (unit=*,fmt='(a)') '---> Reading namelist...'
  open  (unit=60,file='maketopo.nml',status='old')
     read  (unit=60,nml=settings)
  close (unit=60,status='keep')

!----- If this is not a default (CTL) input, convert using the usgstopo subroutine --------!
  if (inpfrmt == 1) then
     call usgstopo(ctlfile,v5dfile,head,scale,beta0,beta1,beta2)
     stop 'Program ended with no problem!' 
  end if

!----- Converting the variable names to lower case, so it will be case insensitive --------!
  call tolower(topo,1)
  call tolower(land,1)

!----- Reading the ctl and extracting the necessary information ---------------------------!
  write (unit=*,fmt='(a)') '---> Reading the ctl file: '//trim(ctlfile)//'...'
  call readctl(ctlfile,hugez,maxvar,binway,template,undefin,xmax,wlon,dlon,ymax,slat,dlat, &
               zmax,pres,tmax,year,month,day,hour,minu,dtime,dtunit,vmax,varname,zvar)

!----- Defining the easternmost longitude and northernmost latitude -----------------------!
  elon=wlon+(xmax-1)*dlon
  nlat=slat+(ymax-1)*dlat

!----- Matching the variables you provided and the list of variables ----------------------!
  write (unit=*,fmt='(a)') '---> Finding the variables you want me to use...'
  idtopo=match(topo,vmax,varname(1:vmax),.true.)
  idland=match(land,vmax,varname(1:vmax),.true.)

!----- Allocate the dataset so we have just what we need ----------------------------------!
  allocate(dataset(xmax,ymax,zmax,vmax),outvar(xmax,ymax),mytopo(xmax,ymax),stat=error)   
  if (error /= 0) stop 'You do not have enough memory to run this program...'
  dataset=undef !---- Just to ensure that values between zvar and zmax have something -----!

!----- Opening file if it is not template -------------------------------------------------!
  if (.not. template) then
    open(unit=12,file=trim(binway),access='direct',form='unformatted',status='old',        &
         action='read',recl=4*xmax*ymax,iostat=error)
    if (error /= 0) then
      write (unit=*,fmt=*) '----------------------- FATAL ERROR!!! -----------------------'
      write (unit=*,fmt=*) '  Problem reading the file ',trim(binway),'!!!' 
      write (unit=*,fmt=*) '  Does this file exist?!'
      write (unit=*,fmt=*) '--------------------------------------------------------------'
      stop 'Execution aborted'
    end if
    binary=binway !Just to make things easier later on.
  end if
  
  if (usethistime > tmax) then
      write (unit=*,fmt=*) '----------------------- FATAL ERROR!!! -----------------------'
      write (unit=*,fmt='(a,i4,a)') '  Usethistime was set to ',usethistime,','
      write (unit=*,fmt='(a,i4,a)') '  but your data has only ',tmax,'times...'
      write (unit=*,fmt=*) '--------------------------------------------------------------'
      stop 'Execution aborted'
  end if
!----- Switching surface flags by 1 so I can read the data --------------------------------!
  where (zvar == 0) zvar = 1
!------------------------------------------------------------------------------------------!
! Read the file the user provided                                                          !
!------------------------------------------------------------------------------------------!
  tloop: do t=1,usethistime
!----- Output something on screen to entretain a bored user -------------------------------!
    write(unit=*,fmt='(a,2(i2.2,a),i4.4,a,2(i2.2,a))') '---> Reading the file for ',       &
                           month,'/',day,'/',year,' - ',hour,':',minu,' GMT...'
!----- If we are dealing with templates, need to open, update and so on... ----------------!
    if(template) then
      binary=actualbin(template,binway,year,month,day,hour,minu)
      open(unit=12,file=binary,access='direct',form='unformatted',status='old',            &
           action='read',recl=4*xmax*ymax,iostat=error)
      posi=0
    end if
!----- If the file exists, proceed, otherwise, skip to the next time ----------------------!
    if (error == 0) then
!----- Loop over variables, reading them --------------------------------------------------!
      vloop: do v=1,vmax
        zloop: do z=1,zvar(v)
          posi=posi+1
          read(unit=12,rec=posi,iostat=error) dataset(:,:,z,v)
          if (error /= 0) then
            write (unit=*,fmt=*) '-------------------- FATAL ERROR!!! --------------------'
            write (unit=*,fmt=*) 'Error while reading '//trim(binary)//'!!!'
            stop 'Execution prematurely halted!'
          end if
        end do zloop
      end do vloop
!----- Switching the missing values by the RALPH2 flag ------------------------------------!
      where (dataset == undefin) dataset = undef
     
!----- Close variable if it is a template -------------------------------------------------!
      if (template) then
        close(unit=12,status='keep')
      end if
!----- Just skip to the next time in case I don't find the binary for this time... --------!
    else
      write (unit=*,fmt=*) '  ---> File not found for this time, I am skipping to the ',   &
                           'next time...'
    end if
!----- Update time ------------------------------------------------------------------------!
    call updatetime(dtime,dtunit,year,month,day,hour,minu)
  end do tloop

!----- Here I can play convert the topography in case you want to -------------------------!
  write (unit=*,fmt='(a)') '---> Converting the topography (if needed)...'
  call tolower(scale,1)
  call convtopo(xmax,ymax,dataset(:,:,1,idtopo),scale,beta0,beta1,beta2,mytopo)
  
!----- Converting the topography data to integer. Even means OK, odd means water ----------!
  write (unit=*,fmt='(a)') '---> Transforming topography into integer...'  
  outvar=2*nint(mytopo)
  where (mytopo < 0. .or. dataset(:,:,1,idland) < 0.01) outvar=outvar+1

!----- Writing the topography data --------------------------------------------------------!
  write (unit=*,fmt='(a)') '---> Writing the Vis5D topography file: '//trim(v5dfile)//'...' 
  call writetopo(v5dfile,head,xmax,ymax,wlon,elon,dlon,slat,nlat,dlat,outvar)

  stop 'Program ended with no problem!' 
end program maketopo
