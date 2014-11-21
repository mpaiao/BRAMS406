!----------------------------------------------------------------
! University of Houston
! Air Quality Modeling and Monitoring Center
! 218 Old Science Building
! Houston, TX 77204-5048
! USA
!
! email: sbkim@math.uh.edu (Dr. Seung-Bum Kim)
!        dwbyun@math.uh.edu (Dr. Daewon W. Byun)
!----------------------------------------------------------------

! Set maximum values of parameters: RAMS
integer, parameter :: mnx=400  ! maximum number of points in x-dir
integer, parameter :: mny=400  ! maximum number of points in y-dir
integer, parameter :: mnz=80   ! maximum number of points in z-dir

! ARL
!  NWFO : Number of derived WFO Variables to be output from FSL routines
!  NARL : Number of STANDARD (underived) vars to be output in ARLWRT
integer, parameter :: nwfo=1 ! number of WFO variables
! JCL(3/14/03) extra variables from coupling between RAMS<=>STILT
!integer, parameter :: narl=18 ! number of ARL variables
integer, parameter :: narl=33 ! number of ARL variables
integer, parameter :: navrb=nwfo+narl
