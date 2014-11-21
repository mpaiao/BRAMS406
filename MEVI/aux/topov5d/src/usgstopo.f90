!------------------------------------------------------------------------------------------!
!    This subroutine reads the USGS ascii dataset, which is always assumed to be the       !
! global dataset, then converts into the vis5d format.                                     !
!------------------------------------------------------------------------------------------!
subroutine usgstopo(usgsfile,v5dfile,head,scale,beta0,beta1,beta2)
   implicit none
   character(len=200)                          , intent(in) :: usgsfile,v5dfile
   character(len=40)                           , intent(in) :: head
   character(len=10)                           , intent(in) :: scale
   real                                        , intent(in) :: beta0,beta1,beta2
   
   integer                                                  :: xmax,ymax,miss,dum,x,y
   real           , allocatable, dimension(:,:)             :: inptopo,inpland,mytopo
   integer(kind=2), allocatable, dimension(:,:)             :: outvar
   
   real    :: dlon,dlat,wlon,elon,slat,nlat
   integer :: ierr
   
   !---------------------------------------------------------------------------------------!
   !  Open the file, read the header.                                                      !
   !---------------------------------------------------------------------------------------!
   open (unit=66,file=trim(usgsfile),form='formatted',status='old',iostat=ierr)
   if (ierr /= 0) stop 'I couldn''t find the input file: !!!'

   read (unit=66,fmt=*) xmax,ymax,dum,miss,dum
   
   !----- Setting the dataset resolution --------------------------------------------------!
   dlon = 360./real(xmax)
   dlat = 180./real(ymax)
   
   !----- Setting the boundaries ----------------------------------------------------------!
   wlon = -180. + 0.5*dlon
   elon =  180. - 0.5*dlon
   slat =  -90. + 0.5*dlat
   nlat =   90. - 0.5*dlat

   !----- Allocating arrays ---------------------------------------------------------------!
   allocate(inptopo(xmax,ymax),inpland(xmax,ymax),mytopo(xmax,ymax),outvar(xmax,ymax))
   
   
   !----- Reading the full file -----------------------------------------------------------!
   read (unit=66,fmt=*) ((inptopo(x,y),x=1,xmax),y=ymax,1,-1)  
   close (unit=66,status='keep')

   !----- Organising the data -------------------------------------------------------------!
   where (inptopo == miss)
     inpland =  1.
     inptopo =  0.
   elsewhere
     inpland = 0.
   end where
  
   !----- Applying any transform that the user may want -----------------------------------!
   call tolower(scale,1)
   call convtopo(xmax,ymax,inptopo,scale,beta0,beta1,beta2,mytopo)

   outvar = 2*nint(mytopo)
   where (inpland == 1.) outvar = outvar + 1
   
   !----- Writing the topography data -----------------------------------------------------!
   write (unit=*,fmt='(a)') '---> Writing the Vis5D topography file: '//trim(v5dfile)//'...' 
   call writetopo(v5dfile,head,xmax,ymax,wlon,elon,dlon,slat,nlat,dlat,outvar)

   deallocate(inptopo,inpland,mytopo,outvar)   

   return
end subroutine usgstopo
