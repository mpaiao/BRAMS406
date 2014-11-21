!------------------------------------------------------------------------------------------!
! Subroutine writetopo.f90                                                                 !
!                                                                                          !
! This subroutine writes the topography file in vis5d friendly format                      !
!------------------------------------------------------------------------------------------!
subroutine writetopo(v5dfile,head,xmax,ymax,wlon,elon,dlon,slat,nlat,dlat,outvar)
   implicit none
   character(len=*)  , intent(in)                        :: v5dfile
   character(len=40) , intent(in)                        :: head
   integer           , intent(in)                        :: xmax,ymax
   real              , intent(in)                        :: wlon,elon,dlon,slat,dlat,nlat
   integer(kind=2)   , intent(in) , dimension(xmax,ymax) :: outvar
   integer                                               :: x,y,c,rec
   integer(kind=2)                                       :: auxi2
   integer                                               :: auxi4
   real                                                  :: auxr4
!------------------------------------------------------------------------------------------!
! Since I don't know fortran binary I/O very well, I will do in three steps.               !
!------------------------------------------------------------------------------------------!
!----- 1st step: open the file and write the header ---------------------------------------!
   open(unit=66, file=trim(v5dfile),status='replace',action='write',form='unformatted',    &
        access='direct',recl=40) ! 40 for the header, plus 24 words
      write(unit=66,rec=1) head
   close (unit=66,status='keep')
   open(unit=66, file=trim(v5dfile),status='old',action='readwrite',form='unformatted',    &
        access='direct',recl=4) ! 40 for the header, plus 24 words
      auxr4=-wlon; call swap32real(auxr4,1); write(unit=66,rec=11) auxr4
      auxr4=-elon; call swap32real(auxr4,1); write(unit=66,rec=12) auxr4
      auxr4= nlat; call swap32real(auxr4,1); write(unit=66,rec=13) auxr4
      auxr4= slat; call swap32real(auxr4,1); write(unit=66,rec=14) auxr4
      auxi4= ymax; call swap32inte(auxi4,1); write(unit=66,rec=15) auxi4
      auxi4= xmax; call swap32inte(auxi4,1); write(unit=66,rec=16) auxi4
   close (unit=66,status='keep')


!----- 2nd step: open the file and write the data -----------------------------------------!
   open(unit=66, file=trim(v5dfile),status='old',action='readwrite',form='unformatted',    &
        access='direct',recl=2) ! 40 for the header, plus 6 words
      rec=32 ! 64 bytes...
      do y=ymax,1,-1
         do x=1,xmax
            rec=rec+1
            auxi2=outvar(x,y)
            call swap16inte(auxi2,1)
            write(unit=66,rec=rec) auxi2
         end do
      end do
   close (unit=66,status='keep')
   return
end subroutine writetopo
