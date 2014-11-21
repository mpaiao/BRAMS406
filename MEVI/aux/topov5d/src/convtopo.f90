!------------------------------------------------------------------------------------------!
! Subroutine convtopo                                                                      !
!                                                                                          !
!    This subroutine converts the actual input topography into something else, based on    !
! your preference. This is because the map may seem too flat or too exagerated sometimes.  !
!------------------------------------------------------------------------------------------!
subroutine convtopo(xmax,ymax,intopo,scale,beta0,beta1,beta2,mytopo)
   implicit none
   character(len=10) , intent(in)                         :: scale
   integer           , intent(in)                         :: xmax,ymax
   real              , intent(in)  , dimension(xmax,ymax) :: intopo
   real              , intent(in)                         :: beta0,beta1,beta2
   real              , intent(out) , dimension(xmax,ymax) :: mytopo
  
   select case (scale)
   case ('linear')
      mytopo=beta0+beta1*intopo
   case ('quadratic')
      mytopo=beta0+beta1*intopo+beta2*intopo**2
   case ('sqrt')
      where (intopo > 0)
         mytopo=beta0+beta1*sqrt(intopo)
      elsewhere
         mytopo=beta0-beta1*sqrt(-intopo)
      end where
   case ('exp')
      mytopo=beta0+beta1*exp(beta2*intopo)
   case ('log','ln')
      where (intopo >= beta0)
         mytopo=beta0+beta1*log(intopo)
      elsewhere
         mytopo=intopo
      end where
   case ('none')
      mytopo=intopo
   case default
      write (unit=*,fmt='(a)') 'Warning: the scale '//trim(scale)//' is invalid.'
      write (unit=*,fmt='(a)') '         I will not apply any conversion'
      mytopo=intopo       
   end select
   return
end subroutine convtopo
