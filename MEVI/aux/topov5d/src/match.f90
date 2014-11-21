!------------------------------------------------------------------------------------------!
! Function match                                                                           !
!                                                                                          !
!    This function looks for the index of the element in the list that corresponds to the  !
! string. This works for character strings and lists only. Argument first is a logical to  !
! know whether the first or the last component should be reported                          !
!------------------------------------------------------------------------------------------!
integer function match(string,vmax,list,first)
!------------------------------------------------------------------------------------------!
!  Variable declaration                                                                    !
!------------------------------------------------------------------------------------------!
  implicit none
!----- Input variables --------------------------------------------------------------------!
  integer          ,                   intent(in) :: vmax
  character(len=*) ,                   intent(in) :: string
  character(len=*) , dimension(vmax) , intent(in) :: list
  logical                            , intent(in) :: first
!----- Internal variables -----------------------------------------------------------------!
  integer                                         :: l
!------------------------------------------------------------------------------------------!   

!----- Loop over list to check whether any string is found --------------------------------!
  match=-1
  matchloop: do l=1,vmax
    if (string == list(l)) then
      match=l
      if (first) exit matchloop
    end if
  end do matchloop
  
  return
end function match

