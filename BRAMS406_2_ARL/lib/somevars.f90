module somevars
  integer :: myngrids,myn3
  integer, allocatable :: mynnzp(:)
  real, allocatable, dimension(:,:) :: mydzmn,mydztn

  contains 
  
  subroutine alloc_somevars(ngrids,n3)
     implicit none
     integer, intent(in) :: ngrids,n3
     if (.not. allocated(mynnzp)) allocate(mynnzp(ngrids))
     if (.not. allocated(mydzmn)) allocate(mydzmn(n3,ngrids))
     if (.not. allocated(mydztn)) allocate(mydztn(n3,ngrids))
     return
  end subroutine alloc_somevars
end module somevars

