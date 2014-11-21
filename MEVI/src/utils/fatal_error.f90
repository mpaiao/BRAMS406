subroutine fatal_error(reason,subr,file)
!------------------------------------------------------------------------------------------!
!   Subroutine that outputs error messages and halts the execution.                        !
!------------------------------------------------------------------------------------------!
   implicit none
   character(len=*), intent(in) :: reason
   character(len=*), intent(in) :: subr,file
  
   write(unit=*,fmt='(a)') ':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::'
   write(unit=*,fmt='(a)') ':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::'
   write(unit=*,fmt='(a)') ':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::'
   write(unit=*,fmt='(a)') ' '
   write(unit=*,fmt='(a)') '---------------------------------------------------------------'
   write(unit=*,fmt='(a)') '                    !!! FATAL ERROR !!!                        '
   write(unit=*,fmt='(a)') '---------------------------------------------------------------'
   write(unit=*,fmt='(a,1x,a)')    '    ---> File:       ',trim(file)
   write(unit=*,fmt='(a,1x,a)')    '    ---> Subroutine: ',trim(subr)
   write (unit=*,fmt='(a,1x,a)')   '    ---> Reason:     ',trim(reason)
   write(unit=*,fmt='(a)') '---------------------------------------------------------------'
   write(unit=*,fmt='(a)') ' MEVI execution halts (see previous error message)...'
   write(unit=*,fmt='(a)') '---------------------------------------------------------------'
   stop 'fatal_error'
end subroutine fatal_error
!==========================================================================================!
!==========================================================================================!
