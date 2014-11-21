!==========================================================================================!
!==========================================================================================!
subroutine tolower(word)
!------------------------------------------------------------------------------------------!
! Subroutine tolower                                                                       !
!                                                                                          !
!    This subroutine converts all common upper-case characters into lowercase.             !
!------------------------------------------------------------------------------------------!
   implicit none
!----- Arguments --------------------------------------------------------------------------!
   character(len=*)    , intent(inout) :: word
!----- Internal variables -----------------------------------------------------------------!
   integer                             :: wmax,w
!------------------------------------------------------------------------------------------!
   wmax=len_trim(word)
   do w=1,wmax
      select case(word(w:w))
      case('A') 
        word(w:w)='a'
      case('B')
        word(w:w)='b'
      case('C')
        word(w:w)='c'
      case('D')
        word(w:w)='d'
      case('E')
        word(w:w)='e'
      case('F')
        word(w:w)='f'
      case('G')
        word(w:w)='g'
      case('H')
        word(w:w)='h'
      case('I')
        word(w:w)='i'
      case('J')
        word(w:w)='j'
      case('K')
        word(w:w)='k'
      case('L')
        word(w:w)='l'
      case('M')
        word(w:w)='m'
      case('N')
        word(w:w)='n'
      case('O')
        word(w:w)='o'
      case('P')
        word(w:w)='p'
      case('Q')
        word(w:w)='q'
      case('R')
        word(w:w)='r'
      case('S')
        word(w:w)='s'
      case('T')
        word(w:w)='t'
      case('U')
        word(w:w)='u'
      case('V')
        word(w:w)='v'
      case('W')
        word(w:w)='w'
      case('X')
        word(w:w)='x'
      case('Y')
        word(w:w)='y'
      case('Z')
        word(w:w)='z'
      case('Á')
        word(w:w)='á'
      case('É')
        word(w:w)='é'
      case('Í')
        word(w:w)='í'
      case('Ó')
        word(w:w)='ó'
      case('Ú')
        word(w:w)='ú'
      case('Ý')
        word(w:w)='ý'
      case('À')
        word(w:w)='à'
      case('È')
        word(w:w)='è'
      case('Ì')
        word(w:w)='ì'
      case('Ò')
        word(w:w)='ò'
      case('Ù')
        word(w:w)='ù'
      case('Â')
        word(w:w)='â'
      case('Ê')
        word(w:w)='ê'
      case('Î')
        word(w:w)='î'
      case('Ô')
        word(w:w)='ô'
      case('Û')
        word(w:w)='û'
      case('Ä')
        word(w:w)='ä'
      case('Ë')
        word(w:w)='ë'
      case('Ï')
        word(w:w)='ï'
      case('Ö')
        word(w:w)='ö'
      case('Ü')
        word(w:w)='ü'
      case('Ã')
        word(w:w)='ã'
      case('Õ')
        word(w:w)='õ'
      case('Ñ')
        word(w:w)='ñ'
      case('Å')
        word(w:w)='å'
      case('Ç')
        word(w:w)='ç'
      end select
   end do
   return
end subroutine tolower
!==========================================================================================!
!==========================================================================================!
