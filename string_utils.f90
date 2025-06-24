! �������������� ����� � UTF-16 (��� WinAPI)
module string_utils
  use win_types
  implicit none

contains

  ! ����������� ������ � ������ �������� UTF-16 � ����������� ����
 function to_wide_null_terminated(text) result(wide)
 
  use iso_c_binding, only: c_char, c_null_char
      implicit none
      character(len=*), intent(in) :: text
      character(kind=c_char), allocatable, target :: wide(:)
      integer :: i, k, n

      n = len_trim(text)
      allocate(wide(2 * n + 2))  ! �� 1 WCHAR ������ (== 2 char) ��� ������������ \0

      k = 1
      do i = 1, n
        wide(k) = text(i:i)
        k = k + 1
        wide(k) = c_null_char
        k = k + 1
      end do

      ! ����������� WCHAR (\0\0)
      wide(k) = c_null_char
      wide(k + 1) = c_null_char
    end function

end module string_utils