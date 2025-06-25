! Преобразование строк в UTF-16 (для WinAPI)
module string_utils
  use win_types

contains

  ! Преобразует строку в массив символов UTF-16 с завершающим нулём
 function to_wide_null_terminated(text) result(wide)
      character(len=*), intent(in) :: text
      character(kind=char), allocatable, target :: wide(:)
      integer :: i, k, n

      n = len_trim(text)
      allocate(wide(2 * n + 2))  ! на 1 WCHAR больше (== 2 char) для завершающего \0

      k = 1
      do i = 1, n
        wide(k) = text(i:i)
        k = k + 1
        wide(k) = char0
        k = k + 1
      end do

      ! Завершающий WCHAR (\0\0)
      wide(k) = char0
      wide(k + 1) = char0
    end function

end module string_utils