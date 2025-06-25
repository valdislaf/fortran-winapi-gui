! Утилита для преобразования компонентов A, R, G, B в формат COLORREF (ARGB)
module color_utils
  use iso_c_binding
  implicit none

contains

  function MakeARGB(A, R, G, B) result(color)
    integer(c_int32_t), intent(in) :: A, R, G, B
    integer(c_int32_t) :: color
    color = IOR(ISHFT(A, 24), IOR(ISHFT(R, 16), IOR(ISHFT(G, 8), B)))
  end function MakeARGB

end module color_utils
