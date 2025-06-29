! Утилита для преобразования компонентов A, R, G, B в формат COLORREF (ARGB)
module color_utils
  use win_types
  
contains
  function MakeARGB(A, R, G, B) result(color)
    integer(int32), intent(in) :: A, R, G, B
    integer(int32) :: color
    color = IOR(ISHFT(A, 24), IOR(ISHFT(R, 16), IOR(ISHFT(G, 8), B)))
  end function MakeARGB

end module color_utils
