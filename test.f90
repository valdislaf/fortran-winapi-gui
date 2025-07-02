program test_getclientrect
  use iso_c_binding
  implicit none

  interface
    function GetClientRect(hWnd, lpRect) bind(C, name="GetClientRect")
      use iso_c_binding
      type(c_ptr), value :: hWnd
      type(c_ptr) :: lpRect
      integer(c_int32_t) :: GetClientRect
    end function
  end interface

  type, bind(C) :: RECT
    integer(c_int32_t) :: left, top, right, bottom
  end type RECT

  type(RECT), target :: rc
  type(c_ptr) :: hwnd
  integer(c_int32_t) :: ok
  integer :: i

   ! 1. Just initialize hwnd as NULL (no window) hwnd = c_null_ptr
  hwnd = c_null_ptr
  rc%left = -1; rc%top = -1; rc%right = -1; rc%bottom = -1

  ok = GetClientRect(hwnd, c_loc(rc))

  print *, 'GetClientRect(NULL) result:', ok
  print *, 'RECT:', rc%left, rc%top, rc%right, rc%bottom

  ! 2. We can try to issue a random HWND (there will almost always be an error)
  do i = 1, 3
    hwnd = transfer(i*123456, c_null_ptr)
    rc%left = -1; rc%top = -1; rc%right = -1; rc%bottom = -1
    ok = GetClientRect(hwnd, c_loc(rc))
    print *, 'Try hwnd=', i*123456, ' result:', ok
    print *, 'RECT:', rc%left, rc%top, rc%right, rc%bottom
  end do

  print *, 'Test completed.'

end program test_getclientrect
