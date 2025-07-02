! Interfaces to WinAPI functions
module win_api
  interface
    function RegisterClassExW(lpWndClass) bind(C, name="RegisterClassExW")
      use standard
      type(ptr), value :: lpWndClass
      integer(int32) :: RegisterClassExW
    end function

    function GetLastError() bind(C, name="GetLastError")
      use standard
      integer(int32) :: GetLastError
    end function

    function CreateWindowExW(dwExStyle, lpClassName, lpWindowName, dwStyle, x, y, nWidth, nHeight, &
                             hWndParent, hMenu, hInstance, lpParam) bind(C, name="CreateWindowExW")
      use standard
      integer(int32), value :: dwExStyle, dwStyle, x, y, nWidth, nHeight
      type(ptr), value :: lpClassName, lpWindowName, hWndParent, hMenu, hInstance, lpParam
      type(ptr) :: CreateWindowExW
    end function

    subroutine ShowWindow(hWnd, nCmdShow) bind(C, name="ShowWindow")
      use standard
      type(ptr), value :: hWnd
      integer(int32), value :: nCmdShow
    end subroutine

    subroutine UpdateWindow(hWnd) bind(C, name="UpdateWindow")
      use standard
      type(ptr), value :: hWnd
    end subroutine

    function GetMessageW(lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax) bind(C, name="GetMessageW")
      use standard
      type(ptr), value :: lpMsg, hWnd
      integer(int32), value :: wMsgFilterMin, wMsgFilterMax
      integer(int32) :: GetMessageW
    end function

    subroutine TranslateMessage(lpMsg) bind(C, name="TranslateMessage")
      use standard
      type(ptr), value :: lpMsg
    end subroutine

    subroutine DispatchMessageW(lpMsg) bind(C, name="DispatchMessageW")
      use standard
      type(ptr), value :: lpMsg
    end subroutine

    function GetSysColorBrush(nIndex) bind(C, name="GetSysColorBrush")
      use standard
      integer(int32), value :: nIndex
      type(ptr) :: GetSysColorBrush
    end function

    function CreateSolidBrush(color) bind(C, name="CreateSolidBrush")
      use standard
      integer(int32), value :: color
      type(ptr) :: CreateSolidBrush
    end function

    function LoadImageW(hInst, lpszName, uType, cxDesired, cyDesired, fuLoad) bind(C, name="LoadImageW")
      use standard
      type(ptr), value :: hInst, lpszName
      integer(int32), value :: uType, cxDesired, cyDesired, fuLoad
      type(ptr) :: LoadImageW
    end function

    function DefWindowProcW(hWnd, Msg, wParam, lParam) bind(C, name="DefWindowProcW")
      use standard
      type(ptr), value :: hWnd
      integer(int32), value :: Msg
      integer(i_ptr), value :: wParam, lParam
      integer(i_ptr) :: DefWindowProcW
    end function

    subroutine PostQuitMessage(nExitCode) bind(C, name="PostQuitMessage")
      use standard
      integer(int32), value :: nExitCode
    end subroutine

    subroutine MoveWindow(hWnd, X, Y, nWidth, nHeight, bRepaint) bind(C, name="MoveWindow")
      use iso_c_binding
      type(c_ptr), value :: hWnd
      integer(c_int32_t), value :: X, Y, nWidth, nHeight
      logical(c_bool), value :: bRepaint
    end subroutine
    
    function GetWindowLongPtrW(hWnd, nIndex) bind(C, name="GetWindowLongPtrW")
      use standard
      type(ptr), value :: hWnd
      integer(int32), value :: nIndex
      integer(i_ptr) :: GetWindowLongPtrW
    end function

    subroutine SetWindowLongPtrW(hWnd, nIndex, dwNewLong) bind(C, name="SetWindowLongPtrW")
      use standard
      type(ptr), value :: hWnd
      integer(int32), value :: nIndex
      integer(i_ptr), value :: dwNewLong
    end subroutine
    
    function GetWindowRect(hWnd, lpRect) bind(C, name="GetWindowRect")
      use standard
      type(ptr), value :: hWnd
      type(ptr) :: lpRect
      logical(bool) :: GetWindowRect
    end function
    
    function GetClientRect(hWnd, lpRect) bind(C, name="GetClientRect")
      use standard
      type(ptr), value   :: hWnd
      type(ptr)          :: lpRect
      integer(int32)     :: GetClientRect
    end function
    
    function InvalidateRect(hWnd, lpRect, bErase) bind(C, name="InvalidateRect")
      use standard
      type(ptr), value :: hWnd        ! HWND
      type(ptr), value :: lpRect      ! LPRECT, can be c_null_ptr
      integer(int32), value :: bErase ! BOOL (1 or 0)
      integer(int32) :: InvalidateRect ! BOOL (0 — error, not 0 — success)
    end function
    
    function LoadCursorW(hInstance, lpCursorName) bind(C, name="LoadCursorW")
      use standard
      type(ptr), value :: hInstance
      type(ptr), value :: lpCursorName
      type(ptr) :: LoadCursorW
    end function    
   
    function BeginPaint(hWnd, lpPaint) bind(C, name="BeginPaint")
      use standard
      type(ptr), value        :: hWnd
      type(ptr)               :: lpPaint
      type(ptr)               :: BeginPaint  ! возвращает HDC
    end function 

      function EndPaint(hWnd, lpPaint) bind(C, name="EndPaint")
        use standard
        type(ptr), value :: hWnd
        type(ptr), value :: lpPaint
        integer(int32)      :: EndPaint    ! возвращает BOOL
      end function

      function FillRect(hdc, lprc, hbr) bind(C, name="FillRect")
        use standard
        type(ptr), value :: hdc     ! HDC
        type(ptr), value :: lprc    ! const RECT*
        type(ptr), value :: hbr     ! HBRUSH
        integer(int32)     :: FillRect
      end function

      function DeleteObject(hObject) bind(C, name="DeleteObject")
        use standard
        type(ptr), value :: hObject
        integer(int32) :: DeleteObject
      end function   
      
      function SendMessageW(hWnd, Msg, wParam, lParam) bind(C, name="SendMessageW")
        use standard
        type(ptr), value :: hWnd
        integer(int32), value :: Msg
        integer(i_ptr), value :: wParam
        integer(i_ptr), value :: lParam
        integer(i_ptr) :: SendMessageW
      end function SendMessageW
      
      function Rectangle(hdc, left, top, right, bottom) bind(C, name="Rectangle")
        use standard
        type(ptr), value :: hdc
        integer(int32), value :: left, top, right, bottom
        integer(int32) :: Rectangle
      end function
  end interface
  
contains

    ! Window message handler (WndProc)
    function WndProc(hWnd, Msg, wParam, lParam) bind(C) result(res)      
      use standard      
      type(AppData), pointer :: appDataInst
      type(ptr), value      :: hWnd
      integer(int32), value   :: Msg
      integer(i_ptr), value :: wParam, lParam
      integer(i_ptr)        :: res 
      integer(int32)          :: width, height, lp32, panelActualWidth
      type(c_ptr) :: appDataPtr    
      integer(i_ptr) :: userData
      type(RECT), target :: rc
      integer(i_ptr) :: newLParam 
      integer(int32) :: resultbool
      

      select case (Msg)
      case (1)  ! WM_CREATE
        appDataPtr = transfer(lParam, c_null_ptr)
        call SetWindowLongPtrW(hWnd, -21, transfer(appDataPtr, 0_i_ptr))
        res = 0
      case (WM_DESTROY)
        ! Window close message — end message loop
        call PostQuitMessage(0)
        res = 0       
          
      case (WM_SIZE) 
        resultbool = GetClientRect(hwnd, c_loc(rc))
        print *, "====>>  ===>> ===>>WndProc WM_SIZE: rc: ", rc%left, rc%top, rc%right, rc%bottom
        print *, "====>>  ===>> ===>>WndProc WM_SIZE GetClientRect HWND = ", transfer(hwnd, 0_i_ptr)

          ! Window resize message
          lp32 = transfer(lParam, int32)
          width  = iand(lp32, 65535)              ! window width = lower 16 bits
          height = iand(ishft(lp32, -16), 65535)  ! window height = upper 16 bits
          print *, "New size: ", width, "x", height

          panelActualWidth = max(80, width / 10)
          userData = GetWindowLongPtrW(hWnd, -21)
          appDataPtr = transfer(userData, appDataPtr)
          call c_f_pointer(appDataPtr, appDataInst)
          
          print *, panelActualWidth, width - panelActualWidth, height
          resultInvalidate = InvalidateRect(hwnd, c_null_ptr, 1)
          call MoveWindow(appDataInst%hwin, panelActualWidth, 0, &
                          width - panelActualWidth , height, .true._c_bool) 
          call UpdateWindow(hwnd)
    
          call MoveWindow(appDataInst%hPanel, 0, 0, panelActualWidth, height, .true._c_bool)  !<== MOVED THIS LINE HERE
          resultbool = SendMessageW(appDataInst%hwin, WM_SIZE, 0_i_ptr, int(ior(width - panelActualWidth, ishft(height, 16)), i_ptr))


          call UpdateWindow(appDataInst%hPanel)
          call UpdateWindow(appDataInst%hwin)
      
          res = 0  ! ← required

      case default
        ! All other messages — default processing
        res = DefWindowProcW(hWnd, Msg, wParam, lParam)
      end select
    end function WndProc
   
    function GraphWndProc(hwnd, uMsg, wParam, lParam) bind(C, name="GraphWndProc") result(retval)
      use standard
      type(ptr), value :: hwnd
      type(PAINTSTRUCT), target :: ps
      type(RECT), target :: rc
      type(ptr) :: hdc
      integer(int32), value :: uMsg
      integer(i_ptr), value :: wParam, lParam
      integer(i_ptr) :: retval
      integer(int32) :: resultbool
      integer(i_ptr) :: resultInvalidate
      integer(i_ptr) :: userData
      type(GraphData), pointer :: pgraphData
      type(ptr) :: pgraphDataPtr
      integer(c_long) :: style
      print *, "GraphWndProc called! hwnd=", transfer(hwnd, 0_i_ptr), " uMsg=", uMsg
      retval = 0
      resultInvalidate = 0

      select case (uMsg)     
      case (1)  ! WM_CREATE
        allocate(pgraphData)
        !pgraphData%hbrush = CreateSolidBrush(MakeARGB(0, 255, 0, 255))  ! Фиолетовая кисть
        pgraphData%hbrush = CreateSolidBrush(int(Z'000000FF', int32))  ! R=255
        pgraphDataPtr = c_loc(pgraphData)
        call SetWindowLongPtrW(hwnd, 0, transfer(pgraphDataPtr, 0_i_ptr))
        print *, "SetWindowLongPtrW done"
  retval = 0
        retval = 0

      case (WM_DESTROY)
          userData = GetWindowLongPtrW(hwnd, 0)
          if (userData /= 0) then
            pgraphDataPtr = transfer(userData, c_null_ptr)
            call c_f_pointer(pgraphDataPtr, pgraphData)
            if (associated(pgraphData)) then
              resultbool = DeleteObject(pgraphData%hbrush)
              deallocate(pgraphData)
            else
              print *, "GraphData not associated in WM_DESTROY"
            end if
          else
            print *, "userData == 0 in WM_DESTROY"
          end if
          retval = 0

      case (WM_SIZE)
        resultbool = GetClientRect(hwnd, c_loc(rc))
        print *, "WM_SIZE: rc: ", rc%left, rc%top, rc%right, rc%bottom
        print *, "WM_SIZE GetClientRect HWND = ", transfer(hwnd, 0_i_ptr)

        resultInvalidate = InvalidateRect(hwnd, c_null_ptr, 1)
          resultbool = GetClientRect(hwnd, c_loc(rc))
          print *, "WM_SIZE: rc: ", rc%left, rc%top, rc%right, rc%bottom
        print *, "WM_SIZE GraphWndProc, result: ", resultInvalidate
        retval = 0

      case (WM_PAINT)
          print *, "sizeof(RECT):", c_sizeof(rc)
          print *, "WM_PAINT GetClientRect HWND = ", transfer(hwnd, 0_i_ptr)
          print *, "SIZEOF(rc) = ", storage_size(rc) / 8
          print *, "WM_PAINT triggered"
          hdc = BeginPaint(hwnd, c_loc(ps))
          resultbool =  Rectangle(hdc, 0, 0, 100, 100)

          print *, "hwnd в WM_PAINT = ", transfer(hwnd, 0_i_ptr)
        
          style = GetWindowLongPtrW(hwnd, -16) ! GWL_STYLE = -16
          print *, "Window style = ", style
          
          print *, "int32 kind = ", kind(0_int32)
          print *, "c_int32_t kind = ", kind(0_c_int32_t)

          
          
          print *, "/________________________________________________________________________________________"
          rc%left = -1111
          rc%top = -2222
          rc%right = -3333
          rc%bottom = -4444          
          resultbool = GetClientRect(hwnd, c_loc(rc))         
          print *, "After GetClientRect call:"
          print *, "Return value: ", resultbool
          print *, "rc: ", rc%left, rc%top, rc%right, rc%bottom
          print *, "sizeof(rc):", c_sizeof(rc)
          print *, "addr rc    :", transfer(c_loc(rc), 0_i_ptr)
          print *, "/________________________________________________________________________________________"
          
            if (resultbool == 0) then
                errorCode = GetLastError()
                print *, "GetClientRect failed! Error code: ", errorCode
            else
                print *, "GetClientRect succeeded. rc: ", rc%left, rc%top, rc%right, rc%bottom
            end if
          print *, "GetClientRect result = ", resultbool
          print *, "rc: ", rc%left, rc%top, rc%right, rc%bottom 
          
          userData = GetWindowLongPtrW(hwnd, 0)
          if (userData /= 0) then
            pgraphDataPtr = transfer(userData, c_null_ptr)
            call c_f_pointer(pgraphDataPtr, pgraphData)
            if (associated(pgraphData)) then
              resultbool = FillRect(hdc, c_loc(rc), pgraphData%hbrush)
            else
              print *, "pgraphData not associated!"
            end if
          else
            print *, "userData == 0, skipping FillRect"
          end if
          print *, "rc: ", rc%left, rc%top, rc%right, rc%bottom
          
          resultbool = EndPaint(hwnd, c_loc(ps))
          retval = 0

      case default
        retval = DefWindowProcW(hwnd, uMsg, wParam, lParam)
      end select
    end function GraphWndProc


end module win_api