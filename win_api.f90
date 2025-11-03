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

    subroutine MoveWindow(hWnd, X, Y, nW, nH, bRepaint) bind(C, name="MoveWindow")
      use standard
      type(ptr), value        :: hWnd
      integer(int32), value :: X, Y, nW, nH
      integer(int32), value :: bRepaint
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
      use iso_c_binding, only: c_ptr, c_int32_t
      type(c_ptr), value :: hWnd     ! HWND
      type(c_ptr), value :: lpRect   ! LPRECT
      integer(c_int32_t) :: GetClientRect
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
      
    ! Таймеры WinAPI
    function SetTimer(hWnd, nIDEvent, uElapse, lpTimerFunc) bind(C, name="SetTimer")
      use standard
      type(ptr), value :: hWnd
      integer(int32), value :: nIDEvent
      integer(int32), value :: uElapse
      type(ptr), value :: lpTimerFunc
      integer(int32) :: SetTimer
    end function

    function KillTimer(hWnd, nIDEvent) bind(C, name="KillTimer")
      use standard
      type(ptr), value :: hWnd
      integer(int32), value :: nIDEvent
      integer(int32) :: KillTimer
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
      integer(int32) :: resultbool
      integer(i_ptr) :: resultInvalidate
      
      !logical(c_bool) :: settrue
      
      select case (Msg)
      case (WM_CREATE) 
       
        appDataPtr = transfer(lParam, c_null_ptr)
        call SetWindowLongPtrW(hWnd, -21, transfer(appDataPtr, 0_i_ptr))
        res = 0
      case (WM_DESTROY)
        ! Удалить таймер
        resultbool =  KillTimer(hWnd, TIMER_ID)
        call PostQuitMessage(0) 
        res = 0
      case (WM_SIZE) 
          ! Window resize message
          lp32 = transfer(lParam, int32)
          width  = iand(lp32, 65535)              ! window width = lower 16 bits
          height = iand(ishft(lp32, -16), 65535)  ! window height = upper 16 bits
          !settrue = .true._c_bool

          panelActualWidth = max(80, width / 10)
          userData = GetWindowLongPtrW(hWnd, -21)
          appDataPtr = transfer(userData, appDataPtr)
          call c_f_pointer(appDataPtr, appDataInst)
          
          resultInvalidate = InvalidateRect(hwnd, nullptr, 1)
          resultInvalidate = InvalidateRect(appDataInst%hwin, nullptr, 1)
          resultInvalidate = InvalidateRect(appDataInst%hPanel, nullptr, 1)
          call MoveWindow(appDataInst%hwin, panelActualWidth, 0, &
                         width - panelActualWidth , height, 1)
          call MoveWindow(appDataInst%hPanel, 0, 0, &
                         panelActualWidth , height, 1)
          
          
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
      
      !type(RECT), target :: rcc
      type(ptr) :: hdc
      integer(int32), value :: uMsg
      integer(i_ptr), value :: wParam, lParam
      integer(i_ptr) :: retval
      integer(int32) :: resultbool
      integer(i_ptr) :: resultInvalidate
      integer(i_ptr) :: userData     
      type(RECT), target :: rcSmall
      type(ptr) :: hRedBrush
      type(AppState), pointer :: st        ! <-- нужен POINTER
      type(ptr) :: p                       ! <-- c_ptr
      real(double) :: dt
                  
      !integer(c_long) :: style
      !!!!!!!!!!!!!print *, "GraphWndProc called! hwnd=", transfer(hwnd, 0_i_ptr), " uMsg=", uMsg
      retval = 0
      resultInvalidate = 0
      
      select case (uMsg)     
      case (WM_CREATE) 
        ! Установить таймер
          resultbool = SetTimer(hwnd, TIMER_ID, 16_int32, nullptr)   ! ~60 FPS

          block
            type(AppState), pointer :: st
            type(ptr) :: p
            allocate(st)

            st%hbg_brush = CreateSolidBrush(MakeARGB(0, 102, 0, 51))  ! background brush

            st%w = 6; st%h = 6
            st%theta  = 0.0d0
            st%theta2 = 0.0d0
            st%omega  = 2.0d0 * 3.141592653589793d0 / 4.0d0  ! one revolution per 4s

            ok = GetClientRect(hwnd, c_loc(rc))
            st%cx = 0.5d0 * real(rc%right,  double)
            st%cy = 0.5d0 * real(rc%bottom, double)
            st%rx = 0.4d0 * real(rc%right,  double)
            st%ry = 0.4d0 * real(rc%bottom, double)

            p = c_loc(st)
            call SetWindowLongPtrW(hwnd, GWLP_USERDATA, transfer(p, 0_i_ptr))
          end block

          retval = 0

      case (WM_DESTROY)
          resultbool = KillTimer(hwnd, TIMER_ID)

          p = transfer(GetWindowLongPtrW(hwnd, GWLP_USERDATA), nullptr)
          if (c_associated(p)) then
            call c_f_pointer(p, st)
            if (associated(st)) then
              if (c_associated(st%hbg_brush)) resultbool = DeleteObject(st%hbg_brush)
              deallocate(st)
            end if
            call SetWindowLongPtrW(hwnd, GWLP_USERDATA, 0_i_ptr)
          end if
         
          retval = 0
          
        case (WM_TIMER)
              if (wParam == int(TIMER_ID, i_ptr)) then
                p = transfer(GetWindowLongPtrW(hwnd, GWLP_USERDATA), nullptr)
                if (c_associated(p)) then
                  call c_f_pointer(p, st)
                  if (associated(st)) then
                    dt = 0.016d0

                    ! fast hand
                    st%theta = st%theta + st%omega * dt
                    if (st%theta >= 6.283185307179586d0) st%theta = st%theta - 6.283185307179586d0

                    ! slow hand = 60x slower
                    st%theta2 = st%theta2 + (st%omega/60.0d0) * dt
                    if (st%theta2 >= 6.283185307179586d0) st%theta2 = st%theta2 - 6.283185307179586d0

                    ! recompute fast dot (screen Y goes down -> same sign as in your paint)
                    st%x = int( nint( st%cx + st%rx * cos(st%theta) ), int32 )
                    st%y = int( nint( st%cy + st%ry * sin(st%theta) ), int32 )

                    ok = InvalidateRect(hwnd, nullptr, 0_int32)
                  end if
                end if
              end if
            retval = 0


      case (WM_SIZE)
          p = transfer(GetWindowLongPtrW(hwnd, GWLP_USERDATA), nullptr)
          if (c_associated(p)) then
            call c_f_pointer(p, st)
            if (associated(st)) then
              ok = GetClientRect(hwnd, c_loc(rc))
              st%cx = 0.5d0 * real(rc%right, kind=double)
              st%cy = 0.5d0 * real(rc%bottom, kind=double)
              st%rx = 0.4d0 * real(rc%right, kind=double)
              st%ry = 0.4d0 * real(rc%bottom, kind=double)
            end if
          end if          
          retval = 0


      case (WM_PAINT)
          hdc = BeginPaint(hwnd, c_loc(ps))
          ok  = GetClientRect(hwnd, c_loc(rc))

          ! Pull AppState
          p = transfer(GetWindowLongPtrW(hwnd, GWLP_USERDATA), nullptr)
          if (c_associated(p)) then
            call c_f_pointer(p, st)
            if (associated(st)) then
              ! 1) background
              if (c_associated(st%hbg_brush)) ok = FillRect(hdc, c_loc(rc), st%hbg_brush)

              ! 2) moving "pixel" (scale 10 if you want)
            !  rcSmall%left   = st%x
            !  rcSmall%top    = st%y
            !  rcSmall%right  = st%x + st%w * 10
            !  rcSmall%bottom = st%y + st%h * 10
            !  hRedBrush      = CreateSolidBrush(MakeARGB(0, 0, 0, 255))
            !  ok             = FillRect(hdc, c_loc(rcSmall), hRedBrush)
            !  ok             = DeleteObject(hRedBrush)

              ! 3) first hand: DDA from center -> (st%x, st%y)
              block
                integer(int32) :: cx, cy, wClient, hClient
                integer(int32) :: dxl, dyl, steps, i, px, py, pix
                real(double) :: fx, fy, stepx, stepy
                type(ptr) :: hBrushLine

                wClient = rc%right - rc%left
                hClient = rc%bottom - rc%top
                cx = rc%left + wClient/2
                cy = rc%top  + hClient/2

                dxl = st%x - cx
                dyl = st%y - cy
                steps = max(1_int32, max(abs(dxl), abs(dyl)))

                fx = real(cx, double);  fy = real(cy, double)
                stepx = real(dxl, double) / real(steps, double)
                stepy = real(dyl, double) / real(steps, double)

                hBrushLine = CreateSolidBrush(MakeARGB(0, 255, 215, 0))  ! golden/yellow
                pix = 2

                do i = 1, steps
                  px = int(nint(fx), int32)
                  py = int(nint(fy), int32)
                  rcSmall%left = px; rcSmall%top = py
                  rcSmall%right = px + pix; rcSmall%bottom = py + pix
                  ok = FillRect(hdc, c_loc(rcSmall), hBrushLine)
                  fx = fx + stepx; fy = fy + stepy
                end do

                ok = DeleteObject(hBrushLine)
              end block

              ! 4) second, slower hand: same direction, omega/60
              block
                integer(int32) :: cx, cy, wClient, hClient, r
                integer(int32) :: x2, y2, dx2, dy2, steps2, i2, px2, py2, pix2
                real(double) :: fx2, fy2, stepx2, stepy2
                type(ptr) :: hBrushLine2

                wClient = rc%right - rc%left
                hClient = rc%bottom - rc%top
                cx = rc%left + wClient/2
                cy = rc%top  + hClient/2
                r  = min(wClient, hClient)/2 - 8

                x2 = cx + int( nint( real(r, double) * cos(st%theta2) ), int32 )
                y2 = cy + int( nint( real(r, double) * sin(st%theta2) ), int32 )  ! same sign as fast

                dx2 = x2 - cx
                dy2 = y2 - cy
                steps2 = max(1_int32, max(abs(dx2), abs(dy2)))

                fx2 = real(cx, double); fy2 = real(cy, double)
                stepx2 = real(dx2, double) / real(steps2, double)
                stepy2 = real(dy2, double) / real(steps2, double)

                hBrushLine2 = CreateSolidBrush(MakeARGB(0, 0, 180, 255))  ! cyan/blue
                pix2 = 2

                do i2 = 1, steps2
                  px2 = int(nint(fx2), int32)
                  py2 = int(nint(fy2), int32)
                  rcSmall%left = px2; rcSmall%top = py2
                  rcSmall%right = px2 + pix2; rcSmall%bottom = py2 + pix2
                  ok = FillRect(hdc, c_loc(rcSmall), hBrushLine2)
                  fx2 = fx2 + stepx2; fy2 = fy2 + stepy2
                end do

                ok = DeleteObject(hBrushLine2)
              end block

            end if
          end if

          ok = EndPaint(hwnd, c_loc(ps))
          retval = 0

      case default
        retval = DefWindowProcW(hwnd, uMsg, wParam, lParam)
      end select
    end function GraphWndProc


end module win_api