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
      type(ptr), value                 :: lpPaint
      type(ptr)               :: BeginPaint  ! возвращает HDC
    end function 

     ! Диагностика: сколько GDI/USER-объектов у процесса
     function GetGuiResources(hProcess, uiFlags) bind(C,name="GetGuiResources")
       use standard
       type(ptr), value :: hProcess
       integer(int32), value :: uiFlags
       integer(int32) :: GetGuiResources
     end function

     ! Текущий процесс (для GetGuiResources)
     function GetCurrentProcess() bind(C,name="GetCurrentProcess")
       use standard
       type(ptr) :: GetCurrentProcess
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
    
    ! --- GDI helpers for double-buffering and lines ---
    function CreateCompatibleDC(hdc) bind(C, name="CreateCompatibleDC")
      use standard
      type(ptr), value :: hdc
      type(ptr) :: CreateCompatibleDC
    end function

    function CreateCompatibleBitmap(hdc, cx, cy) bind(C, name="CreateCompatibleBitmap")
      use standard
      type(ptr), value :: hdc
      integer(int32), value :: cx, cy
      type(ptr) :: CreateCompatibleBitmap
    end function

    function SelectObject(hdc, hgdiobj) bind(C, name="SelectObject")
      use standard
      type(ptr), value :: hdc, hgdiobj
      type(ptr) :: SelectObject
    end function

    function DeleteDC(hdc) bind(C, name="DeleteDC")
      use standard
      type(ptr), value :: hdc
      integer(int32) :: DeleteDC
    end function

    function BitBlt(hdc, x, y, cx, cy, hdcSrc, x1, y1, rop) bind(C, name="BitBlt")
      use standard
      type(ptr), value :: hdc, hdcSrc
      integer(int32), value :: x, y, cx, cy, x1, y1, rop
      integer(int32) :: BitBlt
    end function

    function MoveToEx(hdc, x, y, lpPoint) bind(C, name="MoveToEx")
      use standard
      type(ptr), value :: hdc, lpPoint
      integer(int32), value :: x, y
      integer(int32) :: MoveToEx
    end function

    function LineTo(hdc, x, y) bind(C, name="LineTo")
      use standard
      type(ptr), value :: hdc
      integer(int32), value :: x, y
      integer(int32) :: LineTo
    end function

    function CreatePen(fnPenStyle, width, color) bind(C, name="CreatePen")
      use standard
      integer(int32), value :: fnPenStyle, width, color
      type(ptr) :: CreatePen
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
        integer(int32) :: ix, iy, k, N
        real(double) :: baseX, baseY, step, rad, w_base       
        integer(int32) :: ok   ! <- add this near other locals
        type(ptr)     :: tmpSel   ! для возврата SelectObject
        integer(int32) :: cx, cy, r, r2
        integer(int32) :: x1, y1, dx1, dy1, steps1, i1, px1, py1
        integer(int32) :: x2, y2, dx2, dy2, steps2, i2, px2, py2
        real(double)  :: fx1, fy1, stepx1, stepy1
        real(double)  :: fx2, fy2, stepx2, stepy2
        type(ptr)     :: hBrush1, hBrush2
        integer(int32):: pix
        integer(int32) :: gdiCnt

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
           ! init backbuffer by current client size
            ok = GetClientRect(hwnd, c_loc(rc))
            st%backW = rc%right - rc%left
            st%backH = rc%bottom - rc%top

            st%hMemDC = CreateCompatibleDC(nullptr)
           
            st%nx = 10; st%ny = 10
            N = st%nx * st%ny

            baseX = 40.0d0
            baseY = 40.0d0
            step  = 40.0d0
            rad   = 20.0d0     
            w_base = 2.0d0 * 3.141592653589793d0 / 4.0d0    ! 1 rev / 4 s

            allocate(st%clocks(N))
            allocate(st%omega_fast(N))
            allocate(st%omega_slow(N))

            do iy = 0, st%ny-1
              do ix = 0, st%nx-1
                k = iy*st%nx + ix + 1
                st%clocks(k)%cx = baseX + step*real(ix, double)
                st%clocks(k)%cy = baseY + step*real(iy, double)
                st%clocks(k)%rx = rad
                st%clocks(k)%ry = rad
                st%clocks(k)%theta  = 0.0d0
                st%clocks(k)%theta2 = 0.0d0

                st%omega_fast(k) = w_base          ! per-clock fast speed
                st%omega_slow(k) = w_base/60.0d0   ! per-clock slow speed
              end do
            end do

            st%hbg_brush = CreateSolidBrush(MakeARGB(0, 102, 0, 51))  ! background brush

            st%w = 6; st%h = 6
            st%theta  = 0.0d0
            st%theta2 = 0.0d0
            st%omega  = 2.0d0 * 3.141592653589793d0 / 4.0d0

            ! fixed tiny clock in the top-left corner:
            st%cx = 40.0d0          ! center X (pixels)
            st%cy = 40.0d0          ! center Y
            st%rx = 20.0d0          ! radius X 
            st%ry = 20.0d0          ! radius Y

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
  
        if (associated(st%clocks))     deallocate(st%clocks)
        if (associated(st%omega_fast)) deallocate(st%omega_fast)
        if (associated(st%omega_slow)) deallocate(st%omega_slow)
        if (c_associated(st%hbg_brush)) ignore = DeleteObject(st%hbg_brush)

        if (c_associated(st%hMemDC)) then
          if (c_associated(st%hBmp)) then
            tmpSel = SelectObject(st%hMemDC, st%hBmpOld)
            ignore = DeleteObject(st%hBmp)
          end if
          ignore = DeleteDC(st%hMemDC)
        end if
        deallocate(st)
        retval = 0
          
      case (WM_TIMER)
          if (wParam == int(TIMER_ID, i_ptr)) then
            p = transfer(GetWindowLongPtrW(hwnd, GWLP_USERDATA), nullptr)
            if (c_associated(p)) then
              call c_f_pointer(p, st)
              if (associated(st)) then
                dt = 0.016d0
                N  = size(st%clocks)

                do k = 1, N
                  st%clocks(k)%theta  = st%clocks(k)%theta  + st%omega_fast(k)*dt
                  if (st%clocks(k)%theta  >= 6.283185307179586d0) &
                      st%clocks(k)%theta  = st%clocks(k)%theta  - 6.283185307179586d0

                  st%clocks(k)%theta2 = st%clocks(k)%theta2 + st%omega_slow(k)*dt
                  if (st%clocks(k)%theta2 >= 6.283185307179586d0) &
                      st%clocks(k)%theta2 = st%clocks(k)%theta2 - 6.283185307179586d0
                end do

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
              st%backW = rc%right - rc%left
              st%backH = rc%bottom - rc%top
              ! reallocate bitmap
              if (c_associated(st%hBmp)) then                
                tmpSel = SelectObject(st%hMemDC, st%hBmpOld)
                ignore = DeleteObject(st%hBmp)
              end if
              st%hBmp   = CreateCompatibleBitmap(BeginPaint(hwnd, c_loc(ps)), st%backW, st%backH)
              ignore     = EndPaint(hwnd, c_loc(ps))
              st%hBmpOld = SelectObject(st%hMemDC, st%hBmp)
            end if
          end if
          retval = 0



      case (WM_PAINT)
          hdc = BeginPaint(hwnd, c_loc(ps))
          ok  = GetClientRect(hwnd, c_loc(rc))

          p = transfer(GetWindowLongPtrW(hwnd, GWLP_USERDATA), nullptr)
          if (c_associated(p)) then
            call c_f_pointer(p, st)
            if (associated(st)) then
              ! 1) clear backbuffer (fill background)
              if (c_associated(st%hbg_brush)) ok = FillRect(st%hMemDC, c_loc(rc), st%hbg_brush)

              ! 2) draw all clocks into backbuffer with simple GDI lines
              N = size(st%clocks)
              do k = 1, N
               
                cx = int(nint(st%clocks(k)%cx), int32)
                cy = int(nint(st%clocks(k)%cy), int32)

                ! fast hand endpoint (ellipse)
                x1 = cx + int( nint( st%clocks(k)%rx * cos(st%clocks(k)%theta) ), int32 )
                y1 = cy + int( nint( st%clocks(k)%ry * sin(st%clocks(k)%theta) ), int32 )

                call DrawHand(st%hMemDC, cx, cy, x1, y1, MakeARGB(0,255,215,0), 1)  ! gold

                ! slow hand endpoint (60% of radius, circle)
                r2 = int( 0.60d0 * nint(min(st%clocks(k)%rx, st%clocks(k)%ry)), int32 )
                x2 = cx + int( nint( real(r2,double) * cos(st%clocks(k)%theta2) ), int32 )
                y2 = cy + int( nint( real(r2,double) * sin(st%clocks(k)%theta2) ), int32 )

                call DrawHand(st%hMemDC, cx, cy, x2, y2, MakeARGB(0,  0,180,255), 1) ! cyan/blue
              end do

              ! 3) blit backbuffer -> screen
              ok = BitBlt(hdc, 0, 0, st%backW, st%backH, st%hMemDC, 0, 0, SRCCOPY)
            end if
          end if

          ok  = EndPaint(hwnd, c_loc(ps))
          gdiCnt = GetGuiResources(GetCurrentProcess(), 0)  ! GDI
          !print *,gdiCnt
          retval = 0


      case default
        retval = DefWindowProcW(hwnd, uMsg, wParam, lParam)
      end select      
      
        contains
            subroutine DrawHand(hdc, cx, cy, x, y, colorBGR, width)
              ! Draw a line (clock hand) with a solid pen
              use standard
              type(ptr), value :: hdc
              integer(int32), value :: cx, cy, x, y, colorBGR, width
              type(ptr)    :: hPen, hOld, tmp
              integer(int32) :: ok

              ! create pen and select
              hPen = CreatePen(PS_SOLID, width, colorBGR)
              hOld = SelectObject(hdc, hPen)          ! <- returns previous HGDIOBJ (ptr)

              ok  = MoveToEx(hdc, cx, cy, nullptr)
              ok  = LineTo(hdc, x, y)

              ! restore previous object; swallow return into tmp (also ptr)
              tmp = SelectObject(hdc, hOld)

              ok  = DeleteObject(hPen)                ! BOOL -> int32
            end subroutine DrawHand

      
    end function GraphWndProc


end module win_api