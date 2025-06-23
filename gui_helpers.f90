module gui_helpers
  use win_api
  use win_types
  use standard
  implicit none
contains

    subroutine create_main_window(hwnd, hInstance, appDataPtr, hBrush, wcx, regResult, &
                                classNameW, windowTitleW, iconPathW, cursorPathW)
        type(ptr), intent(out)        :: hwnd
        type(ptr), intent(in)         :: hInstance
        type(c_ptr), intent(in)       :: appDataPtr
        type(ptr), intent(in)         :: hBrush
        type(WNDCLASSEX), intent(out), target :: wcx
        character(kind=char), intent(in), target :: classNameW(:), windowTitleW(:)
        integer(int), intent(out)     :: regResult
        character(kind=char), intent(in), target :: iconPathW(:), cursorPathW(:)


        wcx%cbSize             = c_sizeof(wcx)
        wcx%style              = 0
        wcx%lpfnWndProc        = c_funloc(WndProc)
        wcx%cbClsExtra         = 0
        wcx%cbWndExtra         = 0
        wcx%hInstance          = hInstance
        wcx%hIcon              = LoadImageW(nullptr, c_loc(iconPathW(1)), IMAGE_ICON, 0, 0, LR_LOADFROMFILE)
        wcx%hCursor            = LoadImageW(nullptr, c_loc(cursorPathW(1)), IMAGE_ICON, 0, 0, LR_LOADFROMFILE)
        wcx%hbrBackground      = hBrush
        wcx%lpszMenuName       = nullptr
        wcx%lpszClassName      = c_loc(classNameW(1))
        wcx%hIconSm            = wcx%hIcon

        regResult = RegisterClassExW(c_loc(wcx))

        if (regResult /= 0) then
            print *, "Main class registered successfully."
        else
            print *, "Error registering the main class, code:", GetLastError()
        end if

        hwnd = CreateWindowExW(0, c_loc(classNameW(1)), c_loc(windowTitleW(1)), &
                    WS_OVERLAPPEDWINDOW, 100, 100, 800, 600, nullptr, nullptr, hInstance, appDataPtr)
    end subroutine create_main_window

    subroutine create_panel_window(hPanel, hwndParent, hInstance, hBrush, wcxPanel, regResult, panelClassW, width, height)
      type(ptr), intent(out)        :: hPanel
      type(ptr), intent(in)         :: hwndParent
      type(ptr), intent(in)         :: hInstance
      type(ptr), intent(in)         :: hBrush
      type(WNDCLASSEX), intent(out), target :: wcxPanel
      integer(int), intent(out)     :: regResult
      character(kind=char), intent(in), target :: panelClassW(:)
      integer(int), intent(in)      :: width, height

      wcxPanel%cbSize             = c_sizeof(wcxPanel)
      wcxPanel%style              = 0
      wcxPanel%lpfnWndProc        = c_funloc(WndProc)
      wcxPanel%cbClsExtra         = 0
      wcxPanel%cbWndExtra         = 0
      wcxPanel%hInstance          = hInstance
      wcxPanel%hIcon              = nullptr
      wcxPanel%hCursor            = nullptr
      wcxPanel%hbrBackground      = hBrush
      wcxPanel%lpszMenuName       = nullptr
      wcxPanel%lpszClassName      = c_loc(panelClassW(1))
      wcxPanel%hIconSm            = nullptr

      regResult = RegisterClassExW(c_loc(wcxPanel))
      if (regResult /= 0) then
        print *, "Panel class registered successfully."
      else
        print *, "Error registering the panel class, code:", GetLastError()
      end if

      hPanel = CreateWindowExW(0, c_loc(panelClassW(1)), nullptr, &
               WS_CHILD_VISIBLE, 0, 0, width, height, hwndParent, nullptr, hInstance, nullptr)
    end subroutine create_panel_window

    
    subroutine create_button(hButton, hParent, hInstance, buttonTextW, classButtonW, idButton, regResult)
      type(ptr), intent(out)             :: hButton
      type(ptr), intent(in)              :: hParent, hInstance
      character(kind=char), intent(in), target :: buttonTextW(:), classButtonW(:)
      integer(i_ptr), intent(in)         :: idButton
      integer(int), intent(out)          :: regResult

      type(c_ptr) :: hMenuAsPtr

      hMenuAsPtr = transfer(idButton, hMenuAsPtr)

      hButton = CreateWindowExW(0, c_loc(classButtonW(1)), c_loc(buttonTextW(1)), &
           WS_CHILD_VISIBLE + BS_DEFPUSHBUTTON, 2, 2, 76, 26, hParent, hMenuAsPtr, hInstance, nullptr)

      if (.not. c_associated(hButton)) then
        regResult = GetLastError()
        print *, "The button has not been created! Error code:", regResult
      else
        regResult = 0
        print *, "Button created successfully."
      end if
    end subroutine create_button

end module gui_helpers