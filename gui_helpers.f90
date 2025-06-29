module gui_helpers
  use win_api
  use win_types
  use standard
contains

  ! Создаёт главное окно приложения и регистрирует его класс.
  subroutine create_main_window(hwnd, hInstance, appDataPtr, hBrush, wcx, regResult, &
                                classNameW, windowTitleW, iconPathW, cursorPathW, width, height)
    type(ptr), intent(out)        :: hwnd             ! Возвращаемый дескриптор главного окна
    type(ptr), intent(in)         :: hInstance        ! Дескриптор экземпляра приложения
    type(c_ptr), intent(in)       :: appDataPtr       ! Указатель на пользовательские данные (AppData)
    type(ptr), intent(in)         :: hBrush           ! Кисть фона окна
    type(WNDCLASSEX), intent(out), target :: wcx      ! Структура класса окна
    character(kind=char), intent(in), target :: classNameW(:), windowTitleW(:)
    character(kind=char), intent(in), target :: iconPathW(:), cursorPathW(:)
    integer(int32), intent(out)     :: regResult        ! Код регистрации окна
    integer(int32), intent(in)      :: width, height    ! Размеры панели
    
    ! Настройка структуры класса окна
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

    ! Регистрация класса окна
    regResult = RegisterClassExW(c_loc(wcx))
    if (regResult /= 0) then
      print *, "Main class registered successfully."
    else
      print *, "Error registering the main class, code:", GetLastError()
    end if

    ! Создание главного окна
    hwnd = CreateWindowExW(0, c_loc(classNameW(1)), c_loc(windowTitleW(1)), &
            WS_OVERLAPPEDWINDOW, 100, 100, width, height, nullptr, nullptr, hInstance, appDataPtr)
  end subroutine create_main_window

  ! Регистрирует и создаёт дочернюю панель внутри главного окна
  subroutine create_panel_window(hPanel, hwndParent, hInstance, hBrush, wcxPanel, regResult, panelClassW, width, height)
    type(ptr), intent(out)        :: hPanel           ! Дескриптор панели
    type(ptr), intent(in)         :: hwndParent       ! Родительское окно
    type(ptr), intent(in)         :: hInstance        ! Дескриптор приложения
    type(ptr), intent(in)         :: hBrush           ! Кисть фона
    type(WNDCLASSEX), intent(out), target :: wcxPanel ! Структура класса панели
    character(kind=char), intent(in), target :: panelClassW(:)
    integer(int32), intent(out)     :: regResult        ! Код регистрации
    integer(int32), intent(in)      :: width, height    ! Размеры панели
    integer(int32), parameter :: ID_ARROW = 32512 ! Идентификатор курсора (стрелка)
    type(ptr), parameter :: IDC_ARROW = transfer(ID_ARROW, nullptr)
    
    ! Настройка класса панели
    wcxPanel%cbSize             = c_sizeof(wcxPanel)
    wcxPanel%style              = 0
    wcxPanel%lpfnWndProc        = c_funloc(WndProc)
    wcxPanel%cbClsExtra         = 0
    wcxPanel%cbWndExtra         = 0
    wcxPanel%hInstance          = hInstance
    wcxPanel%hIcon              = nullptr
    wcxPanel%hCursor            = LoadCursorW(nullptr, IDC_ARROW)
    wcxPanel%hbrBackground      = hBrush
    wcxPanel%lpszMenuName       = nullptr
    wcxPanel%lpszClassName      = c_loc(panelClassW(1))
    wcxPanel%hIconSm            = nullptr

    ! Регистрация класса панели
    regResult = RegisterClassExW(c_loc(wcxPanel))
    if (regResult /= 0) then
      print *, "Panel class registered successfully."
    else
      print *, "Error registering the panel class, code:", GetLastError()
    end if

    ! Создание панели
    hPanel = CreateWindowExW(0, c_loc(panelClassW(1)), nullptr, &
             WS_CHILD_VISIBLE, 0, 0, width, height, hwndParent, nullptr, hInstance, nullptr)
  end subroutine create_panel_window

  ! Регистрирует и создаёт графический виджет для отрисовки 2D-графиков
  subroutine create_graph_window(hGraph, hwndParent, hInstance, hPanelBrush, wcxGraph, regResult, graphClassW, cursorPathW, x, y, width, height)
    type(ptr), intent(out)        :: hGraph           ! Дескриптор графического окна
    type(ptr), intent(in)         :: hwndParent, hInstance    
    type(ptr), intent(in)         :: hPanelBrush           ! Кисть фона
    type(WNDCLASSEX), intent(out), target :: wcxGraph ! Структура класса графа
    character(kind=char), intent(in), target :: graphClassW(:)
    integer(int32), intent(out)     :: regResult
    integer(int32), intent(in)      :: x, y, width, height
    character(kind=char), intent(in), target ::  cursorPathW(:)  !Курсор 
    
    wcxGraph%cbSize             = c_sizeof(wcxGraph)
    wcxGraph%style              = 0
    wcxGraph%lpfnWndProc        = c_funloc(GraphWndProc)   ! Отдельная WndProc
    wcxGraph%cbClsExtra         = 0
    wcxGraph%cbWndExtra         = 0
    wcxGraph%hInstance          = hInstance
    wcxGraph%hIcon              = nullptr
    wcxGraph%hCursor            = LoadImageW(nullptr, c_loc(cursorPathW(1)), IMAGE_ICON, 0, 0, LR_LOADFROMFILE)
    wcxGraph%hbrBackground      = hPanelBrush
    wcxGraph%lpszMenuName       = nullptr
    wcxGraph%lpszClassName      = c_loc(graphClassW(1))
    wcxGraph%hIconSm            = nullptr

    regResult = RegisterClassExW(c_loc(wcxGraph))
    if (regResult /= 0) then
      print *, "Graph class registered successfully."
    else
      print *, "Error registering graph class, code:", GetLastError()
    end if

    hGraph = CreateWindowExW(0, c_loc(graphClassW(1)), nullptr, &
             WS_CHILD_VISIBLE, x, y, width, height, hwndParent, nullptr, hInstance, nullptr)
  end subroutine create_graph_window
  
  ! Создаёт кнопку в указанном родительском окне
  subroutine create_button(hButton, hParent, hInstance, buttonTextW, classButtonW, idButton, regResult, x, y, width, height)
    type(ptr), intent(out)                   :: hButton          ! Дескриптор кнопки
    type(ptr), intent(in)                    :: hParent, hInstance
    character(kind=char), intent(in), target :: buttonTextW(:), classButtonW(:)
    integer(i_ptr), intent(in)               :: idButton
    integer(int32), intent(out)                :: regResult
    integer(int32), intent(in)                 :: x, y             ! Координаты кнопки 
    integer(int32), intent(in)                 :: width, height    ! Размеры кнопки 
    type(c_ptr) :: hMenuAsPtr

    ! Преобразование идентификатора кнопки к указателю
    hMenuAsPtr = transfer(idButton, hMenuAsPtr)

    ! Создание кнопки
    hButton = CreateWindowExW(0, c_loc(classButtonW(1)), c_loc(buttonTextW(1)), &
         WS_CHILD_VISIBLE + BS_DEFPUSHBUTTON, x, y, width, height, hParent, hMenuAsPtr, hInstance, nullptr)

    ! Проверка успеха
    if (.not. c_associated(hButton)) then
      regResult = GetLastError()
      print *, "The button has not been created! Error code:", regResult
    else
      regResult = 0
      print *, "Button created successfully."
    end if
  end subroutine create_button

end module gui_helpers
