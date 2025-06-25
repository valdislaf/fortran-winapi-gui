! Главная программа
program WinMain
  use win_api
  use gui_helpers

  ! --- Дескрипторы и структуры WinAPI ---
  type(WNDCLASSEX), target :: wcx        ! Класс главного окна
  type(WNDCLASSEX), target :: wcxPanel   ! Класс панели
  type(MSG_T), target       :: msg_inst  ! Структура для обработки сообщений
  type(ptr)                 :: hwnd       ! Главное окно
  type(ptr)                 :: hInstance  ! Дескриптор приложения
  type(ptr)                 :: hBrush     ! Фоновая кисть для окна
  type(ptr)                 :: hPanelBrush! Фоновая кисть для панели
  type(ptr)                 :: hButton    ! Кнопка (временно переиспользуется)

  ! --- Данные пользователя для окна ---
  type(AppData), target :: appDataInst
  type(c_ptr)           :: appDataPtr

  ! --- Строки WinAPI (именованные ресурсы) ---
  character(kind=char), allocatable, target :: windowTitleW(:)
  character(kind=char), allocatable, target :: classNameW(:)
  character(kind=char), allocatable, target :: panelClassW(:)
  character(kind=char), allocatable, target :: classButtonW(:)
  character(kind=char), allocatable, target :: iconPathW(:)
  character(kind=char), allocatable, target :: cursorPathW(:)
  character(kind=char), allocatable, target :: buttonTextW(:)
  character(kind=char), allocatable, target :: buttonTextW2(:)

  ! --- Прочие параметры ---
  integer(int)              :: regResult             ! Результат регистрации класса окна
  integer(int), parameter   :: panelWidth = 800 / 10 ! Ширина панели

  ! --- Инициализация строк ---
  allocate(cursorPathW(0))  ! ← аналог "инициализации значением по умолчанию"
  cursorPathW    = to_wide_null_terminated("cross.ico")
  iconPathW      = to_wide_null_terminated("favicon.ico")
  classNameW     = to_wide_null_terminated("My window class")
  windowTitleW   = to_wide_null_terminated("Fortran Window")
  panelClassW    = to_wide_null_terminated("PanelClass")
  buttonTextW    = to_wide_null_terminated("Click me")
  buttonTextW2   = to_wide_null_terminated("Click me2")
  classButtonW   = to_wide_null_terminated("Button")

  ! --- Инициализация дескрипторов и кистей ---
  hInstance = nullptr
  appDataInst%hPanel = nullptr
  appDataPtr = c_loc(appDataInst)
  hBrush = CreateSolidBrush(MakeARGB(0, 50, 30, 10))
  hPanelBrush = CreateSolidBrush(MakeARGB(0, 40, 20, 0))

  ! --- Создание главного окна ---
  call create_main_window(hwnd, hInstance, appDataPtr, hBrush, wcx, regResult, &
                          classNameW, windowTitleW, iconPathW, cursorPathW)
  call ShowWindow(hwnd, SW_SHOW)
  call UpdateWindow(hwnd)

  ! --- Создание панели ---
  call create_panel_window(appDataInst%hPanel, hwnd, hInstance, hPanelBrush, &
                           wcxPanel, regResult, panelClassW, panelWidth, 600)
  call ShowWindow(appDataInst%hPanel, SW_SHOW)
  call UpdateWindow(appDataInst%hPanel)

  ! --- Создание кнопок ---
  call create_button(hButton, appDataInst%hPanel, hInstance, buttonTextW, &
                     classButtonW, ID_BUTTON1, regResult, 2, 2, 76, 26)
  call ShowWindow(hButton, SW_SHOW)
  call UpdateWindow(hButton)

  call create_button(hButton, appDataInst%hPanel, hInstance, buttonTextW2, &
                     classButtonW, ID_BUTTON2, regResult, 2, 28, 76, 26)
  call ShowWindow(hButton, SW_SHOW)
  call UpdateWindow(hButton)

  ! --- Установка пользовательских данных окна ---
  ! Важно вызвать после создания всех элементов, чтобы структура AppData была актуальна.
  call SetWindowLongPtrW(hwnd, -21, transfer(c_loc(appDataInst), 0_i_ptr))

  ! --- Цикл обработки сообщений ---
  do while (GetMessageW(c_loc(msg_inst), nullptr, 0, 0) > 0)
    call TranslateMessage(c_loc(msg_inst))
    call DispatchMessageW(c_loc(msg_inst))
  end do

end program WinMain
