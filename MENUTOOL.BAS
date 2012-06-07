'$INCLUDE: 'LINES.BI'

DECLARE SUB LineMaker (n%, l AS LINES)
DECLARE FUNCTION ConvertKey% (a$)
DECLARE FUNCTION ListSelection% (Choice%, m$(), Length%, NextField%)
DECLARE SUB PleaseWait ()
'MENUTOOL.BAS___________________________________________________________MODULE
'                                                                 Version 1.10
'  MENUTOOL.BI contains necessary declarations for insertion

DEFINT A-Z
DECLARE SUB Menu.Format (Full AS STRING, MaxLength%, VK$)
DECLARE SUB Menu.Highlight (k$, R%, C%, Row%, FL%, Last%, Col%, Columns%, ColWidth%, Max%)
DECLARE SUB Menu.Shadow (y1%, x1%, y2%, x2%)
DECLARE FUNCTION Menu$ (Row%, Offset%, MenuDef() AS STRING, Last%)
DECLARE FUNCTION Menu.Valid% (Key$, ValidKeys AS STRING)

CONST FALSE = 0
CONST TRUE = NOT FALSE

' Define names similar to keyboard names with their equivalent key codes.
CONST LEFT = 75, ESC = 27, SPACE = 32
CONST DOWN = 80, DEL = 83, ENTER = 13, UP = 72
CONST HOME = 71, NULL = 0, RIGHT = 77, PgUp = 73
CONST ENDK = 79, INS = 82, TABKEY = 9, PgDn = 81
CONST CTRLD = 4, CTRLG = 7, CTRLS = 19, CTRLV = 22
CONST CTRLE = 5, CTRLH = 8, CTRLX = 24, SHIFTTAB = 15
CONST F1 = 59, F2 = 60, F3 = 61, F4 = 62, F5 = 63, F6 = 64
CONST F7 = 65, F8 = 66, F9 = 67, F10 = 68

'$INCLUDE: 'INITIALS.BI'

' Definitions of routines in other modules
DECLARE FUNCTION GetKey$ (Flash%)
DECLARE SUB RememberScreen (Opt%, Page%, y1%, x1%, y2%, x2%)

FUNCTION ConvertKey% (a$)
      
  SELECT CASE ASC(RIGHT$(a$, 1))
    CASE DOWN, TABKEY
      ConvertKey = TRUE
    CASE UP, SHIFTTAB
      ConvertKey = 1
    CASE ESC, F6
      ConvertKey = FALSE
    CASE F1 TO F5, F7 TO F10, F11, F12
      ConvertKey = ASC(RIGHT$(a$, 1))
    CASE ELSE
      ConvertKey = 2
  END SELECT

END FUNCTION

SUB LineMaker (n, l AS LINES)

  SELECT CASE n
    CASE 0
      l.ul = " "
      l.ur = " "
      l.ll = " "
      l.lr = " "
      l.uh = " "
      l.lh = " "
     
      l.lmh = " "
       l.mh = " "
      l.rmh = " "
     
      l.lv = " "
      l.rv = " "
     
      l.umv = " "
       l.mv = " "
      l.lmv = " "
      l.xs = " "
    CASE 1
      l.ul = "Ú"
      l.ur = "¿"
      l.ll = "À"
      l.lr = "Ù"
      l.uh = "Ä"
      l.lh = "Ä"
     
      l.lmh = "Ã"
       l.mh = "Ä"
      l.rmh = "´"
     
      l.lv = "³"
      l.rv = "³"
     
      l.umv = "Â"
       l.mv = "³"
      l.lmv = "Á"
       l.xs = "Å"
    CASE 2
      l.ul = "É"
      l.ur = "»"
      l.ll = "È"
      l.lr = "¼"
      l.uh = "Í"
      l.lh = "Í"
    
      l.lmh = "Ì"
       l.mh = "Í"
      l.rmh = "¹"
    
      l.lv = "º"
      l.rv = "º"
    
      l.umv = "Ë"
       l.mv = "º"
      l.lmv = "Ê"
       l.xs = "Î"
    CASE 3
      l.ul = "Ü"
      l.ur = "Ü"
      l.ll = "ß"
      l.lr = "ß"
      l.uh = "±"
      l.lh = "±"
     
      l.lmh = "Û"
       l.mh = "²"
      l.rmh = "Û"
     
      l.lv = "Ý"
      l.rv = "Þ"
    
      l.umv = "Ü"
       l.mv = "²"
      l.lmv = "ß"
       l.xs = "²"
    CASE 4
      l.ul = "*"
      l.ur = "*"
      l.ll = "*"
      l.lr = "*"
      l.uh = "*"
      l.lh = "*"
    
      l.lmh = "-"
       l.mh = "*"
      l.rmh = "*"
    
      l.lv = "*"
      l.rv = "*"
    
      l.umv = "*"
       l.mv = "*"
      l.lmv = "*"
      l.xs = "*"
  END SELECT

END SUB

'Menu_________________________________________________________________FUNCTION
'  Creates and Displays a Menu from an array of Options and KeyPress's
'  Allows option to be selected with a key-press or with the arrow keys
'
'USES
'  SUB GetKey
'  SUB Menu.Format
'  SUB Menu.Highlight
'  SUB Menu.Shadow
'  SUB Menu.Valid
'  SUB RememberScreen
'  CONST FALSE
'  SHARED Pref AS INITIALS
'
'PARAMETERS
'  Row       : Row that menu should start on                        -> INTEGER
'  Offset    : Column that menu should start on                     -> INTEGER
'  MenuDef() : Array containing Options, any length            ARRAY-> STRING
'  Last      : Element number of last entry in [MenuDef] array      -> INTEGER
'
'OPTIONS ([Offset])
'  If [Offset] is 0 the menu will be centered on that plane
'
'OPTIONS ([MenuDef])
'  Array element consists of a key, "~", and the option   ex.
'  MenuDef(1) = "D~Choice 4"
'  MenuDef(2) = "Esc~Exit"
'
'OUTPUT
'  SCREEN : Displays boxed menu beginning on line [Row]
'
'VARIABLES
'  a$        : Temporary variable to shorten line                   -> STRING
'  C         : Number of the current screen column for highlight    -> INTEGER
'  Col       : Number of the current menu column for highlight      -> INTEGER
'  Columns   : Number of columns in the menu                        -> INTEGER
'  ColWidth  : Width of each column in the menu, including key      -> INTEGER
'  Edge      : Screen column for left edge of Menu                  -> INTEGER
'  CHeight   : Number of rows per column                            -> INTEGER
'  i         : Loop counter                                         -> INTEGER
'  j         : Temporary variable for number of option to print     -> INTEGER
'  k         : Loop counter                                         -> INTEGER                                                      ->
'  k$        : Key pressed or selected                              -> STRING
'  Max       : Maximum length of all options                        -> INTEGER
'  ShadowX   : Last column that menu uses, including shadow         -> INTEGER
'  ShadowY   : Last row that menu uses, including shadow            -> INTEGER
'  MenuWidth : Total width of menu                                  -> INTEGER
'  R         : Number of the current screen row for highlight       -> INTEGER
'  ValidKeys : List of keys that are valid choices                  -> STRING
'
FUNCTION Menu$ (Row, Offset, MenuDef() AS STRING, Last)
  SHARED Pref AS INITIALS
  DIM ValidKeys AS STRING

'  ValidKeys = ""
 
  ' Find length of longest menu option and add 1 to seperate from border
  FOR i = 1 TO Last
    IF LEN(MenuDef(i)) - INSTR(MenuDef(i), "~") > Max THEN
      Max = LEN(MenuDef(i)) - INSTR(MenuDef(i), "~")
    END IF
  NEXT
  Max = Max + 1

  ' Find the number of columns required to display menu
  Columns = 1
  IF Last > 24 - Row THEN
    Columns = 2
    IF Last > (24 - Row) * 2 THEN Columns = 3
  END IF
 
  ' Find Column height
  CHeight = Last \ Columns + 1
  IF Last MOD Columns = 0 THEN CHeight = CHeight - 1

  ColWidth = Max + 4
  MenuWidth = ColWidth * Columns + 3
 
  ' Center Menu or set left edge at [Offset]
  IF Offset THEN
    Edge = Offset
  ELSE
    Edge = (80 - MenuWidth) \ 2
  END IF
 
  ' Remember the screen and draw Shadow for Menu
  CALL RememberScreen(1, Page, Row, Edge, ShadowY, ShadowX)
  CALL Menu.Shadow(Row, Edge, Row + CHeight + 1, Edge + MenuWidth)
  COLOR Pref.MenuFore, Pref.MenuBack

  ' Create Menu Box and choices
  LOCATE Row, Edge
  PRINT "É" + STRING$(MenuWidth - 2, "Í") + "»";
  FOR i = 1 TO CHeight
    j = i
    LOCATE Row + i, Edge
   '/Begin Single Line output------------------------------------------------\
    PRINT "º";
     FOR k = 1 TO Columns
       IF j + CHeight * (k - Columns) <= Last THEN
         CALL Menu.Format(MenuDef(j), Max, ValidKeys)
       ELSE
         CALL Menu.Format("", Max, ValidKeys)
       END IF
       j = CHeight * k + i
     NEXT
     PRINT " º";
    '\End Single Line output-------------------------------------------------/
  NEXT
  LOCATE Row + CHeight + 1, Edge
  PRINT "È" + STRING$(MenuWidth - 2, "Í") + "¼";

  R = Row + 1
  Col = 1
  C = Edge + 6
  CALL Menu.Highlight("", R, C, Row, CHeight, Last, Col, Columns, ColWidth, Max)
 
  DO
    k$ = UCASE$(GetKey(FALSE))
    CALL Menu.Highlight(k$, R, C, Row, CHeight, Last, Col, Columns, ColWidth, Max)
    IF k$ = CHR$(13) OR k$ = " " THEN
      a$ = MenuDef(R - Row + CHeight * (Col - 1))
      k$ = MID$(a$, 1, INSTR(a$, "~") - 1)
      IF LEFT$(k$, 1) = "*" OR LEFT$(k$, 1) = "¯" THEN
        k$ = MID$(k$, 2, LEN(k$) - 1)
      END IF
    END IF
    SELECT CASE k$
      CASE CHR$(27)
        k$ = "Esc"
      CASE CHR$(0) + CHR$(59) TO CHR$(0) + CHR$(68)
        k$ = "F" + LTRIM$(RTRIM$(STR$(ASC(RIGHT$(k$, 1)) - 58)))
      CASE CHR$(0) + CHR$(133), CHR$(0) + CHR$(134)
        k$ = "F" + LTRIM$(RTRIM$(STR$(ASC(RIGHT$(k$, 1)) - 122)))
    END SELECT
  LOOP UNTIL Menu.Valid(k$, ValidKeys)
  Menu = k$

  CALL RememberScreen(2, Page, Row, Edge, ShadowY, ShadowX)
  COLOR Pref.Fore, Pref.Back

END FUNCTION

'Menu.Format_______________________________________________________________SUB
'  SUB required by Menu. Seperates and creates the option and key press
'
'USES
'  SHARED Pref AS INITIALS
'
'PARAMETERS
'  Full      : Full string containing key press and option          -> STRING
'  MaxLength : Maximum length of all menu options                   -> INTEGER
'  VK$       : String containing all keys that are valid options    -> STRING
'
'OUTPUT
'  SCREEN : Displays key-press and arrow in yellow, option in Pref.Fore
'           color.  Begins at current screen location and leaves cursor at
'           the end of option and spaces to complete column.
'
'VARIABLES
'  EntryStart : Location in [Full] where option part begins         -> STRING
'  Key$       : Key-press portion of [Full]                         -> INTEGER
'  Option$    : Option portion of [Full]                            -> INTEGER
'
SUB Menu.Format (Full AS STRING, MaxLength, VK$)
  SHARED Pref AS INITIALS
      
  EntryStart = INSTR(Full, "~")

  IF Full <> "" THEN
    Key$ = MID$(Full, 1, EntryStart - 1)
    IF LEFT$(Key$, 1) = "*" OR LEFT$(Key$, 1) = "¯" THEN
      VK$ = VK$ + RIGHT$(Key$, LEN(Key$) - 1)
    ELSE
      VK$ = VK$ + Key$ + "~"
    END IF
    Key$ = SPACE$(4 - LEN(Key$)) + Key$ + CHR$(26)
  ELSE
    Key$ = SPACE$(5)
  END IF

  Option$ = RIGHT$(Full, LEN(Full) - EntryStart)
  Option$ = Option$ + SPACE$(MaxLength - (LEN(Full) - EntryStart) - 1)

  COLOR 14
  PRINT Key$;
  COLOR Pref.MenuFore
  PRINT Option$;

END SUB

'Menu.Highlight____________________________________________________________SUB
'  SUB required by Menu. Moves highlight for cursor selection of menu option
'
'USES
'  SHARED Pref AS INITIALS
'
'PARAMETERS
'  k$        : Key pressed                                          -> STRING
'  R         : Current screen row for highlight                     -> INTEGER
'  C         : Current screen column for highlight                  -> INTEGER
'  Row       : Number of the first row for the menu                 -> INTEGER
'  CHeight   : Number of rows per column                            -> INTEGER
'  Last      : Number of the last menu option                       -> INTEGER
'  Col       : Current column of the menu for highlight             -> INTEGER
'  Columns   : Number of columns in the menu                        -> INTEGER
'  ColWidth  : Width of each column in the menu, including key      -> INTEGER
'  Max       : Maximum length of all options                        -> INTEGER
'
'OUTPUT
'  SCREEN : Displays a highlight on the current menu option for selection
'
'VARIABLES
'  i : Loop counter                                                 -> INTEGER
'
SUB Menu.Highlight (k$, R, C, Row, CHeight, Last, Col, Columns, ColWidth, Max)
  SHARED Pref AS INITIALS

  ' Remove highlight from current selection
  COLOR Pref.MenuFore, Pref.MenuBack
  FOR i = 0 TO Max - 2
    LOCATE R, C + i
    PRINT CHR$(SCREEN(R, C + i));
  NEXT

  ' Determine if key passed is a directional key
  SELECT CASE k$
    CASE CHR$(0) + "P" 'DOWN
      IF R < CHeight + Row AND R - Row + CHeight * (Col - 1) < Last THEN
        R = R + 1
      ELSEIF R - Row + CHeight * (Col - 1) < Last THEN
        Col = Col + 1
        C = C + ColWidth
        R = 1 + Row
      ELSEIF R - Row + CHeight * (Col - 1) = Last THEN
        WHILE Col > 1
          Col = Col - 1
          C = C - ColWidth
        WEND
        R = Row + 1
      END IF
    CASE CHR$(0) + "H" 'UP
      IF R > Row + 1 THEN
        R = R - 1
      ELSEIF R - Row + CHeight * (Col - 1) > 1 THEN
        Col = Col - 1
        C = C - ColWidth
        R = CHeight + Row
      ELSEIF R = Row + 1 THEN
        WHILE Col < Columns
          Col = Col + 1
          C = C + ColWidth
        WEND
        R = Row + Last - CHeight * (Columns - 1)
      END IF
    CASE CHR$(0) + "M" 'RIGHT
      IF Col < Columns AND R - Row + CHeight * (Col) - 1 < Last THEN
        Col = Col + 1
        C = C + ColWidth
      ELSEIF R < CHeight + Row THEN
        FOR i = Col TO 2 STEP -1
          C = C - ColWidth
        NEXT
        Col = 1
        R = R + 1
      ELSEIF R = Row + CHeight AND R - Row + CHeight * (Col) - 1 >= Last THEN
        WHILE Col > 1
          Col = Col - 1
          C = C - ColWidth
        WEND
        R = Row + 1
      END IF
    CASE CHR$(0) + "K" 'LEFT
      IF Col > 1 THEN
        Col = Col - 1
        C = C - ColWidth
      ELSEIF R > Row + 1 THEN
        IF (R - 2 - Row) + CHeight * (Columns - 1) < Last THEN d = 1 ELSE d = 2
        FOR i = Col TO Columns - d
          C = C + ColWidth
        NEXT
        Col = Columns - (d - 1)
        R = R - 1
      ELSEIF Col = 1 AND R = Row + 1 THEN
        IF Columns * CHeight = Last THEN
          WHILE Col < Columns
            Col = Col + 1
            C = C + ColWidth
          WEND
          R = Row + CHeight
        ELSE
          WHILE Col < Columns - 1
            Col = Col + 1
            C = C + ColWidth
          WEND
          R = Row + CHeight
        END IF
      END IF
    CASE CHR$(0) + "G" 'HOME
      WHILE Col > 1
        Col = Col - 1
        C = C - ColWidth
      WEND
      R = Row + 1
    CASE CHR$(0) + "O" 'END
      FOR i = Col TO Columns - 1
        C = C + ColWidth
      NEXT
      Col = Columns
      R = Last - CHeight * (Columns - 1) + Row
  END SELECT

  COLOR Pref.HiFore, Pref.HiBack
  FOR i = 0 TO Max - 2
    LOCATE R, C + i
    PRINT CHR$(SCREEN(R, C + i));
  NEXT
  COLOR Pref.MenuFore, Pref.MenuBack

END SUB

SUB Menu.Shadow (y1, x1, y2, x2)
 
  IF x2 = 80 THEN
    xx = 80
  ELSEIF x2 < 80 THEN
    xx = x2 + 1
  END IF
  IF y2 < 25 THEN yy = y2 + 1
  yx = yy
  xy = xx
  IF yx = 0 THEN yx = 25
  IF xy = 0 THEN xy = 80
 
  COLOR 8, 0
  IF xx THEN
    FOR y = y1 + 1 TO yx
      FOR x = x2 TO xx
        LOCATE y, x
        PRINT CHR$(SCREEN(y, x));
      NEXT
    NEXT
  END IF
  IF yy THEN
    FOR x = x1 + 2 TO xy
      LOCATE yy, x
      PRINT CHR$(SCREEN(yy, x));
    NEXT
  END IF
  LOCATE y1, x1

END SUB

'Menu.Valid___________________________________________________________FUNCTION
'  FUNCTION required by Menu. Determines in key press is in [ValidKeys]
'
'USES
'  CONST TRUE
'  CONST FALSE
'
'PARAMETERS
'  Key$      : Key-press to examine                                 -> STRING
'  ValidKeys : String containing all keys that are valid options    -> STRING
'
FUNCTION Menu.Valid (Key$, ValidKeys AS STRING)

  Menu.Valid = FALSE
  IF INSTR(ValidKeys, Key$) THEN
    Menu.Valid = TRUE
  END IF

END FUNCTION

SUB Panel (Opt, Fore, Back, y1, x1, y2, x2, n) STATIC
  SHARED Pref AS INITIALS
  DIM l AS LINES
 
  CALL LineMaker(n, l)
 
  SELECT CASE Opt
    CASE 1
      C = CSRLIN
      P = POS(0)
      IF x2 < 79 THEN
        ShadowX = x2 + 2
      ELSE
        ShadowX = 80
      END IF
      IF y2 < 25 THEN
        ShadowY = y2 + 1
      ELSE
        ShadowY = 25
      END IF
      CALL RememberScreen(1, Page, y1, x1, ShadowY, ShadowX)
      CALL Menu.Shadow(y1, x1, y2, x2 + 1)
      COLOR Fore, Back
      LOCATE y1, x1
      PRINT l.ul; STRING$(x2 - x1 - 1, l.uh); l.ur;
      i = y1 + 1
      WHILE i < y2
        LOCATE i, x1
        PRINT l.lv; STRING$(x2 - x1 - 1, " "); l.rv;
        i = i + 1
      WEND
      LOCATE y2, x1
      PRINT l.ll; STRING$(x2 - x1 - 1, l.lh); l.lr;
    CASE 2
      CALL RememberScreen(2, Page, y1, x1, ShadowY, ShadowX)
      COLOR Pref.Fore, Pref.Back
      LOCATE C, P, 0
  END SELECT

END SUB

