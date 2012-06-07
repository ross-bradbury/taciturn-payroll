DECLARE SUB Hint (H$)
DECLARE SUB HighLight (y%, x%, Length%)
DECLARE FUNCTION ConvertKey% (a$)
DECLARE SUB Panel (Opt%, Fore%, Back%, y1%, x1%, y2%, x2%, N%)
'SSBOUNCE.BAS___________________________________________________________MODULE
'                                                                 Version 1.00
'  SSBOUNCE.BI contains necessary declarations for insertion

DEFINT A-Z
DECLARE SUB Bounce ()
DECLARE SUB Bounce.Color (Collision%, Change%, C%, Max%)
DECLARE SUB Bounce.Screen (MaxX%, MaxY%, MaxC%)
DECLARE SUB Bounce.Pref ()
DECLARE SUB Bounce.Pref.Update (y%, x%)
DECLARE FUNCTION Bounce.Speed% (Speed%)
DECLARE FUNCTION Bounce.Advance% (Corner%, OldCorner%, Addition%, Edge1%, Edge2%)

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
'$INCLUDE: 'CAPSLOCK.BI'
'$INCLUDE: 'EDNUMBER.BI'
'$INCLUDE: 'TEXTTOOL.BI'
'$INCLUDE: 'MENUTOOL.BI'
'$INCLUDE: 'TIMETOOL.BI'

DECLARE SUB TextBounce (ChangeModes%)

' Error handler to determine screen mode to use
BounceScreenError:
  SELECT CASE Mode
	CASE 14
	  Mode = 12
	CASE 12
	  Mode = 9
	CASE 9
	  Mode = 7
	CASE 7
	  Mode = 1
	CASE 1
	  Mode = 0
  END SELECT
  RESUME

SUB Bounce
  SHARED Pref AS INITIALS
  DIM x(Pref.SSPoints, Pref.SSTrails), y(Pref.SSPoints, Pref.SSTrails)
  DIM XAdd(Pref.SSPoints), YAdd(Pref.SSPoints)
  DIM C(ABS(SGN(Pref.SSCubes)) * Pref.SSPoints OR 1)
  DIM Count(ABS(SGN(Pref.SSCubes)) * Pref.SSPoints OR 1)
 
  ' Save old Mouse Position
  CALL MousePosition(oy, ox, 1)
  CALL MouseLocate(10, 10, 1)
 
  ' Initialize Screen
  CALL Bounce.Screen(XMax, YMax, CMax)
  XMin = 0
  YMin = 0
  IF Pref.SSCubes OR Pref.SSPoints = 1 THEN
	XMin = XMin + Pref.SSCubeWidth \ 2
	YMin = XMin
	XMax = XMax - XMin
	YMax = YMax - YMin
  END IF
 
  StartShift$ = ShiftState
  IF MID$(StartShift$, 5, 1) = "1" THEN
	PRINT "Release the Alt key"
	DO
	  StartShift$ = ShiftState
	LOOP WHILE MID$(StartShift$, 5, 1) = "1"
	CLS
  END IF
  Shift = PEEK(&H417)

  IF XMax > 0 THEN
	' Calculate delay for fast processors
	StartTime# = TIMER
	DO
	  W = W + 1
	LOOP UNTIL TIMER > StartTime# + .3
	Warp = W \ 10
	 
	' Initialize start position and color
	p = 1
	FOR i = 1 TO Pref.SSPoints
	  XAdd(i) = Rand(2, 4)
	  YAdd(i) = Rand(2, 4)
	  x(i, o) = INT(RND(1) * XMax) + 1
	  y(i, o) = INT(RND(1) * YMax) + 1
	  ' Initialize colors, ( )'s required to keep value
	  IF Pref.SSCubes OR Pref.SSPoints = 1 THEN
		CALL Bounce.Color(TRUE, (Pref.SSMinChange), C(i), CMax)
	  ELSE
		CALL Bounce.Color(TRUE, (Pref.SSMinChange), C, CMax)
	  END IF
	NEXT
	DO
	  CALL MousePosition(My, Mx, 1)
	  Mp = ABS(My - 10) + ABS(Mx - 10)
	  CALL MouseClick(Mb, 0, Mb)

	  ' Advance poly-line or cube
	  FOR i = 1 TO Pref.SSPoints
		Hit = Bounce.Advance(x(i, p), x(i, o), XAdd(i), XMin, XMax)
		Hit = Bounce.Advance(y(i, p), y(i, o), YAdd(i), YMin, YMax) OR Hit
		IF Pref.SSCubes OR Pres.SSPoints = 1 THEN
		  CALL Bounce.Color(Hit, Count(i), C(i), CMax)
		END IF
	  NEXT
	  IF NOT Pref.SSPoints THEN
		CALL Bounce.Color(Hit, Count, C, CMax)
	  END IF
		 
	  ' Display poly-line or cubes
	  IF Pref.SSCubes OR Pref.SSPoints = 1 THEN
		FOR i = 1 TO Pref.SSPoints
		  LINE (x(i, p) - XMin, y(i, p) - YMin)-(x(i, p) + XMin, y(i, p) + YMin), C(i), B
		NEXT
	  ELSE
		SELECT CASE Pref.SSPoints
		  CASE 2
			IF Pref.SS2PointBox THEN
			  LINE (x(1, p), y(1, p))-(x(2, p), y(2, p)), C, B
			ELSE
			  LINE (x(1, p), y(1, p))-(x(2, p), y(2, p)), C
			END IF
		  CASE ELSE
			LINE (x(1, p), y(1, p))-(x(2, p), y(2, p)), C
			FOR i = 3 TO Pref.SSPoints
			  LINE -(x(i, p), y(i, p)), C
			NEXT
			LINE -(x(1, p), y(1, p)), C
		END SELECT
	  END IF

	  ' Prepare to erase next loop entry and create new in it's place
	  q = p + 1
	  IF q = Pref.SSTrails + 1 THEN q = 0
		 
	  ' Erase old poly-line or cubes
	  IF Pref.SSCubes OR Pref.SSPoints = 1 THEN
		FOR i = 1 TO Pref.SSPoints
		  LINE (x(i, q) - XMin, y(i, q) - YMin)-(x(i, q) + XMin, y(i, q) + YMin), 0, B
		NEXT
	  ELSE
		SELECT CASE Pref.SSPoints
		  CASE 2
			IF Pref.SS2PointBox THEN
			  LINE (x(1, q), y(1, q))-(x(2, q), y(2, q)), 0, B
			ELSE
			  LINE (x(1, q), y(1, q))-(x(2, q), y(2, q)), 0
			END IF
		  CASE ELSE
			LINE (x(1, q), y(1, q))-(x(2, q), y(2, q)), 0
			FOR i = 3 TO Pref.SSPoints
			  LINE -(x(i, q), y(i, q)), 0
			NEXT
			LINE -(x(1, q), y(1, q)), 0
		END SELECT
	  END IF
	  o = p
	  p = p + 1
	  IF p = Pref.SSTrails + 1 THEN p = 0
		 
	  ' Slow down fast processors
	  W = 0
	  DO
		W = W + 1
	  LOOP UNTIL W >= Warp
	 
	LOOP WHILE INKEY$ = "" AND Mp = 0 AND Mb = 0 AND Shift = PEEK(&H417)
  ELSE
	' If no graphics capability, display standard text Bounce
	CALL TextBounce(TRUE)
  END IF
  CALL MouseLocate(oy, ox, 1)
  IF ABS(PEEK(&H417) - Shift) > 8 THEN POKE &H417, Shift

END SUB

'Bounce.Advance_______________________________________________________FUNCTION
'  FUNCTION required by Bounce. Advances a point, by part, and returns true
'  if the corner has bounced off a side.
'
'USES
'  SUB Bounce.Speed
'  CONST TRUE
'  CONST FALSE
'
'PARAMETERS
'  Corner    : Outgoing value of X OR Y part of point               -> INTEGER
'  OldCorner : Incoming value of X OR Y part of point               -> INTEGER
'  Addition  : The value to add or subtract to find new point       -> INTEGER
'  Edge1     : Minimum value for [Corner]                           -> INTEGER
'  Edge2     : Maximum value for [Corner]                           -> INTEGER
'
FUNCTION Bounce.Advance (Corner, OldCorner, Addition, Edge1, Edge2)
 
  Corner = OldCorner + Addition
  Bounce.Advance = FALSE
  IF Corner < Edge1 OR Corner > Edge2 THEN
	Addition = -Bounce.Speed(Addition)
	Corner = OldCorner + Addition
	Bounce.Advance = TRUE
  END IF

END FUNCTION

'Bounce.Color______________________________________________________________SUB
'  SUB required by Bounce. Changes the color if the number of advances
'  exceeds [Pref.SSMinChange] and [Collision] is true.
'  corner has bounced off a side.
'
'USES
'  FUNCTION Rand
'  SHARED Pref AS INITIALS
'
'PARAMETERS
'  Collision : TRUE if point has collided with a wall               -> INTEGER
'  Change    : The current number of advances since last change     -> INTEGER
'  C         : The new value for color                              -> INTEGER
'  Max       : Maximum value for color                              ->  NTEGER
'
SUB Bounce.Color (Collision, Change, C, Max)
  SHARED Pref AS INITIALS

  Change = Change + 1
  IF Change > Pref.SSMinChange AND Collision THEN
	C = Rand(1, Max - 1)
	Change = 0
  END IF

END SUB

SUB Bounce.Pref
  SHARED Pref AS INITIALS
  DIM m$(1 TO 2)
 
  x = 16
  y = 9
  CALL Panel(1, Pref.Fore, Pref.Back, y - 1, x - 1, y + 12, x + 50, 3)
  CALL Bounce.Pref.Update(y, x)
  LOCATE y + 10, x - 1
  PRINT "Û" + STRING$(50, 178) + "Û";
  LOCATE y + 11, x
  PRINT "    Press Alt + "; CHR$(34); "s"; CHR$(34); " to Test the Screen Saver"
 

  ' Set NextFlag and continue editing each field.
  ' NextFlag is cleared when the user presses ENTER.
  NextFlag = TRUE
  DO
	IF NextFlag = FALSE THEN EXIT DO

	IF NextFlag < 2 THEN CurField = CurField - NextFlag
	IF CurField < 1 THEN CurField = 10
	IF CurField > 10 THEN CurField = 1
	NextFlag = TRUE

	SELECT CASE CurField
	  CASE 1
		m$(1) = "1~Active Self Blanking"
		m$(2) = "2~Inactive"
		DO
		  
		  CALL HighLight(y, x + 21, 20)
		  a$ = GetKey(FALSE)
		  IF a$ = CHR$(13) OR a$ = " " THEN
			
			IF Menu(y, x + 21, m$(), 2) = "1" THEN
			  Pref.SSActive = TRUE
			ELSE
			  Pref.SSActive = FALSE
			END IF
			NextFlag = TRUE
		  END IF
		  CALL Bounce.Pref.Update(y, x)
		LOOP UNTIL a$ <> CHR$(13)
		NextFlag = ConvertKey(a$)
	  CASE 2
		DO
		  LOCATE y + 1, x + 22
		  o! = EditNum(Pref.SSTimeOut, 2, 2, NextFlag)
		LOOP UNTIL o! >= .25 AND o! <= 30
		Pref.SSTimeOut = o!
	  CASE 3
		m$(1) = "1~Use Graphics Mode"
		m$(2) = "2~Use Text Mode"
		DO
		  
		  CALL HighLight(y + 2, x + 21, 17)
		  a$ = GetKey(FALSE)
		  IF a$ = CHR$(13) OR a$ = " " THEN
			
			IF Menu(y + 2, x + 21, m$(), 2) = "1" THEN
			  Pref.SSGraphics = TRUE
			ELSE
			  Pref.SSGraphics = FALSE
			END IF
			NextFlag = TRUE
		  END IF
		  CALL Bounce.Pref.Update(y, x)
		LOOP UNTIL a$ <> CHR$(13)
		NextFlag = ConvertKey(a$)
	  CASE 4
		m$(1) = "1~High Resolution Text"
		m$(2) = "2~Normal Resoultion Text"
		DO
		  
		  CALL HighLight(y + 3, x + 21, 22)
		  a$ = GetKey(FALSE)
		  IF a$ = CHR$(13) OR a$ = " " THEN
			
			IF Menu(y + 3, x + 21, m$(), 2) = "1" THEN
			  Pref.SSChangeModes = TRUE
			ELSE
			  Pref.SSChangeModes = FALSE
			END IF
			NextFlag = TRUE
		  END IF
		  CALL Bounce.Pref.Update(y, x)
		LOOP UNTIL a$ <> CHR$(13)
		NextFlag = ConvertKey(a$)
	  CASE 5
		DO
		  LOCATE y + 4, x + 22
		  p = EditNum(CSNG(Pref.SSPoints), 2, 0, NextFlag)
		LOOP UNTIL p >= 1 AND p <= 20
		Pref.SSPoints = p
	  CASE 6
		DO
		  LOCATE y + 5, x + 21
		  p = EditNum(CSNG(Pref.SSTrails), 3, 0, NextFlag)
		LOOP UNTIL p >= 5 AND p <= 500
		Pref.SSTrails = p
	  CASE 7
		m$(1) = "1~Connect all Points"
		m$(2) = "2~Each Point is a Cube"
		DO
		  '
		  CALL HighLight(y + 6, x + 21, 20)
		  a$ = GetKey(FALSE)
		  IF a$ = CHR$(13) OR a$ = " " THEN
			
			IF Menu(y + 6, x + 21, m$(), 2) = "2" THEN
			  Pref.SSCubes = TRUE
			ELSE
			  Pref.SSCubes = FALSE
			END IF
			NextFlag = TRUE
		  END IF
		  CALL Bounce.Pref.Update(y, x)
		LOOP UNTIL a$ <> CHR$(13)
		NextFlag = ConvertKey(a$)
	  CASE 8
		DO
		  LOCATE y + 7, x + 22
		  p = EditNum(CSNG(Pref.SSCubeWidth), 2, 0, NextFlag)
		LOOP UNTIL p >= 0 AND p <= 100
		Pref.SSCubeWidth = p
	  CASE 9
		m$(1) = "1~Make 2 Points a Line"
		m$(2) = "2~Make 2 Points a Box"
		DO
		  
		  CALL HighLight(y + 8, x + 21, 20)
		  a$ = GetKey(FALSE)
		  IF a$ = CHR$(13) OR a$ = " " THEN
			'
			IF Menu(y + 8, x + 21, m$(), 2) = "2" THEN
			  Pref.SS2PointBox = TRUE
			ELSE
			  Pref.SS2PointBox = FALSE
			END IF
			NextFlag = TRUE
		  END IF
		  CALL Bounce.Pref.Update(y, x)
		LOOP UNTIL a$ <> CHR$(13)
		NextFlag = ConvertKey(a$)
	  CASE 10
		DO
		  LOCATE y + 9, x + 20
		  p = EditNum(CSNG(Pref.SSMinChange), 4, 0, NextFlag)
		LOOP UNTIL p >= 0 AND p <= 9999
		Pref.SSMinChange = p
	END SELECT
  LOOP
  
  CALL Panel(2, Pref.Fore, Pref.Back, y - 1, x - 1, y + 8, 70, 1)

END SUB

SUB Bounce.Pref.Update (y, x)
  SHARED Pref AS INITIALS

'  COLOR Pref.Fore, 2
 
  LOCATE y, x
  PRINT "   Screen Saver is : ";
	IF Pref.SSActive THEN
	  PRINT "Active Self Blanking"
	ELSE
	  PRINT "Inactive            "
	END IF
  LOCATE y + 1, x
  PRINT "   Engage Saver in :";
  PRINT USING "  ##.##"; Pref.SSTimeOut;
  PRINT " minutes"
  LOCATE y + 2, x
  PRINT "          Graphics : ";
	IF Pref.SSGraphics THEN
	  PRINT "Use Graphics Mode   "
	ELSE
	  PRINT "Use Text Mode       "
	END IF
  LOCATE y + 3, x
  PRINT "         Text mode : ";
	IF Pref.SSChangeModes THEN
	  PRINT "High Resolution Text  "
	ELSE
	  PRINT "Normal Resolution Text"
	END IF
  LOCATE y + 4, x
  PRINT "  Number of Points :";
  PRINT USING " ###"; Pref.SSPoints
  LOCATE y + 5, x
  PRINT "  Number of Trails :";
  PRINT USING " ###"; Pref.SSTrails
  LOCATE y + 6, x
  PRINT "      Connectivity : ";
	IF Pref.SSCubes THEN
	  PRINT "Each Point is a Cube"
	ELSE
	  PRINT "Connect All Points  "
	END IF
  LOCATE y + 7, x
  PRINT "    Width of Cubes :";
  PRINT USING " ###"; Pref.SSCubeWidth
  LOCATE y + 8, x
  PRINT "   2 Point Objects : ";
	IF Pref.SS2PointBox THEN
	  PRINT "Make 2 Points a Box "
	ELSE
	  PRINT "Make 2 Points a Line"
	END IF
  LOCATE y + 9, x
  PRINT "Change Color Every :";
  PRINT USING " ### "; Pref.SSMinChange;
  PRINT "advances"
 
END SUB

'Bounce.Screen_____________________________________________________________SUB
'  SUB required by Bounce. Determines best graphics screen to use by way of
'  an error trap.
'
'PARAMETERS
'  MaxX : New maximum X value                                       -> INTEGER
'  MaxY : New maximum Y value                                       -> INTEGER
'  MaxC : New number of colors                                      -> INTEGER
'
'VARIABLES
'  Mode : Number of the screen mode, used with error trap     SHARED-> INTEGER
'
SUB Bounce.Screen (MaxX, MaxY, MaxC)
  SHARED Mode
 
  ON ERROR GOTO BounceScreenError
	Mode = 14
	SCREEN Mode
  ON ERROR GOTO 0
  SELECT CASE Mode
	CASE 12
	  MaxX = 640
	  MaxY = 480
	  MaxC = 16
	CASE 9
	  MaxX = 640
	  MaxY = 350
	  MaxC = 16
	CASE 7
	  MaxX = 320
	  MaxY = 200
	  MaxC = 16
	CASE 1
	  MaxX = 320
	  MaxY = 200
	  MaxC = 4
	CASE 0
	  MaxX = 0
	  MaxY = 0
	  MaxC = 0
  END SELECT
END SUB

'Bounce.Speed_________________________________________________________FUNCTION
'  FUNCTION required by Bounce. Changes the current point speed.
'
'PARAMETERS
'  Speed : Current speed of point, used for the sign                -> INTEGER
'
FUNCTION Bounce.Speed (Speed)

  Bounce.Speed = SGN(Speed) * Rand(2, 4)

END FUNCTION

