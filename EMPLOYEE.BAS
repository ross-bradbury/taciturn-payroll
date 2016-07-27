DECLARE SUB ClearLine (y%, z%)
DECLARE SUB Center (Row%, Text$, Fore%, Back%)
DECLARE SUB PleaseWait ()
DECLARE SUB Pause (Alert%)
DECLARE SUB Click ()
DECLARE SUB Pointer (y%, x%)
DECLARE SUB ChooseEmployee (Number%, Auto%, ActiveOnly%, Done AS STRING)
DECLARE SUB Choose.Reverse (Temp$, M$, Number%, Active%, Done%)
DECLARE FUNCTION StringLength% (Text AS STRING)
DECLARE SUB Status2 (a AS INTEGER)
DECLARE FUNCTION Deductions.Modify% (Deduct AS ANY, Top%)
DECLARE FUNCTION Deductions.Delete% (D AS ANY)
DECLARE FUNCTION Deductions% (EmployeeInfo AS ANY)
DECLARE FUNCTION ModifyDeduction% (Deduct AS ANY, Top%)
DECLARE FUNCTION Withholding$ (Holding%)
DECLARE FUNCTION DeleteDeduction% (D AS ANY)
DEFINT A-Z
DECLARE SUB Status (D AS ANY, New%)
DECLARE SUB Deductions.Update (Deduct AS ANY, Top%)
DECLARE SUB DropEmployee ()
DECLARE SUB Panel (Opt%, Fore%, Back%, y1%, x1%, y2%, x2%, N%)
DECLARE SUB HighLight (y%, x%, Length%)
DECLARE SUB Header (Title$, Company%, UseDate%)
DECLARE SUB LoadEmployeeData (Number%, Info AS ANY)
DECLARE SUB Employee.Modify (New%, EmployeeInfo AS ANY)
DECLARE FUNCTION EType$ (EmpType AS STRING)
DECLARE SUB Employee.Add (EmployeeInfo AS ANY)
DECLARE SUB SaveEmployeeData (New%, Info AS ANY)
DECLARE SUB Employee.Update (Info AS ANY, Top%)
DECLARE FUNCTION ListSelection% (Choice%, M$(), Length%, NextField%)
DECLARE FUNCTION ConvertKey% (a$)

CONST FALSE = 0
CONST TRUE = NOT FALSE

'$INCLUDE: 'EDNUMBER.BI'
'$INCLUDE: 'EDSTRING.BI'
'$INCLUDE: 'HELPTOOL.BI'
'$INCLUDE: 'MENUTOOL.BI'
'$INCLUDE: 'SCRNSAVE.BI'
'$INCLUDE: 'SSBOUNCE.BI'
'$INCLUDE: 'TEXTTOOL.BI'
'$INCLUDE: 'TIMETOOL.BI'

'$INCLUDE: 'INITIALS.BI'

'$INCLUDE: 'PAYTYPES.BI'

DIM EmployeeInfo AS EmployeeType
DIM Deduct       AS DeductionType

'x
'x2
'
SUB Choose.Reverse (Temp$, M$, Number, Active, Done)
 
  IF INSTR(LEFT$(Temp$, StringLength(Temp$)), " ") THEN
    x2 = INSTR(Temp$, " ")
    DO
      x = x2
      x2 = INSTR(x + 1, Temp$, " ")
    LOOP UNTIL x2 = 0 OR x2 >= StringLength(Temp$)
    x = x + 1
    Temp$ = MID$(Temp$, x, StringLength(Temp$) - x + 1) + ", " + LEFT$(Temp$, x - 2)
  END IF
         
  IF Active = TRUE XOR Done = 1 THEN
    M$ = LTRIM$(RTRIM$(STR$(Number))) + "~" + Temp$
  ELSEIF Active AND Done THEN
    M$ = "+" + LTRIM$(RTRIM$(STR$(Number))) + "~" + Temp$
  ELSE
    M$ = "*" + LTRIM$(RTRIM$(STR$(Number))) + "~" + Temp$
  END IF

END SUB

'ChooseEmployee____________________________________________________________SUB
'              Creates a menu of employees and Loads their Information
'
SUB ChooseEmployee (Number, Auto, ActiveOnly, Done AS STRING)
  DIM TempInfo AS EmployeeType
  DIM Number(23)
  DIM M$(25)

  PleaseWait

  OPEN "EMPLOYEE.DAT" FOR RANDOM AS 1 LEN = LEN(TempInfo)
    Last = 0
    Current = 0
    DO
      GET #1, Current + 1, TempInfo
      IF Valid(TempInfo.EmpName) THEN
        IF ActiveOnly THEN
          IF TempInfo.Active THEN
            Last = Last + 1
            Temp$ = TempInfo.EmpName
            IF LEN(Done) >= Last THEN DoneEmployee = VAL(MID$(Done, Last, 1))
            Number(Last) = TempInfo.Number
            CALL Choose.Reverse(Temp$, M$(Last), Last, TempInfo.Active, DoneEmployee)
          END IF
        ELSE
          Last = Last + 1
          Temp$ = TempInfo.EmpName
          Number(Last) = TempInfo.Number
          CALL Choose.Reverse(Temp$, M$(Last), Last, TempInfo.Active, DoneEmployee)
        END IF
        Current = Current + 1
      END IF
    LOOP WHILE Valid(TempInfo.EmpName)
  CLOSE 1

  IF Auto AND Last > 0 THEN
    Last = Last + 1
    M$(Last) = "F4~Auto Employee Selection"
  END IF
  Last = Last + 1
  M$(Last) = "Esc~Return to Previous Menu"
 
  IF Auto > 0 THEN
    Number = Number(Auto)
  ELSE
    a$ = Menu(5, 0, M$(), Last)
    SELECT CASE a$
      CASE CHR$(27)
        Number = 0
      CASE "F4"
        Number = -(Current + 1)
      CASE ELSE
        Number = Number(VAL(a$))
    END SELECT
  END IF

END SUB

'Deductions________________________________________________________________SUB
'          Creates Extra Deductions (up to 4) for Employees
'
FUNCTION Deductions (EmployeeInfo AS EmployeeType)
  SHARED Pref AS INITIALS
  DIM Deduct(4) AS DeductionType
  DIM M$(5)

  Deduct(1) = EmployeeInfo.Deduction1
  Deduct(2) = EmployeeInfo.Deduction2
  Deduct(3) = EmployeeInfo.Deduction3
  Deduct(4) = EmployeeInfo.Deduction4

  CALL Panel(1, Pref.Fore, Pref.Back, 4, 1, 25, 80, 0)

  DO
    CALL Header("Employee Setup: Modify Employee Deductions", TRUE, TRUE)

    FOR j = 1 TO 4
      IF Valid(Deduct(j).DName) THEN
        M$(j) = LTRIM$(RTRIM$(STR$(j))) + "~" + Deduct(j).DName
      ELSE
        M$(j) = LTRIM$(RTRIM$(STR$(j))) + "~Inactive Deduction"
      END IF
    NEXT
    M$(5) = "Esc~Exit from Deductions"

    a$ = Menu(4, 0, M$(), 5)
    IF a$ <> "Esc" THEN
      x = VAL(a$)
      Changed = Changed OR Deductions.Modify(Deduct(x), 5)
    END IF
  LOOP UNTIL a$ = "Esc"

  EmployeeInfo.Deduction1 = Deduct(1)
  EmployeeInfo.Deduction2 = Deduct(2)
  EmployeeInfo.Deduction3 = Deduct(3)
  EmployeeInfo.Deduction4 = Deduct(4)

  CALL Panel(2, Pref.Fore, Pref.Back, 4, 1, 25, 80, 0)
  Deductions = Changed

END FUNCTION

FUNCTION Deductions.Delete (D AS DeductionType)
  SHARED Pref AS INITIALS

  CALL ClearLine(4, 25)
  COLOR 14, 4
  LOCATE 5, 10
  PRINT "Deleting a deduction will remove its Name and YTD Amounts from "
  LOCATE , 10
  PRINT "all new pay-stubs.  To merely stop the deduction but keep the  "
  LOCATE , 10
  PRINT "Name and YTD Amount, Abort Delete and set deduction to Inactive"
 
  COLOR Pref.Fore, Pref.Back
  CALL Deductions.Update(D, 15)

  COLOR 14
  LOCATE 9, 10
  PRINT "Do you really want to DELETE this Deduction [Y/N] ? ";
  DO
    a$ = UCASE$(GetKey(TRUE))
  LOOP UNTIL a$ = "Y" OR a$ = "N"
  COLOR 14
  PRINT a$
  IF a$ = "Y" THEN
    SOUND 3500, 1
    SOUND 32767, 1
    SOUND 3500, 1
    SOUND 32767, 1
    SOUND 3500, 1
   
    LOCATE 11, 10
    PRINT "DELETING this deduction can not be undone, continue [Y/N] ? ";
    DO
      a$ = UCASE$(GetKey(TRUE))
    LOOP UNTIL a$ = "Y" OR a$ = "N"
    COLOR 14
    PRINT a$
    IF a$ = "Y" THEN
      D.Active = 0
      D.DType = 0
      D.DName = ""
      D.Amount = 0
      D.AType = 0
      D.Limit = 0
      D.LType = 0
      D.Total = 0
      Deductions.Delete = TRUE
      LOCATE 13, 10
      PRINT "Deduction Deleted. Remember to Save Employee Info to Keep Changes."
      CALL Pause(TRUE)
    END IF
  END IF
 
  COLOR Pref.Fore, Pref.Back
  CALL ClearLine(4, 25)

END FUNCTION

FUNCTION Deductions.Modify (Deduct AS DeductionType, Top)
  SHARED Pref AS INITIALS, Delete, NextFlag
  DIM Old AS DeductionType
  DIM M$(4)

  Old = Deduct
  CALL ClearLine(Top - 1, 19)
 
  IF NOT Valid(Deduct.DName) THEN
    Deduct.Active = 1
    Deduct.DType = 1
    Deduct.AType = 1
    Deduct.LType = 2
  END IF
 
  NextFlag = TRUE

  DO
    CALL Center(Top - 1, "Press 'Esc' or F6 to Save, Abort, or Delete Deduction", 14, 4)
    CALL Deductions.Update(Deduct, Top)
    CALL Status(Old, FALSE)
    DO
      CALL Status(Deduct, TRUE)
      IF NextFlag = FALSE THEN EXIT DO
      IF NextFlag < 2 THEN CurField = CurField - NextFlag
      IF (Deduct.DType = 1 OR Deduct.DType = 3) AND CurField = 5 THEN
        CurField = CurField - NextFlag
      END IF
      IF CurField < 1 THEN CurField = 9
      IF CurField > 9 THEN CurField = 1
      NextFlag = TRUE

      SELECT CASE CurField
        CASE 1
          LOCATE Top + CurField, 44
          M$(1) = "1~Active"
          M$(2) = "2~Inactive"
          '
          IF Pref.Click THEN CALL Click
          CALL HighLight(Top + CurField, 44, 8)
          a$ = GetKey(FALSE)
          IF a$ = CHR$(13) OR a$ = " " THEN
            '
            Deduct.Active = VAL(Menu(Top + CurField, 42, M$(), 2))
            NextFlag = TRUE
          END IF
          CALL Deductions.Update(Deduct, Top)
          NextFlag = ConvertKey(a$)
        CASE 2
          '
          LOCATE Top + CurField, 44
          Deduct.DName = EditString(Deduct.DName, 18, NextFlag)
        CASE 3
          M$(1) = "1~Fixed Weekly Deduction"
          M$(2) = "2~Fixed until Specified Amount"
          M$(3) = "3~Variable Weekly Deduction"
          M$(4) = "4~Variable until Specified Amount"
          '
          IF Pref.Click THEN CALL Click
          CALL HighLight(Top + CurField, 44, 31)
          a$ = GetKey(FALSE)
          IF a$ = CHR$(13) OR a$ = " " THEN
            '
            Deduct.DType = VAL(Menu(Top + CurField, 42, M$(), 4))
            NextFlag = TRUE
          END IF
          CALL Deductions.Update(Deduct, Top)
          NextFlag = ConvertKey(a$)
        CASE 4
          M$(1) = "1~Taxable"
          M$(2) = "2~Not Taxable"
          '
          IF Pref.Click THEN CALL Click
          CALL HighLight(Top + CurField, 44, 11)
          a$ = GetKey(FALSE)
          IF a$ = CHR$(13) OR a$ = " " THEN
            '
            Deduct.Taxable = VAL(Menu(Top + CurField, 42, M$(), 2))
            NextFlag = TRUE
          END IF
          CALL Deductions.Update(Deduct, Top)
          NextFlag = ConvertKey(a$)
        CASE 5
          '
          LOCATE Top + CurField, 44
          Deduct.Total = EditNum(Deduct.Total, 5, 2, NextFlag)
          CALL Deductions.Update(Deduct, Top)
        CASE 6
          M$(1) = "1~Dollar Value"
          M$(2) = "2~Percentage of Gross"
          M$(3) = "3~Percentage of Net"
          '
          IF Pref.Click THEN CALL Click
          CALL HighLight(Top + CurField, 44, 19)
          a$ = GetKey(FALSE)
          IF a$ = CHR$(13) OR a$ = " " THEN
            '
            Deduct.AType = VAL(Menu(Top + CurField, 42, M$(), 3))
            NextFlag = TRUE
          END IF
          IF Deduct.AType = 1 THEN
            IF Deduct.LType = 1 THEN Deduct.LType = 2
          ELSE
            Deduct.LType = 1
          END IF
          CALL Deductions.Update(Deduct, Top)
          NextFlag = ConvertKey(a$)
        CASE 7
          '
          LOCATE Top + CurField, 45
          Deduct.Amount = EditNum(Deduct.Amount, 4, 2, NextFlag)
        CASE 8
          IF Deduct.AType = 1 THEN
            M$(1) = "1~Percentage of Gross"
            M$(2) = "2~Percentage of Net"
            O = 1
          ELSE
            M$(1) = "1~Dollar Value"
            O = 0
          END IF
          '
          IF Pref.Click THEN CALL Click
          CALL HighLight(Top + CurField, 44, 19)
          a$ = GetKey(FALSE)
          IF a$ = CHR$(13) OR a$ = " " THEN
            '
            Deduct.LType = VAL(Menu(Top + CurField, 42, M$(), O + 1)) + O
            NextFlag = TRUE
          END IF
          CALL Deductions.Update(Deduct, Top)
          NextFlag = ConvertKey(a$)
        CASE 9
          '
          LOCATE Top + CurField, 45
          Deduct.Limit = EditNum(Deduct.Limit, 4, 2, NextFlag)
      END SELECT
    LOOP
   
    'Determine whether there is enough information
    NV = FALSE
    IF NOT Valid(Deduct.DName) THEN NV = TRUE: CALL Pointer(Top + 2, 41)
   
    IF Deduct.Amount = 0 AND (Deduct.DType = 1 OR Deduct.DType = 2) THEN
      NV = TRUE
      CALL Pointer(Top + 7, 41)
    END IF
    IF Deduct.Limit = 0 THEN NV = TRUE: CALL Pointer(Top + 9, 41)
    IF (Deduct.DType = 2 OR Deduct.DType = 4) AND Deduct.Total = 0 THEN
      NV = TRUE
      CALL Pointer(Top + 4, 41)
    END IF
    IF NV THEN
      ClearLine 15, 25
      Center 15, "Not Enough Information Has Been Added!", 14, 4
      Center 16, "The information indicated by " + CHR$(34) + ">" + CHR$(34) + " must be entered!", 14, 4
      M$(1) = "1~Continue Editing"
      M$(2) = "D~Delete Deduction"
      M$(3) = "Esc~Exit and Discard Changes"
      SELECT CASE Menu(18, 0, M$(), 3)
        CASE "1"
          ClearLine 15, 25
          NextFlag = 2
        CASE "D"
          'Call Delete FUNCTION, which will return TRUE if deleted
          IF Deductions.Delete(Deduct) THEN
            Deductions.Modify = TRUE
            EXIT FUNCTION
          END IF
        CASE "Esc"
          Deduct = Old
          EXIT DO
      END SELECT
    ELSE
      ClearLine 4, 13
      M$(1) = "1~Continue Editing"
      M$(2) = "2~Accept Changes"
      M$(3) = "D~Delete Deduction"
      M$(4) = "Esc~Exit and Discard Changes"
      SELECT CASE Menu(6, 0, M$(), 4)
        CASE "1"
          ClearLine 22, 23
          NextFlag = 2
        CASE "2"
          Deductions.Modify = TRUE
          EXIT DO
        CASE "D"
          'Call Delete FUNCTION, which will return TRUE if deleted
          IF Deductions.Delete(Deduct) THEN
            Deductions.Modify = TRUE
            EXIT FUNCTION
          END IF
        CASE "Esc"
          Deduct = Old
          EXIT DO
      END SELECT
    END IF
  LOOP

END FUNCTION

SUB Deductions.Update (Deduct AS DeductionType, Top)
  SHARED Pref AS INITIALS
  DIM M$(4)

  LOCATE Top + 1, 1
  PRINT "                  Deduction is currently : ";
    M$(1) = "Active  "
    M$(2) = "Inactive"
    PRINT M$(Deduct.Active)
  PRINT "                          Deduction Name : "; Deduct.DName
  PRINT "                          Deduction Type : ";
    M$(1) = "Fixed Weekly Deduction         "
    M$(2) = "Fixed until Specified Amount   "
    M$(3) = "Variable Weekly Deduction      "
    M$(4) = "Variable until Specified Amount"
    PRINT M$(Deduct.DType)
  PRINT "                            Deduction is : ";
    M$(1) = "Taxable    "
    M$(2) = "Not Taxable"
    PRINT M$(Deduct.Taxable)
  PRINT "     Amount deduction should continue to :";
    IF Deduct.DType = 1 OR Deduct.DType = 3 THEN
      PRINT "   N / A  "
    ELSE
      PRINT USING "$$####.##"; Deduct.Total
    END IF
  PRINT "                   Deduction amount type : ";
    M$(1) = "Dollar Value       "
    M$(2) = "Percentage of Gross"
    M$(3) = "Percentage of Net  "
    PRINT M$(Deduct.AType)
  PRINT "                     Amount of Deduction :  ";
    PRINT USING "####.##"; Deduct.Amount
  PRINT "                  Maximum deduction type : ";
    M$(1) = "Dollar Value       "
    M$(2) = "Percentage of Gross"
    M$(3) = "Percentage of Net  "
    PRINT M$(Deduct.LType)
  PRINT "                       Maximum Deduction :  ";
  PRINT USING "####.##"; Deduct.Limit
    
END SUB

'Employee.Add_______________________________________________________________SUB
'  SubProgram to add a new employee
'
SUB Employee.Add (EmployeeInfo AS EmployeeType)
  DIM Deduct(4) AS DeductionType

  'Reset Variables for new employee
 
  EmployeeInfo.Active = TRUE
  EmployeeInfo.Number = 0
  EmployeeInfo.FileName = ""
  EmployeeInfo.Hired = LEFT$(DATE$, 6) + RIGHT$(DATE$, 2)
  EmployeeInfo.EmpName = ""
  EmployeeInfo.Address = ""
  EmployeeInfo.Town = ""
  EmployeeInfo.State = ""
  EmployeeInfo.Zip = ""
  EmployeeInfo.Social = ""
  EmployeeInfo.USHolding = 1
  EmployeeInfo.USAllowances = 0
  EmployeeInfo.OHHolding = 1
  EmployeeInfo.OHAllowances = 0
  EmployeeInfo.EmpType = 1
  EmployeeInfo.Wage = 0
  EmployeeInfo.Note = ""
  FOR i = 1 TO 4
     Deduct(i).Active = 0
     Deduct(i).DType = 0
     Deduct(i).DName = ""
     Deduct(i).Taxable = 1
     Deduct(i).Amount = 0
     Deduct(i).AType = 0
     Deduct(i).Limit = 0
     Deduct(i).LType = 0
     Deduct(i).Total = 0
  NEXT
  EmployeeInfo.Deduction1 = Deduct(1)
  EmployeeInfo.Deduction2 = Deduct(2)
  EmployeeInfo.Deduction3 = Deduct(3)
  EmployeeInfo.Deduction4 = Deduct(4)

  CALL Employee.Modify(TRUE, EmployeeInfo)

END SUB

'Employee.Modify____________________________________________________________SUB
'              Changes Employee Information
'
SUB Employee.Modify (New, EmployeeInfo AS EmployeeType)
  SHARED Pref AS INITIALS
  DIM TempInfo AS EmployeeType
  DIM M$(4)

  Top = 3
  NextFlag = TRUE

  CALL Employee.Update(EmployeeInfo, Top)

  DO

    ' Set NextFlag and continue editing each field.
    ' NextFlag is cleared when the user presses ENTER.
    DO

      IF NextFlag = FALSE THEN EXIT DO
     
      'Go to the Deduction Modification Screen on F3
      IF NextFlag = 61 THEN Changed = Changed OR Deductions(EmployeeInfo)

      IF NextFlag < 2 THEN CurField = CurField - NextFlag
      IF CurField < 1 THEN CurField = 15
      IF CurField > 15 THEN CurField = 1
      NextFlag = TRUE
    
      SELECT CASE CurField
        CASE 1
          M$(1) = "1~Active"
          M$(2) = "2~Inactive"
          DO
            '
            IF Pref.Click THEN CALL Click
            CALL HighLight(Top + CurField, 34, 8)
            a$ = GetKey(FALSE)
            IF a$ = CHR$(13) OR a$ = " " THEN
              '
              IF Menu(Top + CurField, 34, M$(), 2) = "1" THEN
                EmployeeInfo.Active = TRUE
              ELSE
                EmployeeInfo.Active = FALSE
              END IF
            END IF
            CALL Employee.Update(EmployeeInfo, Top)
          LOOP UNTIL a$ <> CHR$(13)
          NextFlag = ConvertKey(a$)
        CASE 2
          '
          LOCATE Top + CurField, 34
          EmployeeInfo.EmpName = EditString(EmployeeInfo.EmpName, 30, NextFlag)
        CASE 3
          LOCATE Top + CurField, 34
          EmployeeInfo.Address = EditString(EmployeeInfo.Address, 40, NextFlag)
        CASE 4
          LOCATE Top + CurField, 34
          EmployeeInfo.Town = EditString(EmployeeInfo.Town, 20, NextFlag)
        CASE 5
          LOCATE Top + CurField, 34
          EmployeeInfo.State = UCASE$(EditString(EmployeeInfo.State, 2, NextFlag))
          LOCATE Top + CurField, 34
          PRINT EmployeeInfo.State;
        CASE 6
          LOCATE Top + CurField, 34
          EmployeeInfo.Zip = EditString(EmployeeInfo.Zip, 5, NextFlag)
        CASE 7
          '
          LOCATE Top + CurField, 34
          EmployeeInfo.Social = EditString(EmployeeInfo.Social, 11, NextFlag)
        CASE 8
          M$(1) = "1~SINGLE "
          M$(2) = "2~MARRIED"
          DO
            '
            IF Pref.Click THEN CALL Click
            CALL HighLight(Top + CurField, 34, 7)
            a$ = GetKey(FALSE)
            IF a$ = CHR$(13) OR a$ = " " THEN
              '
              EmployeeInfo.USHolding = VAL(Menu(Top + CurField, 34, M$(), 2))
            END IF
            CALL Employee.Update(EmployeeInfo, Top)
          LOOP UNTIL a$ <> CHR$(13)
          NextFlag = ConvertKey(a$)
        CASE 9
          '
          LOCATE Top + CurField, 34
          EmployeeInfo.USAllowances = EditNum(CSNG(EmployeeInfo.USAllowances), 2, 0, NextFlag)
        CASE 10
          M$(1) = "1~SINGLE "
          M$(2) = "2~MARRIED"
          DO
            '
            IF Pref.Click THEN CALL Click
            CALL HighLight(Top + CurField, 34, 7)
            a$ = GetKey(FALSE)
            IF a$ = CHR$(13) OR a$ = " " THEN
              '
              EmployeeInfo.OHHolding = VAL(Menu(Top + CurField, 34, M$(), 2))
            END IF
            CALL Employee.Update(EmployeeInfo, Top)
          LOOP UNTIL a$ <> CHR$(13)
          NextFlag = ConvertKey(a$)
        CASE 11
          '
          LOCATE Top + CurField, 34
          EmployeeInfo.OHAllowances = EditNum(CSNG(EmployeeInfo.OHAllowances), 2, 0, NextFlag)
        CASE 12
          M$(1) = "1~HOURLY"
          M$(2) = "2~SALARY"
          M$(3) = "*3~COMMISION"
          DO
            '
            IF Pref.Click THEN CALL Click
            CALL HighLight(Top + CurField, 34, 10)
            a$ = GetKey(FALSE)
            IF a$ = CHR$(13) OR a$ = " " THEN
              '
              EmployeeInfo.EmpType = VAL(Menu(Top + CurField, 34, M$(), 3))
            END IF
            CALL Employee.Update(EmployeeInfo, Top)
          LOOP UNTIL a$ <> CHR$(13)
          NextFlag = ConvertKey(a$)
        CASE 13
          '
          LOCATE Top + CurField, 35
          EmployeeInfo.Wage = EditNum(EmployeeInfo.Wage, 4, 2, NextFlag)
        CASE 14
          LOCATE Top + CurField, 34
          EmployeeInfo.Hired = EditString(EmployeeInfo.Hired, 8, NextFlag)
        CASE 15
          '
          LOCATE Top + CurField + 2, 1
          EmployeeInfo.Note = EditString(EmployeeInfo.Note, 80, NextFlag)
      END SELECT
    LOOP

    'Determine whether there is enough information
    NV = FALSE
    IF NOT Valid(EmployeeInfo.EmpName) THEN
      NV = TRUE
      CALL Pointer(Top + 2, 31)
     
      ' Position Field________________
      CurField = 2
    END IF
    IF (EmployeeInfo.EmpType = 1 OR EmployeeInfo.EmpType = 3) AND EmployeeInfo.Wage = 0 THEN
      NV = TRUE
      CALL Pointer(Top + 13, 31)
     
      ' Position Field (make sure first wrong field________________
      IF Valid(EmployeeInfo.EmpName) THEN CurField = 13
    END IF
    IF NV THEN
      ClearLine 22, 25
      Center 22, "The information indicated by " + CHR$(34) + ">" + CHR$(34) + " must be entered!", 14, 4
        Center 24, "Press Esc to Exit WITHOUT Saving now, any other key to continue editing. ", 14, Pref.Back
      a$ = GetKey(TRUE)
      IF a$ = CHR$(27) THEN
        EXIT DO
      ELSE
        ClearLine 22, 25
        CALL Employee.Update(EmployeeInfo, Top)
        NextFlag = 2
      END IF
    ELSE
      R = 0
      OPEN "EMPLOYEE.DAT" FOR RANDOM AS 1 LEN = LEN(EmployeeInfo)
        DO
          R = R + 1
          GET 1, R, TempInfo
        LOOP WHILE Valid(TempInfo.EmpName)
        Found = FALSE
        i = 0
        DO
          i = i + 1
          GET #1, i, TempInfo
          IF TempInfo.EmpName = EmployeeInfo.EmpName THEN Found = TRUE
          IF Found AND TempInfo.Number = EmployeeInfo.Number THEN Found = FALSE
        LOOP UNTIL i = R OR Found
      CLOSE 1
      IF NOT Found THEN
        CALL ClearLine(22, 25)
        IF Changed THEN
          LOCATE 23, 6
          COLOR 30, 4
          PRINT "ATTENTION: ";
          COLOR 14
          PRINT "The deductions have been modified, and in order for them"
          LOCATE , 17
          PRINT "to be saved you must choose to Save Changes to Employee.";
        END IF
        M$(1) = "1~Continue Editing"
        M$(2) = "2~Save Changes to Employee"
        M$(3) = "Esc~Exit and Discard Changes"
        SELECT CASE Menu(12, 45, M$(), 3)
          CASE "2"
            CALL SaveEmployeeData(New, EmployeeInfo)
            EXIT DO
          CASE "1"
            CALL Employee.Update(EmployeeInfo, Top)
            NextFlag = 2
          CASE "Esc"
            EXIT DO
        END SELECT
      ELSE
        ClearLine 22, 25
        Center 22, "This Employee's name is already in use by another Employee", 14, 4
        Center 24, "Press Esc to Exit WITHOUT Saving now, any other key to continue editing. ", 14, Pref.Back
        a$ = GetKey(TRUE)
        IF a$ = CHR$(27) THEN
          EXIT DO
        ELSE
          ClearLine 22, 25
          CurField = 2
          CALL Employee.Update(EmployeeInfo, Top)
          NextFlag = 2
        END IF
      END IF
    END IF
  LOOP

END SUB

SUB Employee.Update (Info AS EmployeeType, Top)
  SHARED Pref AS INITIALS

              
  LOCATE Top + 1, 1
  PRINT "         Employee is Currently : ";
    IF Info.Active THEN
      PRINT "Active  "
    ELSE
      PRINT "Inactive"
    END IF
  PRINT "   Employee Name for PRINT-OUT : "; Info.EmpName
  PRINT "              Employee Address : "; Info.Address
  PRINT "                          Town : "; Info.Town
  PRINT "                         State : "; Info.State
  PRINT "                           Zip : "; Info.Zip
  PRINT "        Social Security Number : "; Info.Social
  PRINT "              U.S. Withholding : ";
    SELECT CASE Info.USHolding
      CASE 1
        PRINT "SINGLE "
      CASE 2
        PRINT "MARRIED"
    END SELECT
  PRINT "   U.S. Withholding Allowances : ";
    PRINT USING "##"; Info.USAllowances
  PRINT "              Ohio Withholding : ";
    SELECT CASE Info.OHHolding
      CASE 1
        PRINT "SINGLE "
      CASE 2
        PRINT "MARRIED"
    END SELECT
  PRINT "   Ohio Withholding Allowances : ";
    PRINT USING "##"; Info.OHAllowances
  PRINT "                 Employee Type : ";
    SELECT CASE Info.EmpType
      CASE 1
        PRINT "HOURLY    "
      CASE 2
        PRINT "SALARY    "
      CASE 3
        PRINT "COMMISSION"
    END SELECT
  PRINT "                 Employee Wage : $";
    PRINT USING "####.##"; Info.Wage
  PRINT "           Date Employee Hired : "; Info.Hired
  PRINT
  PRINT STRING$(31, 196); "´ Enter Note Here Ã"; STRING$(30, 196);
  PRINT Info.Note
  PRINT STRING$(80, 196);
  CALL Center(22, "Press Esc or F6 to Save or Cancel Editing of Employee", 14, 4)
  CALL Center(23, "Press F3 to Add Extra Deductions", 14, 4)

END SUB

SUB EmployeeSetup
  SHARED Pref AS INITIALS
  DIM EmployeeInfo AS EmployeeType
  DIM M$(4)

  DO
    CALL Header("Employee Setup", TRUE, TRUE)
  
    M$(1) = "1~Add New Employee"
    M$(2) = "2~Change Employee Information"
    M$(3) = "3~Permanently Erase an Employee"
    M$(4) = "Esc~Return to Main Menu"
    a$ = Menu(5, 0, M$(), 4)

    SELECT CASE a$
      CASE "1"
        CALL Header("Employee Setup: Add New Employee", TRUE, TRUE)
        CALL Panel(1, Pref.Fore, Pref.Back, 4, 1, 25, 80, 0)
        CALL Employee.Add(EmployeeInfo)
        CALL Panel(2, Pref.Fore, Pref.Back, 4, 1, 25, 80, 0)
      CASE "2"
        DO
          CALL Header("Employee Setup: Modify Employee", TRUE, TRUE)
          Number = 0
          CALL ChooseEmployee(Number, FALSE, FALSE, "")
          IF Number THEN
            CALL LoadEmployeeData(Number, EmployeeInfo)
            CALL Panel(1, Pref.Fore, Pref.Back, 4, 1, 25, 80, 0)
            CALL Employee.Modify(FALSE, EmployeeInfo)
            CALL Panel(2, Pref.Fore, Pref.Back, 4, 1, 25, 80, 0)
          END IF
        LOOP WHILE Number
      CASE "3"
        CALL DropEmployee
    END SELECT
  LOOP UNTIL a$ = "Esc"

END SUB

'LoadEmployeeData__________________________________________________________SUB
' Loads the Employee Information
'
SUB LoadEmployeeData (Number, Info AS EmployeeType)

  CALL PleaseWait
  OPEN "EMPLOYEE.DAT" FOR RANDOM AS 1 LEN = LEN(Info)
    GET 1, Number, Info
  CLOSE 1
 
END SUB

'SaveEmployeeData__________________________________________________________SUB
'  Saves Employee Information in (Ref$).PAY
'  (New) specifies whether (Ref$) is to be added to ROSTER.PAY
'
SUB SaveEmployeeData (New, Info AS EmployeeType)
  DIM TempInfo AS EmployeeType

  CALL PleaseWait

  IF New THEN
    ' Declare which characters are valid in the file name
    ValidName$ = "ABCDEFGJHIJKLMNOPQRSTUVWXYZ0123456789"
   
    ' Strip the employee name of all invalid characters
    FOR i = 1 TO LEN(Info.EmpName)
      IF INSTR(ValidName$, UCASE$(MID$(Info.EmpName, i, 1))) THEN
        Temp$ = Temp$ + UCASE$(MID$(Info.EmpName, i, 1))
      END IF
    NEXT i

    IF LEN(Temp$) < 8 THEN
      Temp$ = Temp$ + STRING$(8 - LEN(Temp$), "_")
    END IF
   
    ' Put 8 character file name into Temp$
    Temp$ = LEFT$(Temp$, 8)

    R = 0
    OPEN "EMPLOYEE.DAT" FOR RANDOM AS 1 LEN = LEN(Info)
      DO
        R = R + 1
        GET 1, R, TempInfo
      LOOP WHILE Valid(TempInfo.EmpName)
      DO
        Found = FALSE
        i = 0
        DO
          i = i + 1
          GET #1, i, TempInfo
          IF TempInfo.FileName = Temp$ THEN Found = TRUE
        LOOP UNTIL i = R OR Found
        IF Found THEN
          F = F + 1
          Temp$ = LEFT$(Temp$, 7) + LTRIM$(RTRIM$(STR$(F)))
        END IF
      LOOP WHILE Found
      Info.FileName = Temp$
    CLOSE 1
    Info.Number = R
    OPEN "EMPLOYEE.DAT" FOR RANDOM AS 1 LEN = LEN(Info)
      GET 1, Info.Number, TempInfo
      PUT 1, Info.Number + 1, TempInfo
    CLOSE 1
  END IF

  OPEN "EMPLOYEE.DAT" FOR RANDOM AS 1 LEN = LEN(Info)
    PUT 1, Info.Number, Info
  CLOSE 1

END SUB

SUB Status (D AS DeductionType, New)
  SHARED Pref AS INITIALS
  DIM M$(4)

  IF New THEN
    t$ = "Current"
    x = 40
  ELSE
    t$ = "Old"
    x = 1
  END IF
  LOCATE 15, x
  COLOR 14
  PRINT t$
  COLOR Pref.Fore
  M$(1) = "Active  "
  M$(2) = "Inactive"
  LOCATE , x
  PRINT "Active : "; M$(D.Active)
  LOCATE , x
  PRINT "Name   : "; D.DName
  M$(1) = "Fixed Weekly Deduction   "
  M$(2) = "Fixed until Amount       "
  M$(3) = "Variable Weekly Deduction"
  M$(4) = "Variable until Amount    "
  LOCATE , x
  PRINT "Type   : "; M$(D.DType)
  LOCATE , x
  PRINT "Total  : ";
    IF D.DType = 2 OR D.DType = 4 THEN
      PRINT USING "#####.##"; D.Total;
      CALL Status2(1)
      ELSE
      PRINT "    N / A       "
    END IF
  LOCATE , x
  PRINT "Amount : ";
    PRINT USING "#####.##"; D.Amount;
    CALL Status2(D.AType)
  LOCATE , x
  PRINT "Limit  : ";
    PRINT USING "#####.##"; D.Limit;
    CALL Status2(D.LType)

END SUB

DEFSNG A-Z
SUB Status2 (a AS INTEGER)

  SELECT CASE a
    CASE 1
      PRINT " Dollars"
    CASE 2
      PRINT " % Gross"
    CASE 3
      PRINT " % Net  "
    CASE ELSE
      PRINT "        "
  END SELECT

END SUB

