DECLARE SUB NewDisk ()
DECLARE SUB ClearLine (y%, z%)
DECLARE SUB Header (Title$, Company%, UseDate%)
DECLARE SUB Center (Row%, Text$, Fore%, Back%)
DECLARE SUB HighLight (y%, x%, Length%)
DECLARE SUB PleaseWait ()
DECLARE SUB Pause (Alert%)
DECLARE SUB Hint (H$)
DECLARE SUB Bounce.Pref ()
DECLARE SUB RememberScreen (Opt%, Page%, y1%, x1%, y2%, x2%)
DECLARE SUB BackupSetup ()
DECLARE SUB BackupSetup.Update (x%, y%)
DECLARE SUB Backup ()
DECLARE SUB OldRecords ()
DECLARE SUB ChangeLastweek (INI AS ANY)
DECLARE SUB Payroll (INI AS ANY)
DECLARE FUNCTION Quit% (INI AS ANY)
DEFINT A-Z
DECLARE SUB DateAndTime ()
DECLARE SUB Disk ()
DECLARE SUB GetInputs (Old AS ANY, P AS ANY, Deductions() AS SINGLE)
DECLARE SUB MainMenu (Version$)
DECLARE SUB PayrollData (Mode%, RecNum%, Info AS ANY, FileName AS STRING, Year$)
DECLARE SUB PrintPayStub (P AS ANY, Week%, x%)
DECLARE SUB ScreenDisplay (P AS ANY)
DECLARE SUB LastUsed (Mode!, INI AS ANY)
DECLARE SUB Calculate (P1 AS ANY, Deductions() AS SINGLE, P2 AS ANY)
DECLARE SUB Template (Period AS STRING, Week%, Employee AS ANY)
DECLARE SUB Setup ()
DECLARE SUB PrintSetup ()
DECLARE SUB PrintSetup.Update (x%, y%)
DECLARE SUB CalculateDeduction (i%, D AS ANY, PD() AS ANY, P2 AS ANY, Deductions AS SINGLE)
DECLARE SUB DeductionWarn (i%, D AS ANY, PD() AS ANY, P2 AS ANY, Deductions AS SINGLE)
DECLARE SUB Panel (Opt%, Fore%, Back%, y1%, x1%, y2%, x2%, n%)
DECLARE SUB ChooseEmployee (Number%, Auto%, ActiveOnly%, Done AS STRING)
DECLARE SUB EmployeeSetup ()
DECLARE SUB LoadEmployeeData (Number%, Info AS ANY)

'$INCLUDE: 'INITIALS.BI'
'$INCLUDE: 'CAPSLOCK.BI'
'$INCLUDE: 'EDNUMBER.BI'
'$INCLUDE: 'EDSTRING.BI'
'$INCLUDE: 'HELPTOOL.BI'
'$INCLUDE: 'MENUTOOL.BI'
'$INCLUDE: 'SCRNSAVE.BI'
'$INCLUDE: 'SSBOUNCE.BI'
'$INCLUDE: 'TEXTTOOL.BI'
'$INCLUDE: 'TIMETOOL.BI'

'$INCLUDE: 'PAYTYPES.BI'

COMMON SHARED GlobalDeductionBonus AS INTEGER

CLEAR , , 13000

CONST FALSE = 0
CONST TRUE = NOT FALSE

GlobalDeductionBonus = FALSE


'ON ERROR GOTO Errors        ' -*********************************************************************


CALL PleaseWait


OPEN "MAIN.INI" FOR RANDOM AS 1 LEN = LEN(Pref)
  GET #1, 1, Pref
CLOSE

IF NOT Valid(Pref.Company) THEN
  Pref.SSActive = FALSE
  Pref.SSTimeOut = 5
  Pref.SSGraphics = TRUE
  Pref.SSChangeModes = TRUE
  Pref.SSPoints = 3
  Pref.SSTrails = 15
  Pref.SSMinChange = 300
  Pref.SSCubes = FALSE
  Pref.SSCubeWidth = 10
  Pref.SS2PointBox = FALSE

  Pref.Fore = 11
  Pref.Back = 1
  Pref.HiFore = 15
  Pref.HiBack = 4
  Pref.Entry = 15
  Pref.MenuFore = 0
  Pref.MenuBack = 7
  Pref.Click = FALSE
  Pref.Company = "Eagle Fabrication and Repair, INC."
  Pref.OldAmounts = TRUE

  Pref.ShowNegative = FALSE
  Pref.PrintDev = "LPT1:"
  Pref.PrinterType = 0
  Pref.BackupDev = "A:"
 
  OPEN "MAIN.INI" FOR RANDOM AS 1 LEN = LEN(Pref)
    PUT #1, 1, Pref
  CLOSE
END IF

Pref.Click = FALSE
Pref.Entry = 15

CONST Version$ = "2.46  (01-11-2009)"
'New in v2.46: 2009 Federal Tax Codes (01-11-2009), Publication 15
'              2009 Ohio State Tax Codes (01-11-2009)
'New in v2.45: 2008 Federal Tax Codes (04-13-2008), Publication 15
'              2008 Ohio State Tax Codes (04-13-2008)
'New in v2.44: 2007 Federal Tax Codes (02-04-2007), Publication 15
'              2007 Ohio State Tax Codes (02-04-2007)
'New in v2.43: 2006 Federal Tax Codes (01-15-2006), Publication 15
'              2006 Ohio State Tax Codes (01-15-2006)
'New in v2.42: 2005 Federal Tax Codes (01-09-2005), Publication 15
'New in v2.41: 2004 Federal Tax Codes (02-20-2004), Publication 15
'New in v2.40: 2003 Federal Tax Codes (07-20-2003), Publication 15-T
'New in v2.39: 2003 Federal Tax Codes (01-11-2003), new SSLimit, bonus mode
' reduced to 10% taxable gross for federal
'New in v2.38: 2002 Federal Tax Codes (01-20-2002)
'New in v2.37: 2001 Federal Tax Law changed
'New in v2.36: 2001 Federal Tax Codes (01-01-2001)
'New in v2.35: 2000 Federal Tax Codes (01-05-2000)
'New in v2.34: Stop Social Security deduction after base wage exceeded.
'New in v2.33: 1999 Federal Tax Codes
'New in v2.32: Year selection for old records.


SCREEN 0, 1, 0, 0
WIDTH 80, 25
COLOR Pref.Fore, Pref.Back
CLS

'LOCATE 13, 35
'PRINT "press a key"
'a$ = GetKey$(FALSE)

LOCATE 13, 35
PRINT "           "


 
  CALL MainMenu(Version$)
  OPEN "MAIN.INI" FOR RANDOM AS 1 LEN = LEN(Pref)
    PUT #1, 1, Pref
  CLOSE
 
  ' Backup
  m$(1) = "Y~Backup Files to " + LTRIM$(RTRIM$(Pref.BackupDev))
  m$(2) = "N~Skip Backup"
  a$ = Menu(13, 0, m$(), 2)

  IF a$ = "Y" THEN CALL Backup
 
  END



'_____________________________________________________________________________
Errors:
StopPrint = 0
terminate = 0
IF ERR = 21 THEN
   Error$ = "Printer is out of Paper!  Press a Key When Corrected!"
ELSEIF ERR = 25 THEN
   Error$ = "Printer is Off-Line or not Connected!  Press a Key When Corrected!"
ELSEIF ERR = 70 THEN
   Error$ = "Disk is Write-Protected!  Press a Key When Corrected!"
ELSE
   Error$ = "Error #" + STR$(ERR) + " -- Program Halted."
   terminate = 1
END IF

CALL RememberScreen(1, Page%, 1, 1, 25, 80)

ClearLine 1, 1
COLOR 14, 4
PRINT Error$;
COLOR Pref.Fore, Pref.Back
IF terminate = 1 THEN END
a$ = GetKey(TRUE)
IF a$ = CHR$(0) + CHR$(117) THEN StopPrint = 1

CALL RememberScreen(2, Page%, 1, 1, 25, 80)

COLOR Pref.Fore, Pref.Back
RESUME

'Backup____________________________________________________________________SUB
'      Creates a backup of all files in current directory
'
SUB Backup
SHARED Pref AS INITIALS
 
  CALL Panel(1, Pref.Fore, Pref.Back, 1, 1, 25, 80, 0)
  CLS
  PRINT "Continuing the backup will overwrite all files in the backup"
  PRINT "directory specified in `System Setup.'"
  PRINT "If the files are corrupt now, DO NOT BACKUP now, but RESTORE them."
  PRINT "Do you wish to continue the backup? ";
  Continue$ = UCASE$(GetKey(TRUE))
 
  IF Continue$ = "Y" THEN
    PRINT Continue$
    PRINT
    PRINT "Now Backing-Up Files to " + RTRIM$(LTRIM$(Pref.BackupDev))
'    SHELL "MD " + RTRIM$(LTRIM$(Pref.BackupDev))
'    SHELL "COPY *.* " + RTRIM$(LTRIM$(Pref.BackupDev)) + " /V /Y"
    SHELL "PKZIP -uex " + RTRIM$(LTRIM$(Pref.BackupDev)) + "\PAY2.BAK *.*"
  END IF
  CALL Panel(2, Pref.Fore, Pref.Back, 1, 1, 25, 80, 0)

END SUB

SUB BackupSetup
  SHARED Pref AS INITIALS
  DIM m$(1 TO 2)

  x = 5
  y = 12
  CALL Panel(1, Pref.Fore, Pref.Back, y - 1, x - 1, y + 3, x + 70, 3)
  CALL BackupSetup.Update(x, y)


  ' Set NextFlag and continue editing each field.
  ' NextFlag is cleared when the user presses ENTER.
  NextFlag = TRUE
  DO
    IF NextFlag = FALSE THEN EXIT DO

    IF NextFlag < 1 THEN CurField = CurField - NextFlag
    IF CurField < 1 THEN CurField = 1
    IF CurField > 1 THEN CurField = 1
    NextFlag = TRUE

    SELECT CASE CurField
      CASE 1
        LOCATE y + 1, x + 41
        Pref.BackupDev = EditString(Pref.BackupDev, 28, NextFlag)
          CALL BackupSetup.Update(x, y)
    END SELECT
  LOOP

  CALL Panel(2, Pref.Fore, Pref.Back, y - 1, x - 1, y + 8, 70, 1)

END SUB

SUB BackupSetup.Update (x, y)
  SHARED Pref AS INITIALS

  LOCATE y + 1, x
  PRINT "Drive and Directory for Backup/Restore : "; Pref.BackupDev

END SUB

'Calculations______________________________________________________________SUB
'Calculates Payroll
'
SUB Calculate (P1 AS PayrollType, Deductions() AS SINGLE, P2 AS PayrollType)
  DIM D(1 TO 4) AS DeductionType
  DIM PD(1 TO 2, 1 TO 4) AS PayrollDeductionType
  DIM PDTemp(1 TO 2) AS PayrollDeductionType
  DIM tw AS SINGLE
  DIM TaxableGross AS SINGLE
  DIM PreTaxTotalDeductions AS SINGLE
  DIM SSLimit AS DOUBLE
  CONST PP = 52

  P2.TotalDeduct = 0

  IF P2.Employee.EmpType <> 2 THEN
     P2.RegularAMT = Round(P2.RegularHRS * P2.Employee.Wage, 2)
  END IF
  P2.OvertimeAMT = Round(P2.OvertimeHRS * P2.Employee.Wage * 1.5, 2)
  P2.HolidayAMT = Round(P2.HolidayHRS * P2.Employee.Wage, 2)
  P2.VacationAMT = Round(P2.VacationHRS * P2.Employee.Wage, 2)
  P2.Gross = P2.RegularAMT + P2.OvertimeAMT + P2.HolidayAMT + P2.VacationAMT
    P2.Gross = P2.Gross + P2.BonusAMT
  P2.GrossYTD = P1.GrossYTD + P2.Gross

  P2.RegYTDHRS = P1.RegYTDHRS + P2.RegularHRS
  P2.OvrYTDHRS = P1.OvrYTDHRS + P2.OvertimeHRS
  P2.HolYTDHRS = P1.HolYTDHRS + P2.HolidayHRS
  P2.VacYTDHRS = P1.VacYTDHRS + P2.VacationHRS
  P2.RegYTDAMT = P1.RegYTDAMT + P2.RegularAMT
  P2.OvrYTDAMT = P1.OvrYTDAMT + P2.OvertimeAMT
  P2.HolYTDAMT = P1.HolYTDAMT + P2.HolidayAMT
  P2.VacYTDAMT = P1.VacYTDAMT + P2.VacationAMT
  P2.BonusYTD = P1.BonusYTD + P2.BonusAMT

  ' Begin Extra Deductions  --  Non Taxable Done before Tax Calculation_______
  PD(1, 1) = P1.Deduction1
  PD(1, 2) = P1.Deduction2
  PD(1, 3) = P1.Deduction3
  PD(1, 4) = P1.Deduction4
  PD(2, 1) = P2.Deduction1
  PD(2, 2) = P2.Deduction2
  PD(2, 3) = P2.Deduction3
  PD(2, 4) = P2.Deduction4
  D(1) = P2.Employee.Deduction1
  D(2) = P2.Employee.Deduction2
  D(3) = P2.Employee.Deduction3
  D(4) = P2.Employee.Deduction4

  'Clear all inactive deductions to = 0 * * * * * * * * * * * * * * * * * *
  FOR i = 1 TO 4
    IF D(i).Active <> 1 THEN
        PD(2, i).Amount = 0
    END IF
  NEXT
 
 
  FOR i = 1 TO 4
    IF D(i).Active = 1 AND D(i).Taxable = 2 THEN
  
      PDTemp(1) = PD(1, i)
      PDTemp(2) = PD(2, i)

      CALL CalculateDeduction(i, D(i), PDTemp(), P2, Deductions(i))
    
      PD(1, i) = PDTemp(1)
      PD(2, i) = PDTemp(2)
  
      PreTaxTotalDeductions = PreTaxTotalDeductions - PD(2, i).Amount
    END IF
   
  NEXT ' End of Non Taxable Deductions________________________________________


  TaxableGross = P2.Gross + PreTaxTotalDeductions

  ' Calculate Social Security and Medicare from TRUE Gross Pay _____
  ' P2.Social = -Round(TaxableGross * .062, 2)
  ' P2.Medicare = -Round(TaxableGross * .0145, 2)
   
    P2.Medicare = -Round(P2.Gross * .0145, 2)
   
    'Social security has a wage base limit.  First we check to see if gross
    'wages are above the limit, and then check to see if this is the pay
    'period that puts is above the limit

    ' 2009-01-11 - SSLimit increased to 106800 from 102000
    ' 2008-04-13 - SSLimit increased to 102000 from 97500
    ' 2007-02-04 - SSLimit increased to 97500 from 94200
    ' 2006-01-15 - SSLimit increased to 94200 from 90000
    ' 2005-01-09 - SSLimit increased to 90000 from 87900
    ' 2004-02-20 - SSLimit increased to 87900 from 87000
    ' 2003-01-11 - SSLimit increased to 87000 from 84900
    SSLimit = 106800
    IF P2.GrossYTD < SSLimit THEN
        'Still below the limit, take SS tax on all of it
        P2.Social = -Round(P2.Gross * .062, 2)
    ELSE
        IF P2.GrossYTD - P2.Gross > SSLimit THEN
            'Over the limit before this pay period
            P2.Social = 0
        ELSE
            'went over limit this pay period
            P2.Social = -Round((SSLimit - (P2.GrossYTD - P2.Gross)) * .062, 2)
        END IF
    END IF
  ' ________________________________________________________________

  P2.SocialYTD = P1.SocialYTD + P2.Social
  P2.MedicareYTD = P1.MedicareYTD + P2.Medicare
  P2.TotalDeduct = P2.TotalDeduct + P2.Social + P2.Medicare
 
  ' State Withholding -----------------------------------------Calculation--
  CONST StateMultiplier = 1.276

'  IF P2.Employee.OHAllowances = 15 THEN
  IF GlobalDeductionBonus = TRUE THEN
     
      ' For special tax rate, use 3.5% Taxable Gross
      P2.OHWH = .035 * TaxableGross
  ELSE
      tw = TaxableGross * PP - (650 * P2.Employee.OHAllowances)
      SELECT CASE tw
        CASE IS < 0
          P2.OHWH = 0
        CASE IS <= 5000
          P2.OHWH = (((tw * .005) / PP) * StateMultiplier)
        CASE IS <= 10000
          P2.OHWH = ((((tw - 5000) * .01 + 25) / PP) * StateMultiplier)
        CASE IS <= 15000
          P2.OHWH = ((((tw - 10000) * .02 + 75) / PP) * StateMultiplier)
        CASE IS <= 20000
          P2.OHWH = ((((tw - 15000) * .025 + 175) / PP) * StateMultiplier)
        CASE IS <= 40000
          P2.OHWH = ((((tw - 20000) * .03 + 300) / PP) * StateMultiplier)
        CASE IS <= 80000
          P2.OHWH = ((((tw - 40000!) * .035 + 900) / PP) * StateMultiplier)
        CASE IS <= 100000
          P2.OHWH = ((((tw - 80000!) * .04 + 2300) / PP) * StateMultiplier)
        CASE IS > 100000
          P2.OHWH = ((((tw - 100000!) * .05 + 3100) / PP) * StateMultiplier)
      END SELECT
  END IF
  P2.OHWH = -Round(P2.OHWH, 2)
       
  ' Federal Withholding ---------------------------------------Calculation--
 
  ' tax codes Rev. JAN 2009

  IF GlobalDeductionBonus = TRUE THEN
     
      ' For special tax rate, use 15% Taxable Gross
      'P2.USWH = .15 * TaxableGross
      ' 2003-01-11 Reduced to 10% taxable gross
      P2.USWH = .1 * TaxableGross

  ELSE
      tw = TaxableGross - 70.19 * P2.Employee.USAllowances
      IF P2.Employee.USHolding = 1 THEN
        ' Single Employee -----------------------------------------Calculation
        SELECT CASE tw
          CASE IS <= 51
            P2.USWH = 0
          CASE IS <= 200
            P2.USWH = ((tw - 51) * .1)
          CASE IS <= 681
            P2.USWH = (((tw - 200) * .15) + 14.9)
          CASE IS <= 1621
            P2.USWH = (((tw - 681) * .25) + 87.05)
          CASE IS <= 3338
            P2.USWH = (((tw - 1621) * .28) + 322.05)
          CASE IS <= 7212
            P2.USWH = (((tw - 3338) * .33) + 802.81)
          CASE IS > 7212
            P2.USWH = (((tw - 7212) * .35) + 2081.23)
        END SELECT
      ELSEIF P2.Employee.USHolding = 2 THEN
        ' Married Employee ----------------------------------------Calculation
        SELECT CASE tw
          CASE IS <= 154
            P2.USWH = 0
          CASE IS <= 461
            P2.USWH = ((tw - 154) * .1)
          CASE IS <= 1455
            P2.USWH = (((tw - 461) * .15) + 30.7)
          CASE IS <= 2785
            P2.USWH = (((tw - 1455) * .25) + 179.8)
          CASE IS <= 4165
            P2.USWH = (((tw - 2785) * .28) + 512.3)
          CASE IS <= 7321
            P2.USWH = (((tw - 4165) * .33) + 898.7)
          CASE IS > 7321
            P2.USWH = (((tw - 7321) * .35) + 1940.18)
        END SELECT
      END IF
  END IF
  P2.USWH = -Round(P2.USWH, 2)

  'Calculations Continued -------------------------------------Calculation--
  P2.OHWHYTD = P1.OHWHYTD + P2.OHWH
  P2.USWHYTD = P1.USWHYTD + P2.USWH
  P2.TotalDeduct = P2.TotalDeduct + P2.USWH + P2.OHWH

  IF P2.Gross = 0 THEN P2.TotalDeduct = 0
  P2.NetPay = P2.Gross + P2.TotalDeduct

  ' Non Taxable Deduction Warnings
  FOR i = 1 TO 4
    IF D(i).Active = 1 AND D(i).Taxable = 2 THEN
 
      PDTemp(1) = PD(1, i)
      PDTemp(2) = PD(2, i)

      CALL DeductionWarn(i, D(i), PDTemp(), P2, Deductions(i))
   
      PD(1, i) = PDTemp(1)
      PD(2, i) = PDTemp(2)
 
    PD(2, i).Amount = -PD(2, i).Amount
    PD(2, i).YTD = PD(1, i).YTD + PD(2, i).Amount
    P2.TotalDeduct = P2.TotalDeduct + PD(2, i).Amount
  
    END IF
  
    P2.NetPay = P2.Gross + P2.TotalDeduct

  NEXT ' End of Non Taxable Deductions________________________________________

  ' Continue Extra Deductions -- Taxable Deductions___________________________
  FOR i = 1 TO 4
    IF D(i).Active = 1 AND D(i).Taxable = 1 THEN
   
      PDTemp(1) = PD(1, i)
      PDTemp(2) = PD(2, i)

      CALL CalculateDeduction(i, D(i), PDTemp(), P2, Deductions(i))
      CALL DeductionWarn(i, D(i), PDTemp(), P2, Deductions(i))
     
      PD(1, i) = PDTemp(1)
      PD(2, i) = PDTemp(2)
   
    PD(2, i).Amount = -PD(2, i).Amount
    PD(2, i).YTD = PD(1, i).YTD + PD(2, i).Amount
    P2.TotalDeduct = P2.TotalDeduct + PD(2, i).Amount
   
    END IF

    P2.NetPay = P2.Gross + P2.TotalDeduct
  NEXT
 
  P2.NetPayYTD = P1.NetPayYTD + P2.NetPay
  P2.TotalDeductYTD = P1.TotalDeductYTD + P2.TotalDeduct

  P1.Deduction1 = PD(1, 1)
  P1.Deduction2 = PD(1, 2)
  P1.Deduction3 = PD(1, 3)
  P1.Deduction4 = PD(1, 4)
  P2.Deduction1 = PD(2, 1)
  P2.Deduction2 = PD(2, 2)
  P2.Deduction3 = PD(2, 3)
  P2.Deduction4 = PD(2, 4)
  P2.Employee.Deduction1 = D(1)
  P2.Employee.Deduction2 = D(2)
  P2.Employee.Deduction3 = D(3)
  P2.Employee.Deduction4 = D(4)

  ' End Extra Deductions______________________________________________________

END SUB

'CalculateDeduction___________________________________________________________
SUB CalculateDeduction (i, D AS DeductionType, PD() AS PayrollDeductionType, P2 AS PayrollType, Deductions AS SINGLE)
     
  ' Fixed Deductions
  IF D.DType = 1 OR D.DType = 2 THEN
    SELECT CASE D.AType
      CASE 1
        PD(2).Amount = D.Amount
      CASE 2
        PD(2).Amount = P2.Gross * (D.Amount / 100)
      CASE 3
        PD(2).Amount = P2.NetPay * (D.Amount / 100)
    END SELECT
  END IF
  ' Variable Deductions
  IF D.DType = 3 OR D.DType = 4 THEN
    SELECT CASE D.AType
      CASE 1
        PD(2).Amount = Deductions
      CASE 2
        PD(2).Amount = P2.Gross * (Deductions / 100)
      CASE 3
        PD(2).Amount = P2.NetPay * (Deductions / 100)
    END SELECT
  END IF
      
  ' Round value due to calculations above that may use multiplication
  PD(2).Amount = Round(PD(2).Amount, 2)
      
END SUB

'ChangeLastweek____________________________________________________________SUB
'              Change Last Period's Date
'
SUB ChangeLastweek (INI AS LastTime)
  SHARED Pref AS INITIALS
  DIM OldINI AS LastTime
  DIM m$(1 TO 2)

  OldINI = INI
  CALL Header("Change Week Number", TRUE, FALSE)

  ClearLine 11, 25
  IF INI.Week = 0 THEN
    CALL Panel(1, Pref.Fore, Pref.Back, 12, 20, 18, 60, 3)
    LOCATE 14, 22
    PRINT "This feature can not be used because";
    LOCATE 15, 22
    PRINT "the WEEK NUMBER is currently set for"
    LOCATE 16, 22
    PRINT "the first week of the year."
    CALL Pause(TRUE)
    CALL Panel(2, Pref.Fore, Pref.Back, 12, 20, 18, 60, 3)
  ELSE
    PRINT "     The use of this function is to change last period's week number"
    PRINT "in order to correct wrong entries, wrong dates/weeks, or other errors."
    PRINT "     In normal operation, this should not be needed at any time."
    PRINT
    COLOR 30, 4
    PRINT TAB(10); "CAUTION:";
    COLOR 15, 4
    PRINT " ONCE THE WEEK NUMBER IS CHANGED BACK, IT CAN ONLY BE          ";
    PRINT SPC(18); "ADVANCED BY ENTERING EACH WEEK'S PAYSTUBS!                    "
    CALL Center(17, "WEEK NUMBER CAN NOT BE ADVANCED, ONLY REDUCED ! ! ", 14, 4)
    PRINT
    COLOR Pref.Fore, Pref.Back
    PRINT SPC(6); "If you wish to change date, hold 'Ctrl' and press 'End'"
    PRINT SPC(6); "Pressing any other key will abort.";
    a$ = GetKey(TRUE)
    IF a$ = CHR$(0) + CHR$(117) THEN
      CALL Hint("Edit Week Numberฏณ ENTER to Accept ณ Esc to Cancel")
      DO
        LOCATE 21, 1
        PRINT "Please enter the WEEK NUMBER for LAST period's entries.  The NEW"
        PRINT "entries will be for week (1 +    )";
        LOCATE 22, 31
        Abort = FALSE
        INI.Week = EditNum(ABS(INI.Week), 2, 0, Abort)

      LOOP UNTIL (INI.Week >= 0 AND INI.Week <= 52 AND INI.Week < ABS(OldINI.Week)) OR Abort
      IF Abort = TRUE THEN
        INI = OldINI
      ELSE
        CALL ClearLine(4, 25)
        CALL Panel(1, Pref.Fore, Pref.Back, 6, 20, 12, 60, 3)
        LOCATE 7, 22
        PRINT "Continuing will set Week Number from"
        LOCATE 8, 22
        PRINT "Week"; ABS(OldINI.Week); "to Week"; INI.Week; "and all Pay-stubs"
        LOCATE 9, 22
        PRINT "after week"; INI.Week; "will have to be manually"
        LOCATE 10, 22
        PRINT "re-entered otherwise YTD Values will"
        LOCATE 11, 22
        PRINT "become CORRUPT!"
       
        m$(1) = "Esc~DO NOT change Week Number"
        m$(2) = "Y~ACCEPT change to Week Number"
        a$ = Menu(14, 0, m$(), 2)
       
        CALL Panel(2, Pref.Fore, Pref.Back, 6, 20, 12, 60, 3)
       
        IF a$ = "Y" THEN
          CALL LastUsed(3, INI)
        ELSE
          INI = OldINI
          CALL ClearLine(4, 25)
          CALL Panel(1, Pref.Fore, Pref.Back, 9, 20, 13, 60, 3)
          LOCATE 11, 22
          PRINT "Week Number has NOT been changed."
          CALL Hint("Press any key to continue")
          CALL Pause(TRUE)
          CALL Panel(2, Pref.Fore, Pref.Back, 9, 20, 13, 60, 3)
        END IF
      END IF
    END IF
  END IF

END SUB

DEFSNG A-Z
'DateAndTime_______________________________________________________________SUB
'           Set System Date and Time Using DOS Commands
'
SUB DateAndTime

ClearLine 12, 25
FOR i = 1 TO 2
   IF i = 1 THEN
      change$ = "DATE"
   ELSE
      change$ = "TIME"
   END IF
LOCATE 13, 1
PRINT "You will now be able to set system "; change$; "."
PRINT "Press 'ENTER' to leave unchanged."
SHELL change$
ClearLine 13, 17
NEXT

END SUB

DEFINT A-Z
SUB DeductionWarn (i, D AS DeductionType, PD() AS PayrollDeductionType, P2 AS PayrollType, Deductions AS SINGLE)
  DIM OldAmount AS SINGLE

  OldAmount = PD(2).Amount
     
  ' Reset Flags
  ExceedTotal = FALSE
  ExceedLimit = FALSE
  ExceedNet = FALSE

  ' Make sure deduction does not exceed total for Deduct to Amount
  IF D.DType = 2 OR D.DType = 4 THEN
    IF PD(i).YTD + PD(2).Amount > D.Total THEN
      ExceedTotal = TRUE
      PD(2).Amount = D.Total - PD(i).YTD
    END IF
  END IF
    
  ' Determine if Limits exceeded
  SELECT CASE D.LType
    CASE 1
      IF PD(2).Amount > D.Limit THEN
        PD(2).Amount = D.Limit
        ExceedLimit = TRUE
      END IF
    CASE 2
      IF PD(2).Amount > P2.Gross * (D.Limit / 100) THEN
        PD(2).Amount = Round(P2.Gross * (D.Limit / 100), 2)
        ExceedLimit = TRUE
      END IF
    CASE 3
      IF PD(2).Amount > P2.NetPay * (D.Limit / 100) THEN
        PD(2).Amount = Round(P2.NetPay * (D.Limit / 100), 2)
        ExceedLimit = TRUE
      END IF
  END SELECT
 
  ' Do not exceed total NetPay to prevent a negative paycheck
  IF P2.Gross + (-PD(2).Amount + P2.TotalDeduct) < 0 THEN
    ExceedNet = TRUE
    PD(2).Amount = P2.NetPay
  END IF
 
  ' Pop-up alert panel - - Make sure to alert about most serious problem
  IF ExceedNet THEN
    SELECT CASE D.AType
      CASE 1
        Deductions = PD(2).Amount
      CASE 2
        Deductions = (PD(2).Amount / P2.Gross) * 100
      CASE 3
        Deductions = (PD(2).Amount / P2.NetPay) * 100
    END SELECT
    Alert1$ = "Deduction #" + LTRIM$(STR$(i)) + ": " + LEFT$(D.DName, StringLength(D.DName)) + " has exceeded the"
    Alert2$ = "Employee's total Net Pay."
    Alert3$ = "Deduction #" + LTRIM$(STR$(i)) + " has been reset to $" + RTRIM$(LTRIM$(STR$(PD(2).Amount))) + " to"
    Alert4$ = "prevent a negative paycheck."
  ELSEIF ExceedLimit THEN
    SELECT CASE D.AType
      CASE 1
        Deductions = PD(2).Amount
      CASE 2
        Deductions = (PD(2).Amount / P2.Gross) * 100
      CASE 3
        Deductions = (PD(2).Amount / P2.NetPay) * 100
    END SELECT
    Alert1$ = "Deduction #" + LTRIM$(STR$(i)) + ": " + LEFT$(D.DName, StringLength(D.DName)) + " has exceeded the"
    Alert2$ = "predefined maximum of" + Exceed$ + "."
    Alert3$ = "Deduction #" + LTRIM$(STR$(i)) + " has been reset to the maximum"
    Alert4$ = "of $" + RTRIM$(LTRIM$(STR$(PD(2).Amount))) + "."
  ELSEIF ExceedTotal THEN
    SELECT CASE D.AType
      CASE 1
        Deductions = PD(2).Amount
      CASE 2
        Deductions = (PD(2).Amount / P2.Gross) * 100
      CASE 3
        Deductions = (PD(2).Amount / P2.NetPay) * 100
    END SELECT
    Alert1$ = "Deduction #" + LTRIM$(STR$(i)) + ": " + LEFT$(D.DName, StringLength(D.DName)) + " has exceeded the"
    Alert2$ = "predefined total that the deduction should Continue to."
    Alert3$ = "Deduction #" + LTRIM$(STR$(i)) + " has been reset to equal the"
    Alert4$ = "predefined final amount of " + RTRIM$(LTRIM$(STR$(D.Total))) + "."
  END IF
     
  IF Round(OldAmount, 2) = Round(PD(2).Amount, 2) THEN
    PD(2).Amount = OldAmount
    ExceedLimit = FALSE
    ExceedTotal = FALSE
    ExceedNet = FALSE
  END IF
     
  IF ExceedLimit OR ExceedTotal OR ExceedNet THEN
    SOUND 3500, 1
    SOUND 32767, 1
    SOUND 3500, 1
    SOUND 32767, 1
    SOUND 3500, 1
       
    l = LEN(Alert1$)
    IF l < LEN(Alert2$) THEN l = LEN(Alert2$)
    IF l < LEN(Alert3$) THEN l = LEN(Alert3$)
    IF l < LEN(Alert4$) THEN l = LEN(Alert4$)
    R = l
    l = (80 - l) \ 2 - 2
    R = l + R + 3

    CALL Panel(1, 14, 4, 10, l, 20, R, 1)
    CALL Center(10, "ด EXCESSIVE DEDUCTION ร", 14, 4)
    COLOR 14, 4
    LOCATE 12, l + 2
    PRINT Alert1$
    LOCATE 13, l + 2
    PRINT Alert2$
    LOCATE 15, l + 2
    PRINT Alert3$
    LOCATE 16, l + 2
    PRINT Alert4$
    CALL Center(18, "Press any key to continue", 14, 4)
    CALL Pause(TRUE)
    CALL Panel(2, 14, 4, 10, l, 20, R, 1)
  END IF


END SUB

DEFSNG A-Z
'Disk______________________________________________________________________SUB
'    Menu for Disk Utilities
'
'
SUB Disk
  DIM m$(4)

  m$(1) = "1~Backup Data Files"
  m$(2) = "2~Create New Program Disk"
  m$(3) = "Esc~Return to Main Menu"

  DO
    a$ = Menu(6, 0, m$(), 3)

    SELECT CASE a$
      CASE "1"
        CALL Backup
      CASE "2"
        CALL NewDisk
    END SELECT
  LOOP UNTIL a$ = "Esc"

END SUB

DEFINT A-Z
'DropEmployee______________________________________________________________SUB
'            Removes (ex)employee's Reference name from ROSTER.PAY
'            Retains Reference in FULROSTR.PAY
'            Does NOT remove Employee Information file (Ref$).PAY
'
SUB DropEmployee
  SHARED Pref AS INITIALS
  DIM Info AS EmployeeType
  DIM TempInfo AS EmployeeType

  CALL Header("Employee Setup: ERASE Employee", TRUE, TRUE)
  CALL ChooseEmployee(Number, FALSE, FALSE, "")
  IF Number THEN
    CALL LoadEmployeeData(Number, Info)
    COLOR 14, 4
    LOCATE 5, 8
    PRINT "You are about to PERMANENTLY ERASE "; Info.EmpName
    LOCATE 7, 8
    PRINT "If you only want to remove the employee from the weekly payroll, "
    LOCATE , 8
    PRINT "but want the layed-off or no longer employeed Employee to remain "
    LOCATE , 8
    PRINT "available for later employment, then simply make the Employee    "
    LOCATE , 8
    PRINT "Inactive on the Modify Employee Screen. If the Employee is erased"
    LOCATE , 8
    PRINT "then the Employee will be removed from the Employee List, but the"
    LOCATE , 8
    PRINT "pay-stub records will still be available for reference.          "

    COLOR 14, Pref.Back
    LOCATE 14, 8
    PRINT "Do you really want to PERMANENTLY ERASE this Employee [Y/N] ? ";
    DO
      a$ = UCASE$(GetKey(TRUE))
    LOOP UNTIL a$ = "Y" OR a$ = "N"
    COLOR 14, Pref.Back
    PRINT a$
    IF a$ = "Y" THEN
      SOUND 3500, 1
      SOUND 32767, 1
      SOUND 3500, 1
      SOUND 32767, 1
      SOUND 3500, 1

      LOCATE 16, 8
      PRINT "ERASING this Employee can not be undone, continue [Y/N] ? ";
      DO
        a$ = UCASE$(GetKey(TRUE))
      LOOP UNTIL a$ = "Y" OR a$ = "N"
      COLOR 14, Pref.Back
      PRINT a$
      IF a$ = "Y" THEN
       
        last = 0
        OPEN "EMPLOYEE.DAT" FOR RANDOM AS 1 LEN = LEN(Info)
          DO
            last = last + 1
            GET 1, last, TempInfo
          LOOP WHILE Valid(TempInfo.EmpName)
          IF Number = last - 1 THEN
            GET 1, last, TempInfo
            PUT 1, (last - 1), TempInfo
          ELSEIF Number = 1 THEN
            FOR i = 1 TO last - 1
              GET 1, i + 1, TempInfo
              TempInfo.Number = i
              PUT 1, i, TempInfo
            NEXT
          ELSE
            FOR i = Number TO last - 1
              GET 1, i + 1, TempInfo
              TempInfo.Number = i
              PUT 1, i, TempInfo
            NEXT
          END IF
          TempInfo.EmpName = ""
          PUT 1, last, TempInfo
        CLOSE 1

        LOCATE 18, 8
        PRINT LEFT$(Info.EmpName, StringLength(Info.EmpName)); " has been ERASED."
        CALL Pause(TRUE)
      END IF
    END IF
  END IF

  CLS

END SUB

'GetInputs_________________________________________________________________SUB
'Gets user inputs for new Week's Payroll
'
SUB GetInputs (Old AS PayrollType, P AS PayrollType, Deductions() AS SINGLE)
  DIM DType(1 TO 4), AType(1 TO 4)
  DIM Active(1 TO 4)


  DType(1) = P.Employee.Deduction1.DType
  DType(2) = P.Employee.Deduction2.DType
  DType(3) = P.Employee.Deduction3.DType
  DType(4) = P.Employee.Deduction4.DType
  AType(1) = P.Employee.Deduction1.AType
  AType(2) = P.Employee.Deduction2.AType
  AType(3) = P.Employee.Deduction3.AType
  AType(4) = P.Employee.Deduction4.AType
  Active(1) = P.Employee.Deduction1.Active
  Active(2) = P.Employee.Deduction2.Active
  Active(3) = P.Employee.Deduction3.Active
  Active(4) = P.Employee.Deduction4.Active

  NextFlag = TRUE

  DO
    CALL Calculate(Old, Deductions(), P)
    CALL ScreenDisplay(P)

    IF NextFlag = FALSE THEN EXIT DO
    IF NextFlag < 2 THEN CurField = CurField - NextFlag
    IF CurField < 1 THEN CurField = 9
    ' Salaried Employee
    IF CurField > 1 AND CurField < 5 AND P.Employee.EmpType = 2 THEN CurField = CurField - NextFlag * 3
    ' Deductions
    DO
      Again = FALSE
      IF CurField > 5 AND CurField < 10 THEN
        IF Active(CurField - 5) = 0 OR DType(CurField - 5) < 3 THEN
          CurField = CurField - NextFlag
          Again = TRUE
        END IF
      END IF
    LOOP WHILE Again
    IF CurField > 9 THEN CurField = 1
    NextFlag = TRUE
   
    SELECT CASE CurField
      CASE 1
        IF P.Employee.EmpType = 2 THEN
          LOCATE 7, 28
          PRINT "$";
          P.RegularAMT = EditNum(P.RegularAMT, 5, 2, NextFlag)
          LOCATE 7, 28
          PRINT USING "$$####.##"; P.RegularAMT
        ELSE
          LOCATE 7, 21
          P.RegularHRS = EditNum(P.RegularHRS, 2, 2, NextFlag)
        END IF
      CASE 2
        LOCATE 8, 21
        P.OvertimeHRS = EditNum(P.OvertimeHRS, 2, 2, NextFlag)
      CASE 3
        LOCATE 9, 21
        P.VacationHRS = EditNum(P.VacationHRS, 2, 2, NextFlag)
      CASE 4
        LOCATE 10, 21
        P.HolidayHRS = EditNum(P.HolidayHRS, 2, 2, NextFlag)
      CASE 5
        LOCATE 11, 28
        PRINT "$";
        P.BonusAMT = EditNum(P.BonusAMT, 5, 2, NextFlag)
        LOCATE 11, 28
        PRINT USING "$$###.##"; P.BonusAMT
      CASE 6 TO 9
        LOCATE 7 + CurField, 71
        IF AType(CurField - 5) = 1 THEN
          PRINT "$";
          Deductions(CurField - 5) = EditNum(Deductions(CurField - 5), 4, 2, NextFlag)
        ELSE
          DO
            LOCATE 7 + CurField, 71
            IF AType(CurField - 5) = 2 THEN
              PRINT USING "%###.###G"; Deductions(CurField - 5)
            ELSE
              PRINT USING "%###.###N"; Deductions(CurField - 5)
            END IF
            LOCATE 7 + CurField, 72
            Deductions(CurField - 5) = EditNum(Deductions(CurField - 5), 3, 3, NextFlag)
          LOOP UNTIL Deductions(CurField - 5) <= 100
        END IF
    END SELECT
  LOOP

END SUB

DEFSNG A-Z
SUB LastUsed (Mode, INI AS LastTime)
  DIM TempINI AS LastTime

  SELECT CASE Mode
    CASE 1
      OPEN "WEEKLIST.INI" FOR RANDOM AS 1 LEN = LEN(INI)
        GET #1, 53, INI
      CLOSE 1
    CASE 2
      OPEN "WEEKLIST.INI" FOR RANDOM AS 1 LEN = LEN(INI)
        IF INI.Week <> 0 THEN
          PUT #1, ABS(INI.Week), INI
        END IF
        PUT #1, 53, INI
      CLOSE 1
    CASE 3
      OPEN "WEEKLIST.INI" FOR RANDOM AS 1 LEN = LEN(INI)
        IF INI.Week = 0 THEN
          INI.Period = ""
        ELSE
          GET #1, ABS(INI.Week), TempINI
          INI.Period = TempINI.Period
          PUT #1, ABS(INI.Week), INI
        END IF
        PUT #1, 53, INI
      CLOSE 1
  END SELECT

END SUB

DEFINT A-Z
'MainMenu__________________________________________________________________SUB
'        Title And Main Menu for Entire Program
'
SUB MainMenu (Version$)
  SHARED Pref AS INITIALS
  DIM INI(1 TO 2) AS LastTime
  DIM m$(8)
   
  CALL LastUsed(1, INI(1))
  DO
 
    IF INI(1).Week = 0 THEN INI(1).Period = "New Year"

'    PLAY "mb l16 cd"   '---------------------------------------------
    
    CALL Header("Payroll!  Main Menu", TRUE, TRUE)
    Center 4, "Taciturn Software", Pref.Fore, Pref.Back
    Center 5, STRING$(17, 223), Pref.Fore, Pref.Back
    Center 6, "Payroll! Version " + Version$, Pref.Fore, Pref.Back
    Center 7, STRING$((17 + LEN(Version$)), 223), Pref.Fore, Pref.Back
   
    Center 8, "BONUS Deduction Mode is set in System Setup", 14, Pref.Back
    IF GlobalDeductionBonus = TRUE THEN
        Center 9, "!!!   BONUS DEDUCTION MODE   !!!", Pref.Fore, 4
    ELSE
        Center 9, "                                ", Pref.Fore, Pref.Back
        Center 9, "STANDARD DEDUCTION MODE", Pref.Fore, 2
    END IF

    PRINT TAB(22); "Last Period Ending Date : "; INI(1).Period
    PRINT TAB(23); "Last Period's Week Number :";
     PRINT USING "##"; ABS(INI(1).Week)
   
    IF INI(1).Week < 0 THEN
      CALL Center(11, "PAY-STUB ENTRY FOR WEEK" + STR$(ABS(INI(1).Week)) + " NOT YET COMPLETE", 14, 4)
      CALL Center(12, "PROGRAM MUST BE EXITED AND OPTION 1 CHOSEN TO ENTER NEXT PAYROLL PERIOD", 14, 4)
    END IF

    Hint " It is IMPORTANT that you EXIT this program before you turn off your computer!"

    m$(1) = "1~Payroll"
    m$(2) = "2~Employee Setup"
    m$(3) = "3~Read/Print Old Records"
    m$(4) = "4~Change Last Period's Date"
    m$(5) = "5~Backup/Create New Disk"
    m$(6) = "6~System Setup"
    m$(7) = "Esc~Exit Payroll"

    a$ = Menu(13, 0, m$(), 7)

    SELECT CASE a$
      CASE "1"
        CALL Payroll(INI(1))
        CLS
      CASE "2"
        CALL EmployeeSetup
        CLS
      CASE "3"
        CALL OldRecords
        CLS
      CASE "4"
        CALL ChangeLastweek(INI(1))
        CLS
      CASE "5"
        Disk
        CLS
      CASE "6"
        CALL Setup
      CASE "Esc"
        QuitNow = Quit(INI(1))
    END SELECT
  LOOP UNTIL QuitNow

END SUB

'NewDisk___________________________________________________________________SUB
'      Creates a NEW BACKUP DISK
'
SUB NewDisk
SHARED Pref AS INITIALS

  CALL Panel(1, Pref.Fore, Pref.Back, 1, 1, 25, 80, 0)
  CLS
  PRINT "If "; RTRIM$(LTRIM$(Pref.BackupDev)); " is a removable disk,"
  PRINT "please insert that disk into the drive."
  PRINT
  PRINT RTRIM$(LTRIM$(Pref.BackupDev)); " WILL BE ERASED!!!!"
  PRINT
  PRINT "Do you wish to continue making the Disk or Folder ? ";
  Continue$ = UCASE$(GetKey(TRUE))

  IF Continue$ = "Y" THEN
    PRINT Continue$
    PRINT
    PRINT "Now Creating Backup in " + RTRIM$(LTRIM$(Pref.BackupDev))
    SHELL "COPY PKUNZJR.COM " + RTRIM$(LTRIM$(Pref.BackupDev))
    SHELL "COPY RESTORE.BAK " + RTRIM$(LTRIM$(Pref.BackupDev)) + "\RESTORE.BAT"
    SHELL "COPY CHOICE.COM " + RTRIM$(LTRIM$(Pref.BackupDev))
  END IF
  CALL Panel(2, Pref.Fore, Pref.Back, 1, 1, 25, 80, 0)

END SUB

'OldRecords___________________________________________________________________
SUB OldRecords
  SHARED Pref AS INITIALS
  DIM EmployeeInfo AS EmployeeType
  DIM PayrollInfo AS PayrollType
  DIM m$(53), m2$(3), w$(20)

  DO
    QuitNow = FALSE
    CALL Header("Old Records", TRUE, TRUE)
    CALL Hint("Choose an Employee ฏA '*' Indicates a Currently Inactive Employee")
   
    CALL ChooseEmployee(Number, FALSE, FALSE, "")
 
    IF Number > 0 THEN
      CALL LoadEmployeeData(Number, EmployeeInfo)
      CALL Header("Old Records: " + EmployeeInfo.EmpName, TRUE, TRUE)
     
     
     
      'This section added 12-24-1998 for selection of year of old records
      '********************************************************************
      last = 0
      SHELL "dir " + EmployeeInfo.FileName + ".?? /b /oe > temp.txt"
      w = FREEFILE
      OPEN "temp.txt" FOR INPUT AS w
      wi = 1
      WHILE NOT EOF(w)
        LINE INPUT #w, wtemp$
        wtemp$ = RIGHT$(wtemp$, 2)
        last = last + 1
        w$(last) = wtemp$ + "~Records for '" + wtemp$
      WEND
      CLOSE w
      KILL "temp.txt"
      last = last + 1
      w$(last) = "Esc~Exit / Previous Menu"
      w = last
     
      DoneYear = FALSE
      DO UNTIL DoneYear = TRUE
          a$ = Menu(10, 0, w$(), w)

          Done = FALSE
          IF a$ = "Esc" THEN
            EXIT DO
            'Done = TRUE
            'DoneYear = TRUE
          ELSE
            Year$ = a$
          END IF
         
        
        'End of new addition for year selection
        '*********************************************************************
          
          last = 0
          FOR Record = 1 TO 52
            CALL PayrollData(1, Record, PayrollInfo, EmployeeInfo.FileName, RIGHT$(Year$, 2))
            IF PayrollInfo.Status = 1 THEN
              last = last + 1
              m$(last) = LTRIM$(RTRIM$(STR$(Record))) + "~Week" + STR$(Record) + ", " + PayrollInfo.Period
            END IF
          NEXT
          last = last + 1
         
          m$(last) = "Esc~Exit / Previous Menu"
          DO WHILE Done = FALSE
            a$ = Menu(4, 0, m$(), last)

            IF a$ <> "Esc" THEN
              Record = VAL(a$)
              CALL PayrollData(1, Record, PayrollInfo, EmployeeInfo.FileName, RIGHT$(Year$, 2))
              CALL Template(PayrollInfo.Period, Record, PayrollInfo.Employee)
              CALL ScreenDisplay(PayrollInfo)
              CALL Pause(TRUE)
           
           
              m2$(1) = "1~Print 2 Copies"
              m2$(2) = "2~Ask How Many Copies to Print"
              m2$(3) = "Esc~Return to Week Selection Menu"
              a$ = Menu(11, 0, m2$(), 3)

              SELECT CASE a$
                CASE "1"
                  CALL PrintPayStub(PayrollInfo, Record, 2)
                CASE "2"
                  ' Create Pop-up Panel
                  CALL Panel(1, Pref.Fore, Pref.Back, 11, 20, 16, 60, 3)
                  CALL Center(12, "Record has been Saved", Pref.Fore, Pref.Back)
                  LOCATE 14, 23
                  PRINT "Enter Number of Copies to Print: ";
                  Copies = EditNum((Copies), 2, 0, FALSE)
                             
                  ' Remove Panel
                  CALL Panel(2, Pref.Fore, Pref.Back, 11, 20, 16, 60, 3)

                  CALL PrintPayStub(PayrollInfo, Record, Copies)

             
              END SELECT
            ELSE
              Done = TRUE
            END IF
          LOOP
       LOOP
    ELSE
      QuitNow = TRUE
    END IF
  LOOP UNTIL QuitNow = TRUE

END SUB

'Payroll___________________________________________________________________SUB
'       Begins the Payroll cycle for this week
'
SUB Payroll (SaveINI AS LastTime)
  SHARED Pref AS INITIALS
  DIM m$(6)
  DIM INI(1 TO 2) AS LastTime
  DIM EmployeeInfo AS EmployeeType
  DIM PayInfo(1 TO 2) AS PayrollType
  DIM Deductions(1 TO 4) AS SINGLE
  DIM DoneEmployees AS STRING * 20  ' 20 Employee Limit
  STATIC DateEntry AS LastTime

  ' Get Last End of Period date
  CALL LastUsed(1, INI(1))
 
  IF INI(1).Week < 0 THEN
    INI(1).Week = ABS(INI(1).Week) - 1
    DateEntry.Week = (INI(1).Week + 100)
    DateEntry.Period = INI(1).Period
  END IF
  IF DateEntry.Week - 100 <> INI(1).Week THEN

    ' If week=0 then New Year and there are no old records yet this year
    IF INI(1).Week = 0 THEN INI(1).Period = "New Year"

    ' Get New period Ending
    ClearLine 12, 25
    INI(2).Period = LEFT$(DATE$, 6) + RIGHT$(DATE$, 2)
    LOCATE 14, 22
    PRINT "Enter Period Ending date for Week"; INI(1).Week + 1; ": ";
    NextFlag = FALSE
    DO
      INI(2).Period = EditString(INI(2).Period, 8, NextFlag)
        IF NextFlag THEN EXIT SUB
    LOOP UNTIL Valid(INI(2).Period)
    INI(2).Week = ABS(INI(1).Week) + 1
    DateEntry.Period = INI(2).Period
    DateEntry.Week = INI(1).Week + 100
  ELSE
    INI(2).Period = DateEntry.Period
    INI(2).Week = ABS(INI(1).Week) + 1
  END IF
 
  ' Set Parameter for Week update
  SaveINI.Period = INI(2).Period
  SaveINI.Week = -INI(2).Week

  ' Determine year from user input End of Period
  Year$ = RIGHT$(INI(2).Period$, 2)

  ' Refresh Done Employees
  DoneEmployees = STRING$(20, "0")
  CALL PleaseWait
  CALL Panel(1, 14, 4, 9, 20, 17, 60, 1)
  CALL Center(12, "Refreshing Employee Status", 14, 4)
  CALL Center(14, "Please Wait . . .", 14, 4)
 
  OPEN "EMPLOYEE.DAT" FOR RANDOM AS 1 LEN = LEN(EmployeeInfo)
    last = 0
    Current = 0
    DO
      last = last + 1
      GET #1, last, EmployeeInfo
      IF Valid(EmployeeInfo.EmpName) AND EmployeeInfo.Active THEN
        Current = Current + 1
       
        CALL PayrollData(1, INI(2).Week, PayInfo(1), EmployeeInfo.FileName, Year$)
       
        IF LEN(DoneEmployees) >= last AND PayInfo(1).Status = 1 THEN
          MID$(DoneEmployees, Current, 1) = "1"
        END IF
      END IF
    LOOP WHILE Valid(EmployeeInfo.EmpName)
  CLOSE 1

  CALL Panel(2, 14, 4, 9, 20, 17, 60, 1)
 
  ' End refresh_________

  DO
    CALL ChooseEmployee(Number, TRUE, TRUE, DoneEmployees)
    IF Number > 0 THEN
      AutoPayroll = FALSE
      First = Number
      last = Number
    ELSEIF Number < 0 THEN
      AutoPayroll = TRUE
      First = 1
      last = -Number
    END IF
   
    IF Number <> 0 THEN
      Current = First
      DO
        WHILE AutoPayroll = TRUE AND MID$(DoneEmployees, Current, 1) = "1" AND Current + 1 <= last
          Current = Current + 1
        WEND
       
        IF (AutoPayroll AND MID$(DoneEmployees, Current, 1) = "0") OR AutoPayroll = FALSE THEN
         
   ' Begin correction for INACTIVE
            DO
              CALL LoadEmployeeData(Current, EmployeeInfo)
              IF EmployeeInfo.Active <> TRUE THEN
                Current = Current + 1
              END IF
            LOOP UNTIL EmployeeInfo.Active
         
         
   ' End correction for INACTIVE
         

          DO
            CALL ClearLine(1, 1)
          
            ' Erase All Information in PayrollInfo Array
            REDIM PayInfo(1 TO 2) AS PayrollType

            'Get Last Week's Data if NOT Week 1
            IF INI(1).Week > 0 THEN
              CALL PayrollData(1, INI(1).Week, PayInfo(1), EmployeeInfo.FileName, Year$)
             
              ' If not a good status then do not use
              IF PayInfo(1).Status <> 1 THEN
                REDIM PayInfo(1 TO 2) AS PayrollType
' Alert that last record corrupt or non-existent here
             
              END IF
            END IF

              
            ' Optional line to assign last weeks amounts to this weeks
            IF Pref.OldAmounts = TRUE THEN PayInfo(2) = PayInfo(1)
           
            ' Assign new Period Number to new data
            PayInfo(2).Period = INI(2).Period

            ' Copy Employee Info to new week's record
            PayInfo(2).Employee = EmployeeInfo
            PayInfo(2).Deduction1.DName = EmployeeInfo.Deduction1.DName
            PayInfo(2).Deduction2.DName = EmployeeInfo.Deduction2.DName
            PayInfo(2).Deduction3.DName = EmployeeInfo.Deduction3.DName
            PayInfo(2).Deduction4.DName = EmployeeInfo.Deduction4.DName

            CALL Template(INI(1).Period, INI(1).Week, PayInfo(1).Employee)
            CALL ScreenDisplay(PayInfo(1))
              
            CALL ClearLine(1, 1)

            CALL Center(2, "Press 'Esc' When Done Editing or to Access Options", 14, 4)

            ' Display new record
            CALL Template(INI(2).Period, INI(2).Week, PayInfo(2).Employee)
            CALL ScreenDisplay(PayInfo(2))
                  
            ' Get new data for record
            CALL GetInputs(PayInfo(1), PayInfo(2), Deductions())
                  
            ' Final calculation and display
            CALL Calculate(PayInfo(1), Deductions(), PayInfo(2))
            CALL ScreenDisplay(PayInfo(2))
                  
            m$(1) = "1~Save Record and Print 2 Copies"
            m$(2) = "2~Save Record ONLY - No Print"
            m$(3) = "3~Save and Ask How Many Copies to Print"
            m$(4) = "4~Continue Editing Record"
            m$(5) = "Esc~Return to Employee Selection Menu"
            m = 5
            IF AutoPayroll = TRUE THEN
              m$(5) = "S~Skip This Employee"
              m$(6) = "Esc~Stop Auto Employee Selection"
              m = 6
            END IF
            a$ = Menu(11, 0, m$(), m)

          LOOP WHILE a$ = "4"
        
          SELECT CASE a$
            CASE "1"
              PayInfo(2).Status = 1
              CALL PayrollData(2, INI(2).Week, PayInfo(2), EmployeeInfo.FileName, Year$)
              CALL PrintPayStub(PayInfo(2), INI(2).Week, 2)
              ' Update Employee as Done
              MID$(DoneEmployees, Current, 1) = "1"
            CASE "2"
              PayInfo(2).Status = 1
              CALL PayrollData(2, INI(2).Week, PayInfo(2), EmployeeInfo.FileName, Year$)
              ' Update Employee as Done
              MID$(DoneEmployees, Current, 1) = "1"
            CASE "3"
              PayInfo(2).Status = 1
              CALL PayrollData(2, INI(2).Week, PayInfo(2), EmployeeInfo.FileName, Year$)
            
              ' Create Pop-up Panel
              CALL Panel(1, Pref.Fore, Pref.Back, 11, 20, 16, 60, 3)
              CALL Center(12, "Record has been Saved", Pref.Fore, Pref.Back)
              LOCATE 14, 23
              PRINT "Enter Number of Copies to Print: ";
              Copies = EditNum((Copies), 2, 0, FALSE)
            
              ' Remove Panel
              CALL Panel(2, Pref.Fore, Pref.Back, 11, 20, 16, 60, 3)

              CALL PrintPayStub(PayInfo(2), INI(2).Week, Copies)
              ' Update Employee as Done
              MID$(DoneEmployees, Current, 1) = "1"
            CASE "S"
            CASE "Esc"
              ' Stop Auto Employee Selection
              IF AutoPayroll = TRUE THEN
                last = Current
              END IF
          END SELECT
        END IF
       
        ' Update Current Employee Number for Auto Payroll
        Current = Current + 1
        
      LOOP WHILE Current < last
    END IF
  LOOP UNTIL Number = 0

END SUB

'Payroll Data______________________________________________________________SUB
'Load Payroll information if Opt=1 or saves information of Opt=2
'    Loads data as either 1st or 2nd array depending on x
'
SUB PayrollData (Mode, RecNum, Info AS PayrollType, FileName AS STRING, Year$)

  CALL PleaseWait
  f = FREEFILE
  OPEN FileName + "." + Year$ FOR RANDOM AS f LEN = LEN(Info)
    SELECT CASE Mode
      CASE 1
        GET #f, RecNum, Info
      CASE 2
        PUT #f, RecNum, Info
    END SELECT
  CLOSE f

END SUB

'PrintPayStub______________________________________________________________SUB
'Prints (x) copies of the Pay-Stub #2
'
SUB PrintPayStub (P AS PayrollType, Week, x)
  SHARED Pref AS INITIALS
  STATIC CopiesPrinted
  DIM D(1 TO 4) AS PayrollDeductionType
  DIM Taxable(1 TO 4) AS INTEGER
  DIM WH$(1 TO 2)
  DIM Expd AS STRING, TOn AS STRING, TOff AS STRING
  DIM EOn AS STRING, EOff AS STRING
  DIM UOn AS STRING, UOff AS STRING
  DIM IOn AS STRING, IOff AS STRING
  DIM m$(1 TO 2)

  IF Pref.PrinterType = 1 THEN
    ' These are for the Star 1000/1001
    TOn = CHR$(27) + "w" + CHR$(1)
    TOff = CHR$(27) + "w" + CHR$(0)
    EOn = CHR$(27) + "E"
    EOff = CHR$(27) + "F"
    UOn = CHR$(27) + "-" + CHR$(1) + EOn
    UOff = CHR$(27) + "-" + CHR$(0) + EOff
    IOn = CHR$(27) + "4"
    IOff = CHR$(27) + "5"
  ELSEIF Pref.PrinterType = 2 THEN
    ' For laser printers
    EOn = CHR$(27) + CHR$(40) + CHR$(115) + CHR$(51) + CHR$(66)
    EOff = CHR$(27) + CHR$(40) + CHR$(115) + CHR$(48) + CHR$(66)
    UOn = CHR$(27) + CHR$(38) + CHR$(100) + CHR$(48) + CHR$(68)
    UOff = CHR$(27) + CHR$(38) + CHR$(100) + CHR$(64)
    IOn = CHR$(27) + CHR$(40) + CHR$(115) + CHR$(49) + CHR$(83)
    IOff = CHR$(27) + CHR$(40) + CHR$(115) + CHR$(48) + CHR$(83)
    TOn = EOn
    TOff = EOff
  ELSEIF Pref.PrinterType = 0 THEN
    ' These are the plain ones
    TOn = ""
    TOff = ""
    EOn = ""
    EOff = ""
    UOn = ""
    UOff = ""
    IOn = ""
    IOff = ""
  END IF
 
  IF Pref.ShowNegative THEN
    n = 1
  ELSE
    n = -1
  END IF
 
  WH$(1) = "SINGLE"
  WH$(2) = "MARRIED"

  Taxable(1) = P.Employee.Deduction1.Taxable
  Taxable(2) = P.Employee.Deduction2.Taxable
  Taxable(3) = P.Employee.Deduction3.Taxable
  Taxable(4) = P.Employee.Deduction4.Taxable
  D(1) = P.Deduction1
  D(2) = P.Deduction2
  D(3) = P.Deduction3
  D(4) = P.Deduction4

  f = FREEFILE
  OPEN ShortString(Pref.PrintDev) FOR OUTPUT AS f
  WIDTH #f, 120

  IF UCASE$(LEFT$(ShortString(Pref.PrintDev), 3)) = "LPT" THEN
    m = f
  ELSE
    m = FREEFILE
    OPEN "NUL" FOR OUTPUT AS m
  END IF
  WIDTH #m, 120

    FOR Copies = 1 TO x
      CopiesPrinted = CopiesPrinted + 1
     
      PRINT #f, CHR$(13);
      PRINT #m, EOn;
       PRINT #f, "STATEMENT OF EARNINGS AND DEDUCTIONS        EAGLE FABRICATION AND REPAIR, INC."
      PRINT #f, "                                            610 Locust Street"
      PRINT #f, "NOT NEGOTIABLE                              Oak Harbor, OH 43449"
      PRINT #m, EOff;
       PRINT #f,
      PRINT #f, "Social Security No. ";
       PRINT #m, UOn;
       PRINT #f, USING "\         \"; P.Employee.Social;
       PRINT #m, UOff;
       PRINT #f, SPACE$(12); "Week # ";
       PRINT #m, UOn;
       PRINT #f, USING "##"; Week;
       PRINT #m, UOff;
       PRINT #f, "    Period Ending ";
       PRINT #m, UOn;
       PRINT #f, USING "\      \"; P.Period;
       PRINT #m, UOff;
       PRINT #f,
      PRINT #f,
      PRINT #f, "Gross Pay ";
       PRINT #m, UOn;
       PRINT #f, USING "$$####.##"; P.Gross;
       PRINT #m, UOff;
       PRINT #f, SPACE$(8); "Total Deductions ";
       PRINT #m, UOn;
       PRINT #f, USING "$$####.##"; n * P.TotalDeduct;
       PRINT #m, UOff;
       PRINT #f, SPACE$(8); "Net Pay ";
       PRINT #m, UOn; TOn;
       PRINT #f, USING "$$####.##"; P.NetPay;
       PRINT #m, TOff; UOff
      PRINT #f, STRING$(80, 42)
      PRINT #f, "* Source of Gross * Units * Amounts *   Distributions    *   Y T D   * Current *"
      PRINT #f, "*-----------------*-------*---------*--------------------*-----------*---------*"
      SELECT CASE P.Employee.EmpType
        CASE 2
          PRINT #f, "* Salary          * ----- *";
           PRINT #f, USING "$$####.##"; P.RegularAMT;
           PRINT #f, "* Gross Pay          *";
           PRINT #f, USING "$$#####.##-"; P.GrossYTD;
           PRINT #f, "*";
           PRINT #f, USING "$$####.##"; P.Gross;
           PRINT #f, "*"
          PRINT #f, "*                 *       *         * Social Security    *";
           PRINT #f, USING "$$#####.##-"; n * P.SocialYTD;
           PRINT #f, "*";
           PRINT #f, USING "$$####.##"; n * P.Social;
           PRINT #f, "*"
          PRINT #f, "*                 *       *         * Medicare           *";
           PRINT #f, USING "$$#####.##-"; n * P.MedicareYTD;
           PRINT #f, "*";
           PRINT #f, USING "$$####.##"; n * P.Medicare;
           PRINT #f, "*"
          PRINT #f, "*                 *       *         ";
        CASE ELSE
          PRINT #f, "* Regular Hours   *";
           PRINT #f, USING "###.##-"; P.RegularHRS;
           PRINT #f, "*";
           PRINT #f, USING "$$####.##"; P.RegularAMT;
           PRINT #f, "* Gross Pay "; TAB(58); "*";
           PRINT #f, USING "$$#####.##-"; P.GrossYTD;
           PRINT #f, "*";
           PRINT #f, USING "$$####.##"; P.Gross;
           PRINT #f, "*"
          PRINT #f, "* Overtime Hours  *";
           PRINT #f, USING "###.##-"; P.OvertimeHRS;
           PRINT #f, "*";
           PRINT #f, USING "$$####.##"; P.OvertimeAMT;
           PRINT #f, "* Social Security "; TAB(58); "*";
           PRINT #f, USING "$$#####.##-"; n * P.SocialYTD;
           PRINT #f, "*";
           PRINT #f, USING "$$####.##"; n * P.Social;
           PRINT #f, "*"
          PRINT #f, "* Vacation Hours  *";
           PRINT #f, USING "###.##-"; P.VacationHRS;
           PRINT #f, "*";
           PRINT #f, USING "$$####.##"; P.VacationAMT;
           PRINT #f, "* Medicare "; TAB(58); "*";
           PRINT #f, USING "$$#####.##-"; n * P.MedicareYTD;
           PRINT #f, "*";
           PRINT #f, USING "$$####.##"; n * P.Medicare;
           PRINT #f, "*"
          PRINT #f, "* Holiday Hours   *";
           PRINT #f, USING "###.##-"; P.HolidayHRS;
           PRINT #f, "*";
           PRINT #f, USING "$$####.##"; P.HolidayAMT;
      END SELECT
          
       PRINT #f, "* U.S.Withholding TX"; TAB(58); "*";
       PRINT #f, USING "$$#####.##-"; n * P.USWHYTD;
       PRINT #f, "*";
       PRINT #f, USING "$$####.##"; n * P.USWH;
       PRINT #f, "*"
      PRINT #f, "* Bonus           * ----- *";
       PRINT #f, USING "$$####.##"; P.BonusAMT;
       PRINT #f, "* Ohio in TX "; TAB(58); "*";
       PRINT #f, USING "$$#####.##-"; n * P.OHWHYTD;
       PRINT #f, "*"; : PRINT #f, USING "$$####.##"; n * P.OHWH;
       PRINT #f, "*"
          
      FOR i = 1 TO 4
        IF Valid(D(i).DName) THEN
          PRINT #f, "*                 *       *         * ";
         
          IF Taxable(i) = 2 THEN PRINT #m, IOn;
          PRINT #f, D(i).DName;
          IF Taxable(i) = 2 THEN PRINT #m, IOff;
          PRINT #f, " *";

          PRINT #f, USING "$$#####.##-"; n * D(i).YTD;
           PRINT #f, "*";
           PRINT #f, USING "$$####.##"; n * D(i).Amount;
           PRINT #f, "*"
        ELSE
          PRINT #f, "*                 *       *         *                    *           *         *"
        END IF
      NEXT

      PRINT #f, "*                 *       *         * ";
       PRINT #m, EOn;
       PRINT #f, "Total Deductions   ";
       PRINT #m, EOff;
       PRINT #f, "*";
       PRINT #m, EOn;
       PRINT #f, USING "$$#####.##-"; n * P.TotalDeductYTD;
       PRINT #m, EOff;
       PRINT #f, "*";
       PRINT #m, EOn;
       PRINT #f, USING "$$####.##"; n * P.TotalDeduct;
       PRINT #m, EOff;
       PRINT #f, "*"
      PRINT #f, "*                 *       *         * ";
       PRINT #m, EOn;
       PRINT #f, "Total Net Earnings ";
       PRINT #m, EOff;
       PRINT #f, "*";
       PRINT #m, EOn;
       PRINT #f, USING "$$#####.##-"; P.NetPayYTD;
       PRINT #m, EOff;
       PRINT #f, "*";
       PRINT #m, EOn;
       PRINT #f, USING "$$####.##"; P.NetPay;
       PRINT #m, EOff;
       PRINT #f, "*"
      PRINT #f, STRING$(80, 42)
     
      IF P.Employee.USAllowances = 15 THEN
          ' Special Tax Rates
          PRINT #f, " *U.S. W/H =  15% of Taxable Gross"
          PRINT #f, " *Ohio W/H = 3.5% of Taxable Gross"
      ELSE
          PRINT #f, " *U.S. W/H = "; WH$(P.Employee.USHolding); " with"; P.Employee.USAllowances; "ALLOWANCES"
          PRINT #f, " *Ohio W/H = "; WH$(P.Employee.OHHolding); " with"; P.Employee.OHAllowances; "ALLOWANCES"
      END IF
     
      PRINT #m, EOn;
      PRINT #f, P.Employee.Note;
      PRINT #m, EOff
      PRINT #f, " "; P.Employee.EmpName
      PRINT #f, " "; P.Employee.Address
      PRINT #f, " "; ShortString(P.Employee.Town); ", "; P.Employee.State; " "; P.Employee.Zip;
     
      IF CopiesPrinted MOD 2 THEN
        PRINT #f,
        PRINT #f,
      ELSE
        PRINT #f, "";
      END IF

    NEXT Copies

  IF UCASE$(LEFT$(ShortString(Pref.PrintDev), 3)) <> "LPT" THEN
    CLOSE m
  END IF

  CLOSE f

  StopPrint = FALSE

END SUB

SUB PrintSetup
  SHARED Pref AS INITIALS
  DIM m$(1 TO 3)

  x = 16
  y = 10
  CALL Panel(1, Pref.Fore, Pref.Back, y - 1, x - 1, y + 5, x + 50, 3)
  CALL PrintSetup.Update(x, y)


  ' Set NextFlag and continue editing each field.
  ' NextFlag is cleared when the user presses ENTER.
  NextFlag = TRUE
  DO
    IF NextFlag = FALSE THEN EXIT DO

    IF NextFlag < 2 THEN CurField = CurField - NextFlag
    IF CurField < 1 THEN CurField = 3
    IF CurField > 3 THEN CurField = 1
    NextFlag = TRUE

    SELECT CASE CurField
      CASE 1
        LOCATE y, x + 22
        Pref.PrintDev = EditString(Pref.PrintDev, 28, NextFlag)
          CALL PrintSetup.Update(x, y)
      CASE 2
        m$(1) = "1~Negative"
        m$(2) = "2~Positive"
        DO
          CALL Hint("Press ENTER or Space to change,   or TAB for next fieldฏณ Esc when Done")

          CALL HighLight(y + 1, x + 22, 8)
          a$ = GetKey(FALSE)
          IF a$ = CHR$(13) OR a$ = " " THEN
            CALL Hint("Press indicated key or use   to Choose")
            IF Menu(y + 1, x + 22, m$(), 2) = "1" THEN
              Pref.ShowNegative = TRUE
            ELSE
              Pref.ShowNegative = FALSE
            END IF
            NextFlag = TRUE
          END IF
          CALL PrintSetup.Update(x, y)
        LOOP UNTIL a$ <> CHR$(13)
        NextFlag = ConvertKey(a$)
      CASE 3
        m$(1) = "1~GENERIC"
        m$(2) = "2~Star 1000/1001"
        m$(3) = "3~HP Laserjet II Compatible"
        DO
          CALL Hint("Press ENTER or Space to change,   or TAB for next fieldฏณ Esc when Done")

          CALL HighLight(y + 2, x + 22, 25)
          a$ = GetKey(FALSE)
          IF a$ = CHR$(13) OR a$ = " " THEN
            CALL Hint("Press indicated key or use   to Choose")
            Pref.PrinterType = VAL(Menu(y + 2, x + 22, m$(), 3)) - 1
            NextFlag = TRUE
          END IF
          CALL PrintSetup.Update(x, y)
        LOOP UNTIL a$ <> CHR$(13)
        NextFlag = ConvertKey(a$)
    END SELECT
  LOOP
 
  CALL Panel(2, Pref.Fore, Pref.Back, y - 1, x - 1, y + 8, 70, 1)

END SUB

SUB PrintSetup.Update (x, y)
  SHARED Pref AS INITIALS

  LOCATE y, x
  PRINT "    Print Output To : "; Pref.PrintDev
  LOCATE y + 1, x
  PRINT " Show Deductions As : ";
    IF Pref.ShowNegative THEN
      PRINT "Negative"
    ELSE
      PRINT "Positive"
    END IF
  LOCATE y + 2, x
  PRINT "       Printer Type : ";
    IF Pref.PrinterType = 1 THEN
      PRINT "Star 1000/1001"
    ELSEIF Pref.PrinterType = 2 THEN
      PRINT "HP Laserjet II Compatable"
    ELSEIF Pref.PrinterType = 0 THEN
      PRINT "GENERIC"
    END IF

END SUB

FUNCTION Quit (INI AS LastTime)
  DIM m$(3)
  DIM OldINI AS LastTime
  DIM EmployeeInfo AS EmployeeType
  DIM PayInfo(0 TO 1) AS PayrollType
  DO
    IF INI.Week < 0 THEN
      m$(1) = "1~Exit Payroll! and Close Pay Period"
      m$(2) = "2~Exit Payroll! ONLY, Period MUST BE CLOSED Later"
      m$(3) = "Esc~Return to Main Menu"
      m = 3
    ELSE
      m$(1) = "X~Exit Payroll!"
      m$(2) = "Esc~Return to Main Menu"
      m = 2
    END IF

    a$ = Menu(13, 0, m$(), m)
    SELECT CASE a$
      CASE "1"
        m$(1) = "N~DO NOT End Pay Period Now"
        m$(2) = "Y~End Pay Period Now"
        a$ = Menu(14, 0, m$(), 2)
        SELECT CASE a$
          CASE "N"
            QuitNow = 1
          CASE "Y"
            OldINI = INI
            INI.Week = ABS(INI.Week)
           
            ' Check Employee records______
            CALL PleaseWait
            CALL Panel(1, 14, 4, 9, 20, 17, 60, 1)
            CALL Center(12, "Checking Employee Status", 14, 4)
            CALL Center(14, "Please Wait . . .", 14, 4)

            OPEN "EMPLOYEE.DAT" FOR RANDOM AS 1 LEN = LEN(EmployeeInfo)
              last = 0
              Current = 0
              DO
                last = last + 1
                GET #1, last, EmployeeInfo
'                IF Valid(EmployeeInfo.EmpName) AND EmployeeInfo.Active THEN
                IF Valid(EmployeeInfo.EmpName) THEN
                  Current = Current + 1
                
                  CALL PayrollData(1, INI.Week, PayInfo(0), EmployeeInfo.FileName, RIGHT$(INI.Period, 2))
                  Abort = FALSE
                  IF PayInfo(0).Status <> 1 AND Current <= 20 THEN
                  
                    CALL Panel(1, 14, 4, 7, 15, 20, 66, 1)
                    COLOR 15
                    LOCATE 9, 20
                    PRINT EmployeeInfo.EmpName;
                   ' LOCATE 8, 40
                    IF EmployeeInfo.Active = TRUE THEN
                      PRINT " Active"
                    ELSE
                      PRINT " Inactive"
                    END IF
                    LOCATE 11, 20
                    PRINT "This Employee does not have a record saved"
                    LOCATE 12, 20
                    PRINT "for this payroll period."

                    m$(1) = "1~Update Employee Paystub to $0.00 GROSS"
                    m$(2) = "Esc~Abort and Return to previous menu"
                   
                    a$ = Menu(14, 0, m$(), 2)
                    CALL Panel(2, 14, 4, 7, 15, 20, 66, 1)
                    SELECT CASE a$
                      CASE "1"
                        REDIM PayInfo(0 TO 1) AS PayrollType
                        IF INI.Week = 1 THEN
                 '         REDIM PayInfo(0 TO 1) AS PayrollType
                        ELSE
                 '         CALL PayrollData(1, INI.Week - 1, PayInfo(1), EmployeeInfo.FileName, RIGHT$(INI.Period, 2))
                          CALL PayrollData(1, INI.Week - 1, PayInfo(1), EmployeeInfo.FileName, RIGHT$(INI.Period, 2))
                          PayInfo(0).Employee = EmployeeInfo
                          PayInfo(0).Deduction1.DName = PayInfo(1).Deduction1.DName
                          PayInfo(0).Deduction1.YTD = PayInfo(1).Deduction1.YTD
                          PayInfo(0).Deduction2.DName = PayInfo(1).Deduction2.DName
                          PayInfo(0).Deduction2.YTD = PayInfo(1).Deduction2.YTD
                          PayInfo(0).Deduction3.DName = PayInfo(1).Deduction3.DName
                          PayInfo(0).Deduction3.YTD = PayInfo(1).Deduction3.YTD
                          PayInfo(0).Deduction4.DName = PayInfo(1).Deduction4.DName
                          PayInfo(0).Deduction4.YTD = PayInfo(1).Deduction4.YTD
                          PayInfo(0).TotalDeductYTD = PayInfo(1).TotalDeductYTD
                          PayInfo(0).GrossYTD = PayInfo(1).GrossYTD
                          PayInfo(0).SocialYTD = PayInfo(1).SocialYTD
                          PayInfo(0).MedicareYTD = PayInfo(1).MedicareYTD
                          PayInfo(0).USWHYTD = PayInfo(1).USWHYTD
                          PayInfo(0).OHWHYTD = PayInfo(1).OHWHYTD
                          PayInfo(0).NetPayYTD = PayInfo(1).NetPayYTD
                          PayInfo(0).RegYTDHRS = PayInfo(1).RegYTDHRS
                          PayInfo(0).RegYTDAMT = PayInfo(1).RegYTDAMT
                          PayInfo(0).OvrYTDHRS = PayInfo(1).OvrYTDHRS
                          PayInfo(0).OvrYTDAMT = PayInfo(1).OvrYTDAMT
                          PayInfo(0).VacYTDHRS = PayInfo(1).VacYTDHRS
                          PayInfo(0).VacYTDAMT = PayInfo(1).VacYTDAMT
                          PayInfo(0).HolYTDHRS = PayInfo(1).HolYTDHRS
                          PayInfo(0).HolYTDAMT = PayInfo(1).HolYTDAMT
                          PayInfo(0).BonusYTD = PayInfo(1).BonusYTD
                         
                          PayInfo(0).Period = INI.Period
                        END IF
                        PayInfo(0).Status = 1
                        CALL PayrollData(2, INI.Week, PayInfo(0), EmployeeInfo.FileName, RIGHT$(INI.Period, 2))
                      CASE "Esc"
                        Abort = TRUE
                    END SELECT
                  END IF
                END IF
              LOOP WHILE Valid(EmployeeInfo.EmpName) AND Abort = FALSE
            CLOSE 1

            CALL Panel(2, 14, 4, 9, 20, 17, 60, 1)

            ' End check_________
           
            IF Abort = TRUE THEN
              INI = OldINI
              QuitNow = 1
            ELSE
              CALL LastUsed(2, INI)
              QuitNow = TRUE
            END IF
        END SELECT
      CASE "2"
        INI.Week = -ABS(INI.Week)
        CALL LastUsed(2, INI)
        CALL ClearLine(11, 12)
        CALL Panel(1, 14 + 16, 4, 12, 15, 19, 65, 2)
        CALL Center(13, "You MUST close out this payroll period     ", 14, 4)
        CALL Center(14, "by exiting the program (after the next time", 14, 4)
        CALL Center(15, "it is run) and choosing option 1.  This    ", 14, 4)
        CALL Center(16, "feature is only to be used in the event    ", 14, 4)
        CALL Center(17, "that the program must be exited before all ", 14, 4)
        CALL Center(18, "this week's pay stubs have been entered.   ", 14, 4)
        CALL Hint("Press any key to continue")
        CALL Pause(TRUE)
        QuitNow = TRUE
      CASE "X"
        QuitNow = TRUE
      CASE "Esc"
        QuitNow = FALSE
    END SELECT
  LOOP UNTIL QuitNow < 1
 
  Quit = QuitNow

END FUNCTION

'ScreenDisplay_____________________________________________________________SUB
'Displays Paystub
'
SUB ScreenDisplay (P AS PayrollType)
  SHARED Pref AS INITIALS
  DIM D(1 TO 4) AS DeductionType
  DIM PD(1 TO 4) AS PayrollDeductionType
 
  IF Pref.ShowNegative THEN
    n = 1
  ELSE
    n = -1
  END IF
 
  LOCATE 3, 11
    PRINT USING "$$####.##"; P.Gross;
    LOCATE 3, 44
    PRINT USING "$$####.##"; n * P.TotalDeduct;
    LOCATE 3, 72
    PRINT USING "$$####.##"; P.NetPay

  'Fill In Template_____________________________________________________________
  SELECT CASE P.Employee.EmpType
  CASE 2
     LOCATE 7, 3
     PRINT "Seven Days"
     LOCATE 7, 21
     PRINT "ฤฤฤฤฤ"
     LOCATE 7, 28
     PRINT USING "$$####.##"; P.RegularAMT
  CASE 1, 3
     LOCATE 7, 3
     PRINT "Regular Hours"
     LOCATE 8, 3
     PRINT "Overtime Hours"
     LOCATE 9, 3
     PRINT "Vacation Hours"
     LOCATE 10, 3
     PRINT "Holiday Hours"
    
     COLOR Pref.Entry
     LOCATE 7, 20
     PRINT USING "###.##"; P.RegularHRS
     LOCATE 8, 20
     PRINT USING "###.##"; P.OvertimeHRS
     LOCATE 9, 20
     PRINT USING "###.##"; P.VacationHRS
     LOCATE 10, 20
     PRINT USING "###.##"; P.HolidayHRS
    
     COLOR Pref.Fore
     LOCATE 7, 28
     PRINT USING "$$####.##"; P.RegularAMT
     LOCATE 8, 28
     PRINT USING "$$####.##"; P.OvertimeAMT
     LOCATE 9, 28
     PRINT USING "$$####.##"; P.VacationAMT
     LOCATE 10, 28
     PRINT USING "$$####.##"; P.HolidayAMT
  END SELECT
 
  COLOR Pref.Entry
  LOCATE 11, 28
  PRINT USING "$$####.##"; P.BonusAMT
  COLOR Pref.Fore

  'YTD__________________________________________________________________________
  LOCATE 7, 59
  PRINT USING "$$#####.##"; P.GrossYTD
  LOCATE 8, 59
  PRINT USING "$$#####.##"; n * P.SocialYTD
  LOCATE 9, 59
  PRINT USING "$$#####.##"; n * P.MedicareYTD
  LOCATE 10, 59
  PRINT USING "$$#####.##"; n * P.USWHYTD
  LOCATE 11, 59
  PRINT USING "$$#####.##"; n * P.OHWHYTD
  LOCATE 17, 59
  PRINT USING "$$#####.##"; n * P.TotalDeductYTD
  LOCATE 18, 59
  PRINT USING "$$#####.##"; P.NetPayYTD
  'Current______________________________________________________________________
  LOCATE 7, 71
  PRINT USING "$$####.##"; P.Gross
  LOCATE 8, 71
  PRINT USING "$$####.##"; n * P.Social
  LOCATE 9, 71
  PRINT USING "$$####.##"; n * P.Medicare
  LOCATE 10, 71
  PRINT USING "$$####.##"; n * P.USWH
  LOCATE 11, 71
  PRINT USING "$$####.##"; n * P.OHWH
  LOCATE 17, 71
  PRINT USING "$$####.##"; n * P.TotalDeduct
  LOCATE 18, 71
  PRINT USING "$$####.##"; P.NetPay
  'Earnings_____________________________________________________________________
  LOCATE 15, 20
  PRINT USING "####.##"; P.RegYTDHRS
  LOCATE 16, 20
  PRINT USING "####.##"; P.OvrYTDHRS
  LOCATE 17, 20
  PRINT USING "####.##"; P.VacYTDHRS
  LOCATE 18, 20
  PRINT USING "####.##"; P.HolYTDHRS
  LOCATE 19, 20
  PRINT " ฤฤฤฤฤ"
  LOCATE 15, 28
  PRINT USING "$$####.##"; P.RegYTDAMT
  LOCATE 16, 28
  PRINT USING "$$####.##"; P.OvrYTDAMT
  LOCATE 17, 28
  PRINT USING "$$####.##"; P.VacYTDAMT
  LOCATE 18, 28
  PRINT USING "$$####.##"; P.HolYTDAMT
  LOCATE 19, 28
  PRINT USING "$$####.##"; P.BonusYTD

  'Extra deductions_____________________________________________________________
  D(1) = P.Employee.Deduction1
  D(2) = P.Employee.Deduction2
  D(3) = P.Employee.Deduction3
  D(4) = P.Employee.Deduction4
  PD(1) = P.Deduction1
  PD(2) = P.Deduction2
  PD(3) = P.Deduction3
  PD(4) = P.Deduction4
                              
  FOR i = 1 TO 4
    IF D(i).Active THEN
      LOCATE 12 + i, 39
      PRINT PD(i).DName
      LOCATE 12 + i, 59
      PRINT USING "$$#####.##"; n * PD(i).YTD
      
      IF D(i).Active = 1 THEN COLOR Pref.Entry
      LOCATE 12 + i, 71
      PRINT USING "$$####.##"; n * PD(i).Amount
      COLOR Pref.Fore
    END IF
  NEXT

  IF EmpType$ <> "SALARY" THEN
     LOCATE 19, 39
     PRINT USING "$$###.##"; P.Employee.Wage; : PRINT "/Hour";
  END IF

END SUB

SUB Setup
  DIM m$(1 TO 6)

  m$(1) = "1~Printing"
  m$(2) = "2~Screen Saver"
  m$(3) = "3~System Date/Time"
  m$(4) = "4~Backup/Restore"
  IF GlobalDeductionBonus = TRUE THEN
    m$(5) = "5~Adjust Deductions back to STANDARD MODE (In BONUS MODE Now)"
  ELSE
    m$(5) = "5~Adjust State & Federal Deductions to BONUS MODE            "
  END IF
  m$(6) = "Esc~Return to Main Menu"
  DO
    SELECT CASE Menu(8, 0, m$(), 6)
      CASE "1"
        CALL PrintSetup
      CASE "2"
        CALL Bounce.Pref
      CASE "3"
        CALL DateAndTime
      CASE "4"
        CALL BackupSetup
      CASE "5"
          IF GlobalDeductionBonus = TRUE THEN
            GlobalDeductionBonus = FALSE
            m$(5) = "5~Adjust State & Federal Deductions to BONUS MODE            "
          ELSE
            GlobalDeductionBonus = TRUE
            m$(5) = "5~Adjust Deductions back to STANDARD MODE (In BONUS MODE Now)"
          END IF
      CASE "Esc"
        Done = TRUE
    END SELECT
  LOOP UNTIL Done

END SUB

SUB Template (Period AS STRING, Week, Employee AS EmployeeType)
  SHARED Pref AS INITIALS

  'Template_____________________________________________________________________
  CLS
  LOCATE 2, 10
  PRINT "Period Ending : ";
    COLOR 14: PRINT Period;
    LOCATE 2, 56
    COLOR Pref.Fore: PRINT "Week Number :";
    COLOR 14
    PRINT Week
  COLOR Pref.Fore
  PRINT "Gross Pay "; TAB(27); "Total Deductions "; TAB(64); "Net Pay "
  PRINT "ษอออออออออออออออออหอออออออหอออออออออหออออออออออออออออออออหอออออออออออหอออออออออป";
  PRINT "บ Source of Gross บ Units บ Amounts บ   Distributions    บ   Y T D   บ Current บ";
  PRINT "ฬอออออออออออออออออฮอออออออฮอออออออออฮออออออออออออออออออออฮอออออออออออฮอออออออออน";
  PRINT "บ                 บ       บ         บ Gross Pay          บ           บ         บ";
  PRINT "บ                 บ       บ         บ Social Security    บ           บ         บ";
  PRINT "บ                 บ       บ         บ Medicare           บ           บ         บ";
  PRINT "บ                 บ       บ         บ U.S.Withholding TX บ           บ         บ";
  PRINT "บ Bonus           บ ฤฤฤฤฤ บ         บ Ohio in Tax        บ           บ         บ";
  PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤืฤฤฤฤฤฤฤืฤฤฤฤฤฤฤฤฤถ                    บ           บ         บ";
  PRINT "บ  Year to Date   บ Units บ Amounts บ                    บ           บ         บ";
  PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤืฤฤฤฤฤฤฤืฤฤฤฤฤฤฤฤฤถ                    บ           บ         บ";
  PRINT "บ Regular  Hours  บ       บ         บ                    บ           บ         บ";
  PRINT "บ Overtime Hours  บ       บ         บ                    บ           บ         บ";
  PRINT "บ Vacation Hours  บ       บ         บ Total Deductions   บ           บ         บ";
  PRINT "บ Holiday  Hours  บ       บ         บ Total Net Earnings บ           บ         บ";
  PRINT "บ Bonus           บ       บ         บ                    บ           บ         บ";
  PRINT "ศอออออออออออออออออสอออออออสอออออออออสออออออออออออออออออออสอออออออออออสอออออออออผ";
  PRINT " *U.S. W/H = ";
    SELECT CASE Employee.USHolding
      CASE 1
        PRINT "SINGLE ";
      CASE 2
        PRINT "MARRIED";
    END SELECT
    PRINT " with"; Employee.USAllowances; "ALLOWANCE";
    PRINT TAB(39); CHR$(179); " "; : COLOR 14, 4
    PRINT Employee.EmpName + SPACE$(10);
  COLOR Pref.Fore, Pref.Back
  PRINT " *Ohio W/H = ";
    SELECT CASE Employee.OHHolding
      CASE 1
        PRINT "SINGLE ";
      CASE 2
        PRINT "MARRIED";
    END SELECT
    PRINT " with"; Employee.OHAllowances; "ALLOWANCE";
    PRINT TAB(39); CHR$(179); " "; : COLOR 14, 4
    PRINT Employee.Address;
  COLOR Pref.Fore, Pref.Back
  PRINT TAB(39); CHR$(179); " "; : COLOR 14, 4
   PRINT Employee.Town + Employee.State + "  " + Employee.Zip + SPACE$(11);
  COLOR Pref.Fore, Pref.Back
  PRINT Employee.Note;

END SUB

