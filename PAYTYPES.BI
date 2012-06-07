'Type Definitions_____________________________________________________________
  TYPE DeductionType
    Active  AS INTEGER
    DType   AS INTEGER
    DName   AS STRING * 18
    Taxable AS INTEGER
    Amount  AS SINGLE
    AType   AS INTEGER
    Limit   AS SINGLE
    LType   AS INTEGER
    Total   AS SINGLE
  END TYPE
 
  TYPE EmployeeType
    Active       AS INTEGER
    Hired        AS STRING * 8
    Number       AS INTEGER
    EmpType      AS INTEGER
    EmpName      AS STRING * 30
    FileName     AS STRING * 8
    Address      AS STRING * 40
    Town         AS STRING * 20
    State        AS STRING * 2
    Zip          AS STRING * 5
    Social       AS STRING * 11
    USHolding    AS INTEGER
    USAllowances AS INTEGER
    OHHolding    AS INTEGER
    OHAllowances AS INTEGER
    Wage         AS SINGLE
    Note         AS STRING * 80
    Deduction1   AS DeductionType
    Deduction2   AS DeductionType
    Deduction3   AS DeductionType
    Deduction4   AS DeductionType
  END TYPE

    'Example to assign the DeductionInfo to EmployeeType------------
    'EmployeeInfo.Deduction1=Deduct(1)
    'EmployeeInfo.Deduction2=Deduct(2)
    'EmployeeInfo.Deduction3=Deduct(3)
    'EmployeeInfo.Deduction4=Deduct(4)

  TYPE PayrollDeductionType
    DName   AS STRING * 18
    Taxable AS INTEGER
    Amount  AS SINGLE
    YTD     AS SINGLE
  END TYPE

  TYPE PayrollType
    Status         AS INTEGER
    Period         AS STRING * 8
    RegularHRS     AS SINGLE
    RegularAMT     AS SINGLE
    OvertimeHRS    AS SINGLE
    OvertimeAMT    AS SINGLE
    VacationHRS    AS SINGLE
    VacationAMT    AS SINGLE
    HolidayHRS     AS SINGLE
    HolidayAMT     AS SINGLE
    BonusAMT       AS SINGLE
    Gross          AS SINGLE
    Social         AS SINGLE
    Medicare       AS SINGLE
    USWH           AS SINGLE
    OHWH           AS SINGLE
    TotalDeduct    AS SINGLE
    TotalDeductYTD AS SINGLE
    GrossYTD       AS SINGLE
    SocialYTD      AS SINGLE
    MedicareYTD    AS SINGLE
    USWHYTD        AS SINGLE
    OHWHYTD        AS SINGLE
    NetPay         AS SINGLE
    NetPayYTD      AS SINGLE
    RegYTDHRS      AS SINGLE
    RegYTDAMT      AS SINGLE
    OvrYTDHRS      AS SINGLE
    OvrYTDAMT      AS SINGLE
    VacYTDHRS      AS SINGLE
    VacYTDAMT      AS SINGLE
    HolYTDHRS      AS SINGLE
    HolYTDAMT      AS SINGLE
    BonusYTD       AS SINGLE
    Employee       AS EmployeeType
    Deduction1     AS PayrollDeductionType
    Deduction2     AS PayrollDeductionType
    Deduction3     AS PayrollDeductionType
    Deduction4     AS PayrollDeductionType
  END TYPE

    'Example to assign the Deductions to PayrollType----------------
    'Pay(2).Deduction1=PayDeduction(1)
    'Pay(2).Deduction2=PayDeduction(2)
    'Pay(2).Deduction3=PayDeduction(3)
    'Pay(2).Deduction4=PayDeduction(4)

  TYPE LastTime
    Week    AS INTEGER
    Period  AS STRING * 8
  END TYPE

  TYPE EmpSelectType
    ID     AS STRING * 30
    Number AS INTEGER
  END TYPE

'End Type Definitions_________________________________________________________

