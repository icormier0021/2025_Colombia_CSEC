* Encoding: UTF-8.

/*VACS RECODE - APPLY to BOTH

RECODE H6 (99=SYSMIS) (98=SYSMIS) (2=0) (1=1) INTO Sharedhous.
VARIABLE LABELS  Sharedhous 'Live in shared housing'.
EXECUTE.

RECODE H7H H18 H19 H19A H55A H56 H58 (99=SYSMIS) (98=SYSMIS) (2=0) (1=1) INTO Telehom Govsupp 
    NGOsupp Victimreg Parentdie childouthome childstreet.
VARIABLE LABELS  Telehom 'Internet in home' /Govsupp 'Government financial support' /NGOsupp 
    'NGO or other community organization support' /Victimreg 'Recognized on national victim registry' 
    /Parentdie 'Parent has died due to violence' /childouthome 'child under 18 lived outside of home' 
    /childstreet 'child less than 18 lived on the street'.
EXECUTE.

RECODE H19B (99=SYSMIS) (98=SYSMIS) (ELSE=Copy) INTO Victimregtype.
VARIABLE LABELS  Victimregtype 'Type of conflict victim'.
EXECUTE.

RECODE H20 (99=SYSMIS) (98=SYSMIS) (5=1) (4=2) (3=3) (2=4) (1=5) INTO Moneyworry.
EXECUTE.

RECODE Q2 (99=SYSMIS) (98=SYSMIS) (ELSE=Copy) INTO Age.
VARIABLE LABELS  Age 'Age of respondent'.
EXECUTE.

RECODE Q2A (99=SYSMIS) (98=SYSMIS) (7=0) (ELSE=Copy) INTO Eth.
VARIABLE LABELS  Eth 'Ethnicity of respondent'.
EXECUTE.

RECODE Q3 Q7AA Q13 Q19 Q29 Q58 Q70 Q71 Q306 (99=SYSMIS) (98=SYSMIS) (2=0) 
    (1=1) INTO School Foodins Emp Momaliv Dadaliv Partner Safetyvio Safetywar Bullyvic.
VARIABLE LABELS  School 'Ever attended school' /Foodins 'Food insecurity ' /Emp 'Employment' 
    /Momaliv 'Mother alive' /Dadaliv 'Dad living' /
    /Partner 'Ever had a romantic partner' /Safetyvio  'feel safe from violence' / Safetywar  'feel safe from war' /
    Bullyvic 'bullying victim'. 
EXECUTE.

RECODE Q21 Q31 (99=SYSMIS) (98=SYSMIS) (11=0) (1 thru 10 = 1) INTO Momaway Dadaway.
EXECUTE.

RECODE Q7 (99=SYSMIS) (98=SYSMIS) (1=4) (2=3) (3=2) (4=1) INTO Friend.
VARIABLE LABELS  Friend 'Closeness to friends'.
EXECUTE.

RECODE Q25 Q35 (99=SYSMIS) (98=SYSMIS) (1=4) (2=3) (3=2) (4=1) INTO Momclose Dadclose.
VARIABLE LABELS  Momclose 'Bio mom close' /Dadclose 'Bio dad close'.
EXECUTE.

RECODE Q7AB Q7AD Q7AC (99=SYSMIS) (98=SYSMIS) (2=0) (1=1) INTO Matins1 Matins3 Matins2.
VARIABLE LABELS  Matins1 'Material insecurity 7AB' /Matins3 'Material insecurity AD' /Matins2 
    'Material insecurity 2'.
EXECUTE.

RECODE Q36A Q36B Q36C Q36D Q36E (98=SYSMIS) (99=SYSMIS) (3=1) (2=2) (1=3) INTO Parmonit1 Parmonit2 
    Parmonit3 Parmonit4 Parmonit5.
EXECUTE.

RECODE Q59 (99=SYSMIS) (98=SYSMIS) (ELSE=Copy) INTO sexorient.
VARIABLE LABELS  sexorient 'sexual orientation'.
EXECUTE.

RECODE Q80 Q82 Q84 Q86 
    (1=0) (2=1) (3=1) (99=SYSMIS) (98=SYSMIS) INTO WitparentIPV Witsib Witcomvio Witconflict. 
VARIABLE LABELS  WitparentIPV 'Witness parental IPV' /Witsib 'Witnessing Sibling Abuse' 
    /Witcomvio 'Witness violence in community' /Witconflict 'Witness armed conflict'.
EXECUTE. 

RECODE Q407 Q409 Q500 Q600 Q700A Q700B Q800A Q800B  
    (2=0) (1=1) (99=SYSMIS) (98=SYSMIS) INTO Secact 
    Firstwanted CSEC Sexabuse Rapeattempt1 Rapeattempt2 Rapecomplete1 Rapecomplete2.  
VARIABLE LABELS Secact 'Ever had sex' /Firstwanted 'First sexual experience wanted' /CSEC 'Ever involved in transactional '+
    'sex' /Sexabuse 'Ever been sexually touched when not wanted'. 
EXECUTE.

RECODE Q1205 Q1207 Q1208 Q1209 Q1210 
    (2=0) (1=1) (99=SYSMIS) (98=SYSMIS) INTO Drugs Selfharm SI 
    Suiattempt STI.
VARIABLE LABELS Drugs 'Used drugs in 30 days' 
    /Selfharm 'Self harmed' /SI 'Wanted to be dead' /Suiattempt 'suicide attempt' /STI 'Ever had STI'.
EXECUTE.

RECODE Q1200 (98=SYSMIS) (99=SYSMIS) (97=0) (Else=1) INTO 
    Alcohol.
VARIABLE LABELS  Alcohol 'Ever drank alcoholt'.
EXECUTE.

RECODE Q100A Q100B Q100C Q100D Q116A Q116B Q116C Q116D Q128A Q128B Q128C Q128D Q142A Q142B Q142C 
    Q142D Q300A Q300B Q300C Q300D Q305A Q305B Q305C Q305D Q305E Q305F (99=SYSMIS) (98=SYSMIS) (2=0) (1=1) INTO 
    IPVphys1 IPVphys2 IPVphys3 IPVphys4 Peerphys1 Peerphys2 Peerphys3 Peerphys4 Parenphys1 Parenphys2 
    Parenphys3 Parenphys4 Nonparenphys1 Nonparenphys2 Nonparenphys3 Nonparenphys4 ParentEmotabuse1 
    ParentEmotabuse2 ParentEmotabuse3 ParentEmotabuse4 IPVEmot1 IPVEmot2 IPVEmot3 IPVEmot4 IPVEmot5 IPVEmot6.
EXECUTE.

Recode Q400B (99=SYSMIS) (98=SYSMIS) (2=0) (1 thru Highest=1) INTO 
    Physcorp.
VARIABLE LABELS  Physcorp 'Physical corporal punishment parent'.
EXECUTE.

RECODE Q408 Q417 Q504 Q505 (99=SYSMIS) (98=SYSMIS) (ELSE=Copy) INTO Sexinit Sexpar CSECvicage CSECageperp.
VARIABLE LABELS  Sexinit 'Age at sexual initiation' /Sexpar 'Number of sex partners' / Csecvicage 'age of CSEC victimization first' / CSECageperp 'age of CSEC perp'.
EXECUTE.

RECODE Q503 ('J'=1) ('W'=1) ('99'=SYSMIS) ('98'=SYSMIS) (ELSE=0) INTO CSECtourist.
VARIABLE LABELS  CSECtourist 'Was CSEC perpetrator a tourist'.
EXECUTE.

RECODE Q1206A Q1206B Q1206C Q1206D Q1206E Q1206F (99=SYSMIS) (1=5) (2=4) (3=3) (4=2) (5=1) INTO MHA 
    MHB MHC MHD MHE MHF.
EXECUTE.

/*COMPUTING FROM EXISTING after above is run - scales or collapsing

COMPUTE Foodandmatins= Matins1 + Matins3 + Matins2 + Foodins.
EXECUTE. 

COMPUTE Monit= Parmonit1 + Parmonit2 + Parmonit3 + Parmonit4 + Parmonit5.
EXECUTE.

COMPUTE IPVphystot= IPVphys1 + IPVphys2 +IPVphys3 + IPVphys4.
EXECUTE.

COMPUTE Peerphystotal= Peerphys1 + Peerphys2 + Peerphys3 + Peerphys4.
EXECUTE.

COMPUTE Parentphy= Parenphys1 + Parenphys2 + Parenphys3 + Parenphys4.
EXECUTE.

COMPUTE Nonparentphys= Nonparenphys1 + Nonparenphys2 + Nonparenphys3 + Nonparenphys4. 
EXECUTE. 

COMPUTE Parentemotabuse= ParentEmotabuse1 + ParentEmotabuse2 + ParentEmotabuse3 + ParentEmotabuse4.
EXECUTE.

COMPUTE IPVEmotional= IPVEmot1 + IPVEmot2 + IPVEmot3 + IPVEmot4 + IPVEmot5 + IPVEmot6.
EXECUTE.

RECODE IPVphystot Peerphystotal Parentphy IPVEmotional Parentemotabuse Nonparentphys (SYSMIS=SYSMIS) (0=0) (1 
    thru Highest=1).
EXECUTE.

COMPUTE MHsxs=  MHA + MHB + MHC + MHD + MHE + MHF.
EXECUTE.

COMPUTE Witanyvio= WitparentIPV + Witsib + Witcomvio + Witconflict.
EXECUTE.

RECODE Witanyvio (SYSMIS=SYSMIS) (0=0) (1 
    thru Highest=1).
EXECUTE.

COMPUTE Physviolence= IPVphystot + Peerphystotal + Parentphy + Nonparentphys.
EXECUTE.

RECODE Physviolence (SYSMIS=SYSMIS) (0=0) (1 
    thru Highest=1).
EXECUTE.

COMPUTE Sexabusebroad=Sexabuse + Rapeattempt1 + Rapeattempt2 + Rapecomplete1 + Rapecomplete2.
EXECUTE. 

RECODE Sexabusebroad (SYSMIS=SYSMIS) (0=0) (1 
    thru Highest=1).
EXECUTE.

COMPUTE Sexabusenarrow=Sexabuse + Rapecomplete1 + Rapecomplete2.
EXECUTE.

RECODE Sexabusenarrow (SYSMIS=SYSMIS) (0=0) (1 
    thru Highest=1).
EXECUTE..

COMPUTE IPVtotal=IPVphystot + IPVEmotional.
EXECUTE.

RECODE IPVtotal (SYSMIS=SYSMIS) (0=0) (1 
    thru Highest=1).
EXECUTE.

/*Analyses

/*Filters

RECODE CSECvicage (0=0) (SYSMIS=0) (18 thru 24=0) (13 thru 17=1) INTO Filter_CSECendorsed.
VARIABLE LABELS  Filter_CSECendorsed 'Person was less than 18 when transactional sex occurred'.
EXECUTE.

RECODE Age (13 thru 17=1) (ELSE=0) INTO age_Filter.
VARIABLE LABELS  age_Filter 'under 18'.
EXECUTE.

COMPUTE Full_filter=(age_Filter * 1) + (Filter_CSECendorsed * 2).
EXECUTE.

RECODE Full_filter (0=0) (1 thru Highest=1).
EXECUTE.

RECODE CSEC (SYSMIS=0) (0 thru 1=1) INTO Answer_CSEC.
VARIABLE LABELS  Answer_CSEC 'Answered question about CSEC'.
EXECUTE.

COMPUTE Expand_Filter=(Full_filter*1) + (Answer_CSEC*2).
EXECUTE.

/*Analyses CSEC - restricted to inclusion and without

USE ALL.
COMPUTE filter_$=(Full_filter = 1).
VARIABLE LABELS filter_$ 'Full_filter = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=CSEC CSECtype CSECtourist CSECvicage CSECageperp
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.

FREQUENCIES VARIABLES=CSEC CSECtype CSECtourist CSECvicage CSECageperp
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /ORDER=ANALYSIS.

/*Main analyses bivariate associations

USE ALL.
COMPUTE filter_$=(Full_filter = 1).
VARIABLE LABELS filter_$ 'Full_filter = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=Sex Age Eth Sexorientfinal School Emp Partner Telehom Govsupp NGOsupp 
    Foodandmatins childouthome childstreet Moneyworry Momclose Dadclose Monit Victimreg Safetyvio 
    Safetywar Physviolence IPVphystot Physcorp Parentphy Sexabusebroad Sexabusenarrow Witanyvio 
    WitparentIPV Witconflict IPVtotal Secact Firstwanted Sexinit Sexpar STI Drugs Selfharm SI 
    Suiattempt Alcohol MHsxs
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN SKEWNESS SESKEW KURTOSIS SEKURT
  /ORDER=ANALYSIS.

CROSSTABS
  /TABLES=CSEC BY Eth Sexorientfinal School Emp Partner Telehom Govsupp NGOsupp childouthome 
    childstreet Victimreg Safetyvio Safetywar
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI 
  /CELLS=COUNT EXPECTED ROW COLUMN TOTAL SRESID BPROP 
  /COUNT ROUND CELL.

T-TEST GROUPS=CSEC(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=Age Foodandmatins Moneyworry Momclose Dadclose Monit Sexinit Sexpar MHsxs
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

CROSSTABS
  /TABLES=CSEC BY Physviolence IPVphystot Physcorp Parentphy Sexabusebroad Sexabusenarrow Witanyvio 
    WitparentIPV Witconflict IPVtotal Secact Firstwanted STI Drugs Selfharm SI 
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI 
  /CELLS=COUNT EXPECTED ROW COLUMN TOTAL SRESID BPROP 
  /COUNT ROUND CELL.
