/*-
     Program: cvs-rsp.p
      Module: d-DO
     Version: 01.17.00
 Description: CVR Header RSP
 Last Change: 02/05/2013
  Changed By: paucoc
-*/
{buildinf.i cvs-rsp-p 01.17.00}
/* cvs-rsp.p - * RSP */

/* standard RSP methods */
&GLOB RSPISTRIGGER *
&GLOB RSPCHGFIELDS *
DEF VAR bInitZero AS LOG NO-UNDO.
DEF VAR bCopyPrevAdj AS LOG NO-UNDO.
{cvs-gen.i &section=RSP}
{coueval.i}
{syu033.i}


/* 'get' methods for extra fields */
DEF TEMP-TABLE ttField NO-UNDO
  FIELD rField  AS ROWID
  FIELD rGroup  AS ROWID
  FIELD iCol    AS INT
  FIELD iRow    AS INT
  FIELD cGroup  AS CHA
  FIELD bStart  AS LOG
  FIELD bDummy  AS LOG
  FIELD bDone   AS LOG
  FIELD iSCol   AS INT
  FIELD iSRow   AS INT
  INDEX idx1 iRow iSRow iCol iSCol
  INDEX idx2 iCol
  INDEX idx3 iSRow iSCol cGroup
  INDEX idx4 bStart
  INDEX idx5 rGroup
  INDEX idx6 bDone.

DEF VAR cInlineViews AS CHA NO-UNDO.
DEF VAR rLastCVR     AS ROWID NO-UNDO.
DEF VAR bDebug       AS LOG NO-UNDO.
{cvuitem.i}

FUNCTION checkUserGroup RETURNS LOGICAL (INPUT pcList AS CHAR):
  DEF VAR li AS INT NO-UNDO.
  IF pcList = "" OR pcList = ? 
    THEN RETURN TRUE.
  IF NOT CAN-DO(pcList,getUID()) THEN
  DO:
    FIND sysuser
      WHERE sysuser.su-userid = getUID()
      NO-LOCK.
    DO li = 1 TO NUM-ENTRIES(sysuser.su-group):
      IF CAN-DO(pcList,ENTRY(li,sysuser.su-group)) THEN
      DO:
        pcList = pcList + "," + getUID().
        LEAVE.
      END.
    END.
  END.
  RETURN CAN-DO(pcList,getUID()).
END FUNCTION. 

FUNCTION getTableFieldEnabled RETURNS CHAR
  (INPUT pcName AS CHAR):
  DEF VAR lcINPCUSER AS CHA NO-UNDO.
  CASE pcName:
    WHEN "cvs_complete" THEN
    DO:
      IF tcv_cvrhead.cvs_complete THEN
      DO:
        IF tcv_cvrhead.cob_numr <> 0 OR tcv_cvrhead.cob_numn <> 0
          THEN RETURN "R".
        IF checkUserGroup(getSysParam(getCID(),"CV":u,"INPCUSER":u)) <> TRUE
          THEN RETURN "R".
      END.
    END.
    WHEN "cvs_locked" THEN
    DO:
      IF (tcv_cvrhead.cvs_complete) OR (NOT tcv_cvrhead.cvs_current) THEN
        RETURN "R".
    END.
  END CASE.
  RETURN "".
END FUNCTION. 

FUNCTION getvp_siteRowid RETURNS CHARACTER():
  FIND vp_site OF tcv_cvrhead
    NO-LOCK NO-ERROR.
  RETURN (IF AVAILABLE vp_site THEN STRING(ROWID(vp_site)) ELSE "").
END FUNCTION.

FUNCTION getpi_projectRowid RETURNS CHARACTER():
  FIND FIRST pi_project
    WHERE pi_project.kco            = tcv_cvrhead.kco
    AND   pi_project.pij_contractno = tcv_cvrhead.job_num
    AND   pi_project.pty_type       = "C"
    NO-LOCK NO-ERROR.
  RETURN (IF AVAILABLE pi_project THEN STRING(ROWID(pi_project)) ELSE "").
END FUNCTION.

FUNCTION getjc_periodRowid RETURNS CHARACTER():
  DEF BUFFER jc_job FOR jc_job.
  DEF BUFFER jc_period FOR jc_period.

  FIND jc_job OF tcv_cvrhead
    NO-LOCK NO-ERROR.
  
  IF jc_job.job_usejobdt = TRUE
  THEN
  FIND FIRST jc_period
    WHERE jc_period.kco     = tcv_cvrhead.kco
    AND jc_period.job_num   = tcv_cvrhead.job_num
    AND jc_period.jcp_date >= tcv_cvrhead.cvp_fdate
    NO-LOCK NO-ERROR.
  ELSE
  FIND FIRST jc_period
    WHERE jc_period.kco     = tcv_cvrhead.kco
    AND jc_period.job_num   = ""
    AND jc_period.jcp_date >= tcv_cvrhead.cvp_fdate
    NO-LOCK NO-ERROR.

  RETURN (IF AVAILABLE jc_period THEN STRING(ROWID(jc_period)) ELSE "").
END FUNCTION.

FUNCTION getgl_periodRowid RETURNS CHARACTER():
  DEF BUFFER gl_period FOR gl_period.
  FIND FIRST gl_period
    WHERE gl_period.kco      = tcv_cvrhead.kco
    AND gl_period.glp_fdate >= tcv_cvrhead.cvp_fdate
    NO-LOCK NO-ERROR.
  RETURN (IF AVAILABLE gl_period THEN STRING(ROWID(gl_period)) ELSE "").
END FUNCTION.

{syudoc.i RO_cvs_AppTypeLabel 0 "Appraisal Type Label" X(60) <AppTypeNum>}
FUNCTION getRO_cvs_AppTypeLabel RETURNS CHARACTER
  (INPUT piLabel AS INTEGER):
  DEF VAR lcDescs AS CHA NO-UNDO.
  FIND co_table
    WHERE co_table.kco     = getCID()
    AND co_table.cta_rtype = "LAAPPRTYPE":u
    AND co_table.cta_code  = "LAAPPRTYPE":u
    NO-LOCK NO-ERROR.
  lcDescs = (IF AVAIL co_table THEN co_table.cta_desc
              ELSE "Preliminary,Residual,Definitive,").
  RETURN TRIM(ENTRY(piLabel,lcDescs + ",,,")).
END FUNCTION.  

{syudoc.i RO_cvs_sumitem_cha 0 "CVR Summary Value (Character)" X(60)
<Item>|<ItemKey>|<Offset>|<Type>|<Key1>|<Key2>|<Key3>|<Key4>}
FUNCTION getRO_cvs_sumitem_cha RETURNS CHAR
  (INPUT pcParam AS CHARACTER):
  DEF VAR lcItem AS CHA NO-UNDO.
  DEF VAR lcIKey AS CHA NO-UNDO.
  DEF VAR lcType AS CHA NO-UNDO.
  DEF VAR lcKey  AS CHA NO-UNDO EXTENT 4.
  DEF VAR lcRef  AS CHA NO-UNDO.
  DEF VAR lcOffset AS CHA NO-UNDO.
 
  pcParam  = pcParam + "|||||||||".
  lcItem   = ENTRY(1,pcParam,"|").
  lcIKey   = ENTRY(2,pcParam,"|").
  lcOffset = ENTRY(3,pcParam,"|").
  lcType   = ENTRY(4,pcParam,"|").
  lcKey[1] = ENTRY(5,pcParam,"|").
  lcKey[2] = ENTRY(6,pcParam,"|").
  lcKey[3] = ENTRY(7,pcParam,"|").
  lcKey[4] = ENTRY(8,pcParam,"|").

  IF lcOffset <> "" AND lcOffset <> "0" 
    THEN lcRef = DYNAMIC-FUNCTION("getCVRPeriodRef" IN getService("cvp-rsp"),
                              tcv_cvrhead.kco,tcv_cvrhead.cvs_ref,lcOffset).
    ELSE lcRef = tcv_cvrhead.cvs_ref.
   
  FIND cv_cvrdet
    WHERE cv_cvrdet.kco    = tcv_cvrhead.kco
    AND cv_cvrdet.cvs_ref  = lcRef
    AND cv_cvrdet.cvi_item = lcItem
    AND cv_cvrdet.cdv_ikey = lcIKey
    AND cv_cvrdet.cdv_type = lcType
    AND cv_cvrdet.cdv_key1 = lcKey[1]
    AND cv_cvrdet.cdv_key2 = lcKey[2]
    AND cv_cvrdet.cdv_key3 = lcKey[3]
    AND cv_cvrdet.cdv_key4 = lcKey[4]
    NO-LOCK NO-ERROR.
  RETURN (IF AVAILABLE cv_cvrdet THEN cv_cvrdet.cdv_char[1] ELSE '').
END FUNCTION.

{syudoc.i RO_cvs_sumitem_dec 0 "CVR Summary Value (Decimal)" 
 ->>>,>>>,>>>,>>9.99 
 <Item>|<ItemKey>|<Offset>|<Type>|<Key1>|<Key2>|<Key3>|<Key4>}
FUNCTION getRO_cvs_sumitem_dec RETURNS DECIMAL
  (INPUT pcParam AS CHARACTER):
  DEF VAR lcItem AS CHA NO-UNDO.
  DEF VAR lcIKey AS CHA NO-UNDO.
  DEF VAR lcType AS CHA NO-UNDO.
  DEF VAR lcKey  AS CHA NO-UNDO EXTENT 4.
  DEF VAR ldTmp  AS DEC NO-UNDO.
  DEF VAR lcRef  AS CHA NO-UNDO.
  DEF VAR lcOffset AS CHA NO-UNDO.
 
  pcParam  = pcParam + "|||||||||".
  lcItem   = ENTRY(1,pcParam,"|").
  lcIKey   = ENTRY(2,pcParam,"|").
  lcOffset = ENTRY(3,pcParam,"|").
  lcType   = ENTRY(4,pcParam,"|").
  lcKey[1] = ENTRY(5,pcParam,"|").
  lcKey[2] = ENTRY(6,pcParam,"|").
  lcKey[3] = ENTRY(7,pcParam,"|").
  lcKey[4] = ENTRY(8,pcParam,"|").

  IF lcOffset <> "" AND lcOffset <> "0" 
    THEN lcRef = DYNAMIC-FUNCTION("getCVRPeriodRef" IN getService("cvp-rsp"),
                              tcv_cvrhead.kco,tcv_cvrhead.cvs_ref,lcOffset).
    ELSE lcRef = tcv_cvrhead.cvs_ref.
    
  FIND cv_cvrdet
    WHERE cv_cvrdet.kco    = tcv_cvrhead.kco
    AND cv_cvrdet.cvs_ref  = lcRef
    AND cv_cvrdet.cvi_item = lcItem
    AND cv_cvrdet.cdv_ikey = lcIKey
    AND cv_cvrdet.cdv_type = lcType
    AND cv_cvrdet.cdv_key1 = lcKey[1]
    AND cv_cvrdet.cdv_key2 = lcKey[2]
    AND cv_cvrdet.cdv_key3 = lcKey[3]
    AND cv_cvrdet.cdv_key4 = lcKey[4]
    NO-LOCK NO-ERROR.
  IF AVAIL cv_cvrdet THEN RETURN cv_cvrdet.cdv_value.

  IF lcKey[1] = "" THEN lcKey[1] = "*".
  IF lcKey[2] = "" THEN lcKey[2] = "*".
  IF lcKey[3] = "" THEN lcKey[3] = "*".
  IF lcKey[4] = "" THEN lcKey[4] = "*".
   
  FOR EACH cv_cvrdet
    WHERE cv_cvrdet.kco    = tcv_cvrhead.kco
    AND cv_cvrdet.cvs_ref  = lcRef
    AND cv_cvrdet.cvi_item = lcItem
    AND cv_cvrdet.cdv_ikey = lcIKey
    AND cv_cvrdet.cdv_type = lcType
    AND CAN-DO(lcKey[1],cv_cvrdet.cdv_key1)
    AND CAN-DO(lcKey[2],cv_cvrdet.cdv_key2)
    AND CAN-DO(lcKey[3],cv_cvrdet.cdv_key3)
    AND CAN-DO(lcKey[4],cv_cvrdet.cdv_key4)
    NO-LOCK:
    ldTmp = ldTmp + cv_cvrdet.cdv_value.
  END.
  RETURN ldTmp.
END FUNCTION.

{syudoc.i RO_cvs_sumitem_int 0 "CVR Summary Value (Integer)" 
 ->>>,>>>,>>>,>>9 
 <Item>|<ItemKey>|<Offset>|<Type>|<Key1>|<Key2>|<Key3>|<Key4>}
FUNCTION getRO_cvs_sumitem_int RETURNS INTEGER
  (INPUT pcParam AS CHARACTER):
  RETURN INT(getRO_cvs_sumitem_dec(pcParam)).
END FUNCTION.

{syudoc.i RO_cvs_sumitem_dat 0 "CVR Summary Value (Date)" 99/99/9999 
 <Item>|<ItemKey>|<Offset>|<Type>|<Key1>|<Key2>|<Key3>|<Key4>}
FUNCTION getRO_cvs_sumitem_dat RETURNS DATE
  (INPUT pcParam AS CHARACTER):
  RETURN DATE(getRO_cvs_sumitem_cha(pcParam)).
END FUNCTION.

{syudoc.i RO_cvs_sumitem_log 0 "CVR Summary Value (Logical)" Y/N 
 <Item>|<ItemKey>|<Offset>|<Type>|<Key1>|<Key2>|<Key3>|<Key4>}
FUNCTION getRO_cvs_sumitem_log RETURNS LOGICAL
  (INPUT pcParam AS CHARACTER):
  RETURN (getRO_cvs_sumitem_cha(pcParam) = "Y").
END FUNCTION.



PROCEDURE setRO_cvs_sumitem_cha:   
  DEF INPUT PARAMETER pcValue AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcParam AS CHAR NO-UNDO.
  {try.i}
  RUN setRO_cvs_sumitem
    (pcParam,"character",?,?,?,?,pcValue).
  {end.i}
END PROCEDURE.

PROCEDURE setRO_cvs_sumitem_dec:   
  DEF INPUT PARAMETER pdValue AS DEC  NO-UNDO.
  DEF INPUT PARAMETER pcParam AS CHAR NO-UNDO.
  {try.i}
  RUN setRO_cvs_sumitem
    (pcParam,"decimal",pdValue,?,?,?,?).
  {end.i}
END PROCEDURE.

PROCEDURE setRO_cvs_sumitem_int:   
  DEF INPUT PARAMETER piValue AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcParam AS CHAR NO-UNDO.
  {try.i}
  RUN setRO_cvs_sumitem
    (pcParam,"integer",?,piValue,?,?,?).
  {end.i}
END PROCEDURE.

PROCEDURE setRO_cvs_sumitem_dat:   
  DEF INPUT PARAMETER pjValue AS CHAR NO-UNDO. 
  /* NB need to use char as bug in framework */
  DEF INPUT PARAMETER pcParam AS CHAR NO-UNDO.
  {try.i}
  RUN setRO_cvs_sumitem
    (pcParam,"date",?,?,DATE(pjValue),?,?).
  {end.i}
END PROCEDURE.

PROCEDURE setRO_cvs_sumitem_log:   
  DEF INPUT PARAMETER pbValue AS LOG  NO-UNDO.
  DEF INPUT PARAMETER pcParam AS CHAR NO-UNDO.
  {try.i}
  RUN setRO_cvs_sumitem
    (pcParam,"logical",?,?,?,pbValue,?).
  {end.i}
END PROCEDURE.


PROCEDURE setRO_cvs_sumitem PRIVATE:   
  DEF INPUT PARAMETER pcParam AS CHA NO-UNDO. 
  DEF INPUT PARAMETER pcType  AS CHA NO-UNDO.
  DEF INPUT PARAMETER pdDec   AS DEC NO-UNDO. 
  DEF INPUT PARAMETER piInt   AS INT NO-UNDO.
  DEF INPUT PARAMETER pjDat   AS DAT NO-UNDO.
  DEF INPUT PARAMETER pbLog   AS LOG NO-UNDO.
  DEF INPUT PARAMETER pcCha   AS CHA NO-UNDO.

  DEF VAR lcItem AS CHA NO-UNDO.
  DEF VAR lcIKey AS CHA NO-UNDO.
  DEF VAR lcOffset AS CHA NO-UNDO.
  DEF VAR lcType AS CHA NO-UNDO.
  DEF VAR lcKey  AS CHA NO-UNDO EXTENT 4.
  DEF BUFFER cv_cvrdet FOR cv_cvrdet. 
  
  {try.i}
  {syustamp.i &vars=*}
  pcParam  = pcParam + "|||||||||".
  lcItem = ENTRY(1,pcParam,"|").
  lcIKey = ENTRY(2,pcParam + "|","|").
  lcOffset = ENTRY(3,pcParam,"|").
  lcType   = ENTRY(4,pcParam,"|").
  lcKey[1] = ENTRY(5,pcParam,"|").
  lcKey[2] = ENTRY(6,pcParam,"|").
  lcKey[3] = ENTRY(7,pcParam,"|").
  lcKey[4] = ENTRY(8,pcParam,"|").

  {sy_lock.i
    &table = cv_cvrdet
    &condition = "
      WHERE cv_cvrdet.kco    = tcv_cvrhead.kco
      AND cv_cvrdet.cvs_ref  = tcv_cvrhead.cvs_ref
      AND cv_cvrdet.cvi_item = lcItem
      AND cv_cvrdet.cdv_ikey = lcIKey
      AND cv_cvrdet.cdv_type = lcType
      AND cv_cvrdet.cdv_key1 = lcKey[1]
      AND cv_cvrdet.cdv_key2 = lcKey[2]
      AND cv_cvrdet.cdv_key3 = lcKey[3]
      AND cv_cvrdet.cdv_key4 = lcKey[4]"
    &excludeNoAvail = *
  }

  IF NOT AVAILABLE cv_cvrdet THEN 
  DO:
    CREATE cv_cvrdet.
    ASSIGN
      cv_cvrdet.kco      = tcv_cvrhead.kco
      cv_cvrdet.cvs_ref  = tcv_cvrhead.cvs_ref
      cv_cvrdet.cvi_item = lcItem
      cv_cvrdet.cdv_ikey = lcIKey
      cv_cvrdet.cdv_type = lcType
      cv_cvrdet.cdv_key1 = lcKey[1]
      cv_cvrdet.cdv_key2 = lcKey[2]
      cv_cvrdet.cdv_key3 = lcKey[3]
      cv_cvrdet.cdv_key4 = lcKey[4].
  END.

  ASSIGN 
    cv_cvrdet.cdv_oride   = cv_cvrdet.cdv_recalc
    cv_cvrdet.cdv_upddate = TODAY
    cv_cvrdet.cdv_updtime = TIME.
  cv_cvrdet.cdv_upduser = getUID().
  
  IF cv_cvrdet.cdv_oride 
  THEN
  ASSIGN
    cv_cvrdet.cdv_ordate  = cv_cvrdet.cdv_upddate
    cv_cvrdet.cdv_ortime  = cv_cvrdet.cdv_updtime
    cv_cvrdet.cdv_oruser  = cv_cvrdet.cdv_upduser
    cv_cvrdet.cdv_orgcha  = cv_cvrdet.cdv_char[1]
    cv_cvrdet.cdv_orgval  = cv_cvrdet.cdv_value.
    
  CASE pcType:
    WHEN "decimal" 
    THEN 
    ASSIGN
      cv_cvrdet.cdv_value   = pdDec
      cv_cvrdet.cdv_char[1] = TRIM(STRING(pdDec,"->>>,>>>,>>>,>>>,>>9.99")).
    WHEN "date" 
    THEN 
    ASSIGN
      cv_cvrdet.cvd_date[1] = pjDat
      cv_cvrdet.cdv_char[1] = STRING(pjDat,"99/99/9999").
    WHEN "integer"
    THEN 
    ASSIGN
      cv_cvrdet.cdv_value   = piInt
      cv_cvrdet.cdv_char[1] = TRIM(STRING(piInt,"->>>,>>>,>>>,>>>,>>9.99")).
    WHEN "logical" 
    THEN 
    ASSIGN
      cv_cvrdet.cdv_log[1]  = pbLog
      cv_cvrdet.cdv_char[1] = STRING(pbLog,"Y/N").
    WHEN "character" 
    THEN 
    ASSIGN
      cv_cvrdet.cdv_char[1] = pcCha.
  END CASE.
  
  {syustamp.i &stamp=* &table=cv_cvrdet &TABLE_ID=cdv}
  {end.i &always="RELEASE cv_cvrdet."}
END PROCEDURE.


{syudoc.i getRO_cvs_descfper 0 "CVR Description and Period" X(50)}
FUNCTION getRO_cvs_descfper RETURNS CHARACTER():
  RETURN tcv_cvrhead.cvs_desc + " (" + STRING(tcv_cvrhead.cvp_fdate) + ")".
END FUNCTION.

{syudoc.i getRO_cvs_status 0 "CVR Status" X(15)}
FUNCTION getRO_cvs_status RETURNS CHARACTER():
  CASE tcv_cvrhead.cvs_status:
    WHEN "INCOMPLETE":u THEN RETURN xText("Incomplete").
    WHEN "COMPLETE":u THEN RETURN xText("Complete").
    WHEN "AUTHORISED":u THEN RETURN xText("Author~{is~}ed").
  END CASE.
  RETURN "".
END FUNCTION.

{syudoc.i getRO_las_sitename 0 "Appraisal Site Name" X(40)}
FUNCTION getRO_las_sitename RETURNS CHARACTER():
  FIND la_site
    WHERE la_site.kco = tcv_cvrhead.laa_kco
    AND la_site.las_site  = tcv_cvrhead.las_site
    NO-LOCK NO-ERROR.
  RETURN (IF AVAIL la_site THEN la_site.las_sitename 
            ELSE xText("<No Appraisal Site>")).
END FUNCTION.

{syudoc.i getRO_cvs_prepusername 0 "Prepared By Name" X(30)}
FUNCTION getRO_cvs_prepusername RETURNS CHARACTER():
  FIND sysuser
    WHERE sysuser.su-userid = tcv_cvrhead.cvs_prepuser
    NO-LOCK NO-ERROR.
  RETURN (IF AVAILABLE sysuser THEN sysuser.su-name ELSE "").
END FUNCTION.

FUNCTION getRO_cvs_FirstPeriod RETURNS DATE():
  FIND FIRST cv_period
    WHERE cv_period.kco     = tcv_cvrhead.kco
    AND cv_period.job_num   = tcv_cvrhead.job_num
    AND cv_period.jph_phase = tcv_cvrhead.jph_phase
    AND cv_period.cvs_ref   <> ''
    NO-LOCK NO-ERROR.
  RETURN (IF AVAILABLE cv_period THEN cv_period.cvp_fdate ELSE ?).
END FUNCTION.

FUNCTION getRO_cvs_First12Period RETURNS DATE():
  DEF VAR li AS INT NO-UNDO.
  FOR EACH cv_period
    WHERE cv_period.kco      = tcv_cvrhead.kco
    AND cv_period.job_num    = tcv_cvrhead.job_num
    AND cv_period.jph_phase  = tcv_cvrhead.jph_phase
    AND cv_period.cvp_fdate <= tcv_cvrhead.cvp_fdate
    AND cv_period.cvs_ref   <> ''
    NO-LOCK
    BY cv_period.cvp_fdate DESCENDING:
    li = li + 1.
    IF li > 12
      THEN RETURN cv_period.cvp_fdate.
  END.
  RETURN tcv_cvrhead.cvp_fdate.
END FUNCTION.


FUNCTION getRO_cvs_First6Period RETURNS DATE():
  DEF VAR li AS INT NO-UNDO.
  FOR EACH cv_period
    WHERE cv_period.kco      = tcv_cvrhead.kco
    AND cv_period.job_num    = tcv_cvrhead.job_num
    AND cv_period.jph_phase  = tcv_cvrhead.jph_phase
    AND cv_period.cvp_fdate <= tcv_cvrhead.cvp_fdate
    AND cv_period.cvs_ref   <> ''
    NO-LOCK
    BY cv_period.cvp_fdate DESCENDING:
    li = li + 1.
    IF li > 6
      THEN RETURN cv_period.cvp_fdate.
  END.
  RETURN tcv_cvrhead.cvp_fdate.
END FUNCTION.


FUNCTION getRO_cmh_reppage RETURNS CHARACTER():
  FIND cv_model OF tcv_cvrhead
    NO-LOCK NO-ERROR.
  RETURN TRIM({syattr.i &param=cv_model.cmh_param &attr=cmh_reppage
                        &default=%A4RLAND}).
END FUNCTION.

FUNCTION getRO_journal_post RETURNS LOG():
  IF tcv_cvrhead.cvs_complete AND 
    (tcv_cvrhead.cob_numr <> 0 OR tcv_cvrhead.cob_numn <> 0) THEN 
    RETURN TRUE.
  ELSE 
    RETURN FALSE.
END FUNCTION. 

FUNCTION getRO_budget_post RETURNS LOG():
  RETURN ({syattr.i &param=tcv_cvrhead.cvs_param
                    &attr="BUDGETUPD" &default=N} = "Y").
END FUNCTION. 


FUNCTION getValuecmd_type RETURNS CHARACTER
  (INPUT pcValue AS CHARACTER):
  DEF VAR liTmp AS INT NO-UNDO.
  liTmp = LOOKUP(pcValue,'TEXT,ITEM,FIELD,GROUP,CALC,IVIEW,FCAST').
  RETURN (IF liTmp = 0 THEN pcValue
            ELSE ENTRY(liTmp,'Text Field,CVR Item,RSP Field,Field Group,'
                      + 'Calculation,Inline View,Forecast Item')).
END FUNCTION.


FUNCTION getValueRO_cvs_debugtooltip RETURNS CHAR
  (INPUT pcValue AS CHAR,INPUT pcParam AS CHAR):
  DEF VAR lcTmp   AS CHA   NO-UNDO.
  DEF BUFFER grp-mfield FOR cv_mfield.

  FIND cv_mfield
    WHERE ROWID(cv_mfield) = TO-ROWID(ENTRY(1,pcParam,"|"))
    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE cv_mfield 
    THEN RETURN xText("<No information available>").
  RELEASE cv_mfgrp.
  
  IF ENTRY(1,pcParam,"|") <> ENTRY(2,pcParam,"|") THEN
  DO:
    FIND grp-mfield
      WHERE ROWID(grp-mfield) = TO-ROWID(ENTRY(2,pcParam,"|"))
      NO-LOCK NO-ERROR.
    FIND cv_mview OF grp-mfield
      NO-LOCK.
    FIND cv_mfgrp OF cv_mfield
      NO-LOCK.
  END.
  ELSE DO:
    FIND cv_mview OF cv_mfield
      NO-LOCK.
  END.
  
  lcTmp = "View: " + cv_mview.cmv_view + " - " + cv_mview.cmv_desc + CHR(10).
   
  IF AVAILABLE cv_mfgrp
    THEN lcTmp = lcTmp + "Field Group: " + cv_mfgrp.cmi_group 
                       + " - " + cv_mfgrp.cmi_desc
                       + CHR(10)
                       + "Mask Substitution: " + grp-mfield.cmd_grpsub 
                       + CHR(10).

  lcTmp = lcTmp + CHR(10)
            + "Field Sequence: " + STRING(cv_mfield.cmd_seq) + CHR(10)
            + "Field Type: " + getValuecmd_type(cv_mfield.cmd_type) + CHR(10)
            + "Field Calculation: " + cv_mfield.cmd_calc + CHR(10)
            + "Column Spans: " + STRING(cv_mfield.cmd_colspan) + CHR(10)
            + "Page Width: " + TRIM({syattr.i &param=cv_mfield.cmd_param
                                            &attr=cmd_pwidth &default=" "})
                             + CHR(10)
            + (IF cv_mfield.cmd_type = "ITEM":u
                 AND AVAILABLE cv_mfgrp
                 AND cv_mfgrp.cmi_char[1] = "SUMMARY":u
                 THEN "Summary Level: " + cv_mfield.cmd_ikey + CHR(10)
                    + "Summary Mask: " 
                    + TRIM({syattr.i &param=cv_mfield.cmd_param
                                     &attr=cmd_mask &default=" "}) 
                    + CHR(10)
                 ELSE "").


  CASE cv_mfield.cmd_type:
    WHEN "ITEM" THEN
    DO:
      FIND cv_item OF cv_mfield
        NO-LOCK NO-ERROR.
      lcTmp = lcTmp + CHR(10) 
                + "Item: " + cv_mfield.cvi_item + " - " 
                + (IF AVAILABLE cv_item THEN cv_item.cvi_desc
                     ELSE "*** Item Deleted ***") + CHR(10)
                + "Item Offset: " + TRIM({syattr.i &param=cv_mfield.cmd_param
                                            &attr=cmd_offset &default=" "}).
      IF AVAIL cv_item
        THEN lcTmp = lcTmp + CHR(10)
                 + "Item Type: " + DYNAMIC-FUNCTION("getValuecvi_value"
                                  IN getService("cvi-rsp"),cv_item.cvi_value)
                 + CHR(10)
                 + (IF SUBSTRING(cv_item.cvi_value,4,3) = "RSP"
                      THEN "Item RSP Field: " + cv_item.cvi_field + CHR(10)
                      ELSE "")
                 + (IF SUBSTRING(cv_item.cvi_value,4,3) = "CAL"
                      THEN "Item Calculation: " + cv_item.cvi_calc + CHR(10)
                      ELSE "")
                 + xText("Item Summar~{is~}ed By: ") 
                 + cv_item.cvi_sumtype + CHR(10)
                 + "Data Type: " + cv_item.cvi_dtype + CHR(10)
                 + "Table: " + cv_item.cvi_table + CHR(10)
                 + "Snapshot Item: " + string(cv_item.cvi_snapshot,"Y/N") 
                                     + CHR(10)
                 + "Recalc Dependents: " + STRING(({syattr.i 
                         &param=cv_item.cvi_param &attr=cvi_recalcdep
                         &default=N} BEGINS "Y"),"Y/N").
                                  
    END.
    WHEN "FIELD" THEN
    DO:
      lcTmp = lcTmp + CHR(10)
                + "RSP Field: " + cv_mfield.cmd_value.
    END.
    WHEN "TEXT" THEN
    DO:
      lcTmp = lcTmp + CHR(10)
                + "Text Field: " + cv_mfield.cmd_label.
    END.
  END CASE.
  
  RETURN lcTmp.
END FUNCTION.  


{syudoc.i RO_cvs_IVRowid 0 "Inline View Rowid" X(20) <ViewNum>}
FUNCTION getRO_cvs_IVRowid RETURNS CHARACTER
  (INPUT pcView AS CHAR):
  DEF VAR lcInlineViews AS CHA NO-UNDO.
  lcInlineviews = getVar("inlineViews").
  FIND cv_mview
    WHERE cv_mview.kco = tcv_cvrhead.kco
    AND cv_mview.cmh_model = tcv_cvrhead.cmh_model
    AND cv_mview.cmv_view = ENTRY(INTEGER(pcView),lcInLineViews)
    NO-LOCK NO-ERROR.
  RETURN STRING(ROWID(cv_mview)).
END FUNCTION.

{syudoc.i RO_cvs_IVSection 0 "Inline View Page Section" X(20) <ViewNum>}
FUNCTION getRO_cvs_IVSection RETURNS CHARACTER
  (INPUT pcView AS CHAR):
  DEF VAR lcInlineViews AS CHA NO-UNDO.
  lcInlineviews = getVar("inlineViews").
  FIND cv_mview
    WHERE cv_mview.kco = tcv_cvrhead.kco
    AND cv_mview.cmh_model = tcv_cvrhead.cmh_model
    AND cv_mview.cmv_view = ENTRY(INTEGER(pcView),lcInLineViews)
    NO-LOCK NO-ERROR.
  RETURN (IF cv_mview.cmv_type = "CHBRWS" THEN "%WCV1012BJSC" 
                 ELSE IF cv_mview.cmv_type = "PLOTBRWS" THEN "%WCV1012BCDW" 
                 ELSE IF cv_mview.cmv_type = "SUBBRWS" THEN "%WCV1012BSBS" 
                 ELSE IF cv_mview.cmv_type = "MATBRWS" THEN "%WCV1012BPOH" 
                 ELSE IF cv_mview.cmv_type = "CATBRWS" THEN "%WCV1012BCAT" 
                 ELSE IF cv_mview.cmv_type = "WBSBRWS" THEN "%WCV1012BJWB" 
                 ELSE IF cv_mview.cmv_type = "SECBRWS" THEN "%WCV1012BJCS" 
                 ELSE IF cv_mview.cmv_type = "ACTBRWS" THEN "%WCV1012BJCA" 
                 ELSE IF cv_mview.cmv_type = "JCCBRWS" THEN "%WCV1012BJCC" 
                 ELSE IF cv_mview.cmv_type = "PERBRWS" THEN "%WCV1012BCVP" 
                 ELSE IF cv_mview.cmv_type BEGINS "CHANAL" 
                                                  THEN "%WCV1012BCOA" 
                 ELSE IF cv_mview.cmv_type = "JOBSUM" THEN "%WCV1014SCVS" 
                 ELSE "%WCV1011BCVS").
END FUNCTION.

{syudoc.i RO_cvs_IVViewAs 0 "Inline View As Chart" X(20) <ViewNum>}
FUNCTION getRO_cvs_IVViewAS RETURNS CHARACTER
  (INPUT pcView AS CHAR):
  DEF VAR lcInlineViews AS CHA NO-UNDO.
  lcInlineviews = getVar("inlineViews").
  FIND cv_mview
    WHERE cv_mview.kco = tcv_cvrhead.kco
    AND cv_mview.cmh_model = tcv_cvrhead.cmh_model
    AND cv_mview.cmv_view = ENTRY(INTEGER(pcView),lcInLineViews)
    NO-LOCK NO-ERROR.
  RETURN cv_mview.cmv_chart.
END FUNCTION.

FUNCTION getRO_cvs_itembdquery RETURNS CHARACTER():
  CASE getVar("parentTable"):
    WHEN "jc_wbs" THEN
    DO:
      FIND jc_wbs
        WHERE ROWID(jc_wbs) = TO-ROWID(getVar("jc_wbsRowid"))
        NO-LOCK NO-ERROR.
      IF NOT AVAIL jc_wbs
       THEN throwError({msg.i SY719 'jc_wbs' 'cvs-rsp.getRO_cvs_itemdbquery'}).
      RETURN " AND jc_costcode.jcs_section = '" + jc_wbs.jcs_section + "'"
           + " AND jc_costcode.jca_activity = '" + jc_wbs.jca_activity + "'".
    END.
    WHEN "jc_section" THEN
    DO:
      FIND jc_section
        WHERE ROWID(jc_section) = TO-ROWID(getVar("jc_sectionRowid"))
        NO-LOCK NO-ERROR.
      IF NOT AVAIL jc_section
        THEN throwError({msg.i SY719 'jc_section'
                                     'cvs-rsp.getRO_cvs_itemdbquery'}).
      RETURN " AND jc_costcode.jcs_section = '" + jc_section.jcs_section + "'".
    END.
    WHEN "jc_jactivity" THEN
    DO:
      FIND jc_jactivity
        WHERE ROWID(jc_jactivity) = TO-ROWID(getVar("jc_jactivityRowid"))
        NO-LOCK NO-ERROR.
      IF NOT AVAIL jc_jactivity
        THEN throwError({msg.i SY719 'jc_jactivity'
                                     'cvs-rsp.getRO_cvs_itemdbquery'}).
      RETURN " AND jc_costcode.jca_activity = '" 
                     + jc_jactivity.jca_activity + "'".
    END.
    WHEN "jc_stndcc" THEN
    DO:
      FIND jc_stndcc
        WHERE ROWID(jc_stndcc) = TO-ROWID(getVar("jc_stndccRowid"))
        NO-LOCK NO-ERROR.
      IF NOT AVAIL jc_stndcc
        THEN throwError({msg.i SY719 'jc_stndcc'
                                     'cvs-rsp.getRO_cvs_itemdbquery'}).
      RETURN " AND jc_costcode.jsc_cc = '" + jc_stndcc.jsc_cc + "'".
    END.
    WHEN "co_analysis" THEN
    DO:
      FIND co_analysis
        WHERE ROWID(co_analysis) = TO-ROWID(getVar("co_analysisRowid"))
        NO-LOCK NO-ERROR.
      IF NOT AVAIL co_analysis
        THEN throwError({msg.i SY719 'co_analysis'
                                     'cvs-rsp.getRO_cvs_itemdbquery'}).
      RETURN " AND jc_costcode.jcc_anal[" 
                     + STRING(co_analysis.coa_analysis) + "] = '"
                     + co_analysis.coa_code + "'".
    END.
  END CASE.
  RETURN "".
END FUNCTION.

FUNCTION getRO_cvs_repxquery RETURNS CHARACTER():
  DEF VAR lrView   AS ROWID NO-UNDO.
  DEF VAR lrInline AS ROWID NO-UNDO.
  lrView = TO-ROWID(getVar("cv_mviewRowid")).
  lrInline = TO-ROWID(getVar("inlineViewRowid")).
  FIND cv_mview
    WHERE ROWID(cv_mview) = (IF lrInline <> ? THEN lrInline ELSE lrView)
    NO-LOCK NO-ERROR.
  RETURN (IF AVAIL cv_mview THEN cv_mview.cmv_char[2] ELSE "").
END FUNCTION.

FUNCTION getRO_cvs_repanalset RETURNS CHARACTER():
  DEF VAR lrView   AS ROWID NO-UNDO.
  DEF VAR lrInline AS ROWID NO-UNDO.
  lrView = TO-ROWID(getVar("cv_mviewRowid")).
  lrInline = TO-ROWID(getVar("inlineViewRowid")).
  FIND cv_mview
    WHERE ROWID(cv_mview) = (IF lrInline <> ? THEN lrInline ELSE lrView)
    NO-LOCK NO-ERROR.
  RETURN (IF AVAIL cv_mview AND cv_mview.cmv_type BEGINS "CHANAL"
              THEN SUBSTRING(cv_mview.cmv_type,7,1) ELSE "").
END FUNCTION.

FUNCTION noCVRCondition RETURNS LOG
  (INPUT prRow AS ROWID,INPUT pcParam AS CHAR):
  DEF VAR ljPeriod AS DATE NO-UNDO.
  {try.i}
  ljPeriod = DATE(pcParam) NO-ERROR.

  FIND jc_job WHERE ROWID(jc_job) = prRow NO-LOCK NO-ERROR.
  FIND FIRST cv_cvrhead OF jc_job 
    WHERE cv_cvrhead.cvp_fdate = ljPeriod 
    AND cv_cvrhead.cvs_cvrno <> "SNAPSHOT":u
    NO-LOCK NO-ERROR.
  RETURN (NOT AVAILABLE cv_cvrhead).
  {end.i}
END FUNCTION.

FUNCTION buildPostedFlag RETURNS LOG(INPUT pcParam AS CHAR):
  FIND jc_job 
    WHERE ROWID(jc_job) = TO-ROWID(getVar("jc_jobRowid"))
    NO-LOCK NO-ERROR.
  IF NOT AVAIL jc_job
    THEN throwError({msg.i SY719 'jc_job' 'cvs-rsp.buildPostedFlag'}).
  
  FIND cv_model 
    WHERE cv_model.kco = jc_job.kco
    AND cv_model.cmh_model = jc_job.cmh_model
    NO-LOCK NO-ERROR.
    
  IF AVAILABLE cv_model THEN DO:
    IF CAN-FIND(FIRST cv_item
               WHERE cv_item.kco = cv_model.kco
               AND (IF pcParam = "J" THEN cv_item.cvi_journal = TRUE ELSE TRUE)
               AND (IF pcParam = "B" THEN cv_item.cvi_budget_post = TRUE 
                                     ELSE TRUE)
               AND CAN-FIND(FIRST cv_mfield
                         WHERE cv_mfield.kco     = cv_mode.kco
                         AND cv_mfield.cmh_model = cv_mode.cmh_model
                         AND cv_mfield.cvi_item  = cv_item.cvi_item)) 
        THEN RETURN TRUE.
    ELSE 
      RETURN FALSE.
  END.
  ELSE 
    RETURN FALSE.
END FUNCTION.

FUNCTION buildProgressStatus RETURNS LOGICAL():
  RETURN CAN-FIND(cv_cvrprog
               WHERE cv_cvrprog.kco      = tcv_cvrhead.kco
               AND cv_cvrprog.cdp_type   = 'UPDATE':U
               AND cv_cvrprog.cvs_ref    = tcv_cvrhead.cvs_ref
               AND cv_cvrprog.cdp_progress < 100).
END FUNCTION.

FUNCTION buildNotes RETURNS LOGICAL():
  DEF VAR lrView AS ROWID NO-UNDO.
  lrView = TO-ROWID(getVar('cv_mviewRowid')).
  FIND cv_mview
    WHERE ROWID(cv_mview) = lrView 
    NO-LOCK NO-ERROR.
  RETURN ({syattr.i &param=cv_mview.cmv_param
                    &attr=cmv_notes &default=N} = "Y"
             AND cv_mview.cmv_type <> "JOBSUM":u).
END FUNCTION. 


/* maintained RSP methods */ 
PROCEDURE createAssign:
  DEF VAR lrJob AS ROWID NO-UNDO.
  {try.i}
  lrJob = TO-ROWID(getVar("jc_jobRowid")).
 
  FIND jc_job
    WHERE ROWID(jc_job) = lrJob
    NO-LOCK NO-ERROR.
  IF NOT AVAIL jc_job
    THEN throwError({msg.i SY719 'jc_job' 'cvs-rsp.createAssign'}).
  
  IF jc_job.cmh_model = ""
    THEN throwError({msg.i CV106}).
  
  ASSIGN
    tcv_cvrhead.kco       = jc_job.kco
    tcv_cvrhead.job_num   = jc_job.job_num
    tcv_cvrhead.jph_phase = ""
    tcv_cvrhead.cmh_model = jc_job.cmh_model.
  {end.i}
END PROCEDURE. /* createAssign */


PROCEDURE createDefault:
  DEF BUFFER xcv_cvrhead FOR cv_cvrhead.
  DEF VAR liRef AS INT NO-UNDO INIT 0.
  DEF VAR ljTmp AS DAT NO-UNDO.
  DEF VAR lcPERNEW AS CHA NO-UNDO.
  DEF VAR lcUpdateContext AS CHAR      NO-UNDO.

  {try.i}
  
  lcUpdateContext = getVar('updateContext').
  IF lcUpdateContext = ? THEN lcUpdateContext = ''.
  
  IF NOT CAN-DO("CreateNew",lcUpdateContext) THEN DO:
  FIND LAST xcv_cvrhead
    WHERE xcv_cvrhead.kco     = tcv_cvrhead.kco
    AND xcv_cvrhead.job_num   = tcv_cvrhead.job_num
    AND xcv_cvrhead.jph_phase = tcv_cvrhead.jph_phase
    AND xcv_cvrhead.cvs_cvrno > ""
    AND xcv_cvrhead.cvs_cvrno <> "SNAPSHOT":u
    NO-LOCK NO-ERROR.
  
  IF AVAILABLE xcv_cvrhead THEN 
  DO:
    liRef = INTEGER(xcv_cvrhead.cvs_cvrno) NO-ERROR.
    IF liRef <> 0 AND liRef <> ? THEN
      tcv_cvrhead.cvs_cvrno = STRING(liRef + 1,"9999").
  END.
  ELSE DO:
    tcv_cvrhead.cvs_cvrno = "0001".
  END.
  
  ljTmp = DATE(getVar("CVRPeriod")) NO-ERROR.
  tcv_cvrhead.cvp_fdate    = ljTmp.
  tcv_cvrhead.cvs_prepuser = getUID().
  tcv_cvrhead.cvs_current  = TRUE.
  END.

  tcv_cvrhead.cvs_status   = "INCOMPLETE".
     
  IF getVar("updateContext") = "SNAPSHOT" THEN
  DO:
    ASSIGN
      tcv_cvrhead.cvs_status   = "SNAPSHOT"
      tcv_cvrhead.cvs_current  = FALSE
      tcv_cvrhead.cvs_latest   = FALSE.
  END.
  ELSE DO:
    lcPERNEW = getSysParam(getCID(),"CV":u,"PERNEW":u).
    IF LOOKUP(lcPERNEW,"CALU,CALM") > 0 THEN
    DO:
      FIND cv_period
        WHERE cv_period.kco       = tcv_cvrhead.kco
        AND cv_period.job_num     = ""
        AND cv_period.jph_phase   = ""
        AND cv_period.cpv_current = TRUE
        NO-LOCK NO-ERROR.
      IF NOT AVAILABLE cv_period
        THEN throwError({msg.i CV125}).
      tcv_cvrhead.cvp_fdate = cv_period.cvp_fdate.
    END.
  END.
  {end.i}
END PROCEDURE. /* createDefault */


PROCEDURE preWrite:
  DEF BUFFER xcv_cvrhead FOR cv_cvrhead.
  {try.i}
  IF tcv_cvrhead.cvs_cvrno = ? 
    OR TRIM(tcv_cvrhead.cvs_cvrno) = ""
    THEN throwError({msg.i CV107}).
  
  IF getVar("updateContext") <> "SNAPSHOT" THEN
  DO:
    FIND FIRST xcv_cvrhead
      WHERE xcv_cvrhead.kco     = tcv_cvrhead.kco
      AND xcv_cvrhead.job_num   = tcv_cvrhead.job_num
      AND xcv_cvrhead.jph_phase = tcv_cvrhead.jph_phase
      AND xcv_cvrhead.cvs_cvrno = tcv_cvrhead.cvs_cvrno
      AND ROWID(xcv_cvrhead) <> tcv_cvrhead.rsp_dbrowid
      NO-LOCK NO-ERROR.
    IF AVAILABLE xcv_cvrhead
      THEN throwError({msg.i CV108}).
    IF tcv_cvrhead.cvs_cvrno = "SNAPSHOT"
      THEN throwError({msg.i CV115}).

    IF tcv_cvrhead.cvs_latest = NO
    AND NOT CAN-FIND(FIRST xcv_cvrhead
                  WHERE xcv_cvrhead.kco       = tcv_cvrhead.kco
                  AND xcv_cvrhead.job_num     = tcv_cvrhead.job_num
                  AND xcv_cvrhead.jph_phase   = tcv_cvrhead.jph_phase
                  AND xcv_cvrhead.cvs_latest  = TRUE
                  AND ROWID(xcv_cvrhead)      <> tcv_cvrhead.rsp_dbrowid)
      THEN throwError({msg.i CV142}).
 
    IF tcv_cvrhead.cvs_current = NO
    AND NOT CAN-FIND(FIRST xcv_cvrhead
                  WHERE xcv_cvrhead.kco       = tcv_cvrhead.kco
                  AND xcv_cvrhead.job_num     = tcv_cvrhead.job_num
                  AND xcv_cvrhead.jph_phase   = tcv_cvrhead.jph_phase
                  AND xcv_cvrhead.cvp_fdate   = tcv_cvrhead.cvp_fdate
                  AND xcv_cvrhead.cvs_current = TRUE
                  AND ROWID(xcv_cvrhead)      <> tcv_cvrhead.rsp_dbrowid)
      THEN throwError({msg.i CV143}).
  END.
   
  ASSIGN
    tcv_cvrhead.cvs_savetime = TIME
    tcv_cvrhead.cvs_savedate = TODAY.
  tcv_cvrhead.cvs_saveuser = getUID().
/*  tcv_cvrhead.cvs_locked = tcv_cvrhead.cvs_complete. */
  tcv_cvrhead.cvs_status = (IF tcv_cvrhead.cvs_authorised THEN "AUTHORISED":u
                             ELSE IF tcv_cvrhead.cvs_complete THEN "COMPLETE":u
                             ELSE "INCOMPLETE":u).
  {end.i}
END PROCEDURE. /* preWrite */


PROCEDURE preUpdate:
  {try.i}
  IF CAN-DO(cChangedFields,"cvs_complete") THEN
  DO:
    tcv_cvrhead.cvs_compdate = (IF tcv_cvrhead.cvs_complete
                                  THEN TODAY ELSE ?).
  END.
  IF CAN-DO(cChangedFields,"cvs_locked") THEN
  DO:
    tcv_cvrhead.cvs_lockdate = (IF tcv_cvrhead.cvs_locked
                                  THEN TODAY ELSE ?).
    tcv_cvrhead.cvs_lockby =   (IF tcv_cvrhead.cvs_locked
                                  THEN getUID() ELSE '').
  END.
  {end.i}
END PROCEDURE. /* preUpdate */

PROCEDURE postUpdate:
  {try.i}

  IF CAN-DO(cChangedFields,"cvs_locked") THEN
  DO:
    IF tcv_cvrhead.cvs_locked AND
      getSysParam(tcv_cvrhead.kco,"CV":u,"LOCKUPD":u) = "Y" 
      THEN RUN submitUpdate.
  END.
  {end.i}
END PROCEDURE. /* postUpdate */

PROCEDURE postWrite:
  DEF BUFFER xcv_cvrhead FOR cv_cvrhead.
  DEF BUFFER xcv_period FOR cv_period.
  {syustamp.i &vars=*}
  {try.i}
  IF tcv_cvrhead.cvs_current 
  THEN
  FOR EACH xcv_cvrhead
    WHERE xcv_cvrhead.kco     = tcv_cvrhead.kco
    AND xcv_cvrhead.job_num   = tcv_cvrhead.job_num
    AND xcv_cvrhead.jph_phase = tcv_cvrhead.jph_phase
    AND xcv_cvrhead.cvs_current = TRUE
    AND ROWID(xcv_cvrhead) <> tcv_cvrhead.rsp_dbrowid
    AND xcv_cvrhead.cvp_fdate = tcv_cvrhead.cvp_fdate:
    xcv_cvrhead.cvs_current = NO.
    {syustamp.i &stamp=* &table=xcv_cvrhead &table_id=cvs}
  END.
 
  IF tcv_cvrhead.cvs_latest
  THEN
  FOR EACH xcv_cvrhead
    WHERE xcv_cvrhead.kco     = tcv_cvrhead.kco
    AND xcv_cvrhead.job_num   = tcv_cvrhead.job_num
    AND xcv_cvrhead.jph_phase = tcv_cvrhead.jph_phase
    AND xcv_cvrhead.cvs_latest = TRUE
    AND ROWID(xcv_cvrhead) <> tcv_cvrhead.rsp_dbrowid:
    xcv_cvrhead.cvs_latest = NO.
    {syustamp.i &stamp=* &table=xcv_cvrhead &table_id=cvs}
  END.

  IF getVar("updateContext") <> "SNAPSHOT" 
    THEN RUN updateCalendar.
  {end.i}
END PROCEDURE. /* postWrite */


PROCEDURE preInsert:
  DEF VAR liRef AS INT NO-UNDO.
  {try.i}
  RUN txnno2.p (tcv_cvrhead.kco,"CV_CVRHEAD",OUTPUT liRef).
  tcv_cvrhead.cvs_ref = STRING(liRef,"99999999").
  tcv_cvrhead.cvs_compdate = (IF tcv_cvrhead.cvs_complete
                                  THEN TODAY ELSE ?).

  IF getSysParam(tcv_cvrhead.kco,"CV":u,"LOCKCVR":u) = "Y" THEN 
  DO:                              
    tcv_cvrhead.cvs_lockdate = (IF tcv_cvrhead.cvs_locked
                                  THEN TODAY ELSE ?).
    tcv_cvrhead.cvs_lockby =   (IF tcv_cvrhead.cvs_locked
                                  THEN getUID() ELSE '').
  END.
  {end.i}
END PROCEDURE. /* preInsert */


PROCEDURE postInsert:
  {try.i}
  IF getButton() <> "copy":u 
   AND getVar("updateContext") <> "SNAPSHOT":u
   THEN RUN submitUpdate.
  {end.i}
END PROCEDURE. /* postInsert */

PROCEDURE createCopy:
  DEF INPUT PARAMETER prRowid AS ROWID NO-UNDO.
  DEF VAR liRef AS INT NO-UNDO INIT 0.
  DEF VAR lcTmp AS CHA NO-UNDO.
  DEF VAR lcPERCOPY AS CHA NO-UNDO.
  DEF BUFFER xcv_cvrhead FOR cv_cvrhead.

  {try.i}
  FIND cv_cvrhead
    WHERE ROWID(cv_cvrhead) = prRowid
    NO-LOCK.
  
  IF cv_cvrhead.cvs_sysstat <> "" THEN
  DO:
    lcTmp = (IF cv_cvrhead.cvs_sysstat = "COPYING" THEN xText("copied")
               ELSE IF cv_cvrhead.cvs_sysstat = "DELETING" THEN xText("deleted")
               ELSE IF cv_cvrhead.cvs_sysstat = "UPDATING" 
                        THEN xText("updated")
               ELSE cv_cvrhead.cvs_sysstat).
    throwError({msg.i CV110 lcTmp xText('copy')}).
  END.
  
  BUFFER-COPY cv_cvrhead TO tcv_cvrhead.
  
  FIND LAST xcv_cvrhead
    WHERE xcv_cvrhead.kco     = tcv_cvrhead.kco
    AND xcv_cvrhead.job_num   = tcv_cvrhead.job_num
    AND xcv_cvrhead.jph_phase = tcv_cvrhead.jph_phase
    AND xcv_cvrhead.cvs_cvrno > ""
    AND xcv_cvrhead.cvs_cvrno <> "SNAPSHOT":u
    NO-LOCK NO-ERROR.
  
  IF AVAILABLE xcv_cvrhead THEN 
  DO:
    liRef = INTEGER(xcv_cvrhead.cvs_cvrno) NO-ERROR.
    IF liRef <> 0 AND liRef <> ? THEN
      tcv_cvrhead.cvs_cvrno = STRING(liRef + 1,"9999").
  END.

  ASSIGN
    tcv_cvrhead.cob_num        = 0
    tcv_cvrhead.cob_numr       = 0
    tcv_cvrhead.cob_numn       = 0
    tcv_cvrhead.cvs_complete   = NO
    tcv_cvrhead.cvs_compdate   = ?
    tcv_cvrhead.cvs_locked     = NO 
    tcv_cvrhead.cvs_lockdate   = ?
    tcv_cvrhead.cvs_lockby     = ''
    tcv_cvrhead.cvs_authorised = NO
    tcv_cvrhead.cvs_authby     = ""
    tcv_cvrhead.cvs_authdate   = ?.
  
  lcPERCOPY = getSysParam(getCID(),"CV":u,"PERCOPY":u).
  IF LOOKUP(lcPERCOPY,"CALU,CALM") > 0 THEN
  DO:
    FIND cv_period
      WHERE cv_period.kco       = tcv_cvrhead.kco
      AND cv_period.job_num     = ""
      AND cv_period.jph_phase   = ""
      AND cv_period.cpv_current = TRUE
      NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cv_period
      THEN throwError({msg.i CV125}).
    tcv_cvrhead.cvp_fdate = cv_period.cvp_fdate.
  END.
  {end.i}
END PROCEDURE.  /* createCopy */


PROCEDURE addCopy:
  DEF INPUT PARAMETER prRowid AS ROWID NO-UNDO.
  DEF VAR lcParam AS CHA NO-UNDO.
  DEF VAR lhCRR   AS HANDLE NO-UNDO.
  DEF VAR lcQueue AS CHA NO-UNDO.

  DEF BUFFER cv_cvrprog FOR cv_cvrprog.
  DEF BUFFER cv_cvrhead FOR cv_cvrhead.

  {try.i}
  FIND cv_cvrhead
    WHERE ROWID(cv_cvrhead) = prRowid
    NO-LOCK.
  
  FIND cv_model OF cv_cvrhead
    NO-LOCK NO-ERROR.
  IF NOT AVAIL cv_model
    THEN throwError({msg.i SY719 'cv_model' 'cvs-rsp.addCopy'}).
 
  lcQueue = getSysParam(getCID(),"CV":u,"UPDREPQ":u).
  /* if we come from scheduled generate cvr3081.p then use 
     the queue the scheduler is on */
  IF getVar("scheduledCVRGenerate") = "Y":u 
    THEN lcQueue = getVar("scheduledCVRQueue").
  lhCRR  = getService("crr-rsp").
  lcParam   = "kco=" + STRING(getCID())  
                + "&cv_cvrheadSourceRowid=" + STRING(prRowid)
                + "&cv_cvrheadNewRowid=" + STRING(tcv_cvrhead.rsp_dbrowid)
                + "&FOPProg=&MainArea=%WCV3021RCVS&rtn_code=%WCV3021RCVS"
                + "&crr_queue=" + lcQueue
                + "&initZero=" + STRING(bInitZero,"Y/N").

  DYNAMIC-FUNCTION("setTitle" IN lhCRR,
                    xText("CVR Copy") + " " 
                      + STRING(cv_cvrhead.kco,"999") + "/" 
                      + cv_cvrhead.job_num + "/" + cv_cvrhead.cvs_cvrno
                      + " to " + tcv_cvrhead.cvs_cvrno).
  DYNAMIC-FUNCTION("setModule"  IN lhCRR, "CV").
  DYNAMIC-FUNCTION("setType"    IN lhCRR, "B").
  DYNAMIC-FUNCTION("setProg"    IN lhCRR, "FOP").
  DYNAMIC-FUNCTION("setParam"   IN lhCRR, lcParam).
  DYNAMIC-FUNCTION("submitTask" IN lhCRR).
  
  {sy_lock.i
    &table = cv_cvrprog
    &condition = "
      WHERE cv_cvrprog.kco      = tcv_cvrhead.kco
      AND cv_cvrprog.cdp_type   = 'UPDATE':U
      AND cv_cvrprog.cvs_ref    = tcv_cvrhead.cvs_ref"
    &excludeNoAvail=*
  }
  
  IF NOT AVAIL cv_cvrprog THEN
  DO:
    CREATE cv_cvrprog.
    ASSIGN
      cv_cvrprog.kco        = tcv_cvrhead.kco
      cv_cvrprog.cdp_type   = 'UPDATE':U
      cv_cvrprog.cvs_ref    = tcv_cvrhead.cvs_ref.
  END.
  
  ASSIGN
    cv_cvrprog.cdp_progress = 0
    cv_cvrprog.cdp_desc = xText("Copy Submitted").

  {sy_lock.i
    &table = cv_cvrhead
    &rid = tcv_cvrhead.rsp_dbrowid
  }
  cv_cvrhead.cvs_sysstat = 'COPYING'.
  FIND CURRENT cv_cvrhead NO-LOCK.
  {end.i}
END PROCEDURE.  /* addCopy */


PROCEDURE preDelete:
  DEF VAR lcParam AS CHA NO-UNDO.
  DEF VAR lhCRR   AS HANDLE NO-UNDO.
  DEF VAR lcTmp   AS CHA NO-UNDO.
  DEF VAR lcQueue AS CHA NO-UNDO.

  DEF BUFFER cv_cvrprog FOR cv_cvrprog.
  DEF BUFFER cv_cvrhead FOR cv_cvrhead.
  DEF BUFFER xcv_cvrhead FOR cv_cvrhead.
  
  {try.i}

  IF tcv_cvrhead.cvs_complete THEN throwError({msg.i CV112}).
  
  IF tcv_cvrhead.cvs_current THEN
  DO:
    IF CAN-FIND(FIRST xcv_cvrhead
           WHERE xcv_cvrhead.kco       = tcv_cvrhead.kco
           AND xcv_cvrhead.job_num     = tcv_cvrhead.job_num
           AND xcv_cvrhead.jph_phase   = tcv_cvrhead.jph_phase
           AND xcv_cvrhead.cvp_fdate   = tcv_cvrhead.cvp_fdate
           AND xcv_cvrhead.cvs_cvrno   <> 'SNAPSHOT':u
           AND ROWID(xcv_cvrhead)      <> tcv_cvrhead.rsp_dbrowid)
      THEN throwError({msg.i CV111}).
      ELSE RUN queueMessage IN hController ('Warning',{msg.i CV144}).
  END.
  
  IF tcv_cvrhead.cvs_latest THEN
  DO:
    IF CAN-FIND(FIRST xcv_cvrhead
           WHERE xcv_cvrhead.kco       = tcv_cvrhead.kco
           AND xcv_cvrhead.job_num     = tcv_cvrhead.job_num
           AND xcv_cvrhead.jph_phase   = tcv_cvrhead.jph_phase
           AND xcv_cvrhead.cvs_cvrno   <> 'SNAPSHOT':u
           AND ROWID(xcv_cvrhead)      <> tcv_cvrhead.rsp_dbrowid)
      THEN throwError({msg.i CV113}).
      ELSE RUN queueMessage IN hController ('Warning',{msg.i CV145}).
  END.
 
  RUN cou003.p(INPUT tcv_cvrhead.cvs_param,
               "BUDGETUPD",
               OUTPUT lcTmp).
  
  IF tcv_cvrhead.cob_numr <> 0 OR tcv_cvrhead.cob_numn <> 0 
    OR lcTmp = "Y" THEN throwError({msg.i CV141}).
    
  IF tcv_cvrhead.cvs_sysstat <> "" 
  AND tcv_cvrhead.cvs_sysstat <> "DELETING"
  AND tcv_cvrhead.cvs_sysstat <> "COPYING" THEN
  DO:
    lcTmp = (IF tcv_cvrhead.cvs_sysstat = "COPYING" THEN xText("copied")
               ELSE IF tcv_cvrhead.cvs_sysstat = "DELETING" 
                        THEN xText("deleted")
               ELSE IF tcv_cvrhead.cvs_sysstat = "UPDATING" 
                        THEN xText("updated")
               ELSE tcv_cvrhead.cvs_sysstat).
    throwError({msg.i CV110 lcTmp xText('delete')}).
  END.
  
  FIND cv_model OF tcv_cvrhead
    NO-LOCK NO-ERROR.
  IF NOT AVAIL cv_model
    THEN throwError({msg.i SY719 'cv_model' 'cvs-rsp.preDelete'}).
 
  lhCRR  = getService("crr-rsp").
  lcQueue = getSysParam(getCID(),"CV":u,"UPDREPQ":u).
  /* if we come from scheduled generate cvr3081.p then use 
     the queue the scheduler is on */
  IF getVar("scheduledCVRGenerate") = "Y":u 
    THEN lcQueue = getVar("scheduledCVRQueue").
  lcParam   = "kco=" + STRING(getCID())  
                + "&cv_cvrheadRowid=" + STRING(tcv_cvrhead.rsp_dbrowid)
                + "&FOPProg=&MainArea=%WCV3031RCVS&rtn_code=%WCV3031RCVS"
                + "&crr_queue=" + lcQueue.

  DYNAMIC-FUNCTION("setTitle" IN lhCRR,
                    xText("CVR Delete") + " "
                      + STRING(tcv_cvrhead.kco,"999") + "/" 
                      + tcv_cvrhead.job_num + "/" + tcv_cvrhead.cvs_cvrno).
  DYNAMIC-FUNCTION("setModule"  IN lhCRR, "CV").
  DYNAMIC-FUNCTION("setType"    IN lhCRR, "B").
  DYNAMIC-FUNCTION("setProg"    IN lhCRR, "FOP").
  DYNAMIC-FUNCTION("setParam"   IN lhCRR, lcParam).
  DYNAMIC-FUNCTION("submitTask" IN lhCRR).
  
  {sy_lock.i
    &table = cv_cvrprog
    &condition = "
      WHERE cv_cvrprog.kco      = tcv_cvrhead.kco
      AND cv_cvrprog.cdp_type   = 'UPDATE':U
      AND cv_cvrprog.cvs_ref    = tcv_cvrhead.cvs_ref"
    &excludeNoAvail=*
  }
  
  IF NOT AVAIL cv_cvrprog THEN
  DO:
    CREATE cv_cvrprog.
    ASSIGN
      cv_cvrprog.kco        = tcv_cvrhead.kco
      cv_cvrprog.cdp_type   = 'UPDATE':U
      cv_cvrprog.cvs_ref    = tcv_cvrhead.cvs_ref.
  END.
  
  ASSIGN
    cv_cvrprog.cdp_progress = 0
    cv_cvrprog.cdp_desc = xText("Delete Submitted").

  {sy_lock.i
    &table = cv_cvrhead
    &rid = tcv_cvrhead.rsp_dbrowid
  }

  ASSIGN
    cv_cvrhead.cvs_desc = '** ' + xText('Deletion in Progress') + ' **'
    cv_cvrhead.cvs_sysstat = 'DELETING'.
  tcv_cvrhead.rsp_action = "".
  {end.i}
END PROCEDURE.  /* preDelete */
 
 
PROCEDURE postDelete:
  {try.i}
  RUN updateCalendar.
  {end.i}
END PROCEDURE. /* postDelete */


PROCEDURE updateCalendar:
  DEF BUFFER xcv_cvrhead FOR cv_cvrhead.
  DEF BUFFER xcv_period FOR cv_period.
  {syustamp.i &vars=*}
  {try.i}
  /* store current CVR ref on calendar */
  FIND xcv_cvrhead
    WHERE xcv_cvrhead.kco     = tcv_cvrhead.kco
    AND xcv_cvrhead.job_num   = tcv_cvrhead.job_num
    AND xcv_cvrhead.jph_phase = tcv_cvrhead.jph_phase
    AND xcv_cvrhead.cvs_current = TRUE
    AND xcv_cvrhead.cvp_fdate = tcv_cvrhead.cvp_fdate
    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE xcv_cvrhead
  THEN
  FIND LAST xcv_cvrhead
    WHERE xcv_cvrhead.kco     = tcv_cvrhead.kco
    AND xcv_cvrhead.job_num   = tcv_cvrhead.job_num
    AND xcv_cvrhead.jph_phase = tcv_cvrhead.jph_phase
    AND xcv_cvrhead.cvp_fdate = tcv_cvrhead.cvp_fdate
    AND xcv_cvrhead.cvs_cvrno <> 'SNAPSHOT':u
    NO-LOCK NO-ERROR.

  {sy_lock.i
    &table = xcv_period
    &condition = "
      WHERE xcv_period.kco     = tcv_cvrhead.kco
      AND xcv_period.job_Num   = tcv_cvrhead.job_num
      AND xcv_period.jph_phase = ''
      AND xcv_period.cvp_fdate = tcv_cvrhead.cvp_fdate"
  }
  xcv_period.cvs_ref = (IF AVAILABLE xcv_cvrhead 
                          THEN xcv_cvrhead.cvs_ref ELSE "").
  {syustamp.i &stamp=* &table=xcv_period &table_id=cvp}
  {end.i}
END PROCEDURE. /* updateCalendar */


FUNCTION buildField RETURNS LOGICAL
  (INPUT pcParam AS CHAR):
  DEF BUFFER lcv_period FOR cv_period.
  DEF BUFFER lcv_cvrhead FOR cv_cvrhead.

  {try.i}
  FIND cv_model OF tcv_cvrhead
    NO-LOCK NO-ERROR.
  IF NOT AVAIL cv_model
    THEN throwError({msg.i SY719 'cv_model' 'cvs-rsp.buildField'}).
   
  CASE pcParam:
    WHEN "APPRAISAL" 
      THEN RETURN ({syattr.i &param=cv_model.cmh_param
                             &attr=cmh_appcurr &default=N} BEGINS "Y"
              OR {syattr.i &param=cv_model.cmh_param
                      &attr=cmh_apporig &default=N} BEGINS "Y").
    WHEN "laa_appraisal" 
      THEN RETURN {syattr.i &param=cv_model.cmh_param
                      &attr=cmh_apporig &default=N} BEGINS "Y".
    WHEN "laa_appraisal2" 
      THEN RETURN {syattr.i &param=cv_model.cmh_param
                      &attr=cmh_appcurr &default=N} BEGINS "Y".
    WHEN "laa_appraisal3" 
      THEN RETURN {syattr.i &param=cv_model.cmh_param
                      &attr=cmh_apppre &default=N} BEGINS "Y".
    WHEN "laa_appraisal4" 
      THEN RETURN {syattr.i &param=cv_model.cmh_param
                      &attr=cmh_appres &default=N} BEGINS "Y".
    WHEN "laa_appraisal5" 
      THEN RETURN {syattr.i &param=cv_model.cmh_param
                      &attr=cmh_appdef &default=N} BEGINS "Y".
    WHEN "laa_appraisal6" 
      THEN RETURN {syattr.i &param=cv_model.cmh_param
                      &attr=cmh_appctc &default=N} BEGINS "Y".
    WHEN "cvs_initzero" THEN
    DO:
      &IF {&db-version} >= 10.19 &THEN
      IF CAN-FIND(FIRST cv_item
               WHERE cv_item.kco = cv_model.kco
               AND cv_item.cvi_initzero = TRUE
               AND CAN-FIND(FIRST cv_mfield
                         WHERE cv_mfield.kco     = cv_mode.kco
                         AND cv_mfield.cmh_model = cv_mode.cmh_model
                         AND cv_mfield.cvi_item  = cv_item.cvi_item)) 
      AND getButton() = "copy":u 
        THEN RETURN TRUE.
      &ENDIF
      RETURN FALSE.              
    END.
    WHEN "cob_num" THEN
    DO:                 
      &IF {&db-version} >= 10.19 &THEN
      IF CAN-FIND(FIRST cv_item
               WHERE cv_item.kco = cv_model.kco
               AND cv_item.cvi_journal = TRUE
               AND CAN-FIND(FIRST cv_mfield
                         WHERE cv_mfield.kco     = cv_mode.kco
                         AND cv_mfield.cmh_model = cv_mode.cmh_model
                         AND cv_mfield.cvi_item  = cv_item.cvi_item)) 
        THEN RETURN TRUE.
      &ENDIF
      RETURN FALSE.              
    END.
    WHEN "cob_numr" THEN
    DO:
      &IF {&db-version} >= 10.19 &THEN
      IF CAN-FIND(FIRST cv_item
               WHERE cv_item.kco = cv_model.kco
               AND cv_item.cvi_journal = TRUE
               AND cv_item.cvi_revpost = TRUE
               AND CAN-FIND(FIRST cv_mfield
                         WHERE cv_mfield.kco     = cv_mode.kco
                         AND cv_mfield.cmh_model = cv_mode.cmh_model
                         AND cv_mfield.cvi_item  = cv_item.cvi_item)) 
        THEN RETURN TRUE.
      &ENDIF
      RETURN FALSE.              
    END.
    WHEN "cob_numn" THEN
    DO:
      &IF {&db-version} >= 10.19 &THEN
      IF CAN-FIND(FIRST cv_item
               WHERE cv_item.kco = cv_model.kco
               AND cv_item.cvi_journal = TRUE
               AND cv_item.cvi_revpost = FALSE
               AND CAN-FIND(FIRST cv_mfield
                         WHERE cv_mfield.kco     = cv_mode.kco
                         AND cv_mfield.cmh_model = cv_mode.cmh_model
                         AND cv_mfield.cvi_item  = cv_item.cvi_item)) 
        THEN RETURN TRUE.
      &ENDIF
      RETURN FALSE.              
    END.
    WHEN "cvs_prevadj" THEN
    DO:
      IF getButton() = "add":u
        THEN RETURN TRUE.
      RETURN FALSE.
    END.
  END CASE.
  {end.i}
END FUNCTION.


FUNCTION buildPeriodUpdatable RETURNS LOGICAL():
  IF getButton() = "copy"
    AND getSysParam(getCID(),"CV":u,"PERCOPY":u) = "CALM":u
    THEN RETURN FALSE.
  IF getButton() <> "copy"
    AND getSysParam(getCID(),"CV":u,"PERNEW":u) = "CALM":u
    THEN RETURN FALSE.
  RETURN TRUE.
END FUNCTION.


PROCEDURE generateViews:
  DEF VAR liView AS INT NO-UNDO.
  {try.i}

  FIND cv_cvrhead
    WHERE ROWID(cv_cvrhead) = TO-ROWID(getVar('cv_cvrheadRowid'))
    NO-LOCK NO-ERROR.
  IF NOT AVAIL cv_cvrhead
    THEN throwError({msg.i SY719 'cv_cvrhead' 'cvs-rsp.generateViews'}).
  
  FIND cv_model OF cv_cvrhead
    NO-LOCK NO-ERROR.
  IF NOT AVAIL cv_model
    THEN throwError({msg.i SY719 'cv_model' 'cvs-rsp.generateViews'}).
 
  liView = 1.
  FOR EACH cv_mview OF cv_model
    WHERE cv_mview.cmv_hide = NO
    AND cv_mview.cmv_keyview = YES
    AND cv_mview.cmv_pageview = YES
    NO-LOCK
    BY cv_mview.cmv_seq:
    {try.i}
    RUN addTab (BUFFER cv_mview,SOURCE-PROCEDURE,liView).
    liView = liView + 1.
    {end.i}     
  END.
  {end.i}
END PROCEDURE.  /* generateViews */

PROCEDURE generateMoreViews:
  DEF VAR liView AS INT NO-UNDO.
  {try.i}
  FIND cv_cvrhead
    WHERE ROWID(cv_cvrhead) = TO-ROWID(getVar('cv_cvrheadRowid'))
    NO-LOCK NO-ERROR.

  IF NOT AVAIL cv_cvrhead
    THEN throwError({msg.i SY719 'cv_cvrhead' 'cvs-rsp.generateMoreViews'}).

  FIND cv_model OF cv_cvrhead
    NO-LOCK NO-ERROR.
  IF NOT AVAIL cv_model
    THEN throwError({msg.i SY719 'cv_model' 'cvs-rsp.generateMoreViews'}).
 
  liView = 1.
  FOR EACH cv_mview OF cv_model
    WHERE cv_mview.cmv_hide = NO
    AND cv_mview.cmv_pageview = YES
    NO-LOCK
    BREAK BY cv_mview.cmv_seq:
    {try.i}

    IF cv_mview.cmv_keyview = NO
      THEN RUN addTab (BUFFER cv_mview,SOURCE-PROCEDURE,liView).
    liView = liView + 1.
    {end.i}     
  END.
  {end.i}
END PROCEDURE.  /* generateMoreViews */


PROCEDURE addTab:
  DEF PARAMETER BUFFER bcv_mview FOR cv_mview.
  DEF INPUT PARAMETER phSource AS HANDLE NO-UNDO.
  DEF INPUT PARAMETER piView   AS INT    NO-UNDO.
  DEF BUFFER xcv_mfield FOR cv_mfield.
  
  DEF VAR lcContext AS CHAR NO-UNDO.
  
  FIND cv_cvrhead
    WHERE ROWID(cv_cvrhead) = TO-ROWID(getVar('cv_cvrheadRowid'))
    NO-LOCK NO-ERROR.
  IF NOT AVAIL cv_cvrhead
    THEN throwError({msg.i SY719 'cv_cvrhead' 'cvs-rsp.addTab'}).

  cInLineViews = "".
  FOR EACH cv_mfield OF bcv_mview
    WHERE (cv_mfield.cmd_type = "IVIEW"
             OR cv_mfield.cmd_grpinc <> "")
    NO-LOCK
    BY cv_mfield.cmd_seq:
    {try.i}
    
    IF cv_mfield.cmd_grpinc <> ""
    THEN
    FOR EACH xcv_mfield
      WHERE xcv_mfield.kco     = bcv_mview.kco
      AND xcv_mfield.cmh_model = bcv_mview.cmh_model
      AND xcv_mfield.cmv_view  = ""
      AND xcv_mfield.cmi_group = cv_mfield.cmd_grpinc
      AND xcv_mfield.cmd_type  = "IVIEW"
      BY xcv_mfield.cmd_seq:
      {try.i}
      cInLineViews = cInLineViews + (IF cInlineViews <> "" THEN "," ELSE "")
                        + TRIM({syattr.i &param=xcv_mfield.cmd_param
                                         &attr=cmd_iview &default=" "}).
      {end.i}
    END.
    ELSE IF cv_mfield.cmd_type = "IVIEW":U THEN
    DO:
      cInLineViews = cInLineViews + (IF cInlineViews <> "" THEN "," ELSE "")
                        + TRIM({syattr.i &param=cv_mfield.cmd_param
                                         &attr=cmd_iview &default=" "}).
    END.
    {end.i}
  END.  /* EACH cv_mfield */
 
    IF INDEX(bcv_mview.cmv_sdesc,"~{") <> 0 THEN
     lcContext = DYNAMIC-FUNCTION("replaceData" in hController,
                 "BODY",bcv_mview.cmv_sdesc). 
     ELSE lcContext = bcv_mview.cmv_sdesc.
  
  RUN insertFunction IN phSource
      ((IF SUBSTRING(bcv_mview.cmv_type,1,3) = "FOR"
                 THEN "%WCV1012GCMV" ELSE "%WCV1012SCMV") + STRING(piView),
        "cmv_view=" + bcv_mview.cmv_view
         + "&cv_mviewRowid=" + STRING(ROWID(bcv_mview))
         + "&cv_cvrheadRowid=" + STRING(ROWID(cv_cvrhead))
         + "&updateContext=CVR"
         + "&updateAllowed=" + (IF TRIM({syattr.i &param=bcv_mview.cmv_param
                                           &attr=cmv_update &default=N}) = "Y"
                                  AND cv_cvrhead.cvs_complete = NO
                                  THEN "Y":u ELSE "N":u)
         + "&viewAs=" + bcv_mview.cmv_chart
         + "&cDesc=" + bcv_mview.cmv_desc
         + "&cContext=" + (IF bcv_mview.cmv_sdesc <> ""
                             AND bcv_mview.cmv_keyview
                             THEN lcContext ELSE bcv_mview.cmv_desc)
         + "&stn_code=" 
              + (IF bcv_mview.cmv_type = "CHBRWS" THEN "%WCV1012BJSC" 
                   ELSE IF bcv_mview.cmv_type = "PLOTBRWS" THEN "%WCV1012BCDW" 
                   ELSE IF bcv_mview.cmv_type = "SUBBRWS" THEN "%WCV1012BSBS" 
                   ELSE IF bcv_mview.cmv_type = "CATBRWS" THEN "%WCV1012BCAT" 
                   ELSE IF bcv_mview.cmv_type BEGINS "CHANAL" 
                                                    THEN "%WCV1012BCOA" 
                   ELSE IF bcv_mview.cmv_type = "JOBSUM" THEN "%WCV1014SCVS" 
                   ELSE IF bcv_mview.cmv_type = "MATBRWS" THEN "%WCV1012BPOH" 
                   ELSE IF bcv_mview.cmv_type = "WBSBRWS" THEN "%WCV1012BJWB" 
                   ELSE IF bcv_mview.cmv_type = "SECBRWS" THEN "%WCV1012BJCS" 
                   ELSE IF bcv_mview.cmv_type = "ACTBRWS" THEN "%WCV1012BJCA" 
                   ELSE IF bcv_mview.cmv_type = "JCCBRWS" THEN "%WCV1012BJCC" 
                   ELSE IF bcv_mview.cmv_type = "PERBRWS" THEN "%WCV1012BCVP" 
                   ELSE IF SUBSTRING(bcv_mview.cmv_type,1,3) = "FOR" 
                                                         THEN "%WCV1012GCVS" 
                   ELSE IF bcv_mview.cmv_type = "MAXJSC" THEN "%WCV101MSJSC"
                   ELSE IF bcv_mview.cmv_type = "MAXPAY" THEN "%WCV101MSVVP"
                   ELSE IF bcv_mview.cmv_type = "MAXTG" THEN "%WCV101MSVPG"
                   ELSE IF bcv_mview.cmv_type BEGINS "MAXCHA"
                                                    THEN "%WCV101MSCOA"
                   ELSE "%WCV1011BCVS")
         + (IF bcv_mview.cmv_type BEGINS "CHANAL"
              OR bcv_mview.cmv_type BEGINS "MAXCHA"
              THEN "&coa_analysis=" + SUBSTRING(bcv_mview.cmv_type,7,1)
              ELSE "")
         &IF {&db-version} >= 10.19 &THEN
         + (IF SUBSTRING(bcv_mview.cmv_type,1,3) = "FOR"
              THEN "&ajaxObject=cvacvs02&RowLabels=N&ColLabels=Y"
                 + "&GridHeight=300&fixedRows=0&FixedCols="
                     + STRING(bcv_mview.cmv_fixcols)
              ELSE "")
         &ENDIF     
         + "&inlineViews=" + cInLineViews).
END PROCEDURE. /* addTab */


FUNCTION buildMoreViews RETURNS LOGICAL():
  {try.i}
  FIND cv_cvrhead
    WHERE ROWID(cv_cvrhead) = TO-ROWID(getVar('cv_cvrheadRowid'))
    NO-LOCK NO-ERROR.
  IF NOT AVAIL cv_cvrhead
    THEN throwError({msg.i SY719 'cv_cvrhead' 'cvs-rsp.buildMoreViews'}).

  FIND cv_model OF cv_cvrhead
    NO-LOCK NO-ERROR.
  RETURN (AVAIL cv_model
            AND CAN-FIND(FIRST cv_mview OF cv_model
                    WHERE cv_mview.cmv_hide = NO
                    AND cv_mview.cmv_keyview = NO
                    AND cv_mview.cmv_pageview = YES)).
  {end.i}
END FUNCTION.


PROCEDURE generatePage:
  DEF INPUT PARAMETER pcParam AS CHAR NO-UNDO.
  DEF VAR lrView AS ROWID NO-UNDO.
  DEF VAR lcTLA  AS CHA NO-UNDO.
  DEF VAR li     AS INT NO-UNDO.
  DEF VAR liCol  AS INT NO-UNDO.
  DEF VAR liRow  AS INT NO-UNDO.
  DEF VAR liSCol AS INT NO-UNDO.
  DEF VAR liSRow AS INT NO-UNDO.
  DEF VAR liMax  AS INT NO-UNDO.
  DEF VAR liFrom AS INT NO-UNDO.
  DEF VAR lbSUM30 AS LOG NO-UNDO.

  DEF BUFFER xcv_mfield FOR cv_mfield.
  DEF BUFFER xttField FOR ttField.
  DEF BUFFER yttField FOR ttField.


  {try.i}
  lrView = TO-ROWID(getVar('cv_mviewRowid')).
  cInlineViews = "".
  lbSUM30 = (getSysParam(getCID(),"CV":u,"SUM30":u) = "Y").

  FIND cv_mview
    WHERE ROWID(cv_mview) = lrView 
    NO-LOCK NO-ERROR.
  IF NOT AVAIL cv_mview
    THEN throwError({msg.i SY719 'cv_mview' 'cvs-rsp.generatePage'}).
 
  IF cv_mview.cmv_type = "JOBSUM":U THEN
  DO:
    liCol = ?.
    liRow = ?.
    EMPTY TEMP-TABLE ttField.
    FOR EACH cv_mfield OF cv_mview
      WHERE (LOOKUP(pcParam,"ALL,BULK":u) > 0
               OR cv_mfield.cmd_grpinc <> ""
               OR cv_mfield.cmd_update = TRUE)
      NO-LOCK
      BY cv_mfield.cmd_seq:
      {try.i}
      IF liCol = ?
      THEN
      ASSIGN
        liCol = 1
        liRow = 1.
      ELSE
      ASSIGN
        liCol = (IF cv_mfield.cmd_append THEN liCol + 1 ELSE 1)
        liRow = (IF cv_mfield.cmd_append THEN liRow ELSE liRow + 1).
      CREATE ttField.
      ASSIGN
        ttField.rField = ROWID(cv_mfield)
        ttField.iCol   = liCol
        ttField.cGroup = cv_mfield.cmd_grpinc
        ttField.bStart = TRUE
        ttField.rGroup = (IF ttField.cGroup <> "" THEN ttField.rField ELSE ?)
        ttField.iSRow  = 1
        ttField.iSCol  = 1
        ttField.iRow   = liRow.
      {end.i}
    END.  /* EACH cv_mfield */

    FOR EACH ttField
      WHERE ttField.cGroup > ""
      AND ttField.bDone = NO:
      
      liSCol = 1.
      liSRow = 1.
      FOR EACH xcv_mfield
        WHERE xcv_mfield.kco     = cv_mview.kco
        AND xcv_mfield.cmh_model = cv_mview.cmh_model
        AND xcv_mfield.cmv_view  = ""
        AND xcv_mfield.cmi_group = ttField.cGroup
        AND (LOOKUP(pcParam,"ALL,BULK":u) > 0
               OR xcv_mfield.cmd_update = TRUE)
        NO-LOCK
        BREAK BY xcv_mfield.cmd_seq:
        {try.i}
        IF FIRST(xcv_mfield.cmd_seq) THEN
        DO:
          FIND xttField
            WHERE ROWID(xttField) = ROWID(ttField).
          ASSIGN
            xttField.rField = ROWID(xcv_mfield)
            xttField.iSCol  = liSCol
            xttfield.iSRow  = liSRow.
        END.
        ELSE DO:
          ASSIGN
            liSCol = (IF xcv_mfield.cmd_append THEN liSCol + 1 ELSE 1)
            liSRow = (IF xcv_mfield.cmd_append THEN liSRow ELSE liSRow + 1).
          CREATE xttField.
          ASSIGN
            xttField.rField = ROWID(xcv_mfield)
            xttField.rGroup = ttField.rGroup
            xttField.cGroup = ttField.cGroup
            xttField.iCol   = ttField.iCol
            xttField.iRow   = ttField.iRow
            xttField.iSCol  = liSCol
            xttField.iSRow  = liSRow
            xttField.bDone  = YES.
        END.
        {end.i}
      END. /* EACH xcv_mfield */
      ttField.bDone = YES.
    END.   /* EACH ttField */

    /* XXXX */
    FOR EACH ttField
      WHERE ttField.bStart = TRUE
      NO-LOCK:
        
      FIND LAST xttField
        WHERE xttField.iRow = ttField.iRow
        AND xttField.iSRow > 0
        NO-LOCK NO-ERROR.
      IF NOT AVAILABLE xttField THEN NEXT.
      liMax = xttField.iSRow.
    
      FIND LAST xttField
        WHERE xttField.iRow = ttField.iRow
        AND xttField.iCol = ttField.iCol
        AND xttField.iSRow > 0
        NO-LOCK NO-ERROR.
      IF NOT AVAILABLE xttField THEN NEXT.
      liFrom = xttField.iSRow + 1.
 
      FOR EACH xttField
        WHERE xttField.iRow = ttField.iRow
        AND xttField.iCol = ttField.iCol
        AND xttField.iSRow = 1
        NO-LOCK:
        DO li = liFrom TO liMax:
          CREATE yttField.
          ASSIGN
            yttField.rField = xttField.rField
            yttField.cGroup = xttField.cGroup
            yttField.iCol   = xttField.iCol
            yttField.iRow   = xttField.iRow
            yttField.iSCol  = xttField.iSCol
            yttField.iSRow  = li
            yttField.bDummy = TRUE.
        END.
      END.
    END.
    
    li = 0.
    FOR EACH ttField
      NO-LOCK,
      FIRST cv_mfield 
      WHERE ROWID(cv_mfield) = ttField.rField
      NO-LOCK
      BREAK
      BY ttField.iRow
      BY ttField.iSRow
      BY ttField.iCol
      BY ttField.iSCol:
      {try.i}
      
      li = li + 1.
      IF li > 1 OR lbSUM30 = NO
        THEN RUN addField IN SOURCE-PROCEDURE.
      
      FIND xcv_mfield
        WHERE ROWID(xcv_mfield) = ttField.rGroup
        NO-LOCK NO-ERROR.
      
      IF ttField.bDummy = NO THEN
      DO:
        IF AVAILABLE xcv_mfield
        THEN
        RUN generateField
          (BUFFER xcv_mfield,
           BUFFER cv_mfield,
           BUFFER cv_mview,
           SOURCE-PROCEDURE).
        ELSE
        RUN generateField
          (BUFFER cv_mfield,
           BUFFER cv_mfield,
           BUFFER cv_mview,
           SOURCE-PROCEDURE).
      END.
      ELSE DO:
        RUN updateField IN SOURCE-PROCEDURE
          ("sfd_name="
            + "&sfd_label="
            + "&sfd_update=N"
            + "&sfd_format="
            + "&sfd_hide=" + STRING(cv_mfield.cmd_hide,"Y/N")
            + "&sfd_pos=" + STRING(cv_mfield.cmd_colspan)
            + "&sfd_width="
            + "&sfd_height="
            + "&sfd_viewas="
            + "&sfd_populate="
            + "&sfd_align="
            + "&sfd_sort="
            + "&sfd_class=").
        RUN updateFieldValue IN SOURCE-PROCEDURE
          ("sfd_calc","").
        RUN updateFieldValue IN SOURCE-PROCEDURE
          ("mf_function","").
      END.
      /*IF FIRST(ttField.iRow)
        THEN RUN updateField IN SOURCE-PROCEDURE ("sfd_lspan=-1").
      */
      RUN updateField IN SOURCE-PROCEDURE
         ("sfd_lspan=" + (IF FIRST-OF(ttField.iSRow) THEN "-1" ELSE "0")).
      IF NOT FIRST-OF(ttField.iSRow) 
        THEN RUN updateField IN SOURCE-PROCEDURE ("sfd_append=Y").
        ELSE RUN updateField IN SOURCE-PROCEDURE ("sfd_append=N").
      {end.i}
    END.
  END.
  ELSE DO:
    li = 0.
    FOR EACH cv_mfield OF cv_mview
      WHERE (LOOKUP(pcParam,"ALL,BULK":u) > 0
               OR cv_mfield.cmd_grpinc <> ""
               OR cv_mfield.cmd_update = TRUE)
      NO-LOCK
      BY cv_mfield.cmd_seq:
      {try.i}

      IF cv_mfield.cmd_grpinc <> ""
      THEN
      FOR EACH xcv_mfield
        WHERE xcv_mfield.kco     = cv_mview.kco
        AND xcv_mfield.cmh_model = cv_mview.cmh_model
        AND xcv_mfield.cmv_view  = ""
        AND xcv_mfield.cmi_group = cv_mfield.cmd_grpinc
        AND (LOOKUP(pcParam,"ALL,BULK":u) > 0
              OR xcv_mfield.cmd_update = TRUE)
        NO-LOCK
        BY xcv_mfield.cmd_seq:
        {try.i}
        li = li + 1.
        IF li > 1
          THEN RUN addField IN SOURCE-PROCEDURE.
        RUN generateField
          (BUFFER cv_mfield,
           BUFFER xcv_mfield,
           BUFFER cv_mview,
           SOURCE-PROCEDURE).
        IF li = 1 AND pcParam <> "MULTI":u
          THEN RUN updateField IN SOURCE-PROCEDURE ("sfd_lspan=-1").
        {end.i}
      END.
      ELSE DO:
        li = li + 1.
        IF li > 1
          THEN RUN addField IN SOURCE-PROCEDURE.
        RUN generateField
          (BUFFER cv_mfield,
           BUFFER cv_mfield,
           BUFFER cv_mview,
           SOURCE-PROCEDURE).
        IF li = 1 AND pcParam <> "MULTI":u
          THEN RUN updateField IN SOURCE-PROCEDURE ("sfd_lspan=-1").
      END.
      {end.i}
    END.  /* EACH cv_mfield */
  END.
  {end.i}
END PROCEDURE. /* generatePage */


PROCEDURE generateField:
  DEF PARAMETER BUFFER pcv_mfield FOR cv_mfield.
  DEF PARAMETER BUFFER bcv_mfield FOR cv_mfield.
  DEF PARAMETER BUFFER bcv_mview FOR cv_mview.
  DEF INPUT PARAMETER phSource AS HANDLE NO-UNDO.

  DEF VAR lcSumKey AS CHA NO-UNDO.
  DEF VAR lcMask   AS CHA NO-UNDO.
  DEF VAR lcSub    AS CHA NO-UNDO.
  DEF VAR lcClass  AS CHA NO-UNDO.
  DEF VAR lcName   AS CHA NO-UNDO.
  DEF VAR lcSort   AS CHA NO-UNDO.
  DEF VAR lcLSpan  AS CHA NO-UNDO.
  DEF VAR lhTmp    AS HANDLE NO-UNDO.
  DEF VAR li       AS INT NO-UNDO.
  DEF VAR liWidth  AS INT NO-UNDO.
  DEF VAR liHeight AS INT NO-UNDO.
  DEF VAR lcOffset AS CHA NO-UNDO.
  DEF VAR lcTmp    AS CHA NO-UNDO.
  DEF VAR lcFunc   AS CHA NO-UNDO.
  DEF VAR lbBkDown AS LOG NO-UNDO.
  DEF VAR lcTLA    AS CHA NO-UNDO.
  DEF VAR lCTable  AS CHA NO-UNDO.
  DEF VAR lcMSort  AS CHA NO-UNDO.
  DEF VAR lcOnBlur AS CHA NO-UNDO.
  DEF VAR lcID     AS CHA NO-UNDO.
  DEF VAR lcOnChange AS CHA NO-UNDO.

  FIND cv_style OF bcv_mfield
    NO-LOCK NO-ERROR.
  
  liWidth = INT({syattr.i &param=bcv_mfield.cmd_param
                          &attr=cmd_pwidth &default=0}).
  liHeight = INT({syattr.i &param=bcv_mfield.cmd_param
                          &attr=cmd_pheight &default=0}).
  lcLSpan = (IF cv_mview.cmv_type = "JOBSUM":u 
               AND bcv_mfield.cmd_append = NO THEN "-1" ELSE "0").
  
  CASE bcv_mfield.cmd_type:
    WHEN "FIELD" THEN
    DO:
      IF AVAILABLE cv_style
        THEN lcClass = (IF cv_style.cvf_alertcls <> ""
                          THEN "~{co_config.RO_RangeClass^" 
                               + cv_style.cvf_alertcls + "|~{"
                               + bcv_mfield.cmd_value
                               + "~}~}"
                          ELSE cv_style.cvf_class).
      
      CASE cv_mview.cmv_type:
        WHEN "PLOTBRWS" THEN
        DO:
          lhTmp = BUFFER cv_cvrplot:BUFFER-FIELD(bcv_mfield.cmd_value) 
                    NO-ERROR.
          IF VALID-HANDLE(lhTmp) 
            THEN lcSort = bcv_mfield.cmd_value.
        END.
        WHEN "CHBRWS" OR WHEN "MAXJSC" THEN
        DO:
          lhTmp = BUFFER jc_stndcc:BUFFER-FIELD(bcv_mfield.cmd_value) 
                    NO-ERROR.
          IF VALID-HANDLE(lhTmp) 
            THEN lcSort = bcv_mfield.cmd_value.
        END.
        WHEN "SUBBRWS" THEN
        DO:
          lhTmp = BUFFER sc_subcon:BUFFER-FIELD(bcv_mfield.cmd_value) 
                    NO-ERROR.
          IF VALID-HANDLE(lhTmp) 
            THEN lcSort = bcv_mfield.cmd_value.
        END.
        WHEN "CHANAL1" OR WHEN "CHANAL2" OR WHEN "CHANAL3" 
        OR WHEN "CHANAL4" OR WHEN "MAXCHA1" OR WHEN "MAXCHA2"
        OR WHEN "MAXCHA3"OR WHEN "MAXCHA4" THEN
        DO:
          lhTmp = BUFFER co_analysis:BUFFER-FIELD(bcv_mfield.cmd_value) 
                    NO-ERROR.
          IF VALID-HANDLE(lhTmp) 
            THEN lcSort = bcv_mfield.cmd_value.
        END.
        WHEN "MATBRWS" THEN
        DO:
          lhTmp = BUFFER po_hdr:BUFFER-FIELD(bcv_mfield.cmd_value) 
                    NO-ERROR.
          IF VALID-HANDLE(lhTmp) 
            THEN lcSort = bcv_mfield.cmd_value.
        END.
        WHEN "CATBRWS" THEN
        DO:
          lhTmp = BUFFER co_analysis:BUFFER-FIELD(bcv_mfield.cmd_value) 
                    NO-ERROR.
          IF VALID-HANDLE(lhTmp) 
            THEN lcSort = bcv_mfield.cmd_value.
        END.
        WHEN "WBSBRWS" THEN
        DO:
          lhTmp = BUFFER jc_wbs:BUFFER-FIELD(bcv_mfield.cmd_value) 
                    NO-ERROR.
          IF VALID-HANDLE(lhTmp) 
            THEN lcSort = bcv_mfield.cmd_value.
        END.
        WHEN "SECBRWS" THEN
        DO:
          lhTmp = BUFFER jc_section:BUFFER-FIELD(bcv_mfield.cmd_value) 
                    NO-ERROR.
          IF VALID-HANDLE(lhTmp) 
            THEN lcSort = bcv_mfield.cmd_value.
        END.
        WHEN "ACTBRWS" THEN
        DO:
          lhTmp = BUFFER jc_jactivity:BUFFER-FIELD(bcv_mfield.cmd_value) 
                    NO-ERROR.
          IF VALID-HANDLE(lhTmp) 
            THEN lcSort = bcv_mfield.cmd_value.
        END.
        WHEN "JCCBRWS" THEN
        DO:
          lhTmp = BUFFER jc_costcode:BUFFER-FIELD(bcv_mfield.cmd_value) 
                    NO-ERROR.
          IF VALID-HANDLE(lhTmp) 
            THEN lcSort = bcv_mfield.cmd_value.
        END.
        WHEN "MAXTG" THEN
        DO:
          lhTmp = BUFFER vp_paygrp:BUFFER-FIELD(bcv_mfield.cmd_value) 
                    NO-ERROR.
          IF VALID-HANDLE(lhTmp) 
            THEN lcSort = bcv_mfield.cmd_value.
        END.
        WHEN "MAXPAY" THEN
        DO:
          lhTmp = BUFFER vp_stage:BUFFER-FIELD(bcv_mfield.cmd_value) 
                    NO-ERROR.
          IF VALID-HANDLE(lhTmp)
            THEN lcSort = bcv_mfield.cmd_value.
        END.
        WHEN "PERBRWS" THEN
        DO:
          lhTmp = BUFFER cv_period:BUFFER-FIELD(bcv_mfield.cmd_value) 
                    NO-ERROR.
          IF VALID-HANDLE(lhTmp) THEN
          DO:
            lcSort = bcv_mfield.cmd_value.
          END.
          ELSE DO:
            lhTmp = BUFFER cv_cvrhead:BUFFER-FIELD(bcv_mfield.cmd_value) 
                      NO-ERROR.
            IF VALID-HANDLE(lhTmp)
              THEN lcSort = bcv_mfield.cmd_value.
          END.
        END.
      END CASE.
      
      lcMSort = TRIM({syattr.i &param=bcv_mfield.cmd_param
                               &attr=cmd_msort &default=" "}).
      IF lcMSort <> "" THEN lcSort = lcMSort.
      
      RUN updateField IN phSource
        ("sfd_name=" + bcv_mfield.cmd_value
          + "&sfd_label=" + bcv_mfield.cmd_label
          + "&sfd_update=N"
          + "&sfd_format=" + bcv_mfield.cmd_format
          + "&sfd_hide=" + STRING(bcv_mfield.cmd_hide,"Y/N")
          + "&sfd_append=" + STRING(bcv_mfield.cmd_append,"Y/N")
          + "&sfd_lspan=" + lcLSpan
          + "&sfd_pos=" + STRING(bcv_mfield.cmd_colspan)
          + "&sfd_width=" + STRING(liWidth) 
          + "&sfd_height=" + STRING(liHeight)
          + (IF bDebug 
               THEN "&sfd_viewas=TOOLTIP"
                  + "&sfd_populate=cvs-rsp.RO_cvs_debugtooltip^" 
                        + STRING(ROWID(bcv_mfield)) + "|"
                        + STRING(ROWID(pcv_mfield))                        
               ELSE "&sfd_viewas=" + bcv_mfield.cmd_viewas
                  + "&sfd_populate=" + bcv_mfield.cmd_populate)
          + "&sfd_align=" + bcv_mfield.cmd_align
          + "&sfd_sort=" + lcSort
          + "&sfd_id="
          + "&sfd_class=" + lcClass).
      RUN updateFieldParam IN phsource
        ("sfd_total",STRING(bcv_mfield.cmd_total,"Y/N")).
      RUN updateFieldParam IN phsource
        ("sfd_chartx",STRING(bcv_mfield.cmd_chartx,"Y/N")).
      RUN updateFieldParam IN phsource
        ("sfd_charty",STRING(bcv_mfield.cmd_charty,"Y/N")).
      RUN updateFieldValue IN phSource
        ("sfd_calc",bcv_mfield.cmd_calc).
      RUN updateFieldValue IN phSource
        ("mf_function",bcv_mfield.mf_function).
      RUN updateFieldValue IN phSource
        ("sfd_label",bcv_mfield.cmd_label).
      RUN updateFieldValue IN phSource
        ("sfd_format",bcv_mfield.cmd_format).
    END.
    WHEN "ITEM" THEN
    DO:
      FIND cv_item OF bcv_mfield
        NO-LOCK NO-ERROR.
      lcOffset = TRIM({syattr.i &param=bcv_mfield.cmd_param
                                &attr=cmd_offset &default=" "}).
      lcOnBlur = TRIM({syattr.i &param=bcv_mfield.cmd_param
                                &attr=cmd_onblur &default=" "}).               
      lcID     = TRIM({syattr.i &param=bcv_mfield.cmd_param
                                &attr=cmd_id &default=" "}).
      lcOnChange = TRIM({syattr.i &param=bcv_mfield.cmd_param
                                  &attr=cmd_onchange &default=" "}).                          
      lcFunc = bcv_mfield.mf_function.                          

      lbBkDown = cv_item.cvi_brkdown.

      IF lbBKDown THEN
      DO:
        lcTLA = SUBSTRING(cv_item.cvi_value,1,3).
        IF (bcv_mview.cmv_type = "CHBRWS" AND lcTLA = "JSC")
          OR (bcv_mview.cmv_type = "PLOTBRWS" AND lcTLA = "VWB")
          OR (bcv_mview.cmv_type = "SUBBRWS" AND lcTLA = "SBS")
          OR (bcv_mview.cmv_type = "MATBRWS" AND lcTLA = "POH")
          OR (bcv_mview.cmv_type = "CATBRWS" AND lcTLA = "CAT")
          OR (bcv_mview.cmv_type = "WBSBRWS" AND lcTLA = "JWB")
          OR (bcv_mview.cmv_type = "SECBRWS" AND lcTLA = "JCS")
          OR (bcv_mview.cmv_type = "ACTBRWS" AND lcTLA = "JCA")
          OR (bcv_mview.cmv_type = "JCCBRWS" AND lcTLA = "JCC")
          OR (bcv_mview.cmv_type BEGINS "CHANAL" AND lcTLA = "COA")
          OR (bcv_mview.cmv_type = "JOBSUM" AND lcTLA = "JOB")
          THEN lcFunc = "%WCV1016B" + lcTLA
                           + "&cv_itemRowid=" + STRING(ROWID(cv_item))
                           + "&updateContext=CVR"
                           + "&updateAllowed=" 
                              + (IF TRIM({syattr.i &param=bcv_mview.cmv_param
                                           &attr=cmv_update &default=N}) = "Y"
                                  AND tcv_cvrhead.cvs_complete = NO
                                  THEN "Y":u ELSE "N":u).
        
        IF (LOOKUP(bcv_mview.cmv_type,"CHBRWS,WBSBRWS,SECBRWS,ACTBRWS") > 0
              OR bcv_mview.cmv_type BEGINS "CHANAL")
          AND lcTLA = "JCC" THEN
        DO:
          lcTable = (IF bcv_mview.cmv_type BEGINS "CHANAL" THEN "co_analysis"
                       ELSE ENTRY(LOOKUP(bcv_mview.cmv_type,
                                     "CHBRWS,WBSBRWS,SECBRWS,ACTBRWS"),
                          "jc_stndcc,jc_wbs,jc_section,jc_jactivity")).
          lcTLA = (IF bcv_mview.cmv_type BEGINS "CHANAL" THEN "COA"
                     ELSE ENTRY(LOOKUP(bcv_mview.cmv_type,
                                     "CHBRWS,WBSBRWS,SECBRWS,ACTBRWS"),
                                     "JSC,JWB,JCS,JCA")).
          lcFunc = "%WCV1017B" + lcTLA
                           + "&cv_itemRowid=" + STRING(ROWID(cv_item))
                           + "&updateContext=CVR"
                           + "&updateAllowed=" 
                              + (IF TRIM({syattr.i &param=bcv_mview.cmv_param
                                           &attr=cmv_update &default=N}) = "Y"
                                  AND tcv_cvrhead.cvs_complete = NO
                                  THEN "Y":u ELSE "N":u)
                           + "&parentTable=" + lcTable.
        END.
      END.  /* lbBkDown */
      
      IF bcv_mview.cmv_type = "JOBSUM"
        AND bcv_mfield.cmd_ikey <> ""
        AND bcv_mfield.cmd_ikey <> "CONTRACT"
        AND (cv_item.cvi_table = "jc_stndcc" 
               OR cv_item.cvi_table = "jc_costcode"
               OR cv_item.cvi_table = "co_analysis") THEN
        DO:
          IF bcv_mfield.cmd_ikey = "CATEGORY"
          THEN
          lcSumKey = cv_item.cvi_item + "||" + lcOffset + "|CATEGORY|".
          ELSE
          lcSumKey = cv_item.cvi_item + "||" + lcOffset + "|ANALYSIS|COSTHEAD|"
                          + SUBSTRING(bcv_mfield.cmd_ikey,7,1) + "|".
           
          lcMask = TRIM({syattr.i &param=bcv_mfield.cmd_param
                                  &attr=cmd_mask &default=" "}).

          IF pcv_mfield.cmd_grpinc <> "" 
          THEN
          DO li = 1 TO NUM-ENTRIES(pcv_mfield.cmd_grpsub,"&"):
            lcSub = ENTRY(li,pcv_mfield.cmd_grpsub,"&").
            lcMask = REPLACE(lcMask,"~{" + ENTRY(1,lcSub,"=") + "~}",
                                           ENTRY(2,lcSub,"=")).
            lcFunc = REPLACE(lcFunc,"~{" + ENTRY(1,lcSub,"=") + "~}",
                                           ENTRY(2,lcSub,"=")).
          END.
          lcSumKey = lcSumKey + lcMask.
      END.
      
      IF bcv_mview.cmv_type = "PERBRWS" THEN
      DO:
        lcSumKey = cv_item.cvi_item + "||" + lcOffset + "|CONTRACT|"
                      + tcv_cvrhead.job_num + "|" + tcv_cvrhead.jph_phase.
      END.
      
      lcName = (IF bcv_mview.cmv_type = "CHBRWS" 
                            THEN "RW_jsc_cvritem"
                          ELSE IF bcv_mview.cmv_type = "PLOTBRWS" 
                            THEN "RW_cdw_cvritem"
                          ELSE IF bcv_mview.cmv_type = "SUBBRWS" 
                            THEN "RW_sbs_cvritem"
                          ELSE IF bcv_mview.cmv_type = "MATBRWS" 
                            THEN "RW_poh_cvritem"
                          ELSE IF bcv_mview.cmv_type = "CATBRWS" 
                            THEN "RW_cat_cvritem"
                          ELSE IF bcv_mview.cmv_type = "WBSBRWS" 
                            THEN "RW_jwb_cvritem"
                          ELSE IF bcv_mview.cmv_type = "SECBRWS" 
                            THEN "RW_jcs_cvritem"
                          ELSE IF bcv_mview.cmv_type = "ACTBRWS" 
                            THEN "RW_jca_cvritem"
                          ELSE IF bcv_mview.cmv_type = "JCCBRWS" 
                            THEN "RW_jcc_cvritem"
                          ELSE IF bcv_mview.cmv_type = "PERBRWS" 
                            THEN "xxacv_cvrhead.RO_cvs_sumitem"
                          ELSE IF bcv_mview.cmv_type BEGINS "CHANAL" 
                            THEN "RW_coa_cvritem"
                          ELSE IF lcSumKey <> ""
                            THEN "RO_cvs_sumitem"
                          ELSE "RW_cvs_cvritem")
             + "_" + SUBSTRING(cv_item.cvi_dtype,1,3)
             + "^" + (IF lcSumKey <> "" THEN lcSumKey
                       ELSE bcv_mfield.cvi_item + "|" /*+ bcv_mfield.cmd_ikey*/
                        + "|" + lcOffset).
      lcName = RIGHT-TRIM(lcName,"|").                 

      IF AVAILABLE cv_style
        THEN lcClass = (IF cv_style.cvf_alertcls <> ""
                          THEN "~{co_config.RO_RangeClass^" 
                               + cv_style.cvf_alertcls + "|~{"
                               + lcName
                               + "~}~}"
                          ELSE IF bcv_mfield.cmd_update
                             AND TRIM({syattr.i &param=cv_style.cvf_param
                                      &attr=cvf_inpclass &default=" "}) <> ""
                             THEN TRIM({syattr.i &param=cv_style.cvf_param
                                      &attr=cvf_inpclass &default=" "})
                          ELSE cv_style.cvf_class).
       
      RUN updateField IN phSource
        ("sfd_name=" + lcName
          + "&sfd_label=" + bcv_mfield.cmd_label
          + "&sfd_format=" + bcv_mfield.cmd_format
          + "&sfd_hide=" + STRING(bcv_mfield.cmd_hide,"Y/N")
          + "&sfd_append=" + STRING(bcv_mfield.cmd_append,"Y/N")
          + "&sfd_lspan=" + lcLSpan
          + "&sfd_pos=" + STRING(bcv_mfield.cmd_colspan)
          + "&sfd_width=" + STRING(liWidth) 
          + "&sfd_height=" + STRING(liHeight)
          + "&sfd_update=" + (IF lcOffset <> "" 
                                 OR lbBkDown THEN "N" 
                                 ELSE STRING(bcv_mfield.cmd_update))
          + (IF bDebug 
               THEN "&sfd_viewas=TOOLTIP"
                  + "&sfd_populate=cvs-rsp.RO_cvs_debugtooltip^" 
                        + STRING(ROWID(bcv_mfield)) + "|"
                        + STRING(ROWID(pcv_mfield))                        
               ELSE "&sfd_viewas=" + bcv_mfield.cmd_viewas
                  + "&sfd_populate=" + bcv_mfield.cmd_populate)
          + "&sfd_align=" + bcv_mfield.cmd_align
          + "&sfd_sort=" 
          + "&sfd_class=" + lcClass
          + "&sfd_id=" + lcID).
      RUN updateFieldParam IN phsource
        ("sfd_total",STRING(bcv_mfield.cmd_total,"Y/N")).
      RUN updateFieldParam IN phsource
        ("sfd_chartx",STRING(bcv_mfield.cmd_chartx,"Y/N")).
      RUN updateFieldParam IN phsource
        ("sfd_charty",STRING(bcv_mfield.cmd_charty,"Y/N")).
      RUN updateFieldValue IN phSource
        ("sfd_calc",bcv_mfield.cmd_calc).
      RUN updateFieldValue IN phSource
        ("mf_function",lcFunc).
      RUN updateFieldValue IN phSource
        ("sfd_label",bcv_mfield.cmd_label).
      RUN updateFieldValue IN phSource
        ("sfd_format",bcv_mfield.cmd_format).
      RUN updateFieldValue IN phSource
        ("sfd_onBlur",lcOnBlur).
      RUN updateFieldValue IN phSource
        ("sfd_onChange",lcOnChange).
         
      /*
      IF bcv_mfield.cmd_calc <> "" THEN
      DO:
       {wodebug.i paucoc "'**1' lcname bcv_mfield.cmd_calc lcTmp"}
       lcTmp = DYNAMIC-FUNCTION("getFieldValue" IN SOURCE-PROCEDURE,lcName,"").
       {wodebug.i paucoc "'**2' lcname bcv_mfield.cmd_calc lcTmp"}
       lcTmp = evaluate$('this="' + lcTmp + '";' + bcv_mfield.cmd_calc).
       {wodebug.i paucoc "'**3'lcname bcv_mfield.cmd_calc lcTmp"}
      END.
      */
    END.
    WHEN "CALC" THEN
    DO:
      IF AVAILABLE cv_style
        THEN lcClass = (IF cv_style.cvf_alertcls <> ""
                          THEN "~{co_config.RO_RangeClass^" 
                               + cv_style.cvf_alertcls + "|~{eval."
                               + TRIM({syattr.i &param=bcv_mfield.cmd_param
                                          &attr=cmd_alertvar &default=" "})
                               + "~}~}"
                               ELSE cv_style.cvf_class).
      /*{wodebug.i paucoc "bcv_mfield.cmd_seq lcclass bcv_mfield.cmd_calc"} */
      RUN updateField IN phSource
        ("sfd_name="
          + "&sfd_label=" + bcv_mfield.cmd_label
          + "&sfd_update=N"
          + "&sfd_format=" + bcv_mfield.cmd_format
          + "&sfd_hide=" + STRING(bcv_mfield.cmd_hide,"Y/N")
          + "&sfd_append=" + STRING(bcv_mfield.cmd_append,"Y/N")
          + "&sfd_lspan=" + lcLSpan
          + "&sfd_pos=" + STRING(bcv_mfield.cmd_colspan)
          + "&sfd_width=" + STRING(liWidth) 
          + "&sfd_height=" + STRING(liHeight)
          + (IF bDebug 
               THEN "&sfd_viewas=TOOLTIP"
                  + "&sfd_populate=cvs-rsp.RO_cvs_debugtooltip^" 
                        + STRING(ROWID(bcv_mfield)) + "|"
                        + STRING(ROWID(pcv_mfield))                        
               ELSE "&sfd_viewas=" + bcv_mfield.cmd_viewas
                  + "&sfd_populate=" + bcv_mfield.cmd_populate)
          + "&sfd_align=" + bcv_mfield.cmd_align
          + "&sfd_sort=" 
          + "&sfd_id="
          + "&sfd_class=" + lcClass).
      RUN updateFieldParam IN phsource
        ("sfd_total",STRING(bcv_mfield.cmd_total,"Y/N")).
      RUN updateFieldParam IN phsource
        ("sfd_chartx",STRING(bcv_mfield.cmd_chartx,"Y/N")).
      RUN updateFieldParam IN phsource
        ("sfd_charty",STRING(bcv_mfield.cmd_charty,"Y/N")).
      RUN updateFieldValue IN phSource
        ("sfd_calc",bcv_mfield.cmd_calc).
      RUN updateFieldValue IN phSource
        ("mf_function",bcv_mfield.mf_function).
      RUN updateFieldValue IN phSource
        ("sfd_label",bcv_mfield.cmd_label).
      RUN updateFieldValue IN phSource
        ("sfd_format",bcv_mfield.cmd_format).
      /*
      IF bcv_mfield.cmd_calc <> "" THEN
      DO:
       lcTmp = evaluate$(bcv_mfield.cmd_calc).
      END.
      */
    END.
    WHEN "TEXT" THEN
    DO:
      RUN updateField IN phSource
        ("sfd_name=text^" + bcv_mfield.cmd_label
          + "&sfd_label="
          + "&sfd_update=N"
          + "&sfd_format=" + bcv_mfield.cmd_format
          + "&sfd_hide=" + STRING(bcv_mfield.cmd_hide,"Y/N")
          + "&sfd_append=" + STRING(bcv_mfield.cmd_append,"Y/N")
          + "&sfd_lspan=" + lcLSpan
          + "&sfd_pos=" + STRING(bcv_mfield.cmd_colspan)
          + "&sfd_width=" + STRING(liWidth) 
          + "&sfd_height=" + STRING(liHeight)
          + (IF bDebug 
               THEN "&sfd_viewas=TOOLTIP"
                  + "&sfd_populate=cvs-rsp.RO_cvs_debugtooltip^" 
                        + STRING(ROWID(bcv_mfield)) + "|"
                        + STRING(ROWID(pcv_mfield))                        
               ELSE "&sfd_viewas=" + bcv_mfield.cmd_viewas
                  + "&sfd_populate=" + bcv_mfield.cmd_populate)
          + "&sfd_align=" + bcv_mfield.cmd_align
          + "&sfd_sort=" 
          + "&sfd_id="
          + "&sfd_class=" + (IF AVAIL cv_style 
                               THEN cv_style.cvf_class ELSE "")).
      RUN updateFieldParam IN phsource
        ("sfd_total",STRING(bcv_mfield.cmd_total,"Y/N")).
      RUN updateFieldParam IN phsource
        ("sfd_chartx",STRING(bcv_mfield.cmd_chartx,"Y/N")).
      RUN updateFieldParam IN phsource
        ("sfd_charty",STRING(bcv_mfield.cmd_charty,"Y/N")).
      RUN updateFieldValue IN phSource
        ("mf_function",bcv_mfield.mf_function).
      RUN updateFieldValue IN phSource
        ("sfd_name", "text^" + bcv_mfield.cmd_label).
      RUN updateFieldValue IN phSource
        ("sfd_format",bcv_mfield.cmd_format).
      RUN updateFieldValue IN phSource
        ("sfd_calc","").
    END.
    WHEN "IVIEW" THEN
    DO:
      cInLineViews = cInLineViews + (IF cInlineViews <> "" THEN "," ELSE "")
                        + TRIM({syattr.i &param=bcv_mfield.cmd_param
                                         &attr=cmd_iview &default=" "}).
      RUN updateField IN phSource
        ("sfd_name="
          + "&sfd_label="
          + "&sfd_update=N"
          + "&sfd_format="
          + "&sfd_hide=N"
          + "&sfd_append=" + STRING(bcv_mfield.cmd_append,"Y/N")
          + "&sfd_lspan=" + lcLSpan
   
          
          + "&sfd_pos=" + STRING(bcv_mfield.cmd_colspan)
          + "&sfd_width=" + STRING(liWidth)
          + "&sfd_height=" + STRING(liHeight)

          + "&sfd_viewas=IFRAME"
          + "&sfd_populate="
          + "&sfd_align="
          + "&sfd_sort="
          + "&sfd_class=").
      RUN updateFieldValue IN phSource
        ("mf_function","%WCV1015SCVS"
              + TRIM(STRING(NUM-ENTRIES(cInlineViews)))).
      RUN updateFieldValue IN phSource
        ("sfd_calc","").
    END.
  END CASE.

END PROCEDURE. /* generateField */


PROCEDURE generateBodySpan:
  DEF VAR lcLast   AS CHA NO-UNDO.
  DEF VAR liCols   AS INT NO-UNDO.
  DEF VAR lcLabels AS CHA NO-UNDO.
  DEF VAR lcCols   AS CHA NO-UNDO.
  DEF VAR lrView   AS ROWID NO-UNDO.
  DEF VAR li       AS INT NO-UNDO.

  DEF BUFFER xcv_mfield FOR cv_mfield.

  {try.i}
  lrView = TO-ROWID(getVar('cv_mviewRowid')).
  lcLast = ?.

  FIND cv_mview
    WHERE ROWID(cv_mview) = lrView 
    NO-LOCK NO-ERROR.
  IF NOT AVAIL cv_mview
    THEN throwError({msg.i SY719 'cv_mview' 'cvs-rsp.generateBodySpan'}).
 
  FOR EACH cv_mfield OF cv_mview
    WHERE cv_mfield.cmd_hide = FALSE
    NO-LOCK
    BY cv_mfield.cmd_seq:
    {try.i}

    IF cv_mfield.cmd_grpinc <> ""
    THEN
    FOR EACH xcv_mfield
      WHERE xcv_mfield.kco     = cv_mview.kco
      AND xcv_mfield.cmh_model = cv_mview.cmh_model
      AND xcv_mfield.cmv_view  = ""
      AND xcv_mfield.cmi_group = cv_mfield.cmd_grpinc
      AND xcv_mfield.cmd_hide = FALSE
      NO-LOCK
      BY xcv_mfield.cmd_seq:
      {try.i}
      IF lcLast <> xcv_mfield.cmd_char[1] 
      AND lcLast <> ? THEN
      DO:
        lcLabels = lcLabels + (IF lcLabels <> "" THEN CHR(9) ELSE "")
                            + lcLast.
        lcCols = lcCols + (IF lcCols <> "" THEN CHR(9) ELSE "")
                            + STRING(liCols).
        liCols = 0.
      END.
      liCols = liCols + 1.
      lcLast = xcv_mfield.cmd_char[1].
      {end.i}
    END.
    ELSE DO:
      IF lcLast <> cv_mfield.cmd_char[1] 
      AND lcLast <> ? THEN
      DO:
        lcLabels = lcLabels + (IF lcLabels <> "" THEN CHR(9) ELSE "")
                            + lcLast.
        lcCols = lcCols + (IF lcCols <> "" THEN CHR(9) ELSE "")
                            + STRING(liCols).
        liCols = 0.
      END.
      liCols = liCols + 1.
      lcLast = cv_mfield.cmd_char[1].
    END.
    {end.i}
  END.  /* EACH cv_mfield */

  lcLabels = lcLabels + (IF lcLabels <> "" THEN CHR(9) ELSE "")
                      + lcLast.
  lcCols = lcCols + (IF lcCols <> "" THEN CHR(9) ELSE "")
                     + STRING(liCols).

  DO li = 1 TO NUM-ENTRIES(lcCols,CHR(9)):
    IF li > 1 THEN RUN addField IN SOURCE-PROCEDURE.
    RUN updateField IN SOURCE-PROCEDURE
      ("sfd_name="
        + "&sfd_label=" + ENTRY(li,lcLabels,CHR(9))
        + "&sfd_update=N"
        + "&sfd_format="
        + "&sfd_hide=N"
        + "&sfd_append=N"
        + "&sfd_pos=" + ENTRY(li,lcCols,CHR(9))
        + "&sfd_lspan=0"
        + "&sfd_viewas="
        + "&sfd_align=default"
        + "&sfd_class=").
  END.
  {end.i}
END PROCEDURE. /* generateBodySpan */

PROCEDURE buildCVROptions:
  {try.i}
  FIND jc_job OF tcv_cvrhead NO-LOCK NO-ERROR.
  IF NOT AVAIL jc_job
    THEN throwError({msg.i SY719 'jc_job' 'cvs-rsp.buildCVROption'}).

  IF tcv_cvrhead.cvs_complete = NO
  THEN
  RUN addPageOptionFunction IN SOURCE-PROCEDURE
    (xText('Update'),
     xText('Update CVR Items'),
     "%WCV1011FCVS",
     "cv_cvrheadRowid=" + STRING(tcv_cvrhead.rsp_dbrowid) +
     "&jc_jobRowid=" + STRING(ROWID(jc_job))).

  &IF {&db-version} >= 10.19 &THEN
  IF tcv_cvrhead.cvs_complete = NO
  THEN
  RUN addPageOptionFunction IN SOURCE-PROCEDURE
    (xText('Update'),
     xText('Recalculate Summary Values'),
     "%WCV1012FCVS",
     "cv_cvrheadRowid=" + STRING(tcv_cvrhead.rsp_dbrowid) +
     "&jc_jobRowid=" + STRING(ROWID(jc_job))).
  &ENDIF
  
  RUN addPageOptionFunction IN SOURCE-PROCEDURE
    (xText('Print'),
     xText('Print CVR'),
     "%WCV3011RCVS",
     "cv_cvrheadRowid=" + STRING(tcv_cvrhead.rsp_dbrowid) +
     "&jc_jobRowid=" + STRING(ROWID(jc_job))
        /* no need for this as value is encoding automatically
        + "&rpa_page=" + DYNAMIC-FUNCTION("urlEncode" IN SOURCE-PROCEDURE,
                                                getRO_cmh_reppage(),"QUERY")
        */
        + "&rpa_page=" + getRO_cmh_reppage()
        + "&rs_rpt=" + xText("CVR Print") + " - "
                     + STRING(tcv_cvrhead.kco,"999") + "/"
                     + tcv_cvrhead.job_num + "/"
                     + tcv_cvrhead.cvs_cvrno).

  &IF {&db-version} >= 10.19 &THEN
  IF CAN-FIND(FIRST cv_item
           WHERE cv_item.kco = tcv_cvrhead.kco
           AND cv_item.cvi_brkdown
           AND CAN-FIND(FIRST cv_mfield
                     WHERE cv_mfield.kco     = tcv_cvrhead.kco
                     AND cv_mfield.cmh_model = tcv_cvrhead.cmh_model
                     AND cv_mfield.cvi_item  = cv_item.cvi_item))
  THEN
  RUN addPageOptionFunction IN SOURCE-PROCEDURE
    (xText('Print'),
     xText('Item Breakdown'),
     "%WCV3061RCFB",
     "cv_cvrheadRowid=" + STRING(tcv_cvrhead.rsp_dbrowid) + 
     "&jc_jobRowid=" + STRING(ROWID(jc_job))
        + "&rs_rpt=" + xText("CVR Item Breakdown") + " - "
                     + STRING(tcv_cvrhead.kco,"999") + "/"
                     + tcv_cvrhead.job_num + "/"
                     + tcv_cvrhead.cvs_cvrno).
  &ENDIF                   

  IF CAN-FIND(FIRST cv_mview
           WHERE cv_mview.kco     = tcv_cvrhead.kco
           AND cv_mview.cmh_model = tcv_cvrhead.cmh_model
           AND cv_mview.cmv_type BEGINS 'MAX')
  THEN
  RUN addPageOptionFunction IN SOURCE-PROCEDURE
    (xText('Print'),
     xText('Matrix Views'),
     "%WCV3091RCVS",
     "cv_cvrheadRowid=" + STRING(tcv_cvrhead.rsp_dbrowid) + 
     "&jc_jobRowid=" + STRING(ROWID(jc_job)) +
     "&rs_rpt=" + xText("CVR Matrix Print") + " - "
                + STRING(tcv_cvrhead.kco,"999") + "/"
                + tcv_cvrhead.job_num + "/"
                + tcv_cvrhead.cvs_cvrno).
   
  IF tcv_cvrhead.cvs_complete THEN 
    RUN addPageOptionFunction IN SOURCE-PROCEDURE
      (xText('Post'),
       xText('Post CVR Items'),
       "%WCV3074RCVI",
       "cv_cvrheadRowid=" + STRING(tcv_cvrhead.rsp_dbrowid) +
       "&jc_jobRowid=" + STRING(ROWID(jc_job))
        + "&rs_rpt=" + xText("Post CVR Items") + " - "
                     + STRING(tcv_cvrhead.kco,"999") + "/"
                     + tcv_cvrhead.job_num + "/"
                     + tcv_cvrhead.cvs_cvrno).
  {end.i}
END PROCEDURE. /* buildCVROptions */

FUNCTION allowUpd RETURNS LOGICAL:
  IF getVar("updateContext") = "CVR":u 
    THEN RETURN (getVar("updateAllowed") = "Y").
  RETURN TRUE.
END FUNCTION.  /* allowUpd */

PROCEDURE preProcess:
  rLastCVR = ?.
  bDebug = (getVar("COINSInfo") = "true").
END PROCEDURE. /* preProcess */


PROCEDURE submitUpdate:
  DEF VAR lcParam AS CHA NO-UNDO.
  DEF VAR lhCRR   AS HANDLE NO-UNDO.
  DEF VAR lcQueue AS CHA NO-UNDO.

  DEF BUFFER cv_cvrprog FOR cv_cvrprog.
  DEF BUFFER cv_cvrhead FOR cv_cvrhead.

  {try.i}
  lcQueue = getSysParam(getCID(),"CV":u,"UPDREPQ":u).
  /* if we come from scheduled generate cvr3081.p then use 
     the queue the scheduler is on */
  IF getVar("scheduledCVRGenerate") = "Y":u 
    THEN lcQueue = getVar("scheduledCVRQueue").
  lhCRR  = getService("crr-rsp").
  lcParam = "kco=" + STRING(getCID())  
                + "&cv_cvrheadRowid=" + STRING(tcv_cvrhead.rsp_dbrowid)
                + "&FOPProg=&MainArea=%WCV1011FCVS&rtn_code=%WCV3001RCVS"
                + "&RW_Items=*"
                + "&RW_Plots=yes"
                + "&RW_Summary=yes"
                + "&RW_Appraisal=yes"
                + "&RW_Prorate=yes"
                + "&crr_queue=" + lcQueue
                + "&copyPrevAdj=" + TRIM(STRING(bCopyPrevAdj,"yes/no")).

  DYNAMIC-FUNCTION("setTitle" IN lhCRR,
                    xText("CVR Update") + " - "
                              + STRING(tcv_cvrhead.kco,"999") + "/"
                              + tcv_cvrhead.job_num + "/"
                              + tcv_cvrhead.cvs_cvrno).
  DYNAMIC-FUNCTION("setModule"  IN lhCRR, "CV").
  DYNAMIC-FUNCTION("setType"    IN lhCRR, "B").
  DYNAMIC-FUNCTION("setProg"    IN lhCRR, "FOP").
  DYNAMIC-FUNCTION("setParam"   IN lhCRR, lcParam).
  DYNAMIC-FUNCTION("submitTask" IN lhCRR).
  
  {sy_lock.i
    &table = cv_cvrprog
    &condition = "
      WHERE cv_cvrprog.kco      = tcv_cvrhead.kco
      AND cv_cvrprog.cdp_type   = 'UPDATE':U
      AND cv_cvrprog.cvs_ref    = tcv_cvrhead.cvs_ref"
    &excludeNoAvail=*
  }
  
  IF NOT AVAIL cv_cvrprog THEN
  DO:
    CREATE cv_cvrprog.
    ASSIGN
      cv_cvrprog.kco        = tcv_cvrhead.kco
      cv_cvrprog.cdp_type   = "UPDATE":U
      cv_cvrprog.cvs_ref    = tcv_cvrhead.cvs_ref.
  END.
  
  ASSIGN
    cv_cvrprog.cdp_progress = 0
    cv_cvrprog.cdp_desc = xText("Update Submitted").

  {sy_lock.i
    &table = cv_cvrhead
    &rid = tcv_cvrhead.rsp_dbrowid
  }
  cv_cvrhead.cvs_sysstat = 'UPDATING'.
  FIND CURRENT cv_cvrhead NO-LOCK. 
  {end.i}
END PROCEDURE.

PROCEDURE localCloseObject:
{try.i}
  DELETE PROCEDURE hEval.
{end.i}
END PROCEDURE. 
{buildinf.i cvs-rsp-p}
