/*-
     Program: fmrswj05.p
      Module: d-FM
     Version: 01.00.00
 Description: FM job values
 Last Change: 08/12/2011
  Changed By: robosb
-*/
{buildinf.i fmrswj05-p 01.00.00}
{cos000.i}
{cos001.i}
{dbver.i}
{sy_config.i &co=* &jc=*}

DEF VAR iKco AS INTEGER NO-UNDO.
iKco = getCID().

{fmjobvals.i}

{syrgen.i &table=ttFMJobVals} 
{syujobsec.i &style=2}

FUNCTION getQuery$ RETURNS CHARACTER ():
  DEF VAR lcQuery    AS CHAR  NO-UNDO.

  lcQuery =   
    "FOR EACH jc_job WHERE jc_job.kco = " + STRING(iKco)
     + " " + DYNAMIC-FUNCTION("getJobSelect" IN hFSP) + " "
     + ", EACH sw_job OF jc_job where TRUE "
     + DYNAMIC-FUNCTION("getSWJSelect" IN hFSP)
     + " no-lock".
    
  RETURN lcQuery.
END FUNCTION.

{syufor.i  &tables="jc_job sw_job"
           &query="getQuery$()"
           &main="RUN FMJobValues (sw_job.kco,
                                   sw_job.job_num,
                                   sw_job.swj_num)."}
  
PROCEDURE generateReport:

  RUN ForEachRecord.
  
END. 
{buildinf.i fmrswj05-p}
