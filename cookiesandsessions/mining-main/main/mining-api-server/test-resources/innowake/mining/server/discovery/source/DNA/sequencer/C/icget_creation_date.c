/*
  Start.Documentation
 
       SYSTEM:  Health Net Utilities
       PROGRAM: ICGet_Creation_Date
       TITLE:   
       ALIAS:
       DATE:    7/17/97
       AUTHOR:  ENU (Rewrite of the MACRO version for ALPHA migration)
 
       OVERVIEW:       This program opens up the file of the filename passed. It
                       then gets the creation date of the file and returns it to
                       the address passed. The return address must be the address
                       of a Quadword (long array).
 
       EXAMPLE:
                       MAP(Whymemother) STRING Filename = 255%       ! Need the full filespec
                       DECLARE QUADWORD Creation_Date                ! Quadword (long array)
                       DECLARE LONG Status                           ! Returned Status
                       Status = Icget_Creation_Date(Filename,Creation_Date BY REF)  ! Quadword must be passed by REF.
 
  End.Documentation
*/

#include <rms.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <ssdef.h>
#include <descrip.h>

struct FAB Fabs;
struct RAB Rabs;
struct NAM Nams;
struct XABDAT Xabdat;

int  ICGET_CREATION_DATE (
                struct dsc$descriptor_s *filename, 
                __int64 *creation_date )
{

  int stat;
  Fabs = cc$rms_fab;
  Xabdat = cc$rms_xabdat;

  Fabs.fab$l_fna = filename->dsc$a_pointer;
  Fabs.fab$b_fns = filename->dsc$w_length;

  Fabs.fab$b_fac = FAB$M_GET;
  Fabs.fab$l_xab = (char*) &Xabdat;
  Fabs.fab$b_shr = FAB$M_SHRUPD | FAB$M_SHRPUT |
                   FAB$M_SHRGET | FAB$M_SHRDEL ;

  stat = sys$open ( &Fabs );
  if ( (stat & 1) != 1 )
    return stat;

  stat = sys$close ( &Fabs );

  *creation_date = Xabdat.xab$q_cdt;

  stat = SS$_NORMAL;
  return stat;
}
