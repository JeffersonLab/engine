/*-----------------------------------------------------------------------------
 * Copyright (c) 1993 Southeastern Universities Research Association,
 *                    Continuous Electron Beam Accelerator Facility
 *
 * This software was developed under a United States Government license
 * described in the NOTICE file included as part of this distribution.
 *
 * Stephen A. Wood, 12000 Jefferson Ave., Newport News, VA 23606
 * Email: saw@cebaf.gov  Tel: (804) 249-7367  Fax: (804) 249-5800
 *-----------------------------------------------------------------------------
 * 
 * Description:
 *  daVar RPCGEN input file
 *	
 * Author:  Stephen Wood, CEBAF Hall C
 *
 * Revision History:
 *   $Log$
 *   Revision 1.1  1998/12/07 22:11:10  saw
 *   Initial setup
 *
 *   Revision 1.3  1994/11/07  14:33:12  saw
 *   Add testnamelist structure and readmultiple_test and family rpc's
 *
 *   Revision 1.2  1993/11/24  22:01:35  saw
 *   Add double variable type
 *
 *   Revision 1.1  1993/05/10  21:10:37  saw
 *   Initial revision
 *
 */

%#include <time.h>

#if defined(RPC_HDR) || defined(RPC_XDR) || defined(RPC_SVC) || \
  defined(RPC_CLNT) || defined(RPC_TBL)
#define RPCGEN
#else
#undef RPCGEN
#endif

#include "daVar.h"
const DAVARINT_RPC = DAVARINT;
const DAVARFLOAT_RPC = DAVARFLOAT;
const DAVARDOUBLE_RPC = DAVARDOUBLE;
const DAVARSTRING_RPC = DAVARSTRING;
const DAVARERROR_RPC = 999;

typedef string PNAME<>;
typedef PNAME NAMELIST<>;

union any switch (int valtype) {
 case DAVARINT_RPC:
  int i<>;
 case DAVARFLOAT_RPC:
  float r<>;
 case DAVARDOUBLE_RPC:
  double d<>;
 case DAVARSTRING_RPC:
  string s<>;
 case DAVARERROR_RPC:
  int error;
 default:
  void;
};

struct wany {
  PNAME name;
  any *val;
};

typedef any RVALLIST<>;

typedef wany WVALLIST<>;

typedef int ERRLIST<>;

struct TESTNAMELIST {
    /* Can we have a more generic structure fore this? */
  string test_condition<>;
  int max_time_wait;            /* Seconds before failing */
  int max_event_wait;           /* # of events before failing */
  int prog;
  int vers;
  NAMELIST *NAMELISTP;
};

program DAVARSVR {
  version DAVARVERS {
    int DAVAR_ACKMESSAGE(string) = 101;
    NAMELIST DAVAR_GETLIST(string) = 102;
    RVALLIST DAVAR_READMULTIPLE(NAMELIST) = 103;
    ERRLIST DAVAR_WRITEMULTIPLE(WVALLIST) = 104;
    int DAVAR_READMULTIPLE_TEST(TESTNAMELIST) = 105;
#if defined(RPC_HDR) || defined(RPC_XDR) || defined(RPC_SVC)
    int DAVAR_READMULTIPLE_TEST_CB(RVALLIST) = 106; /* Call Back */
#endif
/*    double HACK(int) = 201;*/
  } = 1;
} = 0x2c0daFF8;

/*
Local Variables:
mode: c
End:
*/
