/*-----------------------------------------------------------------------------
 * Copyright (c) 1993,1994 Southeastern Universities Research Association,
 *                         Continuous Electron Beam Accelerator Facility
 *
 * This software was developed under a United States Government license
 * described in the NOTICE file included as part of this distribution.
 *
 * Stephen A. Wood, 12000 Jefferson Ave., Newport News, VA 23606
 * Email: saw@cebaf.gov  Tel: (804) 249-7367  Fax: (804) 249-5800
 *-----------------------------------------------------------------------------
 * 
 * Description:
 *  Header for RPC handlers
 *	
 * Author:  Stephen A. Wood, CEBAF Hall C
 *
 * Revision History:
 *  $Log$
 *  Revision 1.1  1998/12/07 22:11:09  saw
 *  Initial setup
 *
 *	  Revision 1.3  1994/11/07  14:31:34  saw
 *	  Add pending callback requests structure definition
 *
 *	  Revision 1.2  1993/05/11  17:34:58  saw
 *	  Fix $Log$
 *	  Fix Revision 1.1  1998/12/07 22:11:09  saw
 *	  Fix Initial setup
 *	  Fix
 *	  Revision 1.3  1994/11/07  14:31:34  saw
 *	  Add pending callback requests structure definition
 *
 *
 */

void daVarReadVar(char *name, any *retval);
daVarStatus daVarRegRatr(daVarStruct *varp, char *attribute
			 ,int index, any *retval);
daVarStatus daVarRegWatr(daVarStruct *varp, char *attribute
			 ,int index, any *setval);

#define DAVAR_VALUE "value"
#define DAVAR_TITLE "title"
#define DAVAR_SIZE "size"
#define DAVAR_FLAG "flag"
#define DAVAR_TYPE "type"
#define DAVAR_WATR "watr"
#define DAVAR_RATR "ratr"

#define DAVAR_NOINDEX -12345678

/* Structures for holding information about call back requests */

struct daVarCallBackList {
  struct sockaddr_in *sock_in;	/* Caller socket info */
  struct daVarCallBackList *next;
  struct TESTNAMELIST *list;
  time_t start_time;
};
typedef struct daVarCallBackList daVarCallBackList;
