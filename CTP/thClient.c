/*-----------------------------------------------------------------------------
 * Copyright (c) 1994 Southeastern Universities Research Association,
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
 *  Routines used by a client to retrieve CTP variables
 *	
 * Author:  Stephen Wood, CEBAF Hall C
 *
 * Revision History:
 *  $Log$
 *  Revision 1.3  1999/08/20 15:28:32  saw
 *  Commented out some comments that were not commented out
 *
 *  Revision 1.2  1999/06/30 16:15:40  saw
 *  Fix OSF clnt_create underscore problem
 *
 *  Revision 1.1  1998/12/07 22:11:11  saw
 *  Initial setup
 *
 * Revision 1.4  1995/08/03  13:50:52  saw
 * Add SGI compatibility
 *
 * Revision 1.3  1994/11/07  14:09:42  saw
 * Bug fixes in thGetList_test and callback server code.
 *
 * Revision 1.2  1994/10/17  17:07:28  saw
 * Add thGetList_test and the callback service davar_readmultiple_test_cb_1
 *
 * Revision 1.1  1994/09/27  19:19:09  saw
 * Initial revision
 *
 */
#include <stdio.h>
#include <string.h>
#include <rpc/rpc.h>
#include "daVar.h"
#include "daVarRpc.h"
#include "cfortran.h"

long thCreateList();        /* Move to some  include file */
long thAddToList(long handle, char *pattern);
long thRemoveFromList(long handle, char *pattern);
long thGetList(long handle, CLIENT *clnt);
long thGetList_test(long handle, CLIENT *clnt, char *test_condition,
		    int max_time_wait, int max_event_wait);
long thPrintList(long handle);

FCALLSCFUN0(LONG,thCreateList,THCRLIST,thcrlist);
FCALLSCFUN2(LONG,thAddToList,THADDLIST,thaddlist,LONG,STRING);
FCALLSCFUN2(LONG,thRemoveFromList,THREMLIST,thremlist,LONG,STRING);
#ifdef __osf__
FCALLSCFUN2(LONG,thGetList,THGETLIST,thgetlist,LONG,PVOID);
FCALLSCFUN5(LONG,thGetList_test,THCGETLIST,thcgetlist,LONG,PVOID,STRING,INT,INT);
#else
FCALLSCFUN2(LONG,thGetList,THGETLIST,thgetlist,LONG,LONG);
FCALLSCFUN5(LONG,thGetList_test,THCGETLIST,thcgetlist,LONG,LONG,STRING,INT,INT);
#endif
FCALLSCFUN1(LONG,thPrintList,THPRTLIST,thprtlist,LONG);
/* Don't really understand the following.  What about ultrix */

/*
linux   _
osf
sun
hp    
ultrix  _
*/

#if !defined(__ultrix__) && !defined(linux)
FCALLSCFUN4(LONG,clnt_create,CLNT_CREATE,clnt_create,STRING,LONG,LONG,STRING);
#else
FCALLSCFUN4(LONG,clnt_create,CLNT_CREATE_,clnt_create_,STRING,LONG,LONG,STRING);
#endif

struct thNameNode {
  char *name;
  struct thNameNode *next;
};
typedef struct thNameNode thNameNode;

struct thNameList {
  thNameNode *namehead;
  int nnames;			/* Number of names in list */
  int rpc_made;
  NAMELIST rpc;		/* List in form for RPC argument */
};
typedef struct thNameList thNameList;

TESTNAMELIST *pending_arg=0;
int pending_flag=0;
int callback_result;

long thCreateList(){
  /* Create a handle for a list of variables */
  thNameList *list;

  list = (thNameList *) malloc(sizeof(thNameList));;
  list->namehead = 0;
  list->nnames = 0;
  list->rpc_made = 0;
  list->rpc.NAMELIST_len = 0;
  list->rpc.NAMELIST_val = 0;
  return((long) list);
}
long thAddToList(long handle, char *pattern)
/* Add registered variables to a list of variables to get from a server
   Return the number of variables added
   Should we check for duplicates?  Not now.  No harm in duplicates.
   thRemoveFromList will remove all duplicates though. */
{
  thNameList *list;
  thNameNode *next,*this;
  char **vlist;			/* List of characters matching pattern */
  int count;			/* Number of variables being added */
  int i;

  list = (thNameList *) handle;
  daVarList(pattern,&vlist,&count);
  for(i=0;i<count;i++) {
    next = list->namehead;		/* The current list */
    this = list->namehead = (thNameNode *) malloc(sizeof(thNameNode)); /* New name */
    this->name = (char *) malloc(strlen(vlist[i])+1);
    strcpy(this->name,vlist[i]);
    this->next = next;		/* Attach rest of list */
  }
  list->nnames += count;	/* Perhaps I should just count when needed ? */
  list->rpc_made = 0;		/* RPC format list now out of date. */
  return(list->nnames);
}
long thRemoveFromList(long handle, char *pattern)
{
  thNameList *list;
  thNameNode *this,**thisp;
  char **vlist;			/* List of characters matching pattern */
  int count;			/* Number of variables being removed */
  int nremove;
  int i;

  list = (thNameList *) handle;
  daVarList(pattern,&vlist,&count);
  nremove = 0;
  for(i=0;i<count;i++) {
    this = list->namehead;	/* Start of list */
    thisp = &list->namehead;	/* Pointer to next field of previous name */
    while(this) {
      if(strcasecmp(this->name,vlist[i])==0) { /* Remove matching string */
	*thisp = this->next;
	free(this->name);
	free(this);
	this = *thisp;
	nremove++;
      } else {
        thisp = &(this->next);
	this = this->next;
      }
    }
  }
  list->nnames -= nremove;	/* Perhaps I should just count when needed ? */
  return(list->nnames);
}
long thPrintList(long handle)
     /* For debugging */
{
  thNameList *list;
  thNameNode *next,*this;
  int count;

  list = (thNameList *) handle;
  this = list->namehead;
  printf("Variables attached to handle %d\n",handle);
  count++;
  while(this) {
    printf("%s\n",this->name);
    count++;
    this = this->next;
  }
  return(count);
}
long thGetList(long handle, CLIENT *clnt)
     /* Returns 0 for total success, -1 for total failure, a positive number
	for the number of variables that didn't work */
{
  thNameList *list;
  thNameNode *next,*this;
  int i;
  RVALLIST *vals;
  int nerrors;

  list = (thNameList *) handle;
/*  printf("list->nnames=%d\n",list->nnames);*/
  if(!list->rpc_made) {
    if(list->rpc.NAMELIST_len == 0) {
      list->rpc.NAMELIST_len = list->nnames;
      list->rpc.NAMELIST_val = (char **) malloc(list->rpc.NAMELIST_len
						*sizeof(char *));
    } else if (list->rpc.NAMELIST_len != list->nnames) {
      list->rpc.NAMELIST_len = list->nnames;
      list->rpc.NAMELIST_val = (char **)
	realloc(list->rpc.NAMELIST_val,list->rpc.NAMELIST_len*sizeof(char *));
    }
    this = list->namehead;
    for(i=0;(i<list->nnames && this);i++){
      list->rpc.NAMELIST_val[i] = this->name;
      this = this->next;
    }
  }
  nerrors = 0;
  if(vals = davar_readmultiple_1(&(list->rpc),clnt)) {
    this = list->namehead;
/*    printf("list->rpc.NAMELIST_len=%d\n",list->rpc.NAMELIST_len);*/
    for(i=0;(i<list->rpc.NAMELIST_len && this);i++){
/*      printf("%s\n",this->name);*/
      if(vals->RVALLIST_val[i].valtype != DAVARERROR_RPC){
	if(daVarWriteVar(this->name,&(vals->RVALLIST_val[i])) != S_SUCCESS)
	  nerrors++;
      } else {
	nerrors++;
      }
      this = this->next;
    }
  } else {
    nerrors = -1;
  }
  return(nerrors);
}
#if 0
long thPutList(long handle, CLIENT *clnt)
     /* Returns 0 for total success, -1 for total failure, a positive number
	for the number of variables that didn't work */
{
  thNameList *list;
  thNameNode *next,*this;
  int i;
  WVALLIST vals;
  int nerrors;

  /* Create the write structure */
  list = (thNameList *) handle;
/*  printf("list->nnames=%d\n",list->nnames);*/
  if(!list->rpc_made) {
    if(list->rpc.NAMELIST_len == 0) {
      list->rpc.NAMELIST_len = list->nnames;
      list->rpc.NAMELIST_val = (char **) malloc(list->rpc.NAMELIST_len
						*sizeof(char *));
    } else if (list->rpc.NAMELIST_len != list->nnames) {
      list->rpc.NAMELIST_len = list->nnames;
      list->rpc.NAMELIST_val = (char **)
	realloc(list->rpc.NAMELIST_val,list->rpc.NAMELIST_len*sizeof(char *));
    }
    this = list->namehead;
    for(i=0;(i<list->nnames && this);i++){
      list->rpc.NAMELIST_val[i] = this->name;
      this = this->next;
    }
  }
  nerrors = 0;
  if(vals = davar_readmultiple_1(&(list->rpc),clnt)) {
    this = list->namehead;
/*    printf("list->rpc.NAMELIST_len=%d\n",list->rpc.NAMELIST_len);*/
    for(i=0;(i<list->rpc.NAMELIST_len && this);i++){
/*      printf("%s\n",this->name);*/
      if(vals->RVALLIST_val[i].valtype != DAVARERROR_RPC){
	if(daVarWriteVar(this->name,&(vals->RVALLIST_val[i])) != S_SUCCESS)
	  nerrors++;
      } else {
	nerrors++;
      }
      this = this->next;
    }
  } else {
    nerrors = -1;
  }
  return(nerrors);
}
#endif
#if 0
int tsize=0;
struct timeval timeout;
long servone(int wait)
/* Need to move something that does this into CTP proper */
{
  fd_set readfdset;
  extern int errno;
 
  timeout.tv_sec = wait;
  timeout.tv_usec = 1;

#ifdef hpux
  if(!tsize) tsize = NFDBITS;
#else
  if(!tsize) tsize = getdtablesize(); /* how many descriptors can we have */
#endif

  readfdset = svc_fdset;
  switch(select(tsize, &readfdset, (fd_set *) NULL, (fd_set *) NULL,
		&timeout)) {
  case -1:
    if (errno == EBADF) break;
    perror("select failed");
    break;
  case 0:
    /* perform other functions here if select() timed-out */
    break;
  default:
    svc_getreqset(&readfdset);
  }
}
#endif
long thGetList_test(long handle, CLIENT *clnt, char *test_condition,
		    int max_time_wait, int max_event_wait)
     /* Returns 0 for total success, -1 for total failure, a positive number
	for the number of variables that didn't work */
{
  thNameList *list;
  thNameNode *next,*this;
  int i;
  RVALLIST *vals;
  int *status;
  TESTNAMELIST *arg;
  long servret;

  /* Can return some kind of error if pending_arg is not zero */
  list = (thNameList *) handle;
/*  printf("list->nnames=%d\n",list->nnames);*/
  if(!list->rpc_made) {
    if(list->rpc.NAMELIST_len == 0) {
      list->rpc.NAMELIST_len = list->nnames;
      list->rpc.NAMELIST_val = (char **) malloc(list->rpc.NAMELIST_len
						*sizeof(char *));
    } else if (list->rpc.NAMELIST_len != list->nnames) {
      list->rpc.NAMELIST_len = list->nnames;
      list->rpc.NAMELIST_val = (char **)
	realloc(list->rpc.NAMELIST_val,list->rpc.NAMELIST_len*sizeof(char *));
    }
    this = list->namehead;
    for(i=0;(i<list->nnames && this);i++){
      list->rpc.NAMELIST_val[i] = this->name;
      this = this->next;
    }
  }
  arg = (TESTNAMELIST *) malloc(sizeof(TESTNAMELIST));
  arg->test_condition = (char *) malloc(strlen(test_condition)+1);
  strcpy(arg->test_condition,test_condition);
  arg->max_time_wait = max_time_wait;
  arg->max_event_wait = max_event_wait;
  arg->prog = DAVARSVR;
  arg->vers = DAVARVERS+1;
  arg->NAMELISTP = &list->rpc;
  pending_arg = arg;
  pending_flag = 1;

  if(!(status = davar_readmultiple_test_1(arg,clnt)))
    return(-1);

  /* Now wait for the incoming network call */

  servret = 1;
  while(pending_flag && servret > 0) /* Wait for timeout, completion or failur*/
    servret = daVarServOnce(arg->max_time_wait+10); /* Will wait double?? */
  if(servret == 0) callback_result = -2;	/* Timeout */
  else if(servret == -1) callback_result = -3;

  free(arg->test_condition);
  free(arg);
  pending_arg = 0;

  return(callback_result);
}
int *davar_readmultiple_test_cb_1(RVALLIST *vals, CLIENT *clnt)
{
  static int result;

  TESTNAMELIST *argp;
  thNameNode *next,*this;
  int i;

  if(pending_arg) argp = pending_arg;
  else {
    pending_flag = 0;
    return(&result);		/* What error code ?? */
  }

  callback_result = 0;
  if(argp->NAMELISTP->NAMELIST_len == vals->RVALLIST_len) {
    for(i=0;(i<argp->NAMELISTP->NAMELIST_len);i++){
/*      printf("%s\n",this->name);*/
      if(vals->RVALLIST_val[i].valtype != DAVARERROR_RPC){
	if(daVarWriteVar(argp->NAMELISTP->NAMELIST_val[i]
			 ,&(vals->RVALLIST_val[i])) != S_SUCCESS)
	  callback_result++;
      } else {
	callback_result++;
      }
    }
  } else if (vals->RVALLIST_len>0) {
    printf("Lengths: %d %d",argp->NAMELISTP->NAMELIST_len,vals->RVALLIST_len);
    callback_result = -1;
  } else {
    callback_result = -2;       /* Server send timeout signal */
  }
  pending_flag = 0;
  return(&result);
}
