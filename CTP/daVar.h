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
 *  Include file for daVarRegister.c and anything that uses the routines in it.
 *	
 * Author:  Stephen Wood, CEBAF Hall C
 *
 * Revision History:
 *   $Log$
 *   Revision 1.1  1998/12/07 22:11:09  saw
 *   Initial setup
 *
 *	  Revision 1.6  1994/07/21  20:49:34  saw
 *	  Add NOEOL error code and TRUE/FALSE definitions
 *
 *	  Revision 1.5  1994/06/13  13:39:41  saw
 *	  Change values of some constants
 *
 *	  Revision 1.4  1993/11/29  16:02:02  saw
 *	  Add DADOUBLE definition.
 *
 *	  Revision 1.3  1993/11/23  22:32:43  saw
 *	  Add Double type definition
 *
 *	  Revision 1.2  1993/11/22  16:19:48  saw
 *	  Add Fortran string type.
 *
 *	  Revision 1.1  1993/05/10  20:49:21  saw
 *	  Initial revision
 *
 */

#ifndef _DAVAR_H
#define _DAVAR_H

/* Variable types */

#define DAVARINT 1
#define DAVARFLOAT 2
#define DAVARDOUBLE 3
#define DAVARSTRING 4
#define DAVARFSTRING 5
/* For future use */
#define DAVARINTP 17
#define DAVARFLOATP 18
#define DAVARDOUBLEP 19
#define DAVARSTRINGP 20
#define DAVARFSTRINGP 21
/* Flags */

#define DAVAR_READONLY 1
#define DAVAR_READWRITE 0
#define DAVAR_OBEYMF 2
/* If set, varptr may be changed (it doesn't point to fixed "user" data) */
#define DAVAR_REPOINTOK 4
/* If set, this variable is a dynamically created "parm." type variable.
   It's type and size will be adjusted */
#define DAVAR_DYNAMIC_PAR 8

#ifndef RPCGEN
/* Make sure this stuff is not seen by rpcgen */
typedef long daVarStatus;

typedef struct {
  char *name;			/* Name of the object */
  int type;			/* Object type */
  void *varptr;			/* Pointer to the object */
  char *title;			/* Title string */
  int size;			/* Size or length of object */
  int flag;			/* Read Only and other flags */
  void *opaque;			/* Pointer to arbitrary structure */
  daVarStatus (*rhook)(),(*whook)();
} daVarStruct;
/* Size is number of array elements for int for float.  For strings it
   is the maximum number of characters that space has been allocated for.
   (The actual length can be smaller since the strings are null terminated.)
*/

long daVarRegister(int flag, daVarStruct *args);
long daVarLookup(char *name, daVarStruct *result);
long daVarList(char *pattern, char ***listp, int *count);

typedef int DAINT;
typedef float DAFLOAT;
typedef double DADOUBLE;


#ifndef S_SUCCESS
#define S_SUCCESS 0
#define S_FAILURE -1
#endif

#define S_DAVAR_REPLACED -2
#define S_DAVAR_UNKNOWN -3
#define S_DAVAR_UNKATTR -4 /* Unknown attribute */
#define S_DAVAR_TOOMANY -5 /* Attempted to write too many values to array */
#define S_DAVAR_ILLCONV -6 /* Illegal type conversion of write */
#define S_DAVAR_NOEOL -10 /* Line in a block doesn't have a newline */
#define S_DAVAR_NOINDEX -101 /* Status return from thGetIndex */

#define floatToLong(x) (long) ((x)>0.0 ? (x)+0.5 : (x)-0.5)
#endif

#define STDERR stdout

#ifndef FALSE
#define FALSE (1==0)
#endif
#ifndef TRUE
#define TRUE (1==1)
#endif
#endif
