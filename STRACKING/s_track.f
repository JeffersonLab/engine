       SUBROUTINE S_TRACK(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods :  Finds and fits tracks in SOS focal plane 
*-
*-      Required Input BANKS     SOS_DECODED_DC
*-
*-      Output BANKS             SOS_FOCAL_PLANE
*-                               SOS_DECODED_DC hit coordinates
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 19-JAN-1994   D. F. Geesaman
* $Log$
* Revision 1.1  1994/02/21 16:42:12  cdaq
* Initial revision
*
*-
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'S_TRACK')
*
       logical ABORT
       character*(*) err
       integer*4 ierr
       character*5  line_err
*
       INCLUDE 'gen_data_structures.cmn'
       INCLUDE 'gen_constants.par'
       INCLUDE 'gen_units.par'
*
*--------------------------------------------------------
*
*
      call S_PATTERN_RECOGNITION(ABORT,err)
      if(ABORT) then
           call G_add_path(here,err)
           return
      endif
     
*
      call S_LEFT_RIGHT(ABORT,err)
      if(ABORT) then
           call G_add_path(here,err)
           return
      endif
*
      call S_LINK_STUBS(ABORT,err)
      if(ABORT) then
           call G_add_path(here,err)
           return
      endif
*
      call S_TRACK_FIT(ABORT,err,ierr)
      if(ABORT) then
           call G_add_path(here,err)
           return
      endif
*     Check for internal error in S_TRACK_FIT
      if(ierr.ne.0) then
           line_err=' '
           call CSETDI(ierr,line_err,1,5)
           err='MUNUIT ERROR IN S_TRACK_FIT' // line_err
           call G_add_path(here,err)
           call G_LOG_MESSAGE(err)
      endif                    
      return
      end

