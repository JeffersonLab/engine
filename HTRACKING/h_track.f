       SUBROUTINE H_TRACK(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods :  Finds and fits tracks in HMS focal plane 
*-
*-      Required Input BANKS     HMS_DECODED_DC
*-
*-      Output BANKS             HMS_FOCAL_PLANE
*-                               HMS_DECODED_DC hit coordinates
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 19-JAN-1994   D. F. Geesaman
* $Log$
* Revision 1.1  1994/02/19 06:20:31  cdaq
* Initial revision
*
*-
*-
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'H_TRACK')
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
      call H_PATTERN_RECOGNITION(ABORT,err)
      if(ABORT) then
           call G_add_path(here,err)
           return
      endif
     
*
      call H_LEFT_RIGHT(ABORT,err)
      if(ABORT) then
           call G_add_path(here,err)
           return
      endif
*
      call H_LINK_STUBS(ABORT,err)
      if(ABORT) then
           call G_add_path(here,err)
           return
      endif
*
      call H_TRACK_FIT(ABORT,err,ierr)
      if(ABORT) then
           call G_add_path(here,err)
           return
      endif
*     Check for internal error in H_TRACK_FIT
      if(ierr.ne.0) then
           line_err=' '
           call CSETDI(ierr,line_err,1,5)
           err='ERROR IN H_TRACK_FIT' // line_err
           call G_add_path(here,err)
           call G_LOG_MESSAGE(err)
      endif                    
      return
      end

