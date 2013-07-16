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
* $Log: s_track.f,v $
* Revision 1.5  1996/09/04 20:19:45  saw
* (JRA) Initialize sstubmin variables
*
* Revision 1.4  1995/10/11 12:31:21  cdaq
* (JRA) Only call tracking routines when it is warranted
*
* Revision 1.3  1995/05/22 19:46:00  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/04/13  18:51:49  cdaq
* (DFG) Add call to s_fill_dc_fp_hist
*
* Revision 1.1  1994/02/21  16:42:12  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*7 here
      parameter (here= 'S_TRACK')
*
      logical ABORT
      character*(*) err
      integer*4 ierr
      character*5  line_err
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'sos_tracking.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'

*--------------------------------------------------------
*
*
      if (sdc_tot_hits.ne.0) then
        call S_PATTERN_RECOGNITION(ABORT,err)
        if(ABORT) then
          call G_add_path(here,err)
          return
        endif
     
*
        if (snspace_points_tot.ne.0) then
          call S_LEFT_RIGHT(ABORT,err)
          if(ABORT) then
            call G_add_path(here,err)
            return
          endif
*
          sstubminx = 999999.
          sstubminy = 999999.
          sstubminxp = 999999.
          sstubminyp = 999999.
          call S_LINK_STUBS(ABORT,err)
          if(ABORT) then
            call G_add_path(here,err)
            return
          endif
*
          if (sntracks_fp.ne.0) then
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
*     histogram focal plane tracks
*
            call s_fill_dc_fp_hist(ABORT,err)
            if(ABORT) then
              call g_add_path(here,err)
              return
            endif
*
          endif                         !(sntracks_fp.ne.0)
        endif                           !(snspace_points_tot.ne.0)
      endif                             !(sdc_tot_hits.ne.0)
      
      return
      end
