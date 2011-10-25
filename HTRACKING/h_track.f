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
* Revision 1.5.26.3  2011/10/25 16:12:30  jones
* Elminate unneed variables and include files
*
* Revision 1.5.26.2  2011/10/25 16:07:07  jones
* back to original h_track.f wihtout trying to do only one stub.
*
* Revision 1.5.26.1  2009/09/15 20:37:39  jones
* Add code to track with single stub
*
* Revision 1.5  1996/09/04 13:37:02  saw
* (JRA) Initialize hstubmin variables
*
* Revision 1.4  1995/10/11 12:19:50  cdaq
* (JRA) Only call tracking routines when it is warranted
*
* Revision 1.3  1995/05/22 19:39:30  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/04/13  17:07:57  cdaq
* (DFG) Added histograming call (h_fill_dc_fp_hist)
*
* Revision 1.1  1994/02/19  06:20:31  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*7 here
      parameter (here= 'H_TRACK')
*
      logical ABORT
      character*(*) err
      integer*4 ierr
      character*5  line_err
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'

*
*--------------------------------------------------------
*
*
      ABORT = .false.
      err = ' '

      if (hdc_tot_hits.ne.0) then
        call H_PATTERN_RECOGNITION(ABORT,err)
        if(ABORT) then
          call G_add_path(here,err)
          return
        endif
     
*
        if (hnspace_points_tot.ne.0) then
          call H_LEFT_RIGHT(ABORT,err)
          if(ABORT) then
            call G_add_path(here,err)
            return
          endif
*
          hstubminx = 999999.
          hstubminy = 999999.
          hstubminxp = 999999.
          hstubminyp = 999999.
          call H_LINK_STUBS(ABORT,err)
          if(ABORT) then
            call G_add_path(here,err)
            return
          endif
*
          if (hntracks_fp.ne.0) then
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
*     histogram focal plane tracks
*
            call h_fill_dc_fp_hist(ABORT,err)
            if(ABORT) then
              call g_add_path(here,err)
              return
            endif
*
          endif                         !(hntracks_fp.ne.0)
        endif                           !(hnspace_points_tot.ne.0)
      endif                             !(hdc_tot_hits.ne.0)


         return
         end




