      subroutine g_init_filenames(ABORT, error, env_var)
*----------------------------------------------------------------------
*-    Purpose and Methods:
*-
*-    Read a configuration file with set of filenames and options.
*-    Much of this will be handled by CTP when a string capability is added
*-    to CTP parameter files.  Allowed keywords in config file are
*-    'hist', 'test', 'parm', 'alias', 'data', 'hbook', 'map', 'nevents', 'data'
*-
*-    This routine does the booking of hist, test, and parm files.  This
*-    booking should be moved to another file.
*-
*-    Inputs:
*-
*-    env_var     Environment variable pointing to the config file.
*-
*-    Outputs:
*-
*-    ABORT
*-    error
*-
*-    Created               Steve Wood, CEBAF
*-    Modified   3-Dec-1993 Kevin Beard, Hampton U.
*-    Modified   8-Dec-1993 Kevin Beard; rewrote parsing,added 'data' type
*-    $Log$
*-    Revision 1.2  1994/02/03 18:12:17  cdaq
*-    Use CTP parameter block to get the filenames
*-
c Revision 1.1  1994/02/02  20:08:15  cdaq
c Initial revision
c
*----------------------------------------------------------------------
      implicit none
      SAVE
*
      character*16 here
      parameter (here= 'g_init_filenames')
*
      logical ABORT
      character*(*) error
      character*(*) env_var
*
      include 'gen_filenames.cmn'
      include 'gen_routines.dec'
*
      integer ierr,flag
*
*     Register the variables that contain the filenames and other
*     configuration variables.
*
      error = ' '
      ierr = regparmstring('hist_filename',g_ctp_hist_filename,0)
      if(ierr.ne.0) error = 'unable to register "hist_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('parm_filename',g_ctp_parm_filename,0)
      if(ierr.ne.0) error = 'unable to register "parm_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('test_filename',g_ctp_test_filename,0)
      if(ierr.ne.0) error = 'unable to register "test_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('data_source_filename'
     $     ,g_data_source_filename,0)
      if(ierr.ne.0) error = 'unable to register "data_source_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('alias_filename',g_alias_filename,0)
      if(ierr.ne.0) error = 'unable to register "alias_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('histout_filename',g_histout_filename,0)
      if(ierr.ne.0) error = 'unable to register "histout_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('decode_map_filename'
     $     ,g_decode_map_filename,0)
      if(ierr.ne.0) error = 'unable to register "decode_map_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('alias_filename',g_alias_filename,0)
      if(ierr.ne.0) error = 'unable to register "alias_filename"'
      abort = ierr.ne.0
      ierr = regparmstring('max_events',g_max_events,0)
      if(ierr.ne.0) error = 'unable to register "max_events"'
      abort = ierr.ne.0
*
      if(abort) then
         call g_add_path(here,error)
         return
      endif
*
      
*
*     The variables used for string manipulation should be i*2 or
*     else strange things may happen.
*
*     Book the histograms, tests and parameters
*
      g_hist_rebook = .false.
      g_test_rebook = .false.
      g_parm_rebook = .false.
      g_alias_filename = ' '
      g_data_source_opened = .false.     !not opened yet
      g_data_source_in_hndl= 0           !none
      g_data_source_filename= ' '        !undefined
c     
      call getenv(env_var,g_config_filename)
*
      ABORT= g_config_filename.EQ.' '
      IF(ABORT) THEN
        error= here//':blank environmental variable '//env_var
        RETURN
      ENDIF
*
      ierr =  thload(g_config_filename)         ! Config file is now
      if(ierr.ne.0) goto 999
      ierr = thbook()                             ! a CTP parm file
      if(ierr.eq.0) then
*
*     Need to move this code to another routine.
*
         if(g_ctp_parm_filename.ne.' ') g_parm_rebook = .true.
         if(g_ctp_test_filename.ne.' ') g_test_rebook = .true.
         if(g_ctp_hist_filename.ne.' ') g_hist_rebook = .true.
         if(g_parm_rebook) call thload(g_ctp_parm_filename)
         if(g_test_rebook) call thload(g_ctp_test_filename)
         if(g_hist_rebook) call thload(g_ctp_hist_filename)
*
         if(g_parm_rebook .or. g_test_rebook .or. g_hist_rebook) then
            call thbook
            if(g_alias_filename.ne.' ') then
               ierr = thwhalias(g_alias_filename)
               type *,'called haliaswrite',ierr
            endif
         endif
*
         g_config_loaded = .true.
      else
         g_config_loaded = .false.
      endif
      ABORT= .NOT.g_config_loaded
      IF(ABORT) THEN
        error= ':opened OK, but thload command failed from "'//
     &                                  g_config_filename//'"'
        call G_add_path(here,error)
      ELSE
        error= ' '
      ENDIF
*      IF(echo) type *,' ......exiting '//here//'........'
      return
*
999   g_config_loaded= .FALSE.
      ABORT= .NOT.g_config_loaded
      error= ':unable to open file "'//g_config_filename//'"'
      call G_add_path(here,error)
      return
*
      end
