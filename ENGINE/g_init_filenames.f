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
*-    Revision 1.10  1995/05/11 19:01:29  cdaq
*-    (SAW) Check 0 in g_config_filename in case user doesn't update engine.f
*-
* Revision 1.9  1995/05/11  16:16:11  cdaq
* (SAW) Don't get g_config_filename from environment if it is already set
*       from the command line and allow %d run number substitution in it.
*
* Revision 1.8  1995/04/01  19:46:13  cdaq
* (SAW) One report file for each of g, h, s, c instead of a single report file
*
* Revision 1.7  1994/10/19  19:51:55  cdaq
* (SAW) Add g_label variable for labels on reports
*
* Revision 1.6  1994/06/22  20:57:14  cdaq
* (SAW) Add more variables for reports
*
* Revision 1.5  1994/06/16  03:47:57  cdaq
* (SAW) Blank out filenames for reports
*
* Revision 1.4  1994/03/24  22:02:21  cdaq
* Reorganize for online compatibility
*
* Revision 1.3  1994/02/11  18:34:34  cdaq
* Split off CTP variables registration from initialize routines
*
* Revision 1.2  1994/02/03  18:12:17  cdaq
* Use CTP parameter block to get the filenames
*
* Revision 1.1  1994/02/02  20:08:15  cdaq
* Initial revision
*
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
      include 'hms_filenames.cmn'
      include 'sos_filenames.cmn'
      include 'coin_filenames.cmn'
      include 'gen_routines.dec'
      include 'gen_run_info.cmn'
*
      integer ierr
      character*132 file
*
*-all crucial setup information here; failure is fatal
*
      g_hist_rebook = .true.
      g_test_rebook = .true.
      g_parm_rebook = .true.
      g_report_rebook = .true.
      g_ctp_parm_filename = ' '
      g_ctp_test_filename = ' '
      g_ctp_hist_filename = ' '
      g_data_source_filename= ' '
      g_alias_filename = ' '
      g_histout_filename = ' '
      g_decode_map_filename = ' '
*
      s_recon_coeff_filename = ' '
      h_recon_coeff_filename = ' '
*
      h_report_template_filename = ' '
      s_report_template_filename = ' '
      g_report_template_filename = ' '
      c_report_template_filename = ' '
*
      h_report_output_filename = ' '
      s_report_output_filename = ' '
      g_report_output_filename = ' '
      c_report_output_filename = ' '
*
      h_report_blockname = ' '
      s_report_blockname = ' '
      g_report_blockname = ' '
      c_report_blockname = ' '
*
      g_label = ' '                     ! Label for reports etc.
*
      if(g_config_filename.eq.' '.or.
     $     ichar(g_config_filename(1:1)).eq.0) ! Only if not already set
     $     call getenv(env_var,g_config_filename)
*
      ABORT= g_config_filename.EQ.' '
      IF(ABORT) THEN
        error= here//':blank environmental variable '//env_var
        RETURN
      ENDIF
*
      file = g_config_filename
      call g_sub_run_number(file,gen_run_number)
      ierr =  thload(file)         ! Config file is now a CTP parm file
      if(ierr.ne.0) goto 999
      ierr = thbook()
      if(ierr.eq.0) then
         g_config_loaded = .true.
      else
         g_config_loaded = .false.
      endif

      ABORT= .NOT.g_config_loaded
      IF(ABORT) THEN
        error= ':opened OK, but thload command failed from "'//file//'"'
        call G_add_path(here,error)
      ELSE
        error= ' '
      ENDIF
*      IF(echo) type *,' ......exiting '//here//'........'
      return
*
999   g_config_loaded= .FALSE.
      ABORT= .NOT.g_config_loaded
      error= ':unable to open file "'//file//'"'
      call G_add_path(here,error)
      return
*
      end
