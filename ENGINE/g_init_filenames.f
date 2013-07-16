      subroutine g_init_filenames(ABORT, err, env_var)
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
*-    err
*-
*-    Created               Steve Wood, CEBAF
*-    Modified   3-Dec-1993 Kevin Beard, Hampton U.
*-    Modified   8-Dec-1993 Kevin Beard; rewrote parsing,added 'data' type
* $Log: g_init_filenames.f,v $
* Revision 1.18.12.1  2004/06/30 19:32:32  cdaq
* Add initialition of angle picture filenames (DJG)
*
* Revision 1.18  2003/09/05 15:44:44  jones
* Merge in online03 changes (mkj)
*
* Revision 1.17.2.1  2003/08/14 00:42:22  cdaq
* Modify to be able to write scaler rates for each read to a file (mkj)
*
* Revision 1.17  1996/11/05 21:40:59  saw
* (JRA) Add g_epics_output_filename
*
* Revision 1.16  1996/09/04 14:36:59  saw
* (JRA) Add read of command line parameters
*
* Revision 1.15  1996/04/29 19:47:11  saw
* (JRA) Add g_pedestal_output_filename
*
* Revision 1.14  1996/01/16 18:31:26  cdaq
* (JRA) Add file for tcl stats display, add files for thresholds and pedestals
*
* Revision 1.13  1995/10/09 18:37:52  cdaq
* (SAW) Move g_ctp_database call to engine.f
*
* Revision 1.12  1995/09/01 14:31:03  cdaq
* (JRA) Blank out g_ctp_kinematics_filename
*
* Revision 1.11  1995/07/27  19:35:15  cdaq
* (SAW) Add call to g_ctp_database to set ctp vars by run number
*
* Revision 1.10  1995/05/11  19:01:29  cdaq
* (SAW) Check 0 in g_config_filename in case user doesn't update engine.f
*
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
      character*(*) err
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
      g_ctp_database_filename = ' '
      g_ctp_kinematics_filename = ' '
      g_charge_scaler_filename = ' '
      g_writeout_scaler_filename = ' '
*
      s_recon_coeff_filename = ' '
      h_recon_coeff_filename = ' '
*
      h_report_template_filename = ' '
      s_report_template_filename = ' '
      g_report_template_filename = ' '
      c_report_template_filename = ' '
      g_stats_template_filename = ' '
*
      h_report_output_filename = ' '
      s_report_output_filename = ' '
      g_report_output_filename = ' '
      c_report_output_filename = ' '
      g_stats_output_filename = ' '
      g_bad_output_filename = ' '
      g_epics_output_filename = ' '
*
      h_report_blockname = ' '
      s_report_blockname = ' '
      g_report_blockname = ' '
      c_report_blockname = ' '
      g_stats_blockname = ' '
*
      h_threshold_output_filename = ' '
      s_threshold_output_filename = ' '
      g_pedestal_output_filename = ' '
      h_pedestal_output_filename = ' '
      s_pedestal_output_filename = ' '
*
      h_angle_output_filename = ' '
      s_angle_output_filename = ' '
*
      g_label = ' '                     ! Label for reports etc.
*
      if(g_config_filename.eq.' '.or.
     $     ichar(g_config_filename(1:1)).eq.0) ! Only if not already set
     $     call getenv(env_var,g_config_filename)
*
      call engine_command_line(.false.)
*
      ABORT= g_config_filename.EQ.' '
      IF(ABORT) THEN
        err= here//':blank environmental variable '//env_var
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
        err= ':opened OK, but thbook command failed from "'//file//'"'
        call G_add_path(here,err)
      ELSE
        err= ' '
      ENDIF
*
      return
*
999   g_config_loaded= .FALSE.
      ABORT= .NOT.g_config_loaded
      err= ':unable to open file "'//file//'"'
      call G_add_path(here,err)
      return
*
      end


