      subroutine s_register_variables(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for the SOS
*
*     Purpose : Register all variables that are to be used by CTP, that are
*     connected with the SOS.  This includes externally configured
*     parameters/contants, event data that can be a histogram source, and
*     possible test results and scalers.
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 9-Feb-1994  Stephen A. Wood
*     $Log$
*     Revision 1.2  1994/02/22 18:58:00  cdaq
*     (SAW) Make a call to h_register_param
*
* Revision 1.1  1994/02/11  04:18:56  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*20 here
      parameter (here='s_register_variables')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call s_register_param(ABORT,err)          ! TRACKING ROUTINE
      if(ABORT) then
         g_add_path(here,err)
         return
      endif
*
*     The following variables are all fortran PARAMETERS.  It may not be
*     wise to register them.  (External applications can still get the
*     array sizes with RPC 
*     calls.)
*
      ierr = regparmint('SMAX_DC_HITS',SMAX_DC_HITS
     $     ,'SOS drift chamber max. # of hits')
      if(ierr.ne.0) err = 'unable to register "SMAX_DC_HITS"'
      ABORT = ierr.ne.0 .or. ABORT
*
      ierr = regparmint('SNUM_DC_PLANES',SNUM_DC_PLANES
     $     ,'SOS drift chamber number of planes')
*
      ierr = regparmint('SMAX_SCIN_HITS',SMAX_SCIN_HITS
     $     ,'SOS scintillator max. # of hits')
*
      ierr = regparmint('SNUM_SCIN_PLANES',SNUM_SCIN_PLANES
     $     ,'SOS scintillator number of planes')
*
      ierr = regparmint('SMAX_CAL_BLOCKS',SMAX_CAL_BLOCKS
     $     ,'SOS calorimeter max. # of hits')
*
      ierr = regparmint('SMAX_CER_HITS',SMAX_CER_HITS
     $     ,'SOS cerenkov max. # of hits')
*
      ierr = regparmint('SNTRACKS_MAX',SNTRACKS_MAX
     $     ,'SOS max. # of tracks')
*
      ierr = regparmint('SNTRACKHITS_MAX',SNTRACKHITS_MAX
     $     ,'SOS max. # of track hits')
*
      if(ierr.ne.0) err='unable to register "HNTRACKHITS_MAX"'

      IF(ABORT) THEN
         call G_add_path(here,err)
      ENDIF
*
      return
      end



