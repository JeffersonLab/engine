      subroutine h_register_variables(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for the HMS
*
*     Purpose : Register all variables that are to be used by CTP, that are
*     connected with the HMS.  This includes externally configured
*     parameters/contants, event data that can be a histogram source, and
*     possible test results and scalers.
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 9-Feb-1994  Stephen A. Wood
*     $Log$
*     Revision 1.3  1994/02/22 18:56:45  cdaq
*     (SAW) Make a call to h_register_param
*
* Revision 1.2  1994/02/11  18:36:17  cdaq
* Split off CTP variables registration from initialize routines
*
* Revision 1.1  1994/02/11  04:18:24  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*20 here
      parameter (here='h_register_variables')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
*
      integer ierr
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call h_register_param(ABORT,err)          ! TRACKING ROUTINE
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
      ierr= regparmint('HMAX_DC_HITS',HMAX_DC_HITS
     $     ,'HMS drift chamber max. # hits')
      if(ierr.ne.0) err = 'unable to register "HMAX_DC_HITS"'
      ABORT = ierr.ne.0 .or. ABORT
*
*     Don't do error checking on the rest of the calls, just the last one.
*     If reg calls are failing, you will know it, and the last one should
*     fail too.
*
      ierr = regparmint('HNUM_DC_PLANES',HNUM_DC_PLANES
     $     ,'HMS drift chamber number of planes')
*
      ierr = regparmint('HMAX_SCIN_HITS',HMAX_SCIN_HITS
     $     ,'HMS scintillator max. # hits')
*
      ierr = regparmint('HNUM_SCIN_PLANES',HNUM_SCIN_PLANES
     $     ,'HMS scintillator chamber number of planes')
*
      ierr = regparmint('HMAX_CAL_BLOCKS',HMAX_CAL_BLOCKS
     $     ,'HMS calorimeter number of blocks')
*
      ierr = regparmint('HMAX_CER_HITS',HMAX_CER_HITS
     $     ,'HMS cerenkov chamber max. # of hits')
*
      ierr = regparmint('HNTRACKS_MAX',HNTRACKS_MAX
     $     ,'HMS max. # of tracks')
*
      ierr = regparmint('HNTRACKHITS_MAX',HNTRACKHITS_MAX
     $     ,'HMS max. # of track hits')
      if(ierr.ne.0) err='unable to register "HNTRACKHITS_MAX"'

      IF(ABORT) THEN
         call G_add_path(here,err)
      ENDIF
*
      return
      end




