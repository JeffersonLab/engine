      subroutine h_Ntuple_register(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for the HMS Ntuples 
*
*     Purpose : Register output filename for HMS Ntuple; temporary
*     implementation to be superceeded by CTP Ntuples
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, HU: added Ntuples
* $Log$
* Revision 1.1  1994/04/12 16:15:30  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='h_Ntuple_register')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      integer ierr
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      ierr = regparmstring('HMS_Ntuple',h_Ntuple_filename,0)
      ABORT = ierr.ne.0
      IF(ABORT) THEN
        err = ':unable to register "h_Ntuple_filename"'
        call G_add_path(here,err)
      ENDIF
*
      return
      end
