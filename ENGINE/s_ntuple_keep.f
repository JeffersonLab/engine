      subroutine s_Ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the SOS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
* $Log$
* Revision 1.1  1994/04/12 16:16:28  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='s_Ntuple_keep')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 's_ntuple.cmn'
*
      character*80 directory,name,msg
      integer m
      real rx,ry
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.s_Ntuple_exists) RETURN       !nothing to do
*
      m= m+1
      rx= rx+ 10 + m
      ry= ry+ 10 + m/100.
      s_Ntuple_contents(1)= rx        !dummies for test
      s_Ntuple_contents(2)= ry        !could be filled elsewhere
*
      call HFN(s_Ntuple_ID,s_Ntuple_contents)
*
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      RETURN
      END      
