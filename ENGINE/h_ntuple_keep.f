      subroutine h_Ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the HMS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
* $Log$
* Revision 1.1  1994/04/12 16:15:21  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='h_Ntuple_keep')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_ntuple.cmn'
*
      character*80 directory,name,msg
      integer m
      real rx,ry
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.h_Ntuple_exists) RETURN       !nothing to do
*
      m= m+1
      rx= rx+ m + 20
      ry= ry+ m/100. + 20
      h_Ntuple_contents(1)= rx        !dummies for test
      h_Ntuple_contents(2)= ry        !could be filled elsewhere
*
      call HFN(h_Ntuple_ID,h_Ntuple_contents)
*
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      RETURN
      END      
