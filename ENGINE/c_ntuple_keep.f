      subroutine c_Ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the COIN Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
* $Log$
* Revision 1.1  1994/04/12 16:12:33  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='c_Ntuple_keep')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'c_ntuple.cmn'
*
      character*80 directory,name,msg
      integer m
      real rx,ry
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.c_Ntuple_exists) RETURN       !nothing to do
*
      m= m+1
      rx= rx+ m
      ry= ry+ m/100.
      c_Ntuple_contents(1)= rx        !dummies for test
      c_Ntuple_contents(2)= ry        !could be filled elsewhere
*
      call HFN(c_Ntuple_ID,c_Ntuple_contents)
*
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      RETURN
      END      
