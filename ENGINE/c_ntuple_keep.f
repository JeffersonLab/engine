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
* Revision 1.3  1995/05/22 20:50:43  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/06/17  02:41:25  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:12:33  cdaq
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
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'coin_data_structures.cmn'
      INCLUDE 'gen_event_info.cmn'
*
      logical HEXIST    !CERNLIB function
*
      character*80 directory,name,msg
      integer m
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.c_Ntuple_exists) RETURN       !nothing to do
*
**********begin insert description of contents of COIN tuple ******
      m= 0
      m= m+1
      c_Ntuple_contents(m)= CXRAST         ! beam X rastor
      m= m+1
      c_Ntuple_contents(m)= CYRAST         ! beam Y rastor
      m= m+1
      c_Ntuple_contents(m)= CMISSING_MASS  ! Missing mass of undetected hadron system
      m= m+1
      c_Ntuple_contents(m)= CMISSING_MOM   ! Magnitude of missing momentum 
      m= m+1
      c_Ntuple_contents(m)= CMISSING_MOMX  ! X component of missing momentum
      m= m+1
      c_Ntuple_contents(m)= CMISSING_MOMY  ! Y component of missing momentum
      m= m+1
      c_Ntuple_contents(m)= CMISSING_MOMZ  ! Z component of missing momentum
      m= m+1
      c_Ntuple_contents(m)= CTIME_COIN_COR ! Corrected Coincidence time
      m= m+1
      c_Ntuple_contents(m)= FLOAT(gen_event_ID_number)
***********end insert description of contents of COIN tuple********
*
      ABORT= .NOT.HEXIST(c_Ntuple_ID)
      IF(ABORT) THEN
        call G_build_note(':Ntuple ID#$ does not exist',
     &                        '$',c_Ntuple_ID,' ',0.,' ',err)
        call G_add_path(here,err)
      ELSE
        call HFN(c_Ntuple_ID,c_Ntuple_contents)
      ENDIF
*
      RETURN
      END      
