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
* Revision 1.4  1995/09/01 15:45:21  cdaq
* (JRA) Add spectrometer kinematic vars to ntuple
*
* Revision 1.3  1995/05/22  20:50:43  cdaq
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
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_event_info.cmn'
      include 'hms_scin_parms.cmn'
*
      logical HEXIST    !CERNLIB function
*
      character*80 directory,name,msg
      integer m
      real*8 coin_time,ss_time,hs_time
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.c_Ntuple_exists) RETURN       !nothing to do
*
**********begin insert description of contents of COIN tuple ******
c      coin_time = (hmisc_dec_data(1,10) -2576)/10. ! ???

      hs_time= hstart_time -120
      ss_time= sstart_time -50
      m= 0
      m= m+1
      c_Ntuple_contents(m)= HSX_FP         ! beam X rastor
      m= m+1
      c_Ntuple_contents(m)= HSY_FP         ! beam Y rastor
      m= m+1
      c_Ntuple_contents(m)= HSXP_FP  ! Missing mass of undetected hadron system
      m= m+1
      c_Ntuple_contents(m)= HSYP_FP   ! Magnitude of missing momentum 
      m= m+1
      c_Ntuple_contents(m)= HSX_TAR  ! X component of missing momentum
      m= m+1
      c_Ntuple_contents(m)= HSY_TAR  ! Y component of missing momentum
      m= m+1
      c_Ntuple_contents(m)= HSXP_TAR  ! Z component of missing momentum
      m= m+1
      c_Ntuple_contents(m)= HSYP_TAR ! Corrected Coincidence time
      m= m+1
      c_Ntuple_contents(m)= HS_TIME
      m=m+1
      c_Ntuple_contents(m)= SSX_FP ! Corrected Coincidence time
      m= m+1
      c_Ntuple_contents(m)= SSY_FP ! Corrected Coincidence time
      m= m+1
      c_Ntuple_contents(m)= SSXP_FP ! Corrected Coincidence time
      m= m+1
      c_Ntuple_contents(m)= SSYP_FP ! Corrected Coincidence time
      m= m+1
      c_Ntuple_contents(m)= SSX_TAR ! Corrected Coincidence time
      m= m+1
      c_Ntuple_contents(m)= SSY_TAR ! Corrected Coincidence time
      m= m+1
      c_Ntuple_contents(m)= SSXP_TAR ! Corrected Coincidence time
      m= m+1
      c_Ntuple_contents(m)= SSYP_TAR ! Corrected Coincidence time
      m= m+1
      c_Ntuple_contents(m)= ss_time
      m=m+1
      c_Ntuple_contents(m)= COIN_TIME ! Corrected Coincidence time
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
