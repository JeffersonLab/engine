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
* Revision 1.2  1994/06/17 02:42:33  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:16:28  cdaq
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
      INCLUDE 'gen_data_structures.cmn'
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
      IF(.NOT.s_Ntuple_exists) RETURN       !nothing to do
*
      m= 0
*  
***********************************************************
      m= m+1
      s_Ntuple_contents(m)= SSP	! Lab momentum of chosen track in GeV/c
      m= m+1
      s_Ntuple_contents(m)= SSENERGY! Lab total energy of chosen track in GeV
      m= m+1
      s_Ntuple_contents(m)= SSDELTA	! Spectrometer delta of chosen track
      m= m+1
      s_Ntuple_contents(m)= SSTHETA	! Lab Scattering angle in radians
      m= m+1
      s_Ntuple_contents(m)= SSPHI	! Lab Azymuthal angle in radians
      m= m+1
      s_Ntuple_contents(m)= SSMINV	! Invariant Mass of remaing hadronic system
      m= m+1
      s_Ntuple_contents(m)= SSZBEAM! Lab Z coordinate of intersection of beam
                                ! track with spectrometer ray
      m= m+1
      s_Ntuple_contents(m)= SSDEDX1	! DEDX of chosen track in 1st scin plane
      m= m+1
      s_Ntuple_contents(m)= SSDEDX2	! DEDX of chosen track in 2nd scin plane
      m= m+1
      s_Ntuple_contents(m)= SSDEDX3	! DEDX of chosen track in 3rd scin plane
      m= m+1
      s_Ntuple_contents(m)= SSDEDX4	! DEDX of chosen track in 4th scin plane
      m= m+1
      s_Ntuple_contents(m)= SSBETA	! BETA of chosen track
      m= m+1
      s_Ntuple_contents(m)= SSTRACK_ET	! Total shower energy of chosen track
      m= m+1
      s_Ntuple_contents(m)= SSTRACK_PRESHOWER_E	! preshower of chosen track
      m= m+1
      s_Ntuple_contents(m)= SSTIME_AT_FP
      m= m+1
      s_Ntuple_contents(m)= SSX_FP		! X focal plane position 
      m= m+1
      s_Ntuple_contents(m)= SSY_FP
      m= m+1
      s_Ntuple_contents(m)= SSXP_FP
      m= m+1
      s_Ntuple_contents(m)= SSYP_FP
      m= m+1
      s_Ntuple_contents(m)= SSCHI2PERDEG	! CHI2 per degree of freedom of chosen track.
      m= m+1
      s_Ntuple_contents(m)= SSX_TAR
      m= m+1
      s_Ntuple_contents(m)= SSY_TAR
      m= m+1
      s_Ntuple_contents(m)= SSXP_TAR
      m= m+1
      s_Ntuple_contents(m)= SSYP_TAR
*
      m= m+1
      s_Ntuple_contents(m)= float(SSNUM_FPTRACK)! Index of focal plane track chosen
      m= m+1
      s_Ntuple_contents(m)= float(SSNUM_TARTRACK)! Index of target track chosen
      m= m+1
      s_Ntuple_contents(m)= float(SSID_LUND)! LUND particle ID code -- not yet filled
      m= m+1
      s_Ntuple_contents(m)= float(SSNFREE_FP)
*
      m= m+1
      s_Ntuple_contents(m)= float(gen_event_ID_number)
*
********************************************************
*
      ABORT= .NOT.HEXIST(s_Ntuple_ID)
      IF(ABORT) THEN
        call G_build_note(':Ntuple ID#$ does not exist',
     &                        '$',s_Ntuple_ID,' ',0.,' ',err)
        call G_add_path(here,err)
      ELSE
        call HFN(s_Ntuple_ID,s_Ntuple_contents)
      ENDIF
*
      RETURN
      END      
