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
* Revision 1.2  1994/06/17 02:44:38  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:15:21  cdaq
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
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_event_info.cmn'
*
      logical HEXIST	!CERNLIB function
*
      integer m
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.h_Ntuple_exists) RETURN       !nothing to do
*
************************************************
      m= 0
*  
      m= m+1
      h_Ntuple_contents(m)= HSP	! Lab momentum of chosen track in GeV/c
      m= m+1
      h_Ntuple_contents(m)= HSENERGY! Lab total energy of chosen track in GeV
      m= m+1
      h_Ntuple_contents(m)= HSDELTA	! Spectrometer delta of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTHETA	! Lab Scattering angle in radians
      m= m+1
      h_Ntuple_contents(m)= HSPHI	! Lab Azymuthal angle in radians
      m= m+1
      h_Ntuple_contents(m)= HSMINV	! Invariant Mass of remaing hadronic system
      m= m+1
      h_Ntuple_contents(m)= HSZBEAM! Lab Z coordinate of intersection of beam
                                ! track with spectrometer ray
      m= m+1
      h_Ntuple_contents(m)= HSDEDX1	! DEDX of chosen track in 1st scin plane
      m= m+1
      h_Ntuple_contents(m)= HSDEDX2	! DEDX of chosen track in 2nd scin plane
      m= m+1
      h_Ntuple_contents(m)= HSDEDX3	! DEDX of chosen track in 3rd scin plane
      m= m+1
      h_Ntuple_contents(m)= HSDEDX4	! DEDX of chosen track in 4th scin plane
      m= m+1
      h_Ntuple_contents(m)= HSBETA	! BETA of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTRACK_ET	! Total shower energy of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTRACK_PRESHOWER_E	! preshower of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTIME_AT_FP
      m= m+1
      h_Ntuple_contents(m)= HSX_FP		! X focal plane position 
      m= m+1
      h_Ntuple_contents(m)= HSY_FP
      m= m+1
      h_Ntuple_contents(m)= HSXP_FP
      m= m+1
      h_Ntuple_contents(m)= HSYP_FP
      m= m+1
      h_Ntuple_contents(m)= HSCHI2PERDEG	! CHI2 per degree of freedom of chosen track.
      m= m+1
      h_Ntuple_contents(m)= HSX_TAR
      m= m+1
      h_Ntuple_contents(m)= HSY_TAR
      m= m+1
      h_Ntuple_contents(m)= HSXP_TAR
      m= m+1
      h_Ntuple_contents(m)= HSYP_TAR
*
      m= m+1
      h_Ntuple_contents(m)= float(HSNUM_FPTRACK)! Index of focal plane track chosen
      m= m+1
      h_Ntuple_contents(m)= float(HSNUM_TARTRACK)! Index of target track chosen
      m= m+1
      h_Ntuple_contents(m)= float(HSID_LUND)! LUND particle ID code -- not yet filled
      m= m+1
      h_Ntuple_contents(m)= float(HSNFREE_FP)
*
      m= m+1
      h_Ntuple_contents(m)= float(gen_event_ID_number)
*
************************************************
*
*
      ABORT= .NOT.HEXIST(h_Ntuple_ID)
      IF(ABORT) THEN
        call G_build_note(':Ntuple ID#$ does not exist',
     &                        '$',h_Ntuple_ID,' ',0.,' ',err)
        call G_add_path(here,err)
      ELSE
        call HFN(h_Ntuple_ID,h_Ntuple_contents)
      ENDIF
*
      RETURN
      END
