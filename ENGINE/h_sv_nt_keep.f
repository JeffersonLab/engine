      subroutine h_sv_Nt_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the HMS Sieve slit Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 1-Nov-1994  
* $Log: h_sv_nt_keep.f,v $
* Revision 1.5  1996/11/05 21:42:28  saw
* (DD) Add gas cerenkov to ntuple
*
* Revision 1.4  1996/09/04 15:15:38  saw
* (JRA) Change ntuple contents
*
* Revision 1.3  1996/04/29 19:49:35  saw
* (JRA) Add HCAL_ET
*
* Revision 1.2  1995/05/22 20:50:47  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1995/01/27  20:05:23  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='h_sv_nt_keep')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_sieve_ntuple.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_event_info.cmn'
*
      logical HEXIST                    !CERNLIB function
*
      integer m
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.h_sieve_Ntuple_exists) RETURN !nothing to do
*
************************************************
      m= 0
*  
      m= m+1
      h_sieve_Ntuple_contents(m)= HSX_FP ! X focal plane position 
      m= m+1
      h_sieve_Ntuple_contents(m)= HSY_FP
      m= m+1
      h_sieve_Ntuple_contents(m)= HSXP_FP
      m= m+1
      h_sieve_Ntuple_contents(m)= HSYP_FP
      m= m+1
      h_sieve_Ntuple_contents(m)= HSDELTA
      m= m+1
      h_sieve_Ntuple_contents(m)= HSX_TAR
      m= m+1
      h_sieve_Ntuple_contents(m)= HSY_TAR
      m= m+1
      h_sieve_Ntuple_contents(m)= HSXP_TAR
      m= m+1
      h_sieve_Ntuple_contents(m)= HSYP_TAR
      m= m+1
      h_sieve_Ntuple_contents(m)= HSTRACK_ET
      m= m+1
      h_sieve_Ntuple_contents(m)= HCER_NPE_SUM
      m= m+1
      h_sieve_Ntuple_contents(m)= float(gen_event_ID_number)
*
************************************************
*
*
      ABORT= .NOT.HEXIST(h_sieve_Ntuple_ID)
      IF(ABORT) THEN
        call G_build_note(':Ntuple ID#$ does not exist',
     &       '$',h_sieve_Ntuple_ID,' ',0.,' ',err)
        call G_add_path(here,err)
      ELSE
        call HFN(h_sieve_Ntuple_ID,h_sieve_Ntuple_contents)
      ENDIF
*
      RETURN
      END
