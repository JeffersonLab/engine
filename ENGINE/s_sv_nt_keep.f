      subroutine s_sv_Nt_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the SOS Sieve slit Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 1-Nov-1994  
* $Log: s_sv_nt_keep.f,v $
* Revision 1.3  1996/11/05 21:44:04  saw
* (DD) Add gas cerenkov to ntuple
*
* Revision 1.2  1996/09/04 15:19:37  saw
* (JRA) Modify ntuple contents
*
* Revision 1.1  1995/08/11 16:23:12  cdaq
* Initial revision
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='s_sv_nt_keep')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 's_sieve_ntuple.cmn'
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'sos_data_structures.cmn'
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
      IF(.NOT.s_sieve_Ntuple_exists) RETURN !nothing to do
*
************************************************
      m= 0
*  
      m= m+1
      s_sieve_Ntuple_contents(m)= SSX_FP ! X focal plane position 
      m= m+1
      s_sieve_Ntuple_contents(m)= SSY_FP
      m= m+1
      s_sieve_Ntuple_contents(m)= SSXP_FP
      m= m+1
      s_sieve_Ntuple_contents(m)= SSYP_FP
      m= m+1
      s_sieve_Ntuple_contents(m)= SSDELTA
      m= m+1
      s_sieve_Ntuple_contents(m)= SSX_TAR
      m= m+1
      s_sieve_Ntuple_contents(m)= SSY_TAR
      m= m+1
      s_sieve_Ntuple_contents(m)= SSXP_TAR
      m= m+1
      s_sieve_Ntuple_contents(m)= SSYP_TAR
      m=m+1
      s_sieve_Ntuple_contents(m)= sstrack_et
      m= m+1
      s_sieve_Ntuple_contents(m)= scer_npe_sum
      m= m+1
      s_sieve_Ntuple_contents(m)= float(gen_event_ID_number)

*
************************************************
*
*
      ABORT= .NOT.HEXIST(s_sieve_Ntuple_ID)
      IF(ABORT) THEN
        call G_build_note(':Ntuple ID#$ does not exist',
     &       '$',s_sieve_Ntuple_ID,' ',0.,' ',err)
        call G_add_path(here,err)
      ELSE
        call HFN(s_sieve_Ntuple_ID,s_sieve_Ntuple_contents)
      ENDIF
*
      RETURN
      END
