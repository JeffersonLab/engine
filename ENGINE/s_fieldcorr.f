      SUBROUTINE S_fieldcorr(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : apply correction to SOS
*-   central momentum as function of momentum.
*-
*- 
*-   Output: ABORT		- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  18-Feb-1999 M Jones
*- 
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*11 here
      parameter (here= 'S_fieldcorr')
*
      logical ABORT
      character*(*) err
*
      include 'gen_data_structures.cmn'
      INCLUDE 'sos_data_structures.cmn'
*
*     local variables 
      real*8 sosp0corr
*--------------------------------------------------------
*

      if (genable_sos_fieldcorr .eq. 1999 ) then
        write(*,*) ' ******'
        write(*,*) ' Enabled SOS central momentum correction'
        write(*,*) ' Using J. Volmer parametrization'
        write(*,*) ' Before correction: central  mom = ',spcentral
        sosp0corr=0.45
        if (spcentral .gt. 0.51) sosp0corr=0.496-0.08845*spcentral
     >       -5.743e-4*exp(2.341*(spcentral**2.156))
        spcentral = spcentral*(1+sosp0corr/100.)
        write(*,*) ' After correction: central  mom = ',spcentral
        write(*,*) ' ******'
      elseif (genable_sos_fieldcorr .eq. 2003 ) then
        write(*,*) ' ******'
        write(*,*) ' Enabled SOS central momentum correction'
        write(*,*) ' Using C. Xu parametrization (2003)'
        write(*,*) ' Before correction: central  mom = ',spcentral

        sosp0corr=0.035
             if (spcentral .gt. 0.51)sosp0corr=0.10256 - 0.13242*spcentral
     >       -1.67002e-4*exp(2.33*(spcentral**2.4))
        spcentral = spcentral*(1+sosp0corr/100.)
        write(*,*) ' After correction: central  mom = ',spcentral
        write(*,*) ' ******'
      else
        write(*,*) ' ******'
        write(*,*) ' SOS central momentum correction not enabled'
        write(*,*) ' It is probably wise to enable by setting'
        write(*,*) ' genable_sos_fieldcorr = 2003 '
        write(*,*) ' to use C. Xu parametrization (2003)'
        write(*,*) ' or genable_sos_fieldcorr = 1999 '
        write(*,*) ' to use J. Volmer parametrization'
        write(*,*) ' Set parameter in sosflags.param'
        write(*,*) ' ******'
      endif
c
      ABORT= .FALSE.
      err= ' '
      RETURN
      END
