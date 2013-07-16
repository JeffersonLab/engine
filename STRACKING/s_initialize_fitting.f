      subroutine s_initialize_fitting
*     This subroutine does the MINUIT initialization for track fitting
*
*     d.f. geesaman               8 Sept 1993
* $Log: s_initialize_fitting.f,v $
* Revision 1.3  1996/09/05 20:09:06  saw
* (JRA) Cosmetic
*
* Revision 1.2  1995/05/22 19:45:41  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/02/21  16:14:30  cdaq
* Initial revision
*
*
      implicit none
      external S_FCNCHISQ
      real*8 S_FCNCHISQ
      include "sos_data_structures.cmn"
      include "sos_tracking.cmn"
*     local variables
      integer*4 ierr,dummy
      integer*4 mlunin,mlunsave
      real*8 arglis(10)
      parameter(mlunin=5)
      parameter(mlunsave=10)
*     initialize MINUIT lun settings
      call MNINIT(mlunin,sluno,mlunsave)
*     set print to -1 (no output)
      arglis(1)=-1
      call MNEXCM(S_FCNCHISQ,'SET PRI',arglis,1,ierr,dummy)      
      call MNSETI( ' Track fitting in SOS Spectrometer')
      return
      end
