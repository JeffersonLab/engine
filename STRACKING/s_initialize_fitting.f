      subroutine S_INITIALIZE_FITTING
*     This subroutine does the MINUIT initialization for track fitting
*
*     d.f. geesaman               8 Sept 1993
* $Log$
* Revision 1.1  1994/02/21 16:14:30  cdaq
* Initial revision
*
*
      implicit none
      external S_FCNCHISQ
      real*8 S_FCNCHISQ
      include "gen_data_structures.cmn"
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
