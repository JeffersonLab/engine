      subroutine g_scaler(ABORT,err)
*
*     Purpose: program to copy scaler values to histogram
*       for software scalers (from tests) needs to have 
*       t20.test file to fill the array
*
* $Log$
* Revision 1.1  1998/12/01 20:57:58  saw
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*12 here
      parameter (here= ' ')
*
      logical ABORT
      character*(*) err
*
c      include 't20_data_structures.cmn'
c      include 't20_tracking.cmn'
c      include 't20_geometry.cmn'
c      include 't20_track_histid.cmn'
c      include 't20_bypass_switches.cmn'	
      include 'gen_misc.cmn'

      integer ichan
      real*4  rtdc, rinc
**********************************************************
c note: currently: the old scaler value is first subtracted from histogram,
c       and then the new scaler value is added; alternately (better) the
c       histogram could be cleared first and then updated with current value
c
      do ichan = 1,g_maxscal_h
        rtdc = float(ichan)

        rinc = g_scaler_h1(ichan)-g_scaler_h_old1(ichan)
        call hf1(g_scal_his1,rtdc,rinc)
	g_scaler_h_old1(ichan) = g_scaler_h1(ichan)

        rinc = g_scaler_h2(ichan)-g_scaler_h_old2(ichan)
        call hf1(g_scal_his2,rtdc,rinc)
	g_scaler_h_old2(ichan) = g_scaler_h2(ichan)

        rinc = g_scaler_h3(ichan)-g_scaler_h_old3(ichan)
        call hf1(g_scal_his3,rtdc,rinc)
	g_scaler_h_old3(ichan) = g_scaler_h3(ichan)

        rinc = g_scaler_h4(ichan)-g_scaler_h_old4(ichan)
        call hf1(g_scal_his4,rtdc,rinc)
	g_scaler_h_old4(ichan) = g_scaler_h4(ichan)

        rinc = g_scaler_h5(ichan)-g_scaler_h_old5(ichan)
        call hf1(g_scal_his5,rtdc,rinc)
	g_scaler_h_old5(ichan) = g_scaler_h5(ichan)

        rinc = g_scaler_h6(ichan)-g_scaler_h_old6(ichan)
        call hf1(g_scal_his6,rtdc,rinc)
	g_scaler_h_old6(ichan) = g_scaler_h6(ichan)

        rinc = g_scaler_h7(ichan)-g_scaler_h_old7(ichan)
        call hf1(g_scal_his7,rtdc,rinc)
	g_scaler_h_old7(ichan) = g_scaler_h7(ichan)

        rinc = g_scaler_h8(ichan)-g_scaler_h_old8(ichan)
        call hf1(g_scal_his8,rtdc,rinc)
	g_scaler_h_old8(ichan) = g_scaler_h8(ichan)

      enddo
**********************************************************
c
      RETURN
      END
*********
*     Local Variables:
*     mode: fortran
*     fortran-if-indent: 2
*     fortran-do-indent: 2
*     End:

