      subroutine s_fill_cal_hist(Abort,err)
*
*     routine to fill histograms with sos_cal varibles
*
*     Author:   J. R. Arrington
*     Date:     26 April 1995
*     Copied from:  s_fill_scin_raw_hist
*
*
* $Log$
* Revision 1.4  1995/08/31 18:07:29  cdaq
* (JRA) Move sidcalsumadc filling to s_sparsify_cal
*
* Revision 1.3  1995/07/20  14:49:57  cdaq
* (JRA) Add calorimeter adc sum per hit histogram
*
* Revision 1.2  1995/05/22  19:45:37  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1995/04/27  20:40:22  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      external thgetid
      integer*4 thgetid
      character*50 here
      parameter (here= 's_fill_cal_hist')
*
      logical ABORT
      character*(*) err
      real*4  histval
      integer*4 row,col,ihit
      include 'sos_data_structures.cmn'
      include 'sos_id_histid.cmn'          
      include 'sos_calorimeter.cmn'
*
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*
      if(scal_num_hits .gt. 0 ) then
        do ihit=1,scal_num_hits
          row=scal_rows(ihit)
          col=scal_cols(ihit)
          histval=float(col)
          call hf1(sidcalplane,histval,1.)
          histval=float(row)
          call hf1(sidcalhits(col),histval,1.)
        enddo
      endif

      return
      end

