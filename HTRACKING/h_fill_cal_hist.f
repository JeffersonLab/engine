      subroutine h_fill_cal_hist(Abort,err)
*
*     routine to fill histograms with hms_cal varibles
*
*     Author:   J. R. Arrington
*     Date:     26 April 1995
*     Copied from:  h_fill_scin_raw_hist
*
*
* $Log$
* Revision 1.4  1995/08/31 15:01:15  cdaq
* (JRA) Move hidcalsumadc filling to h_sparsify_cal
*
* Revision 1.3  1995/07/19  18:12:18  cdaq
* (JRA) Add calorimeter adc sum per hit histogram
*
* Revision 1.2  1995/05/22  19:39:10  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1995/04/27  20:41:13  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      external thgetid
      integer*4 thgetid
      character*50 here
      parameter (here= 'h_fill_cal_hist')
*
      logical ABORT
      character*(*) err
      real*4  histval
      integer*4 row,col,ihit
      include 'hms_data_structures.cmn'
      include 'hms_id_histid.cmn'          
      include 'hms_calorimeter.cmn'
*
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*
      if(hcal_num_hits .gt. 0 ) then
        do ihit=1,hcal_num_hits
          row=hcal_rows(ihit)
          col=hcal_cols(ihit)
          histval=float(col)
          call hf1(hidcalplane,histval,1.)
          histval=float(row)
          call hf1(hidcalhits(col),histval,1.)
        enddo
      endif

      return
      end

