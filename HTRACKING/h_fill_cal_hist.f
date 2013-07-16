      subroutine h_fill_cal_hist(Abort,err)
*
*     routine to fill histograms with hms_cal varibles
*
*     Author:   J. R. Arrington
*     Date:     26 April 1995
*     Copied from:  h_fill_scin_raw_hist
*
*
* $Log: h_fill_cal_hist.f,v $
* Revision 1.9  2002/10/02 13:42:43  saw
* Check that user hists are defined before filling
*
* Revision 1.8  1999/02/23 18:37:20  csa
* (JRA) Remove obsolete hf1 call
*
* Revision 1.7  1999/02/03 21:13:23  saw
* Code for new Shower counter tubes
*
* Revision 1.6  1999/01/27 16:02:39  saw
* Check if some hists are defined before filling
*
* Revision 1.5  1998/12/17 22:02:39  saw
* Support extra set of tubes on HMS shower counter
*
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
*     Light in either tube will do
*
      if(hcal_num_hits .gt. 0 ) then
        do ihit=1,hcal_num_hits
          row=hcal_rows(ihit)
          col=hcal_cols(ihit)
          histval=float(col)
          if(hidcalplane.gt.0) call hf1(hidcalplane,histval,1.)
          histval=float(row)
          if(hcal_adcs_pos(ihit).gt.0.1.and.hidcalposhits(col).gt.0)
     $         call hf1(hidcalposhits(col),histval,1.)
          if(hcal_adcs_neg(ihit).gt.0.1.and.hidcalneghits(col).gt.0)
     $         call hf1(hidcalneghits(col),histval,1.)
        enddo
      endif

      return
      end

