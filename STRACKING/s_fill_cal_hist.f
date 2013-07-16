      subroutine s_fill_cal_hist(Abort,err)
*
*     routine to fill histograms with sos_cal varibles
*
*     Author:   J. R. Arrington
*     Date:     26 April 1995
*     Copied from:  s_fill_scin_raw_hist
*
*
* $Log: s_fill_cal_hist.f,v $
* Revision 1.10  2002/12/19 22:05:45  jones
*    sidcalposhits and sidcalneghits are integer*4 not logical
*  so sidcalposhits(col).gt.0 replaces sidcalposhits(col) in if statement
*
* Revision 1.9  2002/07/31 20:20:58  saw
* Only try to fill user hists that are defined
*
* Revision 1.8  1999/02/23 18:58:02  csa
* (JRA) Remove obsolete hf1 call
*
* Revision 1.7  1999/02/03 21:13:45  saw
* Code for new Shower counter tubes
*
* Revision 1.6  1999/01/29 17:34:58  saw
* Add variables for second tubes on shower counter
*
* Revision 1.5  1999/01/27 16:02:45  saw
* Check if some hists are defined before filling
*
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
*     Light in either tube will do
*
      if(scal_num_hits .gt. 0 ) then
        do ihit=1,scal_num_hits
          row=scal_rows(ihit)
          col=scal_cols(ihit)
          histval=float(col)
          if(sidcalplane.gt.0) call hf1(sidcalplane,histval,1.)
          histval=float(row)
          if(scal_adcs_pos(ihit).gt.0.1.and.sidcalposhits(col).gt.0)
     $         call hf1(sidcalposhits(col),histval,1.)
          if(scal_adcs_neg(ihit).gt.0.1.and.sidcalneghits(col).gt.0)
     $         call hf1(sidcalneghits(col),histval,1.)
        enddo
      endif

      return
      end
