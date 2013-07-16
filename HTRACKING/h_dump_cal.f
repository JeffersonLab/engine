      SUBROUTINE H_DUMP_CAL(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze scintillator information for each track 
*-
*-      Required Input BANKS     HMS_CALORIMETER
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 9/7/95
*
* h_dump_cal writes out the raw calorimeterinformation for the final tracks.
* This data is analyzed by independent routines to fit the gains for each
* block.
*
* $Log: h_dump_cal.f,v $
* Revision 1.4  1999/06/10 16:48:04  csa
* (JRA) Added ycal, emeas calculations
*
* Revision 1.3  1998/12/17 22:02:38  saw
* Support extra set of tubes on HMS shower counter
*
* Revision 1.2  1996/01/17 18:17:47  cdaq
* (SAW) Remove extra () pair around implied do loop in write statement
*
* Revision 1.1  1995/10/09 20:16:45  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*10 here
      parameter (here= 'H_DUMP_CAL')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'

      integer*4 blk
      real*4 emeas,ycal

      save

*
*  Write out cal fitting data.
*
*  What should this do for new tubes?
*
      if (abs(hsdelta).le.10 .and. hcer_npe_sum.gt.2) then

! this correction comes from h_corect_cal_pos(neg).f

	ycal=hsy_fp + hcal_1pr_zpos*hsyp_fp
        ycal=min(35.,ycal)
        ycal=max(-35.,ycal)
	emeas=hsp*exp(ycal/165.4)/(1+ycal**2/50000.)

        write(35,'(1x,52(1x,f6.1),1x,e11.4)') 
     &      (hcal_realadc_pos(blk),blk=1,hmax_cal_blocks),emeas

!        if(hcal_num_neg_columns.gt.0) then
!        write(35,'(1x,52(1x,f6.1),1x,e11.4)') 
!     &       (hcal_realadc_neg(blk),blk=1,hmax_cal_blocks),hsp
!        endif
      endif

      RETURN
      END
