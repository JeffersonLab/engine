      SUBROUTINE S_DUMP_CAL(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze scintillator information for each track 
*-
*-      Required Input BANKS     SOS_CALORIMETER
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 9/7/95
*
* s_dump_cal writes out the raw calorimeter information for the final tracks.
* This data is analyzed by independent routines to fit the gains for each
* block.
*
* $Log: s_dump_cal.f,v $
* Revision 1.4  1999/06/10 16:56:30  csa
* (JRA) Added ycal, emeas calculations, changed test condition
*
* Revision 1.3  1999/01/29 17:34:58  saw
* Add variables for second tubes on shower counter
*
* Revision 1.2  1996/01/17 18:07:51  cdaq
* (JRA) Put track and delta cuts on what get's written out
*
* Revision 1.1  1995/10/09 20:17:10  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*10 here
      parameter (here= 'S_DUMP_CAL')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'

      integer*4 blk
      real*4 emeas,ycal

      save

*
*  Write out cal fitting data.
*
*  What should this do for new tubes?
*
      if (abs(ssdelta).le.18 .and. scer_npe_sum.ge.2.0) then

        ycal=ssy_fp + scal_1pr_zpos*ssyp_fp
	ycal=min(35.,ycal)
	ycal=max(-35.,ycal)
        emeas=ssp*exp(-ycal/210.7)/(1+ycal**2/22000.)

        write(36,'(1x,44(1x,f6.1),1x,e11.4)') 
     &       (scal_realadc_pos(blk),blk=1,smax_cal_blocks),emeas
!        if(scal_num_neg_columns.gt.0) then
!          write(36,'(1x,44(1x,f6.1),1x,e11.4)') 
!     &       (scal_realadc_neg(blk),blk=1,smax_cal_blocks),ssp
!        endif
      endif
      RETURN
      END
