      SUBROUTINE C_keep_results(ABORT,err)
* xucc comments begin
* In This file, I notice that Jochen add a line as additional cut for good
* events:  
* .......ccointime_hms .gt. -5.0)
* call c_ntuple_keep(ABORT,err)
* of course, in order to use the value of ccointime_hms
* Jochen added include file:
* include 'coin_data_structures.cmn'
* xucc comments end



*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : Keeps statistics, etc.
*- 
*-   Output: ABORT	- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  20-Nov-1993   Kevin B. Beard for new error standards
* $Log$
* Revision 1.5.8.1  2004/02/26 14:33:51  jones
* Starting code for mduality
*
* Revision 1.5.2.2  2003/08/12 17:35:34  cdaq
* Add variables for e00-108 (hamlet)
*
* Revision 1.5.2.1  2003/07/03 14:06:09  cdaq
* update for fpi-2 (xu)
*
* Revision 1.5.4.1  2003/03/05 22:50:51  xu
* new variables
*
* Revision 1.5  1996/09/04 15:29:30  saw
* * (JRA) Make HSNUM_FPTRACK.gt.0 and SSNUM_FPTRACK.gt.0  instead of
*         HNTRACKS_FP .gt. 0 and HNTRACKS_FP .gt. 0 as criteria for
*         adding to ntuples
*
* Revision 1.4  1996/01/22 15:05:20  saw
* (JRA) Only fill coin ntuple if HMS and SOS both have tracks
*
* Revision 1.3  1996/01/16 21:00:40  cdaq
* no change
*
* Revision 1.2  1994/04/12 17:10:33  cdaq
* (KBB) Add ntuple call
*
* Revision 1.1  1994/02/04  21:07:07  cdaq
* Initial revision
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      include 'hms_data_structures.cmn'
      include 'sos_data_structures.cmn'
      INCLUDE 'hms_scin_parms.cmn'
*     xucc add begin

      include 'coin_data_structures.cmn'

*     add this in order that we can use cointime of hms to get a better candidate
*     for fpi coinsidence event      

*     xucc add end

*
      character*14 here
      parameter (here= 'C_keep_results')
*
      logical ABORT
      character*(*) err
*
*--------------------------------------------------------
*-chance to flush any statistics, etc.
*
*
      ABORT= .FALSE.
      err = ' '
*

*    xucc added begin 
c      if((HSNUM_FPTRACK .gt. 0 .AND. SSNUM_FPTRACK .gt. 0) .AND.   ! check forc tracks
c     > ccointime_hms .gt. -5.0)  ! check for tracks 
c     >  call c_ntuple_keep(ABORT,err)

c     On June 19, Xucc changhed back to remove the cut on ccointime_hms
c     for online purpose.
c
c mkj old cuts removed Dec 4,2003 used for pass1
c.and. hmisc_dec_data(10,1) .gt. 1500
c     >  .and. hmisc_dec_data(10,1) .lt. 2000 )

       if(HSNUM_FPTRACK .gt. 0 .AND. SSNUM_FPTRACK .gt. 0
     > .and. scer_npe_sum.gt.0.2 ) 
     >  call c_ntuple_keep(ABORT,err)
c       write(6,*)'scer_npe=',scer_npe

*   the line "ccointime_hms .gt. -5.0" is added for extra conditions of good
*   pion-electron events in pion form factors to rule out random coinsidence   
*   But I do not understand why we have both ccointime_hms and ccointime_sos.
*   They are of different values.
 

*    xucc added end

*
      IF(ABORT) THEN
         call G_add_path(here,err)
         RETURN
      ELSE
         err= ' '
      ENDIF
*     
      RETURN
      END
