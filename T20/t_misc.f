      subroutine T_MISC(ABORT,err)
*
* $Log$
* Revision 1.1  1998/12/01 20:57:23  saw
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*12 here
      parameter (here= '')
*
      logical ABORT
      character*(*) err
*
      include 't20_data_structures.cmn'
      include 't20_tracking.cmn'
      include 't20_geometry.cmn'
      include 't20_track_histid.cmn'
      include 't20_bypass_switches.cmn'	
      include 't20_misc.cmn'
C      include 't20_reg_polder_structures.cmn'
C==============================================================================
C	THIS ROUTINE TAKES CARE OF THE POLDER "MISCELANEOUS" DETECTOR PACKAGE:
C		THE PARTS OF THE E-POLDER EVENTS READ OUTUPSTAIRS INTO THE 
C		ADCs and TDCs OF ROC (WHAT STEVE SAYS IT IS)

	
      integer*4 ihit,iaddr1,iaddr2        
	
           traw_adc_s11 = 0.
           traw_adc_s12 = 0.
           traw_adc_s21 = 0.
           traw_adc_s22 = 0.
           traw_adc_veto1 = 0.
           traw_adc_veto2 = 0.
           traw_adc_tts1l = 0.
           traw_adc_tts1r = 0.
           traw_adc_tts2l = 0.
           traw_adc_tts2r = 0.
c
           tfl_mhtdc_dc = 0.
           tfl_mhtdc_hms = 0.
           tfl_mhtdc_hms_dc = 0.
           tfl_mhtdc_big1 = 0.
           tfl_mhtdc_ce = 0.
           tfl_mhtdc_polder = 0.
           tfl_mhtdc_h1m3 = 0.
           tfl_mhtdc_h2m3 = 0.
           traw_mhtdc_s11 = 0.
           traw_mhtdc_s12 = 0.
           traw_mhtdc_s21 = 0.
           traw_mhtdc_s22 = 0.
           traw_mhtdc_s1 = 0.
           traw_mhtdc_s2 = 0.
           traw_mhtdc_veto = 0.
c 
           traw_hrtdc_hms = 0.
           traw_hrtdc_dc = 0.
           traw_hrtdc_hms_dc = 0.
           traw_hrtdc_polder = 0.
           traw_hrtdc_hms_nb = 0.
           traw_hrtdc_pldr_nb = 0.
           traw_hrtdc_ce = 0.

	do ihit=1,tmisc_tot_hits
	  iaddr1=tmisc_raw_addr1(ihit)	  ! iaddr1=1 for ADC, =2 for MHTDCs, and =3 for HRTDC
	  iaddr2=tmisc_raw_addr2(ihit)	
c 
           if((iaddr1.ne.1).and.(iaddr1.ne.2).and.(iaddr1.ne.3))then
            write(6,*)'There is a problem in t_misc:  iaddr1 = ',iaddr1
            goto 100
          endif
C-------------------------------------------------------------------------------------
C***** THE ADC (LECROY 1881M - UPSTAIRS)
          if (iaddr1.eq.1) then
           if ((iaddr2.lt.1).or.(iaddr2.gt.32)) then
             write(6,*)'There is a problem in t_misc:  for iaddr1=1, iaddr2=',iaddr2
             goto 25
           else
C           THE START AND VETO DETECTORS
	     if (iaddr2.eq.1) traw_adc_s11=tmisc_raw_data(ihit)
	     if (iaddr2.eq.2) traw_adc_s12=tmisc_raw_data(ihit)
	     if (iaddr2.eq.3) traw_adc_s21=tmisc_raw_data(ihit)
	     if (iaddr2.eq.4) traw_adc_s22=tmisc_raw_data(ihit)
	     if (iaddr2.eq.5) traw_adc_veto1=tmisc_raw_data(ihit)
	     if (iaddr2.eq.6) traw_adc_veto2=tmisc_raw_data(ihit)
	     if (iaddr2.eq.7) traw_adc_tts1l=tmisc_raw_data(ihit)
	     if (iaddr2.eq.8) traw_adc_tts1r=tmisc_raw_data(ihit)
	     if (iaddr2.eq.9) traw_adc_tts2l=tmisc_raw_data(ihit)
	     if (iaddr2.eq.10) traw_adc_tts2r=tmisc_raw_data(ihit)
C           THERE ARE 6 "SPARE" CHANNELS AVAILABLE IN THE ADC (LECROY 1881M),
       	     if ((iaddr2.gt.10).and.(iaddr2.le.32)) then
	        traw_adc(iaddr2)=tmisc_raw_data(ihit)
             endif
           endif
          endif
C--------------------------------------------------------------------------------------
C***** THE MULTIHIT TDCs (LECROY 1877S - UPSTAIRS AND DOWNSTAIRS)
25        continue
          if (iaddr1.eq.2) then
           if ((iaddr2.lt.1).or.(iaddr2.gt.32))then
             write(6,*)'There is a problem in t_misc:  for iaddr1=2, iaddr2=',iaddr2
             goto 50
           else
       	     if (iaddr2.eq.1) tfl_mhtdc_dc = tmisc_raw_data(ihit)
       	     if (iaddr2.eq.2) tfl_mhtdc_hms = tmisc_raw_data(ihit)
       	     if (iaddr2.eq.3) tfl_mhtdc_hms_dc = tmisc_raw_data(ihit)
       	     if (iaddr2.eq.4) tfl_mhtdc_big1 = tmisc_raw_data(ihit)
       	     if (iaddr2.eq.5) tfl_mhtdc_ce = tmisc_raw_data(ihit)
       	     if (iaddr2.eq.6) tfl_mhtdc_polder = tmisc_raw_data(ihit)
       	     if (iaddr2.eq.7) tfl_mhtdc_h1m3 = tmisc_raw_data(ihit)
       	     if (iaddr2.eq.8) tfl_mhtdc_h2m3 = tmisc_raw_data(ihit)
	     if (iaddr2.eq.9) traw_mhtdc_s11 = tmisc_raw_data(ihit)
	     if (iaddr2.eq.10) traw_mhtdc_s12 = tmisc_raw_data(ihit)
	     if (iaddr2.eq.11) traw_mhtdc_s21 = tmisc_raw_data(ihit)
 	     if (iaddr2.eq.12) traw_mhtdc_s22 = tmisc_raw_data(ihit)
 	     if (iaddr2.eq.13) traw_mhtdc_s1 = tmisc_raw_data(ihit)
 	     if (iaddr2.eq.14) traw_mhtdc_s2 = tmisc_raw_data(ihit)
 	     if (iaddr2.eq.15) traw_mhtdc_veto = tmisc_raw_data(ihit)
           endif
          endif
C--------------------------------------------------------------------------------------
C***** THE HIGH RESOLUTION TDC (I CHOSE 16 CHANNEL FOR NOW) UPSTAIRS
50         if (iaddr1.eq.3) then
             if ((iaddr2.lt.1).or.(iaddr2.gt.8)) then
c               write(6,*)'There is a problem in t_misc:  for iaddr1=3, iaddr2=',iaddr2
               goto 100
             endif
             if((iaddr2.ne.2).and.(iaddr2.ne.4).and.(iaddr2.ne.6)) then
c                write(6,*)'t_misc:  in the mhtdc part.  iaddr2 =  ',iaddr2,ihit ,tmisc_raw_data(ihit)
             endif
             if(iaddr2.eq.1)traw_hrtdc_hms = tmisc_raw_data(ihit)
             if(iaddr2.eq.2)traw_hrtdc_dc = tmisc_raw_data(ihit)
             if(iaddr2.eq.3)traw_hrtdc_hms_dc = tmisc_raw_data(ihit)
             if(iaddr2.eq.4)traw_hrtdc_polder = tmisc_raw_data(ihit)
             if(iaddr2.eq.5)traw_hrtdc_hms_nb = tmisc_raw_data(ihit)
             if(iaddr2.eq.6)traw_hrtdc_pldr_nb = tmisc_raw_data(ihit)
             if(iaddr2.eq.7)traw_hrtdc_ce = tmisc_raw_data(ihit)
          endif
C-------------------------------------------------------------------------------------
100     continue
	enddo
c
C***** now do a few combinations of ADCs and TDCs
      traw_adc_s1sum = traw_adc_s11 + traw_adc_s12
      traw_adc_s2sum = traw_adc_s21 + traw_adc_s22
      traw_adc_s1s2sum = traw_adc_s2sum + traw_adc_s1sum
      traw_adc_vetosum = traw_adc_veto1 + traw_adc_veto2
c
      RETURN
      END
*********
*     Local Variables:
*     mode: fortran
*     fortran-if-indent: 2
*     fortran-do-indent: 2
*     End:

