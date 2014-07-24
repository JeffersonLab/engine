      subroutine h_dc_Ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='h_dc_Ntuple_keep')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_dc_ntuple.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'hms_scin_tof.cmn'
      INCLUDE 'gen_scalers.cmn'
      include 'hms_track_histid.cmn'  !temp junk.
      INCLUDE 'hms_calorimeter.cmn'
      INCLUDE 'hms_cer_parms.cmn'
*
      logical HEXIST	!CERNLIB function
*
      integer m
      integer chi_ind(20),i,j,jj,kk
      real*4 chimin,value
      logical found

      real proton_mass
      parameter ( proton_mass = 0.93827247 ) ! [GeV/c^2]
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.h_dc_Ntuple_exists) RETURN       !nothing to do
c
      if (h_dc_Ntuple_max_segmentevents .gt. 0) then
       if (h_dc_Ntuple_segmentevents .gt. h_dc_Ntuple_max_segmentevents) then
        call h_dc_ntuple_change(ABORT,err)
        h_dc_Ntuple_segmentevents = 0
       else
        h_dc_Ntuple_segmentevents = h_dc_Ntuple_segmentevents +1
       endif
      endif
*


* Experiment dependent entries start here.
       evnum=float(gen_event_ID_number)
       evtype= float(gen_event_type)
* scin
       hscin_starttime=hstart_time
       hscin_rfptime(1)=h_rfptime(1)
       hscin_rfptime(2)=h_rfptime(2)
       hscin_rfptime(3)=h_rfptime(3)
       hscin_rfptime(4)=h_rfptime(4)
       hscin_fptimedif(1)=h_fptimedif(1)
       hscin_fptimedif(2)=h_fptimedif(2)
       hscin_fptimedif(3)=h_fptimedif(3)
       hscin_fptimedif(4)=h_fptimedif(4)
       hscin_fptimedif(5)=h_fptimedif(5)
       hscin_fptimedif(6)=h_fptimedif(6)
*
       hdc_ntr=0
       sdc_ntr=0
       if (evtype .gt. 3) return
* fill raster info
       frx_raw_adc=gfrx_raw_adc
       fry_raw_adc=gfry_raw_adc
       frx_adc=gfrx_adc
       fry_adc=gfry_adc
       frx=gfrx
       fry=gfry
*
       if (evtype .eq. 1 .or. evtype .eq. 3) then
       do i=1,20
          chi_ind(i)=i
       enddo
       hdc_ntr=HNTRACKS_FP
       hdc_chi2min=hchi2min_fp
       hdc_x2dmin=hx2dmin_fp
       hdc_y2dmin=hy2dmin_fp
       if (hdc_ntr .gt.HNTRACKS_MAX) hdc_ntr=HNTRACKS_MAX
       do m=1,hdc_ntr
          hdc_xfp(m)=hx_fp(m)
          hdc_xpfp(m)=hxp_fp(m)
          hdc_yfp(m)=hy_fp(m)
          hdc_ypfp(m)=hyp_fp(m)
       enddo
       if ( 1 .eq. 1 )then ! sort by chisq
       jj=1
       kk=chi_ind(1)
       value=hchi2_fp(kk)
       do i=2,hdc_ntr
             if ( hchi2_fp(chi_ind(i)) .lt. value) then
                jj=i
                value=hchi2_fp(chi_ind(i))
              endif
        enddo
        chi_ind(1) = chi_ind(jj)
        chi_ind(jj) = kk
        do i=2,hdc_ntr
           jj=i
           kk=chi_ind(i)
           value=hchi2_fp(kk)
20        continue
           if (value .lt. hchi2_fp(chi_ind(jj-1))) then
               chi_ind(jj)=chi_ind(jj-1)
               jj=jj-1
               goto 20
               endif
           chi_ind(jj)=kk
         enddo

        endif
        do i=1,hdc_ntr
           m=chi_ind(i)
          hdc_chi2(i)=hchi2_fp(m)
          hdc_xptg(i)=hxp_tar(m)
          hdc_ytg(i)=hy_tar(m)
          hdc_yptg(i)=hyp_tar(m)
          hdc_delta(i)=hdelta_tar(m)
          hdc_ptar(i)=hp_tar(m)
          enddo
       endif
       if (evtype .eq. 2 .or. evtype .eq. 3) then
       do i=1,20
          chi_ind(i)=i
       enddo
       sdc_ntr=SNTRACKS_FP
       if (sdc_ntr .gt.SNTRACKS_MAX) sdc_ntr=SNTRACKS_MAX
       do m=1,sdc_ntr
          sdc_xfp(m)=sx_fp(m)
          sdc_xpfp(m)=sxp_fp(m)
          sdc_yfp(m)=sy_fp(m)
          sdc_ypfp(m)=syp_fp(m)
       enddo
       if ( 1 .eq. 1 )then ! sort by chisq
       jj=1
       kk=chi_ind(1)
       value=schi2_fp(kk)
       do i=2,sdc_ntr
             if ( schi2_fp(chi_ind(i)) .lt. value) then
                jj=i
                value=schi2_fp(chi_ind(i))
              endif
        enddo
        chi_ind(1) = chi_ind(jj)
        chi_ind(jj) = kk
        do i=2,sdc_ntr
           jj=i
           kk=chi_ind(i)
           value=schi2_fp(kk)
40        continue
           if (value .lt. schi2_fp(chi_ind(jj-1))) then
               chi_ind(jj)=chi_ind(jj-1)
               jj=jj-1
               goto 40
               endif
           chi_ind(jj)=kk
         enddo

        endif
        do i=1,sdc_ntr
           m=chi_ind(i)
          sdc_chi2(i)=schi2_fp(m)
          sdc_xptg(i)=sxp_tar(m)
          sdc_ytg(i)=sy_tar(m)
          sdc_yptg(i)=syp_tar(m)
          sdc_delta(i)=sdelta_tar(m)
          sdc_ptar(i)=sp_tar(m)
          enddo
      endif
* Fill ntuple for this event
      ABORT= .NOT.HEXIST(h_dc_Ntuple_ID)
      IF(ABORT) THEN
        call G_build_note(':Ntuple ID#$ does not exist',
     &                        '$',h_dc_Ntuple_ID,' ',0.,' ',err)
        call G_add_path(here,err)
      ELSE
c        write(*,*) 'call hfnt  evnum = ',evnum
        call HFNT(h_dc_Ntuple_ID)
      ENDIF
*
      RETURN
      END
