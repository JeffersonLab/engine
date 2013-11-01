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
      integer chi_ind(20),i,j
      real*4 chimin
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
       if (evtype .ne. 1) return
       do i=1,20
          chi_ind(i)=0
       enddo
       dc_ntr=HNTRACKS_FP
       if (dc_ntr .gt.HNTRACKS_MAX) dc_ntr=HNTRACKS_MAX
       do m=1,dc_ntr
          dc_xfp(m)=hx_fp(m)
          dc_xpfp(m)=hxp_fp(m)
          dc_yfp(m)=hy_fp(m)
          dc_ypfp(m)=hyp_fp(m)
       enddo
       do i=1,dc_ntr
          chimin=10000000.
          do j=1,dc_ntr
             if ( hchi2_fp(j) .lt. chimin .and. chi_ind(j) .eq.0) then
                chimin=hchi2_fp(j)
                endif
             enddo
          found = .false.
          do j=1,dc_ntr
            if ( hchi2_fp(j) .eq. chimin .and. chi_ind(j) .eq.0 .and. .not. found) then
              chi_ind(j) = i
              found = .true.
              endif
          enddo 
         enddo
        do i=1,dc_ntr
           m=chi_ind(i)
          dc_chi2(i)=hchi2_fp(m)
          dc_xptg(i)=hxp_tar(m)
          dc_ytg(i)=hy_tar(m)
          dc_yptg(i)=hyp_tar(m)
          dc_delta(i)=hdelta_tar(m)
          dc_ptar(i)=hp_tar(m)
          enddo
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
