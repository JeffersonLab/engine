      SUBROUTINE s_SELECT_BEST_TRACK_PRUNE(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Select the best track through the HMS
*-                              
*-
*-      Required Input BANKS
*-
*-      Output BANKS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*- $Log$
*- Revision 1.1  2005/03/23 16:34:08  jones
*- Add new code s_select_best_track_prune.f (P Bosted)
*-
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 's_SELECT_BEST_TRACK_PRUNE')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'sos_physics_sing.cmn'
      INCLUDE 'sos_calorimeter.cmn'
      INCLUDE 'sos_scin_parms.cmn'
      INCLUDE 'sos_scin_tof.cmn'
      INCLUDE 'sos_tracking.cmn'
c
*
*     local variables 
      integer*4 goodtrack,track,ngood,reject(1000),trk
      logical first,keep(1000)
      real*4 chi2perdeg,chi2min,betap,p
c
      integer*4 i,j
      data first /.true./
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*     Need to test to chose the best track
      sSNUM_FPTRACK = 0
      sSNUM_TARTRACK = 0
        
      if (first) then
        write(*,*) ' sos track selection using pruning method'
        first = .false.
! Make sure limits are reasonable
        sprune_xp    = max(0.04, sprune_xp)
        sprune_yp    = max(0.08, sprune_yp)
        sprune_ytar  = max(4.0,  sprune_ytar)
        sprune_delta = max(25.0, sprune_delta)
        sprune_beta  = max(0.1,  sprune_beta)
        sprune_df    = max(2,  sprune_df)
        sprune_chibeta= max(2.,  sprune_chibeta)
        sprune_fptime= max(5.,  sprune_fptime)
        sprune_npmt  = max(6 ,  sprune_npmt)  
        write(*,'(1x,'' using following SOS limits''/
     >    1x,''abs(xptar)<'',f6.3/
     >    1x,''abs(yptar)<'',f6.3/
     >    1x,''abs(ytar)<'',f6.3/
     >    1x,''abs(delta)<'',f6.3/
     >    1x,''abs(beta-betap)<'',f6.3/
     >    1x,''ndegfreedom trk>='',i2/
     >    1x,''beta chisq>'',f6.1/
     >    1x,''num PMT hits >='',i3/
     >    1x,''abs(fptime-sstart_time_center)<'',f6.1)') 
     >    sprune_xp,sprune_yp,sprune_ytar,sprune_delta,
     >    sprune_beta,sprune_df,sprune_chibeta,sprune_npmt,sprune_fptime
      endif
c
c
      if( sNTRACKS_FP.GT. 0) then
        chi2min= 1e10
        goodtrack = 0

! Initialize all tracks to be good
        do track = 1, sNTRACKS_FP
          keep(track) = .true.
          reject(track)=0
        enddo

! Prune on xptar
        ngood=0
        do track = 1, sNTRACKS_FP
          if( abs(sxp_tar(track)) .lt. sprune_xp .and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, sNTRACKS_FP
            if( abs(sxp_tar(track)) .ge. sprune_xp) then
              keep(track) = .false. 
              reject(track) = reject(track) + 1
            endif
          enddo
        endif

! Prune on yptar
        ngood=0
        do track = 1, sNTRACKS_FP
          if( abs(syp_tar(track)) .lt. sprune_yp .and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, sNTRACKS_FP
            if( abs(syp_tar(track)) .ge. sprune_yp) then
              keep(track) = .false. 
              reject(track) = reject(track) + 2
            endif
          enddo
        endif

! Prune on ytar
        ngood=0
        do track = 1, sNTRACKS_FP
          if( abs(sy_tar(track)) .lt. sprune_ytar .and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, sNTRACKS_FP
            if( abs(sy_tar(track)) .ge. sprune_ytar) then
              keep(track) = .false. 
              reject(track) = reject(track) + 10
            endif
          enddo
        endif

! Prune on delta
        ngood=0
        do track = 1, sNTRACKS_FP
          if( abs(sdelta_tar(track)) .lt. sprune_delta 
     >       .and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, sNTRACKS_FP
            if(abs(sdelta_tar(track)) .ge. sprune_delta) then 
              keep(track) = .false. 
              reject(track) = reject(track) + 20
            endif
          enddo
        endif

! Prune on beta
        ngood=0
        do track = 1, sNTRACKS_FP
          p = sp_tar(track)
          betap = p/sqrt(p*p+spartmass*spartmass)
          if( abs(sbeta(track)-betap) .lt. sprune_beta 
     >       .and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, sNTRACKS_FP
            p = sp_tar(track)
            betap = p/sqrt(p*p+spartmass*spartmass)
            if(abs(sbeta(track)-betap) .ge. sprune_beta) then
              keep(track) = .false. 
              reject(track) = reject(track) + 100
            endif
          enddo
        endif

! Prune on deg. freedom for track chisq
        ngood=0
        do track = 1, sNTRACKS_FP
          if(sNFREE_FP(track) .ge. sprune_df .and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, sNTRACKS_FP
            if(sNFREE_FP(track) .lt. sprune_df) then 
              keep(track) = .false. 
              reject(track) = reject(track) + 200
            endif
          enddo
        endif

! Prune on num pmt hits
        ngood=0
        do track = 1, sNTRACKS_FP
          if(snum_pmt_hit(track) .ge. sprune_npmt.and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, sNTRACKS_FP
            if(snum_pmt_hit(track) .lt. sprune_npmt) then 
              keep(track) = .false. 
              reject(track) = reject(track) + 100000
            endif
          enddo
        endif

! Prune on beta chisqr
        ngood=0
        do track = 1, sNTRACKS_FP
          if(sbeta_chisq(track) .lt. sprune_chibeta .and.
     >       sbeta_chisq(track) .gt. 0.01 .and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, sNTRACKS_FP
            if(sbeta_chisq(track) .ge. sprune_chibeta .or. 
     >       sbeta_chisq(track) .le. 0.01) then
              keep(track) = .false. 
              reject(track) = reject(track) + 1000
            endif
          enddo
        endif

! Prune on fptime
        ngood=0
        do track = 1, sNTRACKS_FP
          if( abs(stime_at_fp(track)-sstart_time_center).lt.sprune_fptime 
     >       .and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, sNTRACKS_FP
            if(abs(stime_at_fp(track)-sstart_time_center).ge.
     >        sprune_fptime) then 
              keep(track) = .false. 
              reject(track) = reject(track) + 2000
            endif
          enddo
        endif

! Prune on Y2 being hit
        ngood=0
        do track = 1, sNTRACKS_FP
          if(sgood_plane_time(track,4).and. keep(track)) ngood = ngood + 1
        enddo
        if(ngood.gt.0) then
          do track = 1, sNTRACKS_FP
            if(.not.sgood_plane_time(track,4)) then
              keep(track) = .false. 
              reject(track) = reject(track) + 10000
            endif
          enddo
        endif

! Prune on X2 being hit
        ngood=0
        do track = 1, sNTRACKS_FP
          if(sgood_plane_time(track,3).and. keep(track)) ngood = ngood + 1
        enddo
        if(ngood.gt.0) then
          do track = 1, sNTRACKS_FP
            if(.not.sgood_plane_time(track,3)) then
              keep(track) = .false. 
              reject(track) = reject(track) + 20000
            endif
          enddo
        endif


! Pick track with best chisq if more than one track passed prune tests
        goodtrack = 1
        do track = 1, sNTRACKS_FP
          chi2perdeg = sCHI2_FP(track)/max(1.,FLOAT(sNFREE_FP(track)))
          if(chi2perdeg .lt. chi2min .and. keep(track)) then
            goodtrack = track
            chi2min = chi2perdeg
          endif                    
        enddo                          
        sSNUM_TARTRACK = goodtrack
        sSNUM_FPTRACK  = goodtrack
      endif
! for debugging
      if( sNTRACKS_FP.GT. 100) then
           write(*,'(/)')
           do trk = 1, sNTRACKS_FP
             write(*,'(3i3,4L2,7f6.1,L2,i9)') trk,sNFREE_FP(trk),
     >          snum_pmt_hit(trk),
     >         sgood_plane_time(trk,1),sgood_plane_time(trk,3),
     >         sgood_plane_time(trk,2),sgood_plane_time(trk,4),
     >         stime_at_fp(trk),sbeta(trk),sbeta_chisq(trk),
     >         sdelta_tar(trk),sy_tar(trk),sxp_tar(trk),syp_tar(trk),
     >         keep(trk),reject(trk)
           enddo
           write(*,'(1x,''good trk='',2i4)') goodtrack
      endif
      return
      end
