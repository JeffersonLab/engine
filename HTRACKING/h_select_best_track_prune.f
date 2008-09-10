      SUBROUTINE H_SELECT_BEST_TRACK_PRUNE(ABORT,err)
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
*- Revision 1.1.8.4  2008/09/10 15:07:57  puckett
*- make sanity check on ytar prune cut angle-dependent
*-
*- Revision 1.1.8.3  2008/07/29 16:35:13  puckett
*- added "pmiss" (p - pel(htheta)) to available prune tests
*-
*- Revision 1.1.8.2  2008/02/07 16:15:33  cdaq
*- removed cuts that don't apply to gep
*-
*- Revision 1.1.8.1  2007/09/10 20:28:01  pcarter
*- Implemented changes to allow compilation on RHEL 3,4,5 and MacOSX
*-
*- Revision 1.1  2005/03/23 16:33:32  jones
*- Add new code s_select_best_track_prune.f (P Bosted)
*-
*
* Revision 1.1  2005/03/08 bosted
* Initial revision
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'H_SELECT_BEST_TRACK_PRUNE')
*
      logical ABORT
      character*(*) err
*
      include 'gen_data_structures.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'hms_calorimeter.cmn'
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'hms_scin_tof.cmn'
      INCLUDE 'hms_tracking.cmn'
c
*
*     local variables 
      integer*4 goodtrack,track,ngood,reject(1000),trk
      logical first,keep(1000)
      real*4 chi2perdeg,chi2min,betap,p
c     new local variables to calculate missing momentum as a prune test:
      real*4 pmiss,theta,phi,pel,pz,px,py,pz_spec
c
c      integer*4 i,j
      data first /.true./
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*     Need to test to chose the best track
      HSNUM_FPTRACK = 0
      HSNUM_TARTRACK = 0
        
      if (first) then
        write(*,*) ' HMS track selection using pruning method'
        first = .false.
! Make sure limits are reasonable
        hprune_xp    = max(0.08, hprune_xp)
        hprune_yp    = max(0.04, hprune_yp)
        hprune_ytar  = max(15.*sin(htheta_lab*degree),  hprune_ytar)
        hprune_delta = max(10.0, hprune_delta)
        hprune_beta  = max(0.1,  hprune_beta)
        hprune_df    = max(1,  hprune_df)
        hprune_chibeta= max(2.,  hprune_chibeta)
        hprune_fptime= max(5.,  hprune_fptime)
        hprune_npmt  = max(2,  hprune_npmt)  
        hprune_pmiss = max(2.,hprune_pmiss) / 100. ! set this value in param file in percent
        write(*,'(1x,'' using following HMS limits''/
     >    1x,''abs(xptar)<'',f6.3/
     >    1x,''abs(yptar)<'',f6.3/
     >    1x,''abs(ytar)<'',f6.3/
     >    1x,''abs(delta)<'',f6.3/
     >    1x,''abs(beta-betap)<'',f6.3/
     >    1x,''ndegfreedom trk>='',i2/
     >    1x,''beta chisq>'',f6.1/
     >    1x,''num PMT hits >='',i3/
     >    1x,''abs(fptime-hstart_time_center)<'',f6.1/
     >    1x,''abs(p-pel(theta))/hpcentral<'',f6.3)') 
     >    hprune_xp,hprune_yp,hprune_ytar,hprune_delta,
     >    hprune_beta,hprune_df,hprune_chibeta,hprune_npmt,hprune_fptime,
     >    hprune_pmiss
      endif
c
c
      if( HNTRACKS_FP.GT. 0) then
        chi2min= 1e10
        goodtrack = 0

! Initialize all tracks to be good
        do track = 1, HNTRACKS_FP
          keep(track) = .true.
          reject(track)=0
        enddo

! Prune on xptar
        ngood=0
        do track = 1, HNTRACKS_FP
          if( abs(hxp_tar(track)) .lt. hprune_xp .and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, HNTRACKS_FP
            if( abs(hxp_tar(track)) .ge. hprune_xp) then
              keep(track) = .false. 
              reject(track) = reject(track) + 1
            endif
          enddo
        endif

! Prune on yptar
        ngood=0
        do track = 1, HNTRACKS_FP
          if( abs(hyp_tar(track)) .lt. hprune_yp .and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, HNTRACKS_FP
            if( abs(hyp_tar(track)) .ge. hprune_yp) then
              keep(track) = .false. 
              reject(track) = reject(track) + 2
            endif
          enddo
        endif

! Prune on ytar
        ngood=0
        do track = 1, HNTRACKS_FP
          if( abs(hy_tar(track)) .lt. hprune_ytar .and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, HNTRACKS_FP
            if( abs(hy_tar(track)) .ge. hprune_ytar) then
              keep(track) = .false. 
              reject(track) = reject(track) + 10
            endif
          enddo
        endif

! Prune on delta
        ngood=0
        do track = 1, HNTRACKS_FP
          if( abs(hdelta_tar(track)) .lt. hprune_delta 
     >       .and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, HNTRACKS_FP
            if(abs(hdelta_tar(track)) .ge. hprune_delta) then 
              keep(track) = .false. 
              reject(track) = reject(track) + 20
            endif
          enddo
        endif

! Prune on beta
        ngood=0
        do track = 1, HNTRACKS_FP
          p = hp_tar(track)
          betap = p/sqrt(p*p+hpartmass*hpartmass)
          if( abs(hbeta(track)-betap) .lt. hprune_beta 
     >       .and. keep(track)) then
c 2/07/08 pyb turned off for gep
c            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, HNTRACKS_FP
            p = hp_tar(track)
            betap = p/sqrt(p*p+hpartmass*hpartmass)
            if(abs(hbeta(track)-betap) .ge. hprune_beta) then
              keep(track) = .false. 
              reject(track) = reject(track) + 100
            endif
          enddo
        endif

! Prune on deg. freedom for track chisq
        ngood=0
        do track = 1, HNTRACKS_FP
          if(HNFREE_FP(track) .ge. hprune_df .and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, HNTRACKS_FP
            if(HNFREE_FP(track) .lt. hprune_df) then 
              keep(track) = .false. 
              reject(track) = reject(track) + 200
            endif
          enddo
        endif

! Prune on num pmt hits
        ngood=0
        do track = 1, HNTRACKS_FP
          if(hnum_pmt_hit(track) .ge. hprune_npmt.and. keep(track)) then
c 2/07/08 pyb turned off for gep
             ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, HNTRACKS_FP
            if(hnum_pmt_hit(track) .lt. hprune_npmt) then 
              keep(track) = .false. 
              reject(track) = reject(track) + 100000
            endif
          enddo
        endif

! Prune on beta chisqr
        ngood=0
        do track = 1, HNTRACKS_FP
          if(hbeta_chisq(track) .lt. hprune_chibeta .and.
     >       hbeta_chisq(track) .gt. 0.01 .and. keep(track)) then
c 2/07/08 pyb turned off for gep
c            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, HNTRACKS_FP
            if(hbeta_chisq(track) .ge. hprune_chibeta .or. 
     >       hbeta_chisq(track) .le. 0.01) then
              keep(track) = .false. 
              reject(track) = reject(track) + 1000
            endif
          enddo
        endif

! Prune on fptime
        ngood=0
        do track = 1, HNTRACKS_FP
          if( abs(htime_at_fp(track)-hstart_time_center).lt.hprune_fptime 
     >       .and. keep(track)) then
            ngood = ngood + 1
          endif
        enddo
        if(ngood.gt.0) then
          do track = 1, HNTRACKS_FP
            if(abs(htime_at_fp(track)-hstart_time_center).ge.
     >        hprune_fptime) then 
              keep(track) = .false. 
              reject(track) = reject(track) + 2000
            endif
          enddo
        endif
! prune on missing momentum: p - pel(theta) / pcentral

        ngood = 0
        do track = 1, hntracks_fp
           p = hp_tar(track)

           pz_spec = p / sqrt(1. + (hxp_tar(track))**2 + (hyp_tar(track))**2 )

           px = pz_spec * hxp_tar(track)
           py = pz_spec * (hyp_tar(track)*coshthetas - sinhthetas)
           pz = pz_spec * (hyp_tar(track)*sinhthetas + coshthetas)

           theta = acos(min(-1.,max(pz/p,1.)))
           phi = atan2(py,px) ! phi is actually not needed

           pel = 2. * hpartmass * gebeam * (hpartmass + gebeam) * cos(theta) / 
     $          (hpartmass**2 + 2.*hpartmass*gebeam + (gebeam*sin(theta))**2)

           pmiss = p - pel

           if(abs(pmiss/hpcentral).lt.hprune_pmiss.and.keep(track)) then
              ngood = ngood + 1
           endif
        enddo

        if(ngood.gt.0) then
           do track = 1,hntracks_fp
              p = hp_tar(track)
              
              pz_spec = p / sqrt(1. + (hxp_tar(track))**2 + (hyp_tar(track))**2 )

              px = pz_spec * hxp_tar(track)
              py = pz_spec * (hyp_tar(track)*coshthetas - sinhthetas)
              pz = pz_spec * (hyp_tar(track)*sinhthetas + coshthetas)

              theta = acos(min(-1.,max(pz/p,1.)))
              phi = atan2(py,px) ! phi is actually not needed

              pel = 2. * hpartmass * gebeam * (hpartmass + gebeam) * cos(theta) / 
     $             (hpartmass**2 + 2.*hpartmass*gebeam + (gebeam*sin(theta))**2)

              pmiss = p - pel
              
              if(abs(pmiss/hpcentral).ge.hprune_pmiss) then
                 keep(track) = .false.
                 reject(track) = reject(track) + 34
              endif
           enddo
        endif


! Pick track with best chisq if more than one track passed prune tests
        goodtrack = 1
        do track = 1, HNTRACKS_FP
          chi2perdeg = HCHI2_FP(track)/max(1.,FLOAT(HNFREE_FP(track)))
          if(chi2perdeg .lt. chi2min .and. keep(track)) then
            goodtrack = track
            chi2min = chi2perdeg
          endif                    
        enddo                          
        HSNUM_TARTRACK = goodtrack
        HSNUM_FPTRACK  = goodtrack

c        write(*,*) 'best track=',goodtrack
c        write(*,*) 't0best,chi2=',htrack_t0best(goodtrack),chi2perdeg
      endif
! for debugging
      if( HNTRACKS_FP.GT. 100) then
           write(*,'(/)')
           do trk = 1, HNTRACKS_FP
             write(*,'(3i3,4L2,7f6.1,L2,i9)') trk,HNFREE_FP(trk),
     >          hnum_pmt_hit(trk),
     >         hgood_plane_time(trk,1),hgood_plane_time(trk,3),
     >         hgood_plane_time(trk,2),hgood_plane_time(trk,4),
     >         htime_at_fp(trk),hbeta(trk),hbeta_chisq(trk),
     >         hdelta_tar(trk),hy_tar(trk),hxp_tar(trk),hyp_tar(trk),
     >         keep(trk),reject(trk)
           enddo
           write(*,'(1x,''good trk='',2i4)') goodtrack
      endif
      return
      end
