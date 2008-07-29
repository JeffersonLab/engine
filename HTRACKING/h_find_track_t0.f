      subroutine h_find_track_t0(track,abort,err)
      
      implicit none
      save

      integer*4 track

      integer hit,plane,wire,ihit,i,j
      real tdrift,ddrift

      character*24 here
      parameter(here='h_find_track_t0')

      include 'hms_data_structures.cmn'
      include 'hms_tracking.cmn'
      include 'hms_geometry.cmn'

      integer istep,nstep,beststep
      real t0step,t0,t0min,t0max,t0slop
      real t0best
      real chi2,minchi2,hitpos,trackpos,residual
      real track_coord(hmax_num_dc_planes)

      integer nfree,ierr

c     this routine assumes that the left-right combination of the hits is correct:

      integer nsmall,nlarge ! number of hits on the track with drift distance "too large"
                            ! number of hits on the track with drift distance "too small"
      integer t0sign
      logical firsttry

      real*8 TT(hnum_fpray_param)
      real*8 AA(hnum_fpray_param,hnum_fpray_param)
      real*8 dray(hnum_fpray_param)
      
      logical abort
      character*(*) err

      real*4 h_drift_dist_calc
      external h_drift_dist_calc

      integer*4 remap(hnum_fpray_param)
      data remap/5,6,3,4/

      abort = .false.
      err=' '
c     revert to default values if user hasn't defined something reasonable:
      if(h_track_t0_slop.lt.0.1.or.h_track_t0_slop.gt.1000.) then
         h_track_t0_slop = 30.
      endif

      if(h_track_t0_step.lt.0.1.or.h_track_t0_step.gt.100.) then
         h_track_t0_step = 1.
      endif

      t0step = h_track_t0_step
      t0slop = h_track_t0_slop

      nstep = int( t0slop / t0step )

      nfree = hntrack_hits(track,1) - hnum_fpray_param

c     if we can't do better than the chi2 we got with t0 = 0., then keep t0 = 0.

      minchi2 = hchi2_fp(track) / float(nfree)

c      write(*,*) 'original chi2=',minchi2

      beststep = -1

c$$$      write(*,*) 'track number=',track
c$$$      write(*,*) 't0min,t0max,t0step,nstep=',t0min,t0max,t0step,nstep
c$$$      write(*,*) 'original chi2=',minchi2

c     count up the number of "too large" and "too small" drift distance hits:

c     initialize h_track_t0best to zero so that it doesn't confuse the histogram with 
c     remembered values from previous events:
      htrack_t0best(track) = 0.

      nlarge = 0
      nsmall = 0

      do ihit=2,hntrack_hits(track,1)+1
         hit = hntrack_hits(track,ihit)
         plane = hdc_plane_num(hit)

         trackpos = hdc_track_coord(track,plane)

         if(hdc_drift_dis(hit).gt.abs(trackpos - hdc_wire_center(hit))) 
     $        then
            nlarge = nlarge + 1
         else if(hdc_drift_dis(hit).lt.abs(trackpos - hdc_wire_center(hit)))
     $           then
            nsmall = nsmall + 1
         endif
      enddo
c     only attempt t0 fitting if there is a clear pattern of most drift distances
c     being too large or too small:

c      write(*,*) 'nlarge,nsmall=',nlarge,nsmall
c      write(*,*) 'mindiff=',h_track_t0_min_ndiff

      if(abs(nlarge - nsmall).ge.h_track_t0_min_ndiff) then 

c          write(*,*) 'nlarge,nsmall=',nlarge,nsmall
c          write(*,*) 'mindiff=',h_track_t0_min_ndiff

c     if drift distances are too large, bias search toward negative t0, 
c     if drift distances are too small, bias search toward positive t0,
         t0min = - t0slop * float(nlarge) / float(nlarge + nsmall)
         t0max = t0slop * float(nsmall) / float(nlarge + nsmall)
      else
         return
      endif

      do istep=0,nstep
         t0 = t0min + float(istep) * t0step

c     loop over hits on the track once and set hdc_wire_coord for each hit:

         do ihit=2,hntrack_hits(track,1)+1
c     get array index of hit
            hit = hntrack_hits(track,ihit)
c     get plane and wire number, needed for drift distance calculation
            plane = hdc_plane_num(hit)
            wire = hdc_wire_num(hit)
c     get drift time of hit and offset by t0
            tdrift = hdc_drift_time(hit) + t0
c     calculate drift distance
            ddrift = h_drift_dist_calc(plane,wire,tdrift)
c     set wire_coord using leftright remembered from h_left_right or 
c     h_redo_track_leftright:
            hdc_wire_coord(hit) = hdc_wire_center(hit) + 
     $           htrack_leftright(track,ihit-1) * ddrift

         enddo ! end loop over hits on the track

c     now do track fitting:
c     vector of constants:
         do i=1,hnum_fpray_param
            TT(i) = 0.
            do ihit=2,hntrack_hits(track,1)+1
               hit = hntrack_hits(track,ihit)
               plane = hdc_plane_num(hit)

               TT(i) = TT(i) + hdc_wire_coord(hit)*
     $              hplane_coeff(remap(i),plane) / 
     $              ( (hdc_sigma(plane))**2 )
            enddo
         enddo
c     matrix of coefficients:
         do i=1,hnum_fpray_param
            do j=1,hnum_fpray_param
               AA(i,j) = 0.
               if(j.lt.i) then
                  AA(i,j) = AA(j,i)
               else
                  do ihit=2,hntrack_hits(track,1)+1
                     hit = hntrack_hits(track,ihit)
                     plane = hdc_plane_num(hit)
                     AA(i,j) = AA(i,j) + hplane_coeff(remap(i),plane) * 
     $                    hplane_coeff(remap(j),plane) / 
     $                    ( (hdc_sigma(plane) )**2 )
                  enddo
               endif ! end test on j<i
            enddo ! end inner loop over j
         enddo ! end outer loop over i
c     matrix inversion:
         call solve_four_by_four(TT,AA,dray,ierr)

         if(ierr.ne.0) then
            dray(1)=10000.
            dray(2)=10000.
            dray(3)=2.
            dray(4)=2.
         else ! got a solution: now calculate the chi2:
            chi2 = 0.
            
            do plane = 1,hdc_num_planes
               track_coord(plane) = 0.
               do j=1,4
                  track_coord(plane) = track_coord(plane) + 
     $                 hplane_coeff(remap(j),plane)*dray(j)
               enddo
            enddo

            do ihit=2,hntrack_hits(track,1)+1
               hit = hntrack_hits(track,ihit)
               plane = hdc_plane_num(hit)
               
               hitpos = hdc_wire_coord(hit)
               trackpos = track_coord(plane)
               residual = hitpos - trackpos
               
               chi2 = chi2 + (residual/hdc_sigma(plane))**2
            enddo
            
            chi2 = chi2 / nfree
            
            if(chi2.lt.minchi2) then
               minchi2 = chi2
               t0best = t0
               beststep = istep
            endif
c            write(*,*) 't0,chi2(t0)=',t0,chi2
         endif ! end test on track fitting error
      enddo ! end loop over t0

      if(beststep.gt.0.and.beststep.lt.nstep) then ! set all the final track variables 
c     go through the track fitting rigmarole one last time:

         do ihit=2,hntrack_hits(track,1)+1
            hit = hntrack_hits(track,ihit)
            plane = hdc_plane_num(hit)
            wire = hdc_wire_num(hit)

            tdrift = hdc_drift_time(hit) + t0best

            ddrift = h_drift_dist_calc(plane,wire,tdrift)
c     final final coordinate
            hdc_wire_coord(hit) = hdc_wire_center(hit) + 
     $           htrack_leftright(track,ihit-1) * ddrift
         enddo
            
c     track fitting:
         do i=1,hnum_fpray_param
            TT(i) = 0.
            do ihit=2,hntrack_hits(track,1)+1
               hit = hntrack_hits(track,ihit)
               plane = hdc_plane_num(hit)
               TT(i) = TT(i) + hdc_wire_coord(hit) * 
     $              hplane_coeff(remap(i),plane) / 
     $              ( ( hdc_sigma(plane) )**2 )
            enddo
         enddo

         do i=1,hnum_fpray_param
            do j=1,hnum_fpray_param
               AA(i,j) = 0.
               if(j.lt.i) then
                  AA(i,j) = AA(j,i)
               else
                  do ihit=2,hntrack_hits(track,1)+1
                     hit = hntrack_hits(track,ihit)
                     plane = hdc_plane_num(hit)
                     AA(i,j) = AA(i,j) + hplane_coeff(remap(i),plane)*
     $                    hplane_coeff(remap(j),plane) / 
     $                    ( (hdc_sigma(plane))**2 )
                  enddo
               endif
            enddo
         enddo
              
         call solve_four_by_four(TT,AA,dray,ierr)

         if(ierr.ne.0) then
            dray(1)=10000.
            dray(2)=10000.
            dray(3)=2.
            dray(4)=2.
         else
            chi2 = 0.
c     we absolutely don't want to alter the original drift times and drift distances for the hits, 
c     since they may not be unique to this track. However, for ntuple storage purposes, we may 
c     want to save the t0 so we can recover the shifted drift times and distances later.
            do plane=1,hdc_num_planes
               hdc_track_coord(track,plane) = 0.
               do j=1,hnum_fpray_param
                  hdc_track_coord(track,plane) = hdc_track_coord(track,plane) + 
     $                 hplane_coeff(remap(j),plane)*dray(j)
               enddo
            enddo
            
            do ihit=2,hntrack_hits(track,1)+1
               hit = hntrack_hits(track,ihit)
               plane = hdc_plane_num(hit)
               
               hitpos = hdc_wire_coord(hit)
               trackpos = hdc_track_coord(track,plane)
               
               residual = hitpos - trackpos

               chi2 = chi2 + (residual/hdc_sigma(plane))**2
               
               hdc_single_residual(track,plane) = residual
            enddo

         endif
         
         hx_fp(track) = dray(1)
         hy_fp(track) = dray(2)
         hz_fp(track) = 0.
         hxp_fp(track) = dray(3)
         hyp_fp(track) = dray(4)

         nfree = hntrack_hits(track,1) - hnum_fpray_param

         hchi2_fp(track) = chi2

         hnfree_fp(track) = nfree

         htrack_t0best(track) = t0best
         
c         write(*,*) 't0best,final chi2=',t0best,chi2 / float(nfree)

c$$$         if(chi2/float(nfree)<100.) then
c$$$            write(*,*) 'track,htrack_t0best(track),chi2=',track,htrack_t0best(track),chi2/float(nfree)
c$$$         endif
      else                      ! no value of t0 was found with a better chi2 than t0 = 0
c     reset hdc_wire_coord to its original value based on t0 = 0
         htrack_t0best(track) = 0.
         do ihit=2,hntrack_hits(track,1)+1
            hit = hntrack_hits(track,ihit)
            hdc_wire_coord(hit) = hdc_wire_center(hit) + 
     $           htrack_leftright(track,ihit-1)*hdc_drift_dis(hit)
         enddo
      endif ! end test on whether a t0 was found that gives better chi2 than t0=0
      
      return
      end
