      subroutine t_test_straw_analyze
      implicit none
      save

*     File: t_test_straw_analyze.f
*     Author: R. Gilman, 20 Dec 1996
*     Modified: 21 Jan 1997 (saw).  Edit for inclusion in Hall C analyzer
*
*     Purpose: decode straw chamber data, prepare for histogramming raw quantites,
*       setup for tracking...
*     Based on simple FPP analysis routines for Hall A
* $Log$
* Revision 1.2  1997/05/20 18:30:36  saw
* (Ron Gilman) Code update
*
* Revision 1.1  1997/05/20 18:28:49  saw
* Initial revision
*
*
*     include files...
      include 't20_test_detectors.cmn'
      include 't20_test_histid.cmn'
      include 't20_reg_polder_structures.cmn'
      include 't20_data_structures.cmn'

      integer*4 lun_calib               ! Set this somewhere else
      parameter (lun_calib=62)

*     local variables
      integer*4 nplane, idiff
      integer*4 i, j, n, kk, ihit, noff, iok
      real*4 pos
*      real*4 slope, bint, pos1, pos2, dpos

*     most intializations have been moved into ctp file
*         terplay/PARAM/t20_test_detectors.param

*     first get hits out of input array and group into local plane hit
*     structures the TDC appears to be LIFO (info from Glen Collins) but the
*     SAW decoder reverses the order into FIFO.  Thus, leading edges should
*     always precede the trailing edges.  This is assumed,to simplify the
*     decoding...
*
*     IF THIS IS NOT THE CASE, JUST GET ELEMENTS OUT OF THE INPUT ARRAYS
*     FROM LAST TO FIRST!
*
*     The time scale is in 1/2 ns! and is left in those units...
*     positions will be in cm and angles will be in radians

*     look through the raw data to associate the edges into hits,
*     the way the decoder works, should get le hit immediately followed by
*     te hit - the decoder reverses the readout order

      call t_test_scint_analyze
      call t_test_straw_initialize
      call t_test_straw_gethits
      if(ttst_raw_tot_hits.le.0)then
        return
      endif
      call t_test_setup_track_input
c      call t_test_throwout_hits
      call t_test_set_trackcode

*     if hits distributed okay, get track and do things with it...

      noff = -3
      do i = 1, 2
         ttst_track_ntracks = 0
         noff = noff + 4
         ttst_nxytrack(i) = 0
         ttst_nxytracktried(i) = 0
        
         if(ttst_straw_xygddmx(i).gt.2 .and.
     +        ttst_straw_xygddmx(i).lt.6 .and.
     +        ttst_straw_xyplnsht(i).gt.2) then

            ttst_nxytracktried(i) = 1
            call trk4(ttst_straw_planes_hit(noff),
     +           ttst_track_hitarray(1,1,noff),
     +           ttst_track_ntracks,
     +           ttst_track_params(1,i) )
            ttst_nxytrack(i) = ttst_track_ntracks
            if(ttst_track_ntracks.eq.1)then
c     increment a single chisq (resolution) histogram
               call hf1(ttst_hid_strawres,ttst_track_params(3,i),1.)
c     work out the good,bad,missing wire hists
               do j = 1, 4
                  nplane = j + noff - 1
                  pos = ttst_track_params(2,i) + 
     +                 ttst_track_params(1,i) *
     +                 (ttst_straw_z0+ttst_straw_z(nplane))
                  pos = pos - ttst_straw_x1(nplane)
                  idiff = nint(pos/ttst_straw_spacing +
     +                 ttst_straw_sctr(nplane))
                  if(i.eq.1)kk = 56*(j-1)
                  if(i.eq.2)kk = 224 + 24*(j-1)
                  if(ttst_straw_planes_hit(nplane).eq.0)then
                     call hf1(ttst_hid_strawhitms,float(kk+idiff),1.)
                  else
                     iok = 0
                     do ihit = 1, ttst_straw_planes_hit(nplane)
                        n = ttst_track_hitarray(1,ihit,nplane)
c     if(n.eq.idiff)then
                        if(n.le.idiff+1 .and. n.ge.idiff-1)then
                           iok = 1
                           call hf1(ttst_hid_strawhitgd,float(kk+n),1.)
c     call hf1(ttst_hid_planeoffs(nplane),0.,1.)
                        else
                           call hf1(ttst_hid_strawhitbd,float(kk+n),1.)
                           n = n-idiff
c     call hf1(ttst_hid_planeoffs(nplane),
c     +                float(n),1.)
                        endif                   !if there are hits on planes
                     enddo                      !loop over hits in plane
                     if(iok.eq.0)then
                        call hf1(ttst_hid_strawhitms,float(kk+idiff),1.)
                     endif                      !if no hit where expected
                  endif                         !if there are hits in plane
               enddo                            !loop over planes
            endif                               !if there is track
         endif                                  !if okay #hits to try to track
      enddo                                     !loop over two coordinates

c     rotate track if needed
      call t_test_project_track

c     work out the track codes from the arrays...
      call hf1(ttst_hid_trackcode, 0., 1.)
      do i = 1, 2
         j = 2*i - 1
         if(ttst_nxytracktried(i).eq.1)then
            call hf1(ttst_hid_trackcode, float(j), 1.)
c            type *, i, ttst_track_chisqcut, ttst_track_params(3,i)
            if(ttst_nxytrack(i).gt.0 .and. 
     +           ttst_track_params(3,i).le.0.2)
     +           call hf1(ttst_hid_trackcode, float(j+1), 1.)
         endif
      enddo
      if(ttst_nxytracktried(1).eq.1 .and. 
     +     ttst_nxytracktried(2).eq.1) then
         call hf1(ttst_hid_trackcode, float(5), 1.)
         if(ttst_nxytrack(1).eq.1 .and. ttst_nxytrack(2).eq.1.and.
     +        ttst_track_params(3,1).le.0.2 .and.
     +        ttst_track_params(3,2).le.0.2 )
     +        call hf1(ttst_hid_trackcode, float(6), 1.)
      endif

      return
      end
c     ----------------------------------------------------------------------
      subroutine t_test_straw_initialize
      implicit none
      save
      include 't20_test_detectors.cmn'
      integer*4 i, j
     
c      ttst_straw_goodedge = 0
      ttst_straw_goodhit = 0
      ttst_straw_gooddemux = 0
      ttst_straw_xgddmx = 0
      ttst_straw_ygddmx = 0
      ttst_straw_xplnsht = 0
      ttst_straw_yplnsht = 0
      do i = 1, ttst_n_straw_planes
        ttst_straw_planes_hit(i) = 0
      enddo
      do i = 1, ttst_n_straw_wgs
        ttst_straw_hits(i) = 0
      enddo

      do i = 1, 2
         ttst_num_oot(i) = 0
         do j = 1, 3
            ttst_track_params(j,i) = -9999.
         enddo
         ttst_track_pos_est(i) = 0.
         ttst_track_angle(i) = -9999.
         ttst_nxytrack(i) = 0
         ttst_num_oot(i) = 0
         ttst_avetim_oot(i) = 0
      enddo
      ttst_track_ntracks = 0

      ttst_stpld_xposdiff = -9999.
      ttst_stpld_yposdiff = -9999.
      ttst_stpld_thposdiff = -9999.
      ttst_stpld_phposdiff = -9999.

      return
      end

c     ----------------------------------------------------------------------
      subroutine t_test_straw_gethits
      implicit none
      save
      include 't20_data_structures.cmn'
      include 't20_test_histid.cmn'
      include 't20_test_detectors.cmn'
      integer*4 lun_calib               ! Set this somewhere else
      parameter (lun_calib=62)
      integer*4 str_group,str_plane,str_ggroup
      integer*4 i,n,oldwg,tim,ilete,oldtim
      integer*4 wgs(50),times(50),wids(50),straws(50),planes(50)
      integer*4 ioot1, nfp, idiff, kk, itype
      
      if(
     +  ttst_reallyraw_out.gt.0 .and.
     +  ttst_reallyraw_out.gt.reallyraw_written ) then
        type *, 't_test_straw_analyze called, tot_hits=',
     +    ttst_raw_tot_hits
        reallyraw_written = reallyraw_written + 1
      endif
      if(ttst_raw_tot_hits.le.0)then
        return
      endif

      if( 
     +  ttst_raw_ntuples_out.gt.0 .and. 
     +  raw_ntuples_written.eq.0 .and.
     +  ttst_raw_tot_hits.gt.0 ) then
        open(unit=lun_calib,file='t20_test_hits.ntuple',status='new')
      endif
     
c     combine edges into pairs of le/te edges...

      nfp = 0
      oldwg = 0
      do i =  ttst_raw_tot_hits, 1, -1
         str_plane = ttst_raw_plane_num(i)
         str_group = ttst_raw_group_num(i)
         str_ggroup = str_group + ttst_straw_plane_group_off(str_plane)
         tim = ttst_raw_tdc(i)
         ilete = iand(tim,'10000'X)
         if(ilete.ne.0)ilete=1
         tim = iand(tim,'FFFF'X)
c     iand generic, jiand 4 byte, iiand 2 byte
         if(
     +        ttst_reallyraw_out.gt.0 .and.
     +        ttst_reallyraw_out.gt.reallyraw_written ) then
            type *,i,ttst_raw_plane_num(i),ttst_raw_group_num(i),
     +           str_group,ilete,tim
         endif
         if(ilete.eq.0)then
            oldwg = str_ggroup
            oldtim = tim
         else
c     only get trailing edge immediately after a LE
            if(str_ggroup.eq.oldwg)then          !same group :)
               nfp = nfp + 1
               if(nfp.gt.50)nfp = 50
               planes(nfp) = str_plane
               wgs(nfp) = oldwg
               times(nfp) = oldtim
               wids(nfp) = oldtim - tim
c     demux here and now!
               idiff = 0
               do kk = 1, 9
                  if(wids(nfp).gt.ttst_dmx(kk,str_ggroup)) idiff=kk
               enddo
               if(idiff.gt.0.and.idiff.le.8) then
                  itype = ttst_straw_type(str_plane)
                  idiff = ttst_type_order(idiff,itype)
                  straws(nfp) = 8*(str_group-1) + idiff
               else
                  nfp = nfp - 1
               endif
c     done demux!
               if(
     +              ttst_raw_ntuples_out.gt.0 .and.
     +              raw_ntuples_written.lt.ttst_raw_ntuples_out ) then
                  raw_ntuples_written = raw_ntuples_written + 1
                  write(lun_calib,'(3f6.0)') float(oldwg),
     +                 float(oldtim),float(wids(nfp))
                  if(raw_ntuples_written.eq.ttst_raw_ntuples_out)then
                     write(6,*)' t_test_straw_analyze wrote',
     +                    raw_ntuples_written,' ntuples'
                     raw_ntuples_written = raw_ntuples_written + 1
                  endif
               endif
               call hf1(ttst_hid_straw_wletdc,float(oldtim),1.)
               call hf1(ttst_hid_straw_letdc,float(oldtim),1.)
            endif
         endif
      enddo

c     have found all le/te pairs, save good and bad (oot=out of time) hits

      do i =  1, nfp
         if(times(i).ge.ttst_TDC_min .and. times(i).le.ttst_TDC_max)then
            n = ttst_straw_hits(wgs(i)) + 1
            if(n.gt.8)n = 8
            ttst_straw_hits(wgs(i)) = n
            ttst_straw_tdc(n,wgs(i)) = times(i)
            ttst_straw_wid(n,wgs(i)) = wids(i)
            ttst_straw_num(n,wgs(i)) = straws(i)
            ttst_straw_goodhit = ttst_straw_goodhit + 1
            call hf2(ttst_hid_dmxchck,float(wgs(i)),float(wids(i)),1.)
            call hf1(ttst_hid_straw_width,float(wids(i)),1.)
            call hf1(ttst_hid_wg,float(wgs(i)),1.)
            n = 8*ttst_straw_plane_group_off(planes(i)) + straws(i)
            call hf1(ttst_hid_strawmap,float(n),1.)
         else
            ioot1 = 0
            do n = 1, 2
               if(ioot1.eq.0)then
                  if(ttst_num_oot(n).eq.0)then
                     ttst_num_oot(n) = 1
                     ttst_avetim_oot(n) = times(i)
                     ttst_wg_oot(1,n) = wgs(i)
                     ttst_tim_oot(1,n) = times(i)
                     ttst_wid_oot(1,n) = wids(i)
                     ttst_str_oot(1,n) = straws(i)
                     ttst_pln_oot(1,n) = planes(i)
                     ioot1 = 1
	          else
                     if(times(i).gt.ttst_avetim_oot(n)-200 .and.
     +	                times(i).lt.ttst_avetim_oot(n)+200) then
                        ttst_num_oot(n) = ttst_num_oot(n) + 1
                        if(ttst_num_oot(n).lt.11)then
                           ttst_wg_oot(ttst_num_oot(n),n) = wgs(i)
                           ttst_tim_oot(ttst_num_oot(n),n) = times(i)
                           ttst_wid_oot(ttst_num_oot(n),n) = wids(i)
                           ttst_str_oot(ttst_num_oot(n),n) = straws(i)
                           ttst_pln_oot(ttst_num_oot(n),n) = planes(i)
                        endif
                        ioot1 = 1
                     endif
                  endif
               endif
            enddo
         endif
      enddo

      call t_test_oot_track
      
      return
      end
c     ----------------------------------------------------------------------
      subroutine t_test_oot_track
      implicit none
      save
      include 't20_test_detectors.cmn'
      include 't20_test_histid.cmn'
      integer*4 i, j, k, np
      integer*4 nxy(8), nxyt(2), ntf
      real*4 z(8), xy(8), t, avexy(2)
      real*4 rcept(2), slope(2)
      real*4 xbar, ybar, xsqbar, xybar, diff
    
*     this routine attempts to track 1-2 oot tracks using wire positions only
*     try to track oot hits if a likely-to-be-trackable number of them
*     cannot get hits out quite like do for usual tracking...


      do i = 1, 2
         if(ttst_num_oot(i).gt.10)ttst_num_oot(i)=10
         if(ttst_num_oot(i).gt.6)then
            do j = 1, 8
               nxy(j) = 0
               xy(j) = 0.
               enddo
            do j = 1, ttst_num_oot(i)
               np = ttst_pln_oot(j,i)
               if(nxy(np).eq.0)then
                  nxy(np) = 1
                  z(np) = ttst_straw_z(np) + ttst_straw_z0
                  xy(np) = 
     +                 (ttst_str_oot(j,i)-ttst_straw_sctr(np)) *
     +                 ttst_straw_spacing + ttst_straw_x1(np)
               else                             !boot, 1 hit/plane unless close
                  t = (ttst_str_oot(j,i)-ttst_straw_sctr(np)) *
     +                 ttst_straw_spacing + ttst_straw_x1(np)
                  if(abs(t-xy(np)).gt.2.5) then
                     if(nxy(np).eq.1) nxy(np) = 0
                  else
                     xy(np) = xy(np)*nxy(np) + t
                     nxy(np) = nxy(np)+1
                     xy(np) = xy(np) / nxy(np)
                  endif
               endif
            enddo
c     have accumulated all of the x and y hits... make sure all
co    hits close together, and if enough try to track...
c            type *,'accumulated all oots into tracking arrays...'
c            type *,'num_oots=',ttst_num_oot(i)
c            type *,(nxy(j),j=1,8)
c            type *,(z(j),j=1,8)
c            type *,(xy(j),j=1,8)
            ntf = 0
            do j = 1, 2
               slope(j) = -999.
               rcept(j) = -99999999.
               avexy(j) = 0.
               nxyt(j) = 0
               do k = 4*j-3, 4*j
                  if(nxy(k).gt.0)then
                     avexy(j) = avexy(j) + xy(k)
                     nxyt(j) = nxyt(j) + 1
                  endif
               enddo
               if(nxyt(j).gt.0)avexy(j) = avexy(j)/nxyt(j)
               nxyt(j) = 0
               do k = 4*j-3, 4*j
                  if(abs(xy(k)-avexy(j)).gt.2.0)nxy(k) = 0
                  if(nxy(k).gt.0)nxyt(j) = nxyt(j) + 1
               enddo
c               type *,'nxyt=',nxyt(j)
               if(nxyt(j).gt.2)then
                  xbar   = 0
                  ybar   = 0
                  xsqbar = 0
                  xybar  = 0
                  do k = 4*j-3, 4*j
                     if(nxy(k).gt.0)then
                        xbar   = xbar + z(k)
                        xsqbar = xsqbar + z(k)**2
                        ybar   = ybar + xy(k)
                        xybar  = xybar + z(k)*xy(k)
                     endif
                  enddo
                  diff   = nxyt(j)*xsqbar - xbar**2
                  if(diff.ne.0.)then
                     slope(j)  = (nxyt(j)*xybar - xbar*ybar) / diff
                     rcept(j)  = -1.*(xybar*xbar-xsqbar*ybar) / diff
                     ntf = ntf + 1
c     histogram x, y here!
                     xbar = rcept(j) + slope(j)*
     +                    (ttst_straw_z0+ttst_straw_zchmbr)
                     if(j.eq.1)then
                        call hf2(ttst_hid_oot_thvx,xbar,slope(j),1.)
                     else
                        call hf2(ttst_hid_oot_phvy,xbar,slope(j),1.)
                     endif
                  endif
               endif
            enddo
            if(ntf.eq.2)then
c     histogram x_vs_y here
               call hf2(ttst_hid_oot_yvx,rcept(1),rcept(2),1.)
            endif
         endif
      enddo

      return
      end
c     ----------------------------------------------------------------------
      subroutine t_test_setup_track_input
      implicit none
      save
      include 't20_test_detectors.cmn'
      include 't20_test_histid.cmn'
      integer*4 i, j, k, ihit, drift_time
      integer*4 ngroup, nplane, noff, itype
      real*8 avepos(8), ave, driftdistance

*      now have captured into the arrays the pairs of hits...
*      count up planes hit, also work out demultiplex and straws hit...
*      increment the arrays that will be used for tracking
*      have also ordered everything by global wiregroup, and thus
*      by  plane number

      do i = 1, 8
         avepos(i) = 0.
      enddo
      do ngroup=1, ttst_n_straw_wgs
         if(ttst_straw_hits(ngroup).gt.0)then
            nplane = ttst_plane_of_group(ngroup)
            noff = 8*(ngroup-ttst_straw_plane_group_off(nplane)-1)
            itype = ttst_straw_type(nplane)
            do i = 1, ttst_straw_hits(ngroup)
               ttst_straw_planes_hit(nplane) =
     +              ttst_straw_planes_hit(nplane) + 1
               ihit = ttst_straw_planes_hit(nplane)
               if(ihit.gt.max_track_hit) ihit=max_track_hit
               ttst_straw_gooddemux = ttst_straw_gooddemux + 1
c     have a good hit and a good demux, so store it for tracking!
c     hitarray 1=straw #, 2=drift distance 3=z of plane 4=x/y of straw wire
               ttst_track_hitarray(1,ihit,nplane) =
     +              ttst_straw_num(i,ngroup)
               ttst_track_hitarray(3,ihit,nplane) =
     +              ttst_straw_z(nplane) + ttst_straw_z0
               ttst_track_hitarray(4,ihit,nplane) =
     +              (ttst_straw_num(i,ngroup)-ttst_straw_sctr(nplane))
     +              * ttst_straw_spacing + ttst_straw_x1(nplane)
               avepos(nplane) = avepos(nplane) + 
     +            ttst_track_hitarray(4,ihit,nplane)
*     readjust tim with tim offset... 
*     do not do above, affects writing raw ntuples
*     the readjustment could move us out of the drift table range, so ensure
*     it does not!
               ttst_straw_tdc(i,ngroup) = ttst_straw_tdc(i,ngroup)
     +              - ttst_t0(ngroup)
               drift_time = ttst_straw_tdc(i,ngroup)-ttst_TDC_min
               if(drift_time.lt.1)drift_time = 1
               if(drift_time.gt.400)drift_time = 400
               driftdistance = 
     +              ttst_drift_max * ttst_drift_table(drift_time)
               ttst_track_hitarray(2,ihit,nplane) = driftdistance
               call hf1(ttst_hid_driftdist,
     +              ttst_track_hitarray(2,ihit,nplane),1.)
               call hf1(ttst_hid_drifttime,float(drift_time),1.)
*     find ``real'' drift time, and use simple constant velocity, for
*     comparison to better table algoithym
               drift_time = ttst_drift_t0-ttst_straw_tdc(i,ngroup)
               ttst_track_dxpos2(ihit,nplane) =
     +              ttst_drift_v * drift_time
               if(ttst_track_dxpos2(ihit,nplane).gt.ttst_drift_max)
     +              ttst_track_dxpos2(ihit,nplane) = ttst_drift_max
               if(ttst_track_dxpos2(ihit,nplane).lt. 0.)
     +              ttst_track_dxpos2(ihit,nplane) = 0.
               call hf1(ttst_hid_driftdistv0,
     +              ttst_track_dxpos2(ihit,nplane),1.)
               call hf1(ttst_hid_drifttimet0,float(drift_time),1.)
*     last chance to remove hit if drift time/distance not sufficiently
*     good... keep it in center 99% of te drift range... or  remove
               if(driftdistance.lt.0.005*ttst_drift_max .or.
     +              driftdistance.gt.0.995*ttst_drift_max) then
                  ttst_straw_planes_hit(nplane) =
     +                 ttst_straw_planes_hit(nplane) - 1
                  ttst_straw_gooddemux = ttst_straw_gooddemux - 1
               endif
            enddo                               ! loop over # hits      
         endif                                  ! if   any straws in group hit
      enddo                                     ! loop over wiregroups in plane

*     work out average position in x, y, and
*     make new summary plot of hits on planes...
*     add up # of x, y hits, xyplaneshit, total # of hits: for later use

      do i = 1, 2
        ave = 0
        j = 0
        do nplane = 4*i-3, 4*i
          j = j + ttst_straw_planes_hit(nplane)
          ave = ave + avepos(nplane)
          k = 10*(nplane-1)+ ttst_straw_planes_hit(nplane)
          call hf1(ttst_hid_numhitsonplanes,float(k),1.)
          ttst_straw_xygddmx(i) = ttst_straw_xygddmx(i) + 
     +         ttst_straw_planes_hit(nplane)
          if(ttst_straw_planes_hit(nplane).gt.0)
     +         ttst_straw_xyplnsht(i) = ttst_straw_xyplnsht(i) + 1
        enddo
        if(j.ne.0)ttst_track_pos_est(i) = ave/j
        call hf1(ttst_hid_xoryhits,float(ttst_straw_xygddmx(i)),1.)
      enddo

*     adjust positions os x,y's to account for plane to plane rotations...
*     assume small angle ==> theta = sin(theta) = tan(theta)
*     (correction good to 0.3% even for 100 mr)

      do i = 1, 2
         do nplane = 4*i-3, 4*i
            if(ttst_straw_planes_hit(nplane).gt.0)then
               do ihit = 1, ttst_straw_planes_hit(nplane)
                  ave = 0.001 * ttst_rotate_xyplane(nplane) *
     +                 ttst_track_pos_est(3-i)
c                  if(ttst_straw_xygddmx(i).le.5)
c     +              type *,nplane,ttst_track_hitarray(4,ihit,nplane),ave
                  ttst_track_hitarray(4,ihit,nplane) =
     +                 ttst_track_hitarray(4,ihit,nplane) + ave
               enddo
            endif
         enddo
      enddo

      return
      end

c     ----------------------------------------------------------------------
      subroutine t_test_set_trackcode
      implicit none
      save
      include 't20_test_detectors.cmn'
      include 't20_test_histid.cmn'
c      integer*4 i

c      do i = 1, 2

c        ttst_track_code = 0
c        if(ttst_straw_xygddmx(i).lt.3)ttst_track_code = 1
c        if(ttst_straw_xygddmx(i).eq.3)then
c          if(ttst_straw_xyplnsht(i).lt.3)then
c            ttst_track_code = 2
c          else
c            ttst_track_code = 3
c          endif
c        endif
        
c        if(ttst_straw_xygddmx(i).eq.4)then
c          if(ttst_straw_xyplnsht(i).lt.4)then
c            ttst_track_code = 4
c          else
c            ttst_track_code = 5
c          endif
c        endif
        
c        if(ttst_straw_xygddmx(i).eq.5)then
c          if(ttst_straw_xyplnsht(i).lt.4)then
c            ttst_track_code = 6
c          else
c            ttst_track_code = 7
c          endif
c        endif
        
c        if(ttst_track_code.eq.0)ttst_track_code = 8
c        ttst_track_code = ttst_track_code + 10*(i-1)
c        call hf1(ttst_hid_trackcode, float(ttst_track_code), 1.)
c      enddo
        
      return
      end
c     ----------------------------------------------------------------------
c     rotate / offset chamber track into polder coordinate system...
      subroutine t_test_project_track
      implicit none
      save
      include 't20_test_detectors.cmn'
      integer*4 i
      real*8 xych(2)

      do i = 1, 2
         ttst_track_angle(i) = atan(ttst_track_params(1,i))
     +        + ttst_rotate_ang(i)
         ttst_track_chmbrpos(i) = ttst_track_params(2,i) +
     +        ttst_track_params(1,i) *
     +        (ttst_straw_zchmbr+ttst_straw_z0) +
     +        ttst_straw_xoff(i)
      enddo
      xych(1) = cos(ttst_rotate_ang(3)) * ttst_track_chmbrpos(1) +
     +     sin(ttst_rotate_ang(3)) * ttst_track_chmbrpos(2)
      xych(2) = cos(ttst_rotate_ang(3)) * ttst_track_chmbrpos(2) -
     +     sin(ttst_rotate_ang(3)) * ttst_track_chmbrpos(1)
      do i = 1, 2
         ttst_track_chmbrpos(i) = xych(i)
         ttst_track_params(1,i) = tan(ttst_track_angle(i))
         ttst_track_params(2,i) = ttst_track_chmbrpos(i) -
     +        ttst_track_params(1,i) *
     +        (ttst_straw_zchmbr+ttst_straw_z0)
         ttst_track_scintpos(i) = ttst_track_params(2,i) +
     +        ttst_track_params(1,i) * ttst_straw_zscint
      enddo

c     note that chamber pos is measured in z relative to the _z0 parameter,
c     but scintillator position (POLDER start scntillator that is) is measured
c     relative to the origin of the coordinate system, the POLDER target

      return
      end

c     ----------------------------------------------------------------------

      subroutine t_test_stpld_comp
      implicit none
      save
      include 't20_data_structures.cmn'
      include 't20_test_detectors.cmn'
      include 't20_test_histid.cmn'
      include 't20_reg_polder_structures.cmn'

      integer*4 opened/0/
      integer*4 lun_calib               ! Set this somewhere else
      parameter (lun_calib=63)
      
c     need to compare tracks to polder tracks....
      ttst_good_comp = 0
      ttst_stpld_xposdiff = ttst_track_params(2,1) - tdeuton1_x(3)
      ttst_stpld_yposdiff = ttst_track_params(2,2) - tdeuton1_y(3)
      ttst_stpld_thposdiff = ttst_track_angle(1) - atan(tdeuton1_dir(1))
      ttst_stpld_phposdiff = ttst_track_angle(2) - atan(tdeuton1_dir(2))
      
*     write out track nutples only for 4x + 4y = 8 hit events?
c      type *,'nt trk write:',ttst_straw_xgddmx,ttst_straw_ygddmx
c      type *,ttst_straw_xplnsht,ttst_straw_yplnsht
c      type *,tdeuton1_x(1),tdeuton1_dir(1)
c      type *,tdeuton1_y(1),tdeuton1_dir(2)
      if(ttst_straw_xgddmx.lt.4)return
      if(ttst_straw_ygddmx.lt.4)return
      if(ttst_straw_xplnsht.lt.4)return
      if(ttst_straw_yplnsht.lt.4)return
c     if the polder chambers do not track, they will give positions of 0.,0.
c     so let's make sure as first step that neither chamber gives 0's in
c     both x and y...
c     the way the algorithym appears to work, an angle is calculated even if
c     a chamber does not track
      if(tdeuton1_x(1).eq.0. .and. tdeuton1_y(1).eq.0.)return
      if(tdeuton1_x(2).eq.0. .and. tdeuton1_y(2).eq.0.)return
      if(tnbpartch.ge.2)return	  !looks like >= 2 particles in polder mwpcs
      if(abs(tdeuton1_x(1)).gt.10.)return
      if(abs(tdeuton1_x(2)).gt.10.)return

      ttst_good_comp = 1

      if( ttst_track_ntuples_out.le.0) return
      if( track_ntuples_written.gt.ttst_track_ntuples_out) return
      if( track_ntuples_written.eq.ttst_track_ntuples_out) then
        write(6,*)' t_test_straw_analyze wrote',
     +    track_ntuples_written,'track ntuples'
        track_ntuples_written = track_ntuples_written + 1
      endif
      if( opened.eq.0 ) then
        opened = 1
      open(unit=lun_calib,file='t20_test_track.ntuple',status='new')
      endif

      track_ntuples_written = track_ntuples_written + 1
c      type *,track_ntuples_written,' trks written'
      write(lun_calib,'(4(f7.2,f7.4))')
     +  tdeuton1_x(1),tdeuton1_dir(1),tdeuton1_y(1),tdeuton1_dir(2),
     +  ttst_track_chmbrpos(1),ttst_track_params(1,1),
     +  ttst_track_chmbrpos(2),ttst_track_params(1,2)

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      following is the tracking routine, copied over and slightly modified
c      from the Rutgers / LA analysis... all variables in this routine will
c      be left as local variables, all histogramming, etc., will be done above
c
c       the following subroutine trk4 is taken from LA analysis code by RG
c       modified by RG 1/30/97 to use for t20 analysis
c	note that this routine assumes the hits come in ordered, all hits
c       on a plane are together, and wire number is monotonic 
c       
       subroutine trk4(nhit,hit,ntrack,track)
c       input:
       integer*4 nhit(*)          !number of hits(plane)
       real*4 hit(4,10,4)         !hit info(info type, hit on plane, plane)
c                     hit(1,x,y)=wire#
c			 (2,x,y)=drift distance
c			 (3,x,y)=z position
c			 (4,x,y)=x position
c                        (5,x,y)=residual, track to (pos+/-dpos)?
c	              hit(5,x,y)=group (for residual histing) - not used here
c       output:
       integer*4 ntrack           !# of tracks
       real*4 track(3)          !track information
c                     track(1,i)=parameter a in az+b
c                          (2,i)=parameter b in az+b
c                          (3,i)=chi2 (actually just a rms)
c                          (4,i)=hit channel in x1   0. if not used, 
c                          (5,i)=hit channel in x2   positive if L-side hit
c                          (6,i)=hit channel in x3   negative if R-side hit
c                          (7,i)=hit channel in x4
c     the second index above was to handle multiple tracks, removing
       common /effarray/nhgood(80,4), nhbad(80,4), sigbad(80,4)
c       local variables:
       integer*4 lrfit(10)
       integer*4 iok3, iok43, iok4, utra(10), nwtra(10), nptra(10)
       integer*4 ufnd(10,3), lrfnd(10,3), lrf3(10)
       real*4    chfnd(3), sfnd(3), xfnd(3)
       real*4    ztra(10), xtra(10), dxtra(10), dxfit(10)
       real*4    chi0, chif, chf3(10)
       real*4    xmin, xmax
       integer*4 nhitx
c
c       initialize
       iok3 = -1
       iok4 = -1
       iok43 = -1
       chif = -1.
       xat0 = -1000.
       slope = -1000.
       track(1) = slope
       track(2) = xat0
       track(3) = chif
c
c       start analysis
       ntrack = 0
       nhitx = nhit(1)+nhit(2)+nhit(3)+nhit(4)
       nhitx0 = nhitx
       nplane = 0
       do i = 1, 4
         if(nhit(i).gt.0)nplane = nplane + 1
         enddo
c
c       bag out if too few hits / too few planes
       if(nhitx.lt.3 .or. nplane.lt.3)return
c
c       fill arrays
       k = 0
       xmin = 99.
       xmax = -99.
       do i=1, 4
         if(nhit(i).gt.0)then
           do j = 1, nhit(i)
             k = k + 1
             ztra(k) = hit(3,j,i)
             xtra(k) = hit(4,j,i)
             dxtra(k) = hit(2,j,i)
             utra(k) = 1
             dxfit(k) = 0.
             nwtra(k) = hit(1,j,i)
c             nptra(k) = 1 + nint( 0.33333*(ztra(k)-zoff) )
             nptra(k) = i
             if(nptra(k).gt.4 .or. nptra(k).lt.1)nptra(k) = 0
             if(xtra(k).lt.xmin)xmin = xtra(k)
             if(xtra(k).gt.xmax)xmax = xtra(k)
             enddo
           endif
         enddo
c
1       continue              !come here if remove hits
c
c       analyze 3 hit events, require 3 planes to be hit
       if(nhitx.eq.3)then
         chi0 = 0.0
         chif = 0.0
         call trackit
     +     (nhitx,ztra,xtra,dxtra,utra,dxfit,lrfit,xat0,slope,chi0,chif)
         if(chif.gt.0.)then
           chif = sqrt(chif/3.)
           else
           chif = 0.
           endif
         if(chi0.gt.0.)then
           chi0 = sqrt(chi0/3.)
           else
           chi0 = 0.
           endif
         endif
c       analyze tracks with 4 hits on 3 planes - straight line + arc
       if(nhitx.eq.4 .and. nplane.eq.3)then
         do i = 1, 3
           if(ztra(i).eq.ztra(i+1))j = i
           enddo
c       if two hits on adjacent wires, fit as if 4 planes
         if( abs(xtra(j)-xtra(j+1)) .lt. 1.1 )then       !adjacent wires
           iok4 = 3
           call trackit
     +    (nhitx,ztra,xtra,dxtra,utra,dxfit,lrfit,xat0,slope,chi0,chif)
           if(chif.gt.0.)then
             chif = sqrt(chif/4.)
             else
             chif = 0.
             endif
           if(chi0.gt.0.)then
             chi0 = sqrt(chi0/4.)
             else
             chi0 = 0.
             endif
           go to 2
           endif
c       try to delete worse of two hits -- one that gives worse straight
c       line fit
         nhitx = 3
         utra(j) = -1
         call trackit
     +   (nhitx,ztra,xtra,dxtra,utra,dxfit,lrfit,xat0,slope,chi0,chif)
         chisave = chi0
         utra(j) = 1
         utra(j+1) = -1
         call trackit
     +   (nhitx,ztra,xtra,dxtra,utra,dxfit,lrfit,xat0,slope,chi0,chif)
         if(chisave.lt.chi0)then
           utra(j) = -1
           utra(j+1) = 1
           endif
         iok43 = 1
         go to 1
         endif

2       continue

c       analyze 4 hit / 4 plane events
       if(nhitx.eq.4 .and. nplane.eq.4)then
         call trackit
     +   (nhitx,ztra,xtra,dxtra,utra,dxfit,lrfit,xat0,slope,chi0,chif)
         if(chif.gt.0.)then
           chif = sqrt(chif/4.)
           else
           chif = 0.
           endif
         if(chi0.gt.0.)then
           chi0 = sqrt(chi0/4.)
           else
           chi0 = 0.
           endif
c       if chisq large, see if should eliminate one of the hits
         if(chif.gt.0.15)then              !more than four x's nominal chisq
           nhitx = 3
           chmin = 9999999.
           nmin = 0
           do i = 1, 4
             utra(i) = -1
             call trackit
     +    (nhitx,ztra,xtra,dxtra,utra,dxf3,lrf3,x03,slop3,ch03,chf3(i))
             chf3(i) = sqrt(chf3(i)/3.)
             utra(i) = 1
             if(chf3(i).lt.chmin)then
               chmin = chf3(i)
               nmin = i
               endif
             enddo
           if(nmin.ne.0.)then
             iok3 = 1
             do i = 1, 4
               if(i.ne.nmin .and. chf3(i).lt.1.5*chmin)iok3 = -1
               enddo
             if(iok3.eq.1)then
               utra(nmin) = -1
               iok3 = -1
               iok43 = 1
               go to 1
               endif
             endif
           nhitx = 4
           endif
         endif

c
c       5 or 6 hit events: require track to have nhits, nhits-1, or nhits-2
c              ==> no 3+3 two tracks found for nhitx=6
c                     1 track w/ double on one plane
c                     1 track w/ 2 accidentals (3 or 4 planes)
c                     2 tracks sharing 1 hit (3 or 4 planes)
c       increase and let it consider 7,8 hit events...
       if(nhitx.ge.5 .and. nhitx.le.10)then
         do i = 1, nhitx-1
           if(ztra(i).eq.ztra(i+1) .and.
     +        abs(xtra(i)-xtra(i+1)) .gt. 1.1) go to 5
           enddo
c                     try for nhitx hit track if same plane hits adjacent
         call trackit
     +   (nhitx,ztra,xtra,dxtra,utra,dxfit,lrfit,xat0,slope,chi0,chif)
         if(chif.gt.0.)then
           chif = sqrt(chif/nhitx)
           else
           chif = 0.
           endif
         if(chi0.gt.0.)then
           chi0 = sqrt(chi0/nhitx)
           else
           chi0 = 0.
           endif
         if(chif.lt.0.15)go to 3
c                     try for 4 hit track otherwise, or if 5 hit chisq bad
5         nhitx = nhitx - 1
         chmin = 1.5
         nmin = 0
         do i = 1, nhitx0
           utra(i) = -1
           call trackit
     +     (nhitx,ztra,xtra,dxtra,utra,dxf3,lrf3,x03,slop3,ch03,chf3(i))
           chf3(i) = sqrt(chf3(i)/nhitx)
           if(chf3(i).lt.chmin)then
             chmin = chf3(i)
             nmin = i
             chfnd(1) = chf3(nmin)
             do l = 1, nhitx0
               ufnd(l,1) = utra(l)
               lrfnd(l,1) = lrf3(l)
               enddo
             xfnd(1) = x03
             sfnd(1) = slop3
             endif
           utra(i) = 1
           enddo
         if(nmin.ne.0.)then
           chif = chfnd(1)
           do l = 1, nhitx0
             utra(l) = ufnd(l,1)
             lrfit(l) = lrfnd(l,1)
             enddo
           xat0 = xfnd(1)
           slope = sfnd(1)
           go to 3
           endif
c       do following analysis for 5,6 hits only. For more hits, have
c       to get good fit from all or all - 1
         if(nhitx0.gt.6)return
c       if no good 4/5 hit tracks, try for good 3 hit track
c       require each hit to be on different plane
c       if 2 good 3 hit tracks, keep only one with better chisq
c       above comments similar for nhitx0 = 6
c       loop over 2 hits to remove...
         nhitx = nhitx - 1
         chmin = 1.5
         nmin = 0
         do i = 1, nhitx0-1
           do j = i+1, nhitx0
             do k = 1, nhitx0-1
               do l = k+1, nhitx0
                 if(k.ne.i .and. k.ne.j .and. l.ne.i .and. l.ne.j)then
                   if(ztra(k).eq.ztra(l))go to 4
                   endif
                 enddo
               enddo
             do l = 1, nhitx0
               if(l.eq.i .or. l.eq.j)then
                 utra(l) = -1
                 else
                 utra(l) = 1
                 endif
               enddo
             call trackit
     +     (nhitx,ztra,xtra,dxtra,utra,dxf3,lrf3,x03,slop3,ch03,chf3(1))
             chf3(1) = sqrt(chf3(1)/nhitx)
             if(chf3(1).lt.chmin .and. nmin.le.1)then
               nmin = nmin + 1
               chfnd(nmin) = chf3(1)
               do l = 1, nhitx0
                 ufnd(l,nmin) = utra(l)
                 lrfnd(l,nmin) = lrf3(l)
                 enddo
               xfnd(nmin) = x03
               sfnd(nmin) = slop3
               endif
4             continue
             enddo
           enddo
         if(nmin.eq.0)then
           return
           endif
         if(nmin.eq.2)then                     !select better of 2 tracks
           if(chfnd(1).lt.chfnd(2))nmin = 1
           endif
          xat0 = xfnd(nmin)
         slope = sfnd(nmin)
         chif = chfnd(nmin)
         do l = 1, nhitx0
           utra(l) = ufnd(l,nmin)
           lrfit(l) = lrfnd(l,nmin)
           enddo
         endif
3       continue         

c       can have 2 tracks if 6 or more hits
c       bag out for now if more than 4
       if(nhitx.gt.10)then
         return
         endif

c      following was setup for topdrawer plot, remove it
c       put positions at each plane into array
c        do i = 1, 4
c          z(i) = zoff + zplane(i)
c          x(i) = xat0 + slope*z(i)
c          enddo

c       work out good / bad statistics
c       do i = 1, nhitx0
c         if(nptra(i).ne.0)then
c         if(utra(i).eq.1)then
c           nhgood(nwtra(i),nptra(i)) =
c     +       nhgood(nwtra(i),nptra(i)) + 1
c           else
c           if(x(nptra(i)).gt.-50.)then
c             nhbad(nwtra(i),nptra(i)) =
c     +         nhbad(nwtra(i),nptra(i)) + 1
c             if(x(nptra(i)).gt.xtra(i))then
c               sigbad(nwtra(i),nptra(i)) =
c     +           sigbad(nwtra(i),nptra(i)) +
c     +           x(nptra(i)) - xtra(i) - dxtra(i)
c               else
c               sigbad(nwtra(i),nptra(i)) =
c     +            sigbad(nwtra(i),nptra(i)) +
c     +            x(nptra(i)) - xtra(i) + dxtra(i)
c               endif
c             endif
c           endif
c           endif
c         enddo

c       hist chi sq -- actually sigma in units of mm....
c       call hf1(653, chi0, 1.)
c       call hf1(655, chif, 1.)


c       histogram the residuals...on 4hit tracks only
c       see sigbad stuff above for times when not hit
c         k = 0
c         do i = 1, 4
c           if(nhit(i).gt.0)then
c             do j=1, nhit(i)
c               k = k + 1
c               if(utra(k).eq.1)then
c                 call hf1(612+i, dxfit(k), 1.)
c                 igr = hit(5,j,i)
c                 call hf1(616+igr, dxfit(k), 1.)
c                 endif
c               enddo
c             endif
c           enddo

c       store results of one track fit in track(*,*) array
       ntrack     = 1
       track(1) = slope
       track(2) = xat0
       track(3) = chif

c       k = 0
c       do i = 1, 4
c         track(3+i) = 0.
c         if(nhit(i).eq.1)then
c           k = k + 1
c           if(lrfit(k).ne.0)track(3+i) = hit(1,1,i) * lrfit(k)
c           endif
c         if(nhit(i).gt.1)then
c           do j = 1, nhit(i)
c             k = k + 1
c             if(lrfit(k).ne.0 .and. track(3+i).eq.0.)
c     +           track(3+i) = hit(1,1,i) * lrfit(k)
c             enddo
c           endif
c         enddo
c
       return
c
c       all tracks need to have >=3 hits
c
c       accpt varies across chamber, but is generally only a little
c       larger than 45 degrees - set 60 degrees to maximum, ==> ~+/-6cm
c       from one plane to the next to determine which hits are in road
c
c       technique is to find set of 3 hits
c       see if can include more as part of track
c       try to put track to remaining hits
c       -avoid repeating set of hits
c       -find best fit set of tracks in some sense
c
       end

       subroutine trackit(nt,zt,xt,dxt,ut,dxf,lrf,x0,tht,chi0,chif)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       subroutine track(ntr,ztra,xtra,dxtra,dxfit,lr,x0,thtx,chibest)
c
c       fits hits nominal positions z, x, xdrift to a straight line
c       x = mz + b
c
c       ntr      number of hits
c       ztra     z positions of track hits (cm)
c       xtra     x positions of track hits (cm)
c       dxtra    drift distances for track (cm)
c       dxfit    (fit distance from wire - drift distance from wire) (cm)
c       lr       ``left/right'' of track at each wire
c       x0       b of the fit (cm)
c       thtx     arctan(m) from the fit (mr)
c       chibest  best fit chi square, assuming sigma = 0.1 (cm, if all else is)
c
c       first find best fit using wire positions only, to get starting estimate
c                 of track
c       then loop over each combination of l/r's wth drift distances, to get
c                 best fit for each combination, leading to best fit
c       iterate using previous theta values to start another fit to improve
c                 theta; it appears wires + 2 iterations with drift distances
c                 are quite enough.
c
c       subroutine track(nt,zt,xt,dxt,dxf,lrf,x0,tht,chi0,chif)
       implicit none
       real zt(10), xt(10), dxt(10), dxf(10), x0, tht, chi0, chif
       integer nt, lrf(10), nlrin
       integer lr(10), i, j, ibetin, ibetter
       integer nloop, iloop(10)
       integer nlr, ju(10), ut(10), ninput
       real    xx(10), zz(10), dxx(10), del(10)
       real    driftfit(10)
       real    xbar, ybar, xsqbar, xybar
       real    slope, ctg, stg, rcept
       real    chisq, dx2, chibest
       real    chiterm(10), diff
       real    sigma/1.0/, pi/3.1415926/

c                     first move input data into local arrays
       j = 0
       do i = 1, nt
900      j = j + 1
        if(j.gt.10)then
          write(*,*)' err in trackit -- found',i-1,' of',nt,
     +      ' hits w/',j-1,' inputs'
          chi0 = 9999999.
          chif = 9999999.
          return
          endif
         if(ut(j).ne.1)go to 900
         lr(i) = 0
         ju(i) = j
         enddo
       ninput = j
       nlr = nt
       nlrin = nlr
       chi0 = 0.
c                     other setup
1      ibetin = 2
       chibest = 999998.
c                     first estimate theta from wire positions only
       xbar   = 0
       ybar   = 0
       xsqbar = 0
       xybar  = 0
       do i = 1, nt
         dxx(i) = dxt(ju(i))
         xx(i) = xt(ju(i))
         zz(i) = zt(ju(i))
         xbar   = xbar + zz(i)
         xsqbar = xsqbar + zz(i)**2
         ybar   = ybar + xx(i)
         xybar  = xybar + zz(i)*xx(i)
         enddo
       xbar   = xbar / nlr
       xsqbar = xsqbar / nlr
       ybar   = ybar / nlr
       xybar  = xybar / nlr
       diff   = xsqbar - xbar**2
       if(diff.ne.0.)then
         slope  = (xybar - xbar*ybar) / diff
         ctg    = 1./sqrt(1.+slope**2)
         stg    = slope*ctg
         else
         ctg    = 0.
         stg    = 1.
         endif
c
       nloop = 2**nlr
       do i = 1, nloop
c                     set up all +/- combinations to loop over
         call seti(nlr,i,iloop)
         ibetter = 1
100         xbar   = 0
         ybar   = 0
         xsqbar = 0
         xybar  = 0
         do j = 1, nlr
           xx(j) = xt(ju(j)) + iloop(j)*dxx(j)*ctg
           zz(j) = zt(ju(j)) - iloop(j)*dxx(j)*stg
c                     calculate best track for this combination
c                     y=f(x) <---> x=f(z)
           xbar   = xbar + zz(j)
           xsqbar = xsqbar + zz(j)**2
           ybar   = ybar + xx(j)
           xybar  = xybar + zz(j)*xx(j)
           enddo
         xbar   = xbar / nlr
         xsqbar = xsqbar / nlr
         ybar   = ybar / nlr
         xybar  = xybar / nlr
         diff   = xsqbar - xbar**2
         if(diff.ne.0.)then
           slope  = (xybar - xbar*ybar) / diff
           ctg    = 1./sqrt(1.+slope**2)
           stg    = slope*ctg
           rcept  = -1.*(xybar*xbar-xsqbar*ybar) / diff
           else
           ctg    = 0.
           stg    = 1.
           rcept = -999999.
           slope = 999999.
           endif
c                     iterate a few times to converge
         if(ibetter.le.ibetin)then
           ibetter = ibetter + 1
           go to 100
           endif
c                     calculate chisq
         chisq = 0.
         do j = 1, nlr
           dx2 = slope * zt(ju(j)) + rcept - xt(ju(j))
            driftfit(j) = dx2 * ctg
c      adjust dxfit sign if track on opposite side of wire from drift distance
c      dx2>0 --> track on + side of wire, dx2<0 --> track on - side of wire
c      lr = 1--> track on + side of wire, lr=-1 --> track on - side of wire
c      use iloop if lr = 0...    note dx always +
c...   note: lr always 0 in Los Alamos case, but do not change code below
            if(driftfit(j).gt.0) then              !track on + side of wire
              if(lr(j).eq.1 .or. (lr(j).eq.0.and.iloop(j).eq.1)) then
                del(j) = driftfit(j) - dxx(j)       !track +, drift +
                else
                del(j) = driftfit(j) + dxx(j)       !track +, drift -
                endif
              else                            !track on - side of wire
              if(lr(j).eq.-1 .or. (lr(j).eq.0.and.iloop(j).eq.-1))then
                del(j) = driftfit(j) + dxx(j)        !track -, drift -
                else
                del(j) = driftfit(j) - dxx(j)        !track -, drift +
                endif
              endif
           chiterm(j) = (del(j)/sigma)**2
           chisq = chisq + chiterm(j)
           enddo
c                       save if best fit so far
         if(chisq.lt.chibest)then
c                     save lr input to fit, not side of wire of fit
           do j = 1, ninput
             dxf(j) = 0.
             lrf(j) = 0
             enddo
           do j = 1, nlr
             dxf(ju(j)) = del(j)
              if(iloop(j).lt.0)then
               lrf(ju(j)) = -1
               else
               lrf(ju(j)) = 1
               endif
             enddo
           tht = slope
            x0 = rcept
           chibest = chisq
           endif
         enddo
       if(chi0.le.0)chi0 = chibest
       chif = chibest

c      do not refit... leaving out worse chisq point if chisq too large
c      only do once
c       with sigma = 0.1 cm, and want point off by 3+ cm, require
c       chi > 30**2 ==> 1000
c       if(nlrin.eq.nlr .and. chibest.gt.(1000.) .and. nlr.gt.3)then
c         dx2 = chiterm(1)
c         i1 = 1
c         do j = 1, nlr
c           lrf(ju(j)) = 0
c          enddo
c         do j = 2, nlr
c           if(chiterm(j).gt.dx2)then
c             dx2 = chiterm(j)
c             i1 = j
c             endif
c           enddo
c         nlr = nlr - 1
c         if(i1.le.nlr)then
c           do j = i1, nlr-1
c             ju(j) = ju(j+1)
c             enddo
c           endif
c         lrf(i1) = -99
c         go to 1
c         endif

       return
       end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       subroutine seti(ntr,i,iloop)
c                     subroutine to set phases for loop combinations
        implicit none
       integer*4 ntr, i, iloop(10)
       integer*4 j, k, jn, jrem
c
       
       j = i-1
       do k = 1, ntr
         jn   = j/2
         jrem = j - 2*jn
         if(jrem.eq.0)then
           iloop(k) = 1
           else
           iloop(k) = -1
           endif
         j = jn
         enddo
       return
       end
       
ccccccccccccc put scintillator analysis here....
      subroutine t_test_scint_analyze
      implicit none


*	subroutine: t_test_scint_analyze.f
*	written: R. Gilman, Dec 20 1996
*	purpose: Use raw scintillator times to get positions in scintillators,
*		recalibrate scintillators, and get time offsets to improve
*		chamber resolution.
*               The 4th phototube is assumed to be the self timing one.
***************************************************************************
* Local Variables

      include 't20_test_detectors.cmn'
      include 't20_data_structures.cmn'
      include 't20_misc.cmn'

      integer*4 i,j
      real*4 t,f

c     the test scintillators are plugged into adcs 7 - 10 of the polder
c     strat adc, thus the addr1 is 1 and the addr2 is 7 - 10...
c     by initializing all the values to -10000, any valu not found will cause
c     a negative mean value, making it clear the event is not a proper
c     average
c     at some point the histograms should be used to determine pedestal
c     values, but we need to look at the pedestals or hope this is auto-done
c     by hall c software...

      do j = 1, 4
        ttst_scin_rawadc(j) = -10000
        ttst_scin_psadc(j) = -10000
      enddo

      i = 1
      do i = 1, tmisc_tot_hits
        if(tmisc_raw_addr1(i).eq.1)then
          if(tmisc_raw_addr2(i).gt.6 .and. tmisc_raw_addr2(i).lt.11)then
            j = tmisc_raw_addr2(i) - 6
            ttst_scin_rawadc(j) = tmisc_raw_data(i)
            ttst_scin_psadc(j) = tmisc_raw_data(i)
     +                         - ttst_scin_peds(j)
          endif
        endif
      enddo

*     work out geo and arith means for sintillators
      ttst_scin_amean_adc = 0
      ttst_scin_nzadcs = 0
      f = 1.
      do j = 1, 4
        if(ttst_scin_psadc(j).gt.0.)then
          ttst_scin_nzadcs = ttst_scin_nzadcs + 1
          ttst_scin_amean_adc = ttst_scin_amean_adc +
     +                          ttst_scin_psadc(j)  
          f = f * ttst_scin_psadc(j)
        endif
      enddo
      t = ttst_scin_nzadcs
      if(ttst_scin_nzadcs.gt.0)then
        ttst_scin_amean_adc = ttst_scin_amean_adc / ttst_scin_nzadcs
        ttst_scin_gmean_adc = nint(f ** (1./t))
        else
        ttst_scin_amean_adc = -1
        ttst_scin_gmean_adc = -1
      endif
      
*     work out means of two signals for scint 1 and for scint 2
      ttst_scin_adc1m = (ttst_scin_psadc(1) + ttst_scin_psadc(3))/2
      ttst_scin_adc2m = (ttst_scin_psadc(2) + ttst_scin_psadc(4))/2
         
*     work out scintillator times
*       call ttst_scinadccor
*       do i = 1, 4
*         ttst_scin_tdccor(i) = ttst_scin_tdc(i) + ttst_scin_timeoff(i)
*         if(ttst_scin_off_parm.eq.1)
*           ttst_scin_tdccor(i) = ttst_scin_tdccor(i) + ttst_scin_adccor(i)
*         enddo
*         
*       ttst_scin_time1 = 0.5*(ttst_scint_tdc(1) + ttst_scint_tdc(2))
*       ttst_scin_time2 = 0.5*(ttst_scint_tdc(3) + ttst_scint_tdc(4))
*       ttst_scin_tim = 0.5*(ttst_scin_time1 + ttst_scin_time2)
*       ttst_scin_timecor1 = 
*     +   0.5*(ttst_scint_tdccor(1) + ttst_scint_tdccor(2))
*       ttst_scin_timecor2 = 
*     +   0.5*(ttst_scint_tdccor(3) + ttst_scint_tdccor(4))
*       ttst_scin_timcor = 
*     +   0.5*(ttst_scin_timecor1 + ttst_scin_timecor2)
 
*      work out scintillator positions
*      The factor of 1/2 corrects for the fact
*      that one tube is earlier and the other is later, due to position offset.
*      The sign of the subtraction in the following equation must be checked.     
*       ttst_scin_pos1 = ttst_scin_v_corr * 0.5 *
*     +     (ttst_scint_tdccor(1) - ttst_scint_tdccor(2))
*       ttst_scin_pos2 = ttst_scin_v_corr * 0.5 *
*     +   (ttst_scint_tdccor(3) - ttst_scint_tdccor(4))
*       ttst_scin_pos = 0.5*(ttst_scin_pos1 + ttst_scin_pos2)

*      calculate t0 offset correction for this event...
*      t0 is the time the particle actually passes through the chamber,
*      relative to the trigger time, this varies from event to event due
*      to positions, ...
*      in this routine calculate the time the p[article passes through the
*      scintilator, based on the scintillator measure of position... a
*      0th order track from the chamber wires would probably give a better
*      measure, so leave as 0 for now...
*       ttst_t0_correction = 0.
       
       return
       end


