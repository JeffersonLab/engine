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
c>    File : fpp_diagnose.f       
c>    Authors :   E.J. Brash and M.K. Jones
c>                July, 1996
c>    Revised extensively 11/25/96 - 12/13/96 by R. Gilman and M. Jones
c>    to generalize analysis routines, treating all plane groups with
c>    same code rather than as special cases
c>    Revised 12/14/96 by RG to include some autocalibration types of hists
c>
c>    Called by:  dplotana.f
c>    Description:  Diagnosis of FPP data.
* $Log$
* Revision 1.1  1997/05/20 18:28:49  saw
* Initial revision
*
*
*     include files...
      include 't20_data_structures.cmn'
      include 't20_test_detectors.cmn'
      include 't20_test_histid.cmn'

      integer*4 LUN_CALIB               ! Set this somewhere else
      parameter (LUN_CALIB=63)

*     local variablesu
      integer*4 oldwiregroup, ngroup, tim
      integer*4 nplane, itype, idiff, tdiff
      integer*4 i, j, n, kk, ilete, ihit, noff, itot, iok
      integer*4 raw_ntuples_written
      integer*4 track_ntuples_written
      integer*4 drift_time
      real*4 degr, pos
      real*4 slope, bint, pos1, pos2, dpos

      data degr/0.01745329252/
*     most intializations have been moved into ctp file
*         terplay/PARAM/t20_test_detectors.param

c     philosophy: planes are grouped together, each group is analyzed together

*     first get hits out of input array and group into local plane hit
*     structures the TDC appears to be LIFO (info from Glen Collins) but the
*     SAW decoder reverses the order into FIFO.  Thus, leading edges should
*     always precede the trailing edges.  This is assumed,to simplify the
*     decoding...
*
*     IF THIS IS NOT THE CASE, JUST GET ELEMENTS OUT OF THE INPUT ARRAYS
*     FROM LAST TO FIRST!
*
*     Here we assume the time scale is ns; probably it is 0.5 ns and we need
*     to divide everything by 2 in the end...

      oldwiregroup = 0
      ttst_straw_goodedge = 0
      ttst_straw_gooddemux = 0
      do i = 1, ttst_n_straw_wgs
        ttst_straw_hits(i) = 0
      enddo
c      write(6,*)' '
c      write(6,*)'ttst_raw_tot_hits =', ttst_raw_tot_hits
      do i = ttst_raw_tot_hits, 1, -1
        ttst_straw_plane = ttst_raw_plane_num(i)
        ttst_straw_ggroup = 
     +       ttst_straw_plane_group_off(ttst_straw_plane)
     +       + ttst_raw_group_num(i)
        tim = ttst_raw_tdc(i)
c        write(6,*)'hit ',i,' plane ',ttst_straw_plane,' wg ',
c     +       ttst_straw_ggroup
c        write(6,*)'tim =',tim,' g-grp=',ttst_straw_ggroup,' old-grp=',
c     +    oldwiregroup
        ilete = iand(tim,'10000'X)
        tim = iand(tim,'FFFF'X)
        if(ilete.eq.0)then            !leading edge when bit 16 = 0, later doc
c		iand generix, jiand 4 byte, iiand 2 byte
c          write(6,*)' LE time =',tim,' for g-grp=',ttst_straw_ggroup
          if(tim.ge.ttst_TDC_min .and. tim.le.ttst_TDC_max)then
c          if(tim.ge.2000 .and. tim.le.2400)then
            n = ttst_straw_hits(ttst_straw_ggroup) + 1
            if(n.gt.16)n = 16
            oldwiregroup = ttst_straw_ggroup
            ttst_straw_tims(n,oldwiregroup) = tim
          endif

        else                !get trailing edge immediately after a LE only

          if(ttst_straw_ggroup.eq.oldwiregroup)then !same group :)
c            write(6,*)' matching g-grp for TE! TE tim=',tim
            n = ttst_straw_hits(ttst_straw_ggroup) + 1
            if(n.gt.16)n = 16
            ttst_straw_hits(ttst_straw_ggroup) = n
            ttst_straw_wids(n,oldwiregroup) =
     +           ttst_straw_tims(n,oldwiregroup) - tim
            ttst_straw_goodedge = ttst_straw_goodedge + 2
c            write(6,*)' now ',ttst_straw_goodedge,' good edges'
            call hf1(ttst_hid_straw_letdc,
     +               float(ttst_straw_tims(n,oldwiregroup)),1.)
            call hf1(ttst_hid_straw_width,
     +               float(ttst_straw_wids(n,oldwiregroup)),1.)
            call hf1(ttst_hid_wg,
     +               float(oldwiregroup),1.)
            if(ttst_raw_ntuples_out.gt.0)then
              if(raw_ntuples_written.lt.ttst_raw_ntuples_out) then
                raw_ntuples_written = raw_ntuples_written + 1
                write(lun_calib,*) oldwiregroup,
     +               ttst_straw_tims(n,oldwiregroup),
     +               ttst_straw_wids(n,oldwiregroup)
              endif
              if(raw_ntuples_written.eq.ttst_raw_ntuples_out)then
                write(6,*)' t_test_straw_analyze wrote',
     +               raw_ntuples_written,' ntuples'
                raw_ntuples_written = raw_ntuples_written + 1
              endif
            else
              raw_ntuples_written = 0
            endif
          endif
          oldwiregroup = 0
        endif
      enddo
      ttst_straw_goodhit = ttst_straw_goodedge/2
c      write(6,*)ttst_straw_goodhit,' good hits found of',
c     +  ttst_raw_tot_hits


*      now have captured into the arrays the pairs of hits...
*      count up planes hit, also work out demultiplex and straws hit...
*      have also ordered everything by global wiregroup, and thus
*      by  plane number

      do i = 1, ttst_n_straw_planes
        ttst_straw_planes_hit(i) = 0
      enddo

      do ngroup=1, ttst_n_straw_wgs
c         if (debug_flag) write(*,*) ' Straw Chambers planes',
c     +                              name(ngroup)
c         if(debug_flag) write(*,*) ' PLANE   NHITS   WG   LE (ns)',
c     +                        '   TE (ns)   WID (ns)  STRAW   WIRE'

        if(ttst_straw_hits(ngroup).gt.0)then
          nplane = ttst_plane_of_group(ngroup)
          noff = ngroup - ttst_straw_plane_group_off(nplane) - 1
          noff = 8*noff
          itype = ttst_straw_type(nplane)
          do i = 1, ttst_straw_hits(ngroup)
            tdiff = ttst_straw_wids(i,ngroup)
            idiff = 0
            ttst_straw_straw(i,ngroup) = 0
            do kk = 1, 9
              if(tdiff.gt.ttst_dmx(kk,ngroup)) idiff=kk
            enddo
            if(idiff.gt.0.and.idiff.le.8) then
              idiff = ttst_type_order(idiff,itype)
              ttst_straw_straw(i,ngroup) = noff + idiff
            call hf1(ttst_hid_strawmap(nplane),
     +               float(ttst_straw_straw(i,ngroup)),1.)
              ttst_straw_planes_hit(nplane) =
     +          ttst_straw_planes_hit(nplane) + 1
              ihit = ttst_straw_planes_hit(nplane)
              if(ihit.gt.max_track_hit) ihit=max_track_hit
              ttst_straw_gooddemux = ttst_straw_gooddemux + 1
c     have a good hit and a good demux, so store it for tracking!
c     following line is just sample of what will be needed
              ttst_track_hitarray(3,ihit,nplane) =
     +          ttst_straw_z(nplane)
              ttst_track_hitarray(1,ihit,nplane) =
     +          ttst_straw_straw(i,ngroup)
              ttst_track_hitarray(4,ihit,nplane) =
     +        (ttst_straw_straw(i,ngroup)-ttst_straw_sctr(nplane))
     +         * ttst_straw_spacing + ttst_straw_x1(nplane)
              drift_time = ttst_straw_tims(i,ngroup)-ttst_TDC_min
              ttst_track_hitarray(2,ihit,nplane) = 
     +        ttst_drift_max * ttst_drift_table(drift_time)
              call hf1(ttst_hid_driftdist,
     +          ttst_track_hitarray(2,ihit,nplane),1.)
              call hf1(ttst_hid_drifttime,float(drift_time),1.)
              drift_time = ttst_drift_t0-ttst_straw_tims(i,ngroup)
              ttst_track_dxpos2(ihit,nplane) =
     +        ttst_drift_v * drift_time
              if(ttst_track_dxpos2(ihit,nplane).gt.ttst_drift_max)
     +           ttst_track_dxpos2(ihit,nplane) = ttst_drift_max
              if(ttst_track_dxpos2(ihit,nplane).lt. 0.)
     +           ttst_track_dxpos2(ihit,nplane) = 0.
              call hf1(ttst_hid_driftdistv0,
     +          ttst_track_dxpos2(ihit,nplane),1.)
              call hf1(ttst_hid_drifttimet0,float(drift_time),1.)
            endif      ! if   idiff 
          enddo        ! loop over # hits      
        endif          ! if   any straws in group hit
      enddo            ! loop over wiregroups in plane

*       write out ntuple if needed, count total number of hits
      itot = 0
      do i = 1, ttst_n_straw_planes
        itot = itot + ttst_straw_planes_hit(i)
      enddo

*       cont up number of x, y hits, xyplaneshit
      ttst_straw_xgddmx = 0
      ttst_straw_ygddmx = 0
      ttst_straw_xplnsht = 0
      ttst_straw_yplnsht = 0
      do i = 1, 4
       ttst_straw_xgddmx = ttst_straw_xgddmx + 
     +     ttst_straw_planes_hit(i)
       if(ttst_straw_planes_hit(i).gt.0)
     +  ttst_straw_xplnsht = ttst_straw_xplnsht + 1
       ttst_straw_ygddmx = ttst_straw_ygddmx +
     +     ttst_straw_planes_hit(i+4)
       if(ttst_straw_planes_hit(i+4).gt.0)
     +  ttst_straw_yplnsht = ttst_straw_yplnsht + 1
      enddo

*     write out nutples only for 8 hit events?
      if(ttst_track_ntuples_out.gt.0.and.itot.gt.0
     +   .and. ttst_straw_xgddmx.eq.4
     +   .and. ttst_straw_ygddmx.eq.4
     +   .and. ttst_straw_xplnsht.eq.4 
     +   .and. ttst_straw_yplnsht.eq.4 )then
        if(track_ntuples_written.lt.ttst_track_ntuples_out) then
          track_ntuples_written = track_ntuples_written + 1
          write(lun_calib,'(i5,16f7.3)')itot,
     +      (ttst_straw_z(nplane),ttst_track_hitarray(1,1,nplane),
     +       nplane=1,8)
c          do nplane = 1, ttst_n_straw_planes
c            if(ttst_straw_planes_hit(nplane).gt.0)then
c              do ihit = 1, ttst_straw_planes_hit(nplane)
c                write(lun_calib,'(2i5,3f10.4)')
c     +                 nplane, ttst_track_straw(ihit,nplane),
c     +                 ttst_straw_z(nplane),
c     +                 ttst_track_xpos(ihit,nplane),
c     +                 ttst_track_dxpos(ihit,nplane)
c              enddo
c            endif
c          enddo
        endif
        if(track_ntuples_written.eq.ttst_track_ntuples_out)then
          write(6,*)' t_test_straw_analyze wrote',
     +      track_ntuples_written,' track ntuples'
          endif
      else
        track_ntuples_written = 0
      endif

*     if enough hits, move into tracking arrays and get track...
      noff = -3
      do i = 1, 2
        ttst_track_ntracks = 0
        noff = noff + 4
        if(ttst_straw_xygddmx(i).gt.2 .and.
     +     ttst_straw_xygddmx(i).lt.6 .and.
     +     ttst_straw_xyplnsht(i).gt.2) then

          call trk4(ttst_straw_planes_hit(noff),
     +              ttst_track_hitarray(1,1,noff),
     +              ttst_track_ntracks,
     +              ttst_track_params(1,i) )
        endif
c     histogram crude efficiency spectrum
        call hf1(ttst_hid_xoryhits,
     +       float(ttst_straw_xygddmx(i)),1.)
        if(ttst_track_ntracks.gt.0)then
          call hf1(ttst_hid_evtstrcked,
     +       float(ttst_straw_xygddmx(i)),1.)
c     some housekeeping so ctp histograms intercepts and angles
          ttst_track_angle(i) = atan(ttst_track_params(1,i))/degr
c     increment a single resolution histogram
          call hf1(ttst_hid_strawres,
     +       ttst_track_params(3,i),1.)
c     work out the good,bad,missing wire hists
          do j = 1, 4
            nplane = j + noff - 1
            pos = ttst_track_params(2,i) + 
     +            ttst_track_params(1,i)*ttst_straw_z(nplane)
            if(j.eq.1)then
              pos1 = ttst_track_hitarray(4,1,nplane)
              if(pos.gt.pos1)then
                pos1 = pos1 + ttst_track_hitarray(2,1,nplane)
              else
                pos1 = pos1 - ttst_track_hitarray(2,1,nplane)
              endif
            endif
            if(j.eq.2)then
              pos2 = ttst_track_hitarray(4,1,nplane)
              if(pos.gt.pos2)then
                pos2 = pos2 + ttst_track_hitarray(2,1,nplane)
              else
                pos2 = pos2 - ttst_track_hitarray(2,1,nplane)
              endif
              slope = (pos2 - pos1) / 
     +          (ttst_straw_z(2)-ttst_straw_z(1))
              bint = pos2 - ttst_straw_z(2)*slope
            endif
            pos = pos - ttst_straw_x1(nplane)
            idiff = nint(pos/ttst_straw_spacing + ttst_straw_sctr(nplane))
            if(i.eq.1)kk = 56*(j-1)
            if(i.eq.2)kk = 224 + 24*(j-1)
            if(ttst_straw_planes_hit(nplane).eq.0)then
              call hf1(ttst_hid_strawhitms,float(kk+idiff),1.)
            else
              iok = 0
              do ihit = 1, ttst_straw_planes_hit(nplane)
                n = ttst_track_hitarray(1,ihit,nplane)
                  if(j.eq.3 .or. j.eq.4)then
                    dpos = slope*ttst_straw_z(nplane) + bint
     +                 - ttst_track_hitarray(4,1,nplane)
                    if(dpos.lt.0.)then
                      dpos = dpos + ttst_track_hitarray(2,1,nplane)
                    else
                      dpos = dpos - ttst_track_hitarray(2,1,nplane)
                    endif
                    call hf1(ttst_hid_linoff(nplane),dpos,1.)
                  endif
                if(n.eq.idiff)then
                  iok = 1
                  call hf1(ttst_hid_strawhitgd,float(kk+n),1.)
                  call hf1(ttst_hid_planeoffs(nplane),0.,1.)
                else
                  call hf1(ttst_hid_strawhitbd,float(kk+n),1.)
                  n = n-idiff
                  call hf1(ttst_hid_planeoffs(nplane),
     +                float(n),1.)
                endif     !if there are hits on planes
              enddo       !loop over hits in plane
              if(iok.eq.0)then
                call hf1(ttst_hid_strawhitms,float(kk+idiff),1.)
c               write(6,*)' no hit where epected: wire,off=',
c     +           ihit,kk
              endif       !if no hit where expected
            endif         !if there are hits in plane
          enddo           !loop over planes
        endif             !if there is track
      enddo               !loop over two coordinates

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
c       real*4    x(4), z(4)
       real*4    ztra(10), xtra(10), dxtra(10), dxfit(10)
       real*4    chi0, chif, chf3(10)
       real*4    xmin, xmax
c       real*4    zoff/2.03/
c       real*4    zplane(4)
c       data    zplane/0.0, 3.333, 6.667, 10.0/
       integer*4 nhitx
c
c       initialize
       iok3 = -1
       iok4 = -1
       iok43 = -1
       chif = -1.
       xat0 = -1000.
       slope = -1000.
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
         if(chif.gt.3.0)then              !more than four x's nominal chisq
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
         if(chif.lt.1.5)go to 3
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
       real    sigma/0.1/, pi/3.1415926/

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
