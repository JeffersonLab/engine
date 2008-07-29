      subroutine b_prune_clusters(abort,err)

      implicit none
      save

      character*16 here
      parameter(here='b_prune_clusters')

      logical abort 
      character*(*) err

      include 'bigcal_data_structures.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_geometry.cmn'
      include 'bigcal_tof_parms.cmn'

      logical keep(bigcal_all_nclstr_max)
      
      integer ngood

      integer iclust

      abort=.false.
      err=' '

      ngood = 0

c     initialize all clusters to good:
      do iclust=1,bigcal_all_nclstr
         keep(iclust) = .true.
      enddo

c     prune on second max:

      if(b_prune_flags(1).ne.0) then
      
         ngood = 0
         
         do iclust=1,bigcal_all_nclstr
            if((.not.bigcal_second_max(iclust)).and.keep(iclust)) then
               ngood = ngood + 1
            endif
         enddo
         if(ngood.gt.0) then
            do iclust=1,bigcal_all_nclstr
               if(bigcal_second_max(iclust)) then
                  keep(iclust) = .false.
               endif
            enddo
         endif
      endif

c     prune clusters that are too big:

      if(b_prune_flags(2).ne.0) then
      
         ngood = 0
         
         do iclust=1,bigcal_all_nclstr
            if(keep(iclust).and.(.not.bigcal_too_long_x(iclust)).and.
     $           (.not.bigcal_too_long_y(iclust))) then
               ngood = ngood + 1
            endif
         enddo
         if(ngood.gt.0) then
            do iclust=1,bigcal_all_nclstr
               if(bigcal_too_long_x(iclust).or.bigcal_too_long_y(iclust)) then
                  keep(iclust) = .false.
               endif
            enddo
         endif
      endif

c     prune on cluster energy before pruning on edge max or cluster size:

      if(b_prune_flags(3).ne.0) then

         ngood = 0
         
         do iclust=1,bigcal_all_nclstr
            if(keep(iclust).and.bigcal_all_clstr_etot(iclust).ge.
     $           b_prune_eclust(1).and.bigcal_all_clstr_etot(iclust).le.
     $           b_prune_eclust(2)) then
               ngood = ngood + 1
            endif
         enddo
         if(ngood.gt.0) then
            do iclust=1,bigcal_all_nclstr
               if(bigcal_all_clstr_etot(iclust).lt.b_prune_eclust(1).or.
     $              bigcal_all_clstr_etot(iclust).gt.b_prune_eclust(2))then
                  keep(iclust) = .false.
               endif
            enddo
         endif
      endif

c     prune on cluster size (want >1 in x and y for real showers)

      if(b_prune_flags(4).ne.0) then

         ngood = 0
         
         do iclust=1,bigcal_all_nclstr
            if(keep(iclust).and.bigcal_all_clstr_ncellx(iclust).gt.1.and.
     $           bigcal_all_clstr_ncelly(iclust).gt.1) then
               ngood = ngood + 1
            endif
         enddo
         if(ngood.gt.0) then
            do iclust=1,bigcal_all_nclstr
               if(bigcal_all_clstr_ncellx(iclust).le.1.or.
     $              bigcal_all_clstr_ncelly(iclust).le.1) then
                  keep(iclust) = .false.
               endif
            enddo
         endif
      endif

c     prune on edge max:

      if(b_prune_flags(5).ne.0) then
      
         ngood = 0
         do iclust=1,bigcal_all_nclstr
            if(keep(iclust).and.(.not.bigcal_edge_max(iclust))) then
               ngood = ngood + 1
            endif
         enddo
         if(ngood.gt.0) then
            do iclust=1,bigcal_all_nclstr
               if(bigcal_edge_max(iclust))then
                  keep(iclust) = .false.
               endif
            enddo
         endif
      endif

c     prune on group of 8 TDC hits:
      
      if(b_prune_flags(6).ne.0) then

         ngood = 0
         
         do iclust=1,bigcal_all_nclstr
            if(keep(iclust).and.bigcal_all_clstr_ncell8(iclust).gt.0) then
               ngood = ngood + 1
            endif
         enddo
         if(ngood.gt.0) then
            do iclust=1,bigcal_all_nclstr
               if(bigcal_all_clstr_ncell8(iclust).le.0) then
                  keep(iclust) = .false.
               endif
            enddo
         endif
      endif

c     prune on group of 64 TDC hits: 
      
      if(b_prune_flags(7).ne.0) then

         ngood = 0
         
         do iclust=1,bigcal_all_nclstr
            if(keep(iclust).and.bigcal_all_clstr_ncell64(iclust).gt.0) then
               ngood = ngood + 1
            endif
         enddo
         if(ngood.gt.0) then
            do iclust=1,bigcal_all_nclstr
               if(bigcal_all_clstr_ncell64(iclust).le.0) then
                  keep(iclust) = .false.
               endif
            enddo
         endif
      endif

c     prune on trms:

      if(b_prune_flags(8).ne.0) then
      
         ngood = 0
         
         do iclust=1,bigcal_all_nclstr
            if(keep(iclust).and.bigcal_all_clstr_t8rms(iclust).le.b_timing_cut) then
               ngood = ngood + 1
            endif
         enddo
         if(ngood.gt.0) then
            do iclust=1,bigcal_all_nclstr
               if(bigcal_all_clstr_t8rms(iclust).gt.b_timing_cut)then
                  keep(iclust) = .false.
               endif
            enddo
         endif
      endif

c     now set the "keep" flag for each cluster
c     at this point, no longer keep clusters that don't satisfy, at a minimum, the following:
c     1. no second max
c     2. cluster size < maximum
      
      bigcal_all_nclust_good = 0
      
      do iclust=1,bigcal_all_nclstr
         bigcal_clstr_keep(iclust)=.false.
         if(keep(iclust).and..not.bigcal_second_max(iclust).and..not.
     $        (bigcal_too_long_x(iclust).or.bigcal_too_long_y(iclust))) 
     $        then
            bigcal_clstr_keep(iclust)=.true.
            bigcal_all_nclust_good = bigcal_all_nclust_good + 1
         endif
      enddo
      
      return
      end
