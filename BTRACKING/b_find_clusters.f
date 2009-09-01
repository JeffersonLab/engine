      subroutine b_find_clusters(ncluster,nmaximum,ABORT,err)

      implicit none
      save
      
      character*16 here
      parameter (here= 'b_find_clusters')
      
      logical ABORT
      character*(*) err
      
      include 'bigcal_data_structures.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'bigcal_geometry.cmn'

      integer i
      integer ihit
      integer irow
      integer icol
      integer icell,jcell
      integer ixmax,iymax,ihitmax,nmaximum
      integer ncluster,ncellclst,icellclst,nbadlist
      integer ixlo(0:2),ixhi(0:2),iylo,iyhi,lengthx,lengthy
      real emax,ecell,esum,asum,xmom_clst,ymom_clst
      integer ix2max,iy2max,celldiffx,celldiffy

      logical found_cluster
     
      integer cluster_temp_irow(bigcal_clstr_ncell_max)
      integer cluster_temp_icol(bigcal_clstr_ncell_max)
      real cluster_temp_ecell(bigcal_clstr_ncell_max)
      real cluster_temp_acell(bigcal_clstr_ncell_max)
      real cluster_temp_xcell(bigcal_clstr_ncell_max)
      real cluster_temp_ycell(bigcal_clstr_ncell_max)
      logical cluster_temp_bad_chan(bigcal_clstr_ncell_max)

      real xcenter,ycenter,xcell,ycell

      real copyreal
      integer copyint
      logical copybool

      abort=.false.
      err=' '

c     Strategy: Find Maximum, then build cluster around it using "add_neighbors"

c      nmaximum = 0
c      ncluster = 0

c     initialize cluster trimming parameters if user hasn't defined something reasonable:

      if(bigcal_clstr_nxmom_max.lt.1.or.bigcal_clstr_nxmom_max.gt.
     $     max(bigcal_clstr_ncellx_max,7)) then
         bigcal_clstr_nxmom_max = 2
      endif
      
      if(bigcal_clstr_nymom_max.lt.1.or.bigcal_clstr_nymom_max.gt.
     $     max(bigcal_clstr_ncelly_max,7)) then
         bigcal_clstr_nymom_max = 2
      endif

      if(bigcal_clstr_nxecl_max.lt.bigcal_clstr_nxmom_max
     $     .or.bigcal_clstr_nxecl_max.gt.max(bigcal_clstr_ncellx_max,7)) 
     $     then
         bigcal_clstr_nxecl_max = max(2,bigcal_clstr_nxmom_max)
      endif
      
      if(bigcal_clstr_nyecl_max.lt.bigcal_clstr_nymom_max
     $     .or.bigcal_clstr_nyecl_max.gt.max(bigcal_clstr_ncelly_max,7)) 
     $     then
         bigcal_clstr_nyecl_max = max(2,bigcal_clstr_nymom_max)
      endif

 102  continue
      found_cluster = .false.

      emax = b_min_emax
      ixmax = 0
      iymax = 0
      ihitmax = 0

      icellclst = 0
      ncellclst = 0
      nbadlist = 0
      
      do icell=1,bigcal_clstr_ncell_max
         cluster_temp_irow(icell) = 0
         cluster_temp_icol(icell) = 0
         cluster_temp_ecell(icell) = 0.
         cluster_temp_acell(icell) = 0.
         cluster_temp_xcell(icell) = 0.
         cluster_temp_ycell(icell) = 0.
         cluster_temp_bad_chan(icell) = .false.
      enddo
c     it should never happen that we find a max in a channel that is in the bad channels list
c     because the routine that initializes the bad channel list zeroes the calibration constant, 
c     so regardless of the adc value, the "ecell" value should be zero!
      do ihit=1,bigcal_all_ngood
         irow = bigcal_all_iygood(ihit)
         icol = bigcal_all_ixgood(ihit)
         ecell = bigcal_all_ecell(ihit)
         if(ecell.gt.emax) then
            emax = ecell
            ixmax = icol
            iymax = irow
            ihitmax = ihit
         endif
      enddo

c     check that max is at least one block away from the edge
      if(ixmax.ge.1.and.iymax.ge.1.and.ixmax.le.32.and.iymax.le.56.and.
     $     .not. (iymax.gt.32.and.ixmax.gt.30) ) then

         nmaximum = nmaximum + 1
         icellclst = icellclst + 1
         ncellclst = ncellclst + 1
c     initialize all "bad cluster" flags to false
         bigcal_edge_max(nmaximum) = .false.
         bigcal_not_enough(nmaximum) = .false.
         bigcal_too_long_x(nmaximum) = .false.
         bigcal_too_long_y(nmaximum) = .false.
         bigcal_below_cut(nmaximum) = .false.
         bigcal_above_max(nmaximum) = .false.
         bigcal_second_max(nmaximum) = .false.
     
c     check for maximum at edge condition:
         
         if(iymax.eq.1.or.ixmax.eq.1.or.iymax.eq.56.or.(iymax.gt.32.and.
     $        ixmax.eq.30).or.ixmax.eq.32) then
            bigcal_edge_max(nmaximum) = .true.
         endif

         cluster_temp_irow(icellclst) = iymax
         cluster_temp_icol(icellclst) = ixmax
         cluster_temp_xcell(icellclst) = bigcal_all_xgood(ihitmax)
         cluster_temp_ycell(icellclst) = bigcal_all_ygood(ihitmax)
         cluster_temp_ecell(icellclst) = bigcal_all_ecell(ihitmax)
         cluster_temp_acell(icellclst) = bigcal_all_adc_good(ihitmax)
         bigcal_all_ecell(ihitmax) = 0.
         bigcal_all_adc_good(ihitmax) = 0.
         bigcal_all_iygood(ihitmax) = 0
         bigcal_all_ixgood(ihitmax) = 0
         bigcal_all_ygood(ihitmax) = 0.
         bigcal_all_xgood(ihitmax) = 0.
         
         if(iymax.le.32) then
            icell = ixmax + 32*(iymax-1)
         else 
            icell = ixmax + 30*(iymax-33) + bigcal_prot_maxhits
         endif

         bigcal_all_good_det(icell) = 0.

         if(bigcal_bad_chan_list(icell)) then
            bigcal_all_good_det(icell) = -1.
            nbadlist = nbadlist + 1
            cluster_temp_bad_chan(icellclst) = .true.
         endif

         !write(*,*) 'found max, adding nearest neighbors'

c     this is the nearest-neighbors adding loop!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 103     call b_add_neighbors(icellclst,ncellclst,nbadlist,bigcal_clstr_ncell_max,
     $        cluster_temp_icol,cluster_temp_irow,cluster_temp_xcell,
     $        cluster_temp_ycell,cluster_temp_ecell,cluster_temp_acell,
     $        cluster_temp_bad_chan,abort,err)
         if(abort) then
            call g_add_path(here,err)
            return
         endif
         icellclst = icellclst + 1
         if(icellclst.le.ncellclst) goto 103

         !write(*,*) 'finished adding nearest neighbors'
c     end of the nearest-neighbors adding loop!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         
c 105     continue
c     now subject clusters to a series of checks. If all are passed, add cluster
c     to the array!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if(ncellclst.lt.bigcal_clstr_ncell_min) then
            bigcal_not_enough(nmaximum) = .true.
            goto 104
         endif
c     if at least two cells, sort cluster array in order of decreasing energy:
         do icell=1,ncellclst
            do jcell=icell+1,ncellclst
               if(cluster_temp_ecell(jcell).gt.cluster_temp_ecell(icell)
     $              )then ! switch positions in the array
                  copyint = cluster_temp_icol(icell)
                  cluster_temp_icol(icell) = cluster_temp_icol(jcell)
                  cluster_temp_icol(jcell) = copyint
                  
                  copyint = cluster_temp_irow(icell)
                  cluster_temp_irow(icell) = cluster_temp_irow(jcell)
                  cluster_temp_irow(jcell) = copyint

                  copyreal = cluster_temp_ecell(icell)
                  cluster_temp_ecell(icell) = cluster_temp_ecell(jcell)
                  cluster_temp_ecell(jcell) = copyreal

                  copyreal = cluster_temp_acell(icell)
                  cluster_temp_acell(icell) = cluster_temp_acell(jcell)
                  cluster_temp_acell(jcell) = copyreal

                  copyreal = cluster_temp_xcell(icell)
                  cluster_temp_xcell(icell) = cluster_temp_xcell(jcell)
                  cluster_temp_xcell(jcell) = copyreal

                  copyreal = cluster_temp_ycell(icell)
                  cluster_temp_ycell(icell) = cluster_temp_ycell(jcell)
                  cluster_temp_ycell(jcell) = copyreal

                  copybool = cluster_temp_bad_chan(icell)
                  cluster_temp_bad_chan(icell) = cluster_temp_bad_chan(jcell)
                  cluster_temp_bad_chan(jcell) = copybool
               endif
            enddo
         enddo
         
c$$$         write(*,252) 'ncell=',ncellclst
c$$$         do icell=1,ncellclst
c$$$            write(*,251) '(ix,iy,E)=(',cluster_temp_icol(icell),', ',
c$$$     $           cluster_temp_irow(icell),', ',cluster_temp_ecell(icell),
c$$$     $           ')'
c$$$         enddo
c$$$ 251        format(A11,I2,A2,I2,A2,F8.5,A1)
c$$$ 252        format(A6,I2)
         
c     compute length in x and y. Start with y, and then treat x differently depending on whether 
c     cluster has section overlap. Also accumulate esum

         iylo = 57
         iyhi = 0
         
         ixlo(0) = 33
         ixhi(0) = 0
         ixlo(1) = 33
         ixlo(2) = 31
         ixhi(1) = 0
         ixhi(2) = 0

         esum = 0.

         asum = 0.

         do icell=1,ncellclst
            irow = cluster_temp_irow(icell)
            icol = cluster_temp_icol(icell)

            if(irow.gt.iyhi) iyhi = irow
            if(irow.lt.iylo) iylo = irow
            if(icol.gt.ixhi(0)) ixhi(0) = icol
            if(icol.lt.ixlo(0)) ixlo(0) = icol

            if(irow.le.32) then
               if(icol.gt.ixhi(1)) ixhi(1) = icol
               if(icol.lt.ixlo(1)) ixlo(1) = icol
            else
               if(icol.gt.ixhi(2)) ixhi(2) = icol
               if(icol.lt.ixlo(2)) ixlo(2) = icol
            endif

c     intelligently calculate the cluster energy sum: 
c     restrict how far away from the maximum we can be to include
c     a block in the sum:
            if(abs(irow-cluster_temp_irow(1)).le.bigcal_clstr_nyecl_max) 
     $           then
c     1. max in Prot and current block in RCS
               if(cluster_temp_irow(1).le.32.and.irow.gt.32) then
                  if(abs(icol-bigcal_ixclose_prot(cluster_temp_icol(1)))
     $                 .le.bigcal_clstr_nxecl_max) then
                     esum = esum + cluster_temp_ecell(icell)
                     asum = asum + cluster_temp_acell(icell)
                  endif
c     2. max in RCS and current block in Prot
               else if(cluster_temp_irow(1).gt.32.and.irow.le.32) then
                  if(abs(icol-bigcal_ixclose_rcs(cluster_temp_icol(1)))
     $                 .le.bigcal_clstr_nxecl_max) then
                     esum = esum + cluster_temp_ecell(icell)
                     asum = asum + cluster_temp_acell(icell)
                  endif
c     3. both blocks in same section
               else
                  if(abs(icol-cluster_temp_icol(1)).le.bigcal_clstr_nxecl_max)
     $                 then
                     esum = esum + cluster_temp_ecell(icell)
                     asum = asum + cluster_temp_acell(icell)
                  endif
               endif
            endif
         enddo

         if(nbadlist.gt.0) then
c$$$            write(*,*) 'WARNING: cluster contains at least one'//
c$$$     $           'channel from the bad channels list'
c$$$            write(*,*) 'bypassing normal cluster checks'
            goto 106            ! don't subject a cluster containing channels from the bad list to 
c     the same checks as a cluster with no bad channels, just add it to the cluster array and move on.
         endif
         lengthy = iyhi - iylo + 1

c$$$         write(*,253) 'length y = ',lengthy
c$$$
c$$$ 253     format(A11,I2)

         if(iylo .le. 32 .and. iyhi .ge. 33) then ! cluster overlaps section boundary!
            lengthx = max(ixhi(1)-ixlo(1)+1,ixhi(2)-ixlo(2)+1)
         else ! cluster doesn't overlap!
            lengthx = ixhi(0) - ixlo(0) + 1
         endif

c$$$         write(*,253) 'length x = ',lengthx

         if(lengthx.lt.1.or.lengthy.lt.1) then
            bigcal_not_enough(nmaximum) = .true.
            goto 104
         endif

         if(lengthx.gt.bigcal_clstr_ncellx_max) then
            bigcal_too_long_x(nmaximum) = .true.
         endif
         if(lengthy.gt.bigcal_clstr_ncelly_max) then
            bigcal_too_long_y(nmaximum) = .true.
         endif

         if(bigcal_too_long_x(nmaximum).or.bigcal_too_long_y(nmaximum))
     $        then
c            goto 104
         endif
         if(esum.lt.b_cluster_cut) then
            bigcal_below_cut(nmaximum) = .true.
            goto 104
         endif
         
         if(esum.gt.b_cluster_max) then
            bigcal_above_max(nmaximum) = .true.
            goto 104
         endif

         ix2max = cluster_temp_icol(2)
         iy2max = cluster_temp_irow(2)

         celldiffy = int(abs(float(iy2max - cluster_temp_irow(1))))

         if(celldiffy.gt.1.and.ncellclst.gt.1) then
            if(cluster_temp_ecell(2)/cluster_temp_ecell(1).gt.b_min_2max(1).and.
     $           cluster_temp_ecell(2).gt.b_min_2max(2)) then
               bigcal_second_max(nmaximum) = .true.
c               goto 104
            endif
         else if(ncellclst.gt.1) then
            if(cluster_temp_irow(1).eq.32.and.iy2max.eq.33) then
               celldiffx = int(abs(float(ix2max - 
     $              bigcal_ixclose_prot(cluster_temp_icol(1)))))
            else if(cluster_temp_irow(1).eq.33.and.iy2max.eq.32) then
               celldiffx = int(abs(float(ix2max - 
     $              bigcal_ixclose_rcs(cluster_temp_icol(1)))))
            else
               celldiffx=int(abs(float(ix2max - cluster_temp_icol(1))))
            endif

            if(celldiffx.gt.1) then
               if(cluster_temp_ecell(2)/cluster_temp_ecell(1).gt.b_min_2max(1).and.
     $              cluster_temp_ecell(2).gt.b_min_2max(2)) then
                  bigcal_second_max(nmaximum) = .true.
c                  goto 104
               endif
            endif
         endif

c     IF WE'VE MADE IT TO THIS POINT, IT SHOULD MEAN THAT ALL THE CLUSTER CHECKS WERE PASSED!!!
c     SO FILL THE CLUSTER ARRAY!!!
c     ALTERNATIVELY, IT MAY MEAN THAT THERE IS AT LEAST 1 BADLIST CHANNEL IN THE CLUSTER, AND WE 
c     DON'T WANT TO STOP CLUSTER FINDING BECAUSE OF IT! IF A BADLIST CHANNEL IS ADJACENT TO THE MAXIMUM, 
c     IT IS LIKELY THAT WE WILL FIND IT, BUT IF A BADLIST CHANNEL SHOULD HAVE BEEN THE MAXIMUM, THERE IS 
c     ONLY A SMALL CHANCE OF FINDING A MAXIMUM NEXT TO IT, DEPENDING ON B_MIN_EMAX
         
 106     found_cluster = .true.
         ncluster = ncluster + 1

         bigcal_clstr_keep(ncluster) = .true.

         bigcal_all_clstr_ncell(ncluster) = ncellclst
         bigcal_all_clstr_ncellx(ncluster) = lengthx
         bigcal_all_clstr_ncelly(ncluster) = lengthy
         bigcal_all_clstr_nbadlist(ncluster) = nbadlist
         bigcal_all_clstr_iymax(ncluster) = cluster_temp_irow(1)
         bigcal_all_clstr_ixmax(ncluster) = cluster_temp_icol(1)
        
         bigcal_all_clstr_iylo(ncluster) = iylo
         bigcal_all_clstr_iyhi(ncluster) = iyhi
         do i=0,2
            bigcal_all_clstr_ixlo(ncluster,i+1) = ixlo(i)
            bigcal_all_clstr_ixhi(ncluster,i+1) = ixhi(i)
         enddo

         xmom_clst = 0.
         ymom_clst = 0.

         xcenter = cluster_temp_xcell(1)
         ycenter = cluster_temp_ycell(1)

         do icell=1,ncellclst
            
            bigcal_all_clstr_iycell(ncluster,icell) = 
     $           cluster_temp_irow(icell)
            bigcal_all_clstr_ixcell(ncluster,icell) = 
     $           cluster_temp_icol(icell)
            bigcal_all_clstr_ecell(ncluster,icell) = 
     $           cluster_temp_ecell(icell)
            bigcal_all_clstr_acell(ncluster,icell) = 
     $           cluster_temp_acell(icell)
            bigcal_all_clstr_xcell(ncluster,icell) = 
     $           cluster_temp_xcell(icell)
            bigcal_all_clstr_ycell(ncluster,icell) = 
     $           cluster_temp_ycell(icell)
            bigcal_clstr_bad_chan(ncluster,icell) = 
     $           cluster_temp_bad_chan(icell)
            
            xcell = cluster_temp_xcell(icell)
            ycell = cluster_temp_ycell(icell)
            ecell = cluster_temp_ecell(icell)
c            acell = cluster_temp_acell(icell)

c     intelligent cluster moment calculation: restrict how far away from the maximum
c     we allow blocks to be in order to include them in the calculation
c     i.e., "trim" the clusters down to size

            if(abs(cluster_temp_irow(icell)-cluster_temp_irow(1)).le.
     $           bigcal_clstr_nymom_max) then

               if(cluster_temp_irow(1).le.32.and.cluster_temp_irow(icell)
     $              .gt.32) then
                  if(abs(cluster_temp_icol(icell)-
     $                 bigcal_ixclose_prot(cluster_temp_icol(1))).le.
     $                 bigcal_clstr_nxmom_max) then
                     xmom_clst = xmom_clst + ecell*(xcell-xcenter)/esum
                     ymom_clst = ymom_clst + ecell*(ycell-ycenter)/esum
                  endif
               else if(cluster_temp_irow(1).gt.32.and.cluster_temp_irow(icell)
     $                 .le.32) then
                  if(abs(cluster_temp_icol(icell)-
     $                 bigcal_ixclose_rcs(cluster_temp_icol(1))).le.
     $                 bigcal_clstr_nxmom_max) then
                     xmom_clst = xmom_clst + ecell*(xcell-xcenter)/esum
                     ymom_clst = ymom_clst + ecell*(ycell-ycenter)/esum
                  endif
               else 
                  if(abs(cluster_temp_icol(icell)-cluster_temp_icol(1)).le.
     $                 bigcal_clstr_nxmom_max) then
                     xmom_clst = xmom_clst + ecell*(xcell-xcenter)/esum
                     ymom_clst = ymom_clst + ecell*(ycell-ycenter)/esum
                  endif
               endif
            endif

            do ihit=1,bigcal_all_ngood
               if(bigcal_all_iygood(ihit).eq.cluster_temp_irow(icell)
     $              .and.bigcal_all_ixgood(ihit).eq.cluster_temp_icol(icell)) 
     $              then        ! zero this hit so it won't be used again
                  bigcal_all_ecell(ihit) = 0.
                  bigcal_all_adc_good(ihit) = 0.
                  bigcal_all_iygood(ihit) = 0
                  bigcal_all_ixgood(ihit) = 0
                  bigcal_all_ygood(ihit) = 0.
                  bigcal_all_xgood(ihit) = 0.
               endif
            enddo
         enddo

         bigcal_all_clstr_xmom(ncluster) = xmom_clst
         bigcal_all_clstr_ymom(ncluster) = ymom_clst
         bigcal_all_clstr_etot(ncluster) = esum
         bigcal_all_clstr_atot(ncluster) = asum

         if(bbypass_calc_shower_coord.ne.0) then ! use xcenter + xmom
            bigcal_all_clstr_x(ncluster) = xcenter + xmom_clst
            bigcal_all_clstr_y(ncluster) = ycenter + ymom_clst
         endif

c$$$         if(nbadlist.gt.0) then
c$$$            call b_print_cluster(ncluster,abort,err)
c$$$            if(abort) then
c$$$               call g_add_path(here,err)
c$$$               return
c$$$            endif
c$$$         endif

         if(bdebug_print_clusters.ne.0) then
            call b_print_cluster(ncluster,abort,err)
            if(abort) then
               call g_add_path(here,err)
               return
            endif
         endif

c$$$      else 
c$$$         if(ixmax.eq.1.or.iymax.eq.1.or.iymax.eq.56.or.ixmax.eq.32.or.
c$$$     $        (iymax.gt.32.and.ixmax.eq.30)) then
c$$$            nmaximum = nmaximum + 1
c$$$            bigcal_edge_max(nmaximum) = .true.
c$$$            bigcal_not_enough(nmaximum) = .false.
c$$$            bigcal_too_long_x(nmaximum) = .false.
c$$$            bigcal_too_long_y(nmaximum) = .false.
c$$$            bigcal_below_cut(nmaximum) = .false.
c$$$            bigcal_above_max(nmaximum) = .false.
c$$$            bigcal_second_max(nmaximum) = .false.
c$$$
c$$$c     zero ecell and adc good for hit array, but leave detector array untouched
c$$$
c$$$            bigcal_all_ecell(ihitmax) = 0.
c$$$            bigcal_all_adc_good(ihitmax) = 0.
c$$$
c$$$            goto 102
c$$$
c$$$         endif
      endif
      
 104  continue

      if(found_cluster.and.ncluster.lt.bigcal_all_nclstr_max) goto 102
      
      bigcal_all_nclstr = ncluster
      bigcal_nmaxima = nmaximum
      
      return
      end
