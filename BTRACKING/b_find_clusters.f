      subroutine b_find_clusters(ABORT,err)

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

      integer ihit,jhit,khit
      integer irow,jrow,krow
      integer icol,jcol,kcol
      integer icell,jcell,kcell
      integer i8,j8,k8
      integer i64,j64,k64
      integer ixmax,iymax,ihitmax,nmaximum
      integer ncluster,ncellclst,icellclst
      integer ixlo(0:2),ixhi(0:2),iylo,iyhi,lengthx,lengthy
      real emax,ecell,esum,xmom_clst,ymom_clst
      integer ix2max,iy2max,celldiffx,celldiffy

      logical found_cluster
     
      integer cluster_temp_irow(bigcal_clstr_ncell_max)
      integer cluster_temp_icol(bigcal_clstr_ncell_max)
      real cluster_temp_ecell(bigcal_clstr_ncell_max)
      real cluster_temp_xcell(bigcal_clstr_ncell_max)
      real cluster_temp_ycell(bigcal_clstr_ncell_max)

      real xcenter,ycenter,xcell,ycell

      real copyreal
      integer copyint

      abort=.false.
      err=' '

c     Strategy: Find Maximum, then build cluster around it using "add_neighbors"

      nmaximum = 0
      ncluster = 0

 102  continue
      found_cluster = .false.

      emax = b_min_emax
      ixmax = 0
      iymax = 0
      ihitmax = 0

      icellclst = 0
      ncellclst = 0
      
      do icell=1,bigcal_clstr_ncell_max
         cluster_temp_irow(icell) = 0
         cluster_temp_icol(icell) = 0
         cluster_temp_ecell(icell) = 0.
         cluster_temp_xcell(icell) = 0.
         cluster_temp_ycell(icell) = 0.
      enddo

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
      if(ixmax.ge.2.and.iymax.ge.2.and.ixmax.le.31.and.iymax.le.55.and.
     $     .not. (iymax.gt.32.and.ixmax.gt.29) ) then

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
     
         cluster_temp_irow(icellclst) = iymax
         cluster_temp_icol(icellclst) = ixmax
         cluster_temp_xcell(icellclst) = bigcal_all_xgood(ihitmax)
         cluster_temp_ycell(icellclst) = bigcal_all_ygood(ihitmax)
         cluster_temp_ecell(icellclst) = bigcal_all_ecell(ihitmax)
         bigcal_all_ecell(ihitmax) = 0.
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

         !write(*,*) 'found max, adding nearest neighbors'

c     this is the nearest-neighbors adding loop!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 103     call b_add_neighbors(icellclst,ncellclst,bigcal_clstr_ncell_max,
     $        cluster_temp_icol,cluster_temp_irow,cluster_temp_xcell,
     $        cluster_temp_ycell,cluster_temp_ecell,abort,err)
         if(abort) then
            call g_add_path(here,err)
            return
         endif
         icellclst = icellclst + 1
         if(icellclst.le.ncellclst) goto 103
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

                  copyreal = cluster_temp_xcell(icell)
                  cluster_temp_xcell(icell) = cluster_temp_xcell(jcell)
                  cluster_temp_xcell(jcell) = copyreal

                  copyreal = cluster_temp_ycell(icell)
                  cluster_temp_ycell(icell) = cluster_temp_ycell(jcell)
                  cluster_temp_ycell(jcell) = copyreal
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

            esum = esum + cluster_temp_ecell(icell)
         enddo

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
            goto 104
         endif
c$$$         write(*,254) 'esum = ',esum
 254     format(A7,F8.5)
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

         if(celldiffy.gt.1) then
            bigcal_second_max(nmaximum) = .true.
            goto 104
         else
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
               bigcal_second_max(nmaximum) = .true.
               goto 104
            endif
         endif

c     IF WE'VE MADE IT TO THIS POINT, IT SHOULD MEAN THAT ALL THE CLUSTER CHECKS WERE PASSED!!!
c     SO FILL THE CLUSTER ARRAY!!!
         
         found_cluster = .true.
         ncluster = ncluster + 1

         bigcal_all_clstr_ncell(ncluster) = ncellclst
         bigcal_all_clstr_ncellx(ncluster) = lengthx
         bigcal_all_clstr_ncelly(ncluster) = lengthy
         bigcal_all_clstr_iymax(ncluster) = cluster_temp_irow(1)
         bigcal_all_clstr_ixmax(ncluster) = cluster_temp_icol(1)
        
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
            bigcal_all_clstr_xcell(ncluster,icell) = 
     $           cluster_temp_xcell(icell)
            bigcal_all_clstr_ycell(ncluster,icell) = 
     $           cluster_temp_ycell(icell)
            
            xcell = cluster_temp_xcell(icell)
            ycell = cluster_temp_ycell(icell)
            ecell = cluster_temp_ecell(icell)

            xmom_clst = xmom_clst + ecell*(xcell-xcenter)/esum
            ymom_clst = ymom_clst + ecell*(ycell-ycenter)/esum

            do ihit=1,bigcal_all_ngood
               if(bigcal_all_iygood(ihit).eq.cluster_temp_irow(icell)
     $              .and.bigcal_all_ixgood(ihit).eq.cluster_temp_icol(icell)) 
     $              then        ! zero this hit so it won't be used again
                  bigcal_all_ecell(ihit) = 0.
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

         if(bdebug_print_clusters.ne.0) then
            call b_print_cluster(ncluster,abort,err)
            if(abort) then
               call g_add_path(here,err)
               return
            endif
         endif

      else 
         if(ixmax.eq.1.or.iymax.eq.1.or.iymax.eq.56.or.ixmax.eq.32.or.
     $        (iymax.gt.32.and.ixmax.eq.30)) then
            nmaximum = nmaximum + 1
            bigcal_edge_max(nmaximum) = .true.
         endif
      endif
      
 104  continue

      if(found_cluster) goto 102

      bigcal_all_nclstr = ncluster
      bigcal_nmaxima = nmaximum

      return
      end
