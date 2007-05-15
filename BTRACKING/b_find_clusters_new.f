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

      integer*4 ihit,jhit,khit
      integer*4 icell,jcell,kcell
      integer*4 itdc,jtdc,ktdc
      integer*4 ilogic,jlogic,klogic
      integer*4 irow,jrow,krow,icol,jcol,kcol
      real*4 ecell,xcell,ycell
      integer*4 irow8,icol8
      integer*4 igroup64,ihalf64
      integer*4 maxcelldiff,celldiffx,celldiffy
      real*4 clstr_temp_ecell(BIGCAL_CLSTR_NCELL_MAX)
      integer*4 clstr_temp_ixcell(BIGCAL_CLSTR_NCELL_MAX)
      integer*4 clstr_temp_iycell(BIGCAL_CLSTR_NCELL_MAX)
      real*4 clstr_temp_xcell(BIGCAL_CLSTR_NCELL_MAX)
      real*4 clstr_temp_ycell(BIGCAL_CLSTR_NCELL_MAX)
      real*4 clstr_temp_esum

      real*4 max,xmax,ymax,minxdiff
      integer*4 ixmax,iymax,ihitmax,ixmin,iymin
      integer*4 ncellclust
      integer*4 ncluster
      
      integer*4 nrows

      integer*4 itemp,jtemp,ktemp
      real*4 irtemp,jrtemp,krtemp

      real*4 trigtime
      real*4 xmom,ymom,clstr_time
      real*4 tavg

      integer*4 ntdc

      logical tdchit,trighit
      logical second_max
      logical found_cluster

      trigtime = BIGCAL_REF_TIME

      maxcelldiff = int(sqrt(float(BIGCAL_CLSTR_NCELL_MAX))) / 2

      ncluster = 0

 101  continue
      found_cluster = .false.

      ! zero temporary cluster quantities:
      do icell=1,BIGCAL_CLSTR_NCELL_MAX
         clstr_temp_ecell(icell) = 0.
         clstr_temp_xcell(icell) = 0.
         clstr_temp_ycell(icell) = 0.
         clstr_temp_ixcell(icell) = 0
         clstr_temp_iycell(icell) = 0
      enddo

      max = 0.
      ixmax = 0
      iymax = 0

      ! first step is to find first hit with good timing and with maximum amplitude
      do ihit=1,BIGCAL_PROT_NGOOD
         tdchit = .false.
         trighit = .false.
         ecell = BIGCAL_PROT_ECELL(ihit)
         irow = BIGCAL_PROT_IYGOOD(ihit)
         icol = BIGCAL_PROT_IXGOOD(ihit)
         icell = icol + BIGCAL_PROT_NX * (irow - 1)
         xcell = BIGCAL_PROT_XGOOD(ihit)
         ycell = BIGCAL_PROT_YGOOD(ihit)
         ! determine which tdc channel (group of 8) we are in
         irow8 = irow
         icol8 = (icol - 1) / 8 + 1
         itdc = icol8 + (irow8 - 1)*BIGCAL_MAX_GROUPS
         ! determine which trigger group (group of 64) we are in
         ihalf64 = (icol - 1) / 16 + 1
         igroup64 = (irow - 1) / 3 + 1
         ilogic = igroup64 + (ihalf64 - 1) * BIGCAL_LOGIC_GROUPS / 2

         if(abs(BIGCAL_TIME_DET(itdc) - trigtime).le.b_timing_cut) then
            tdchit = .true.
         endif
         
         if(abs(BIGCAL_TRIG_TIME_DET(ilogic) - trigtime).le.b_timing_cut 
     $        ) then
            trighit = .true.
         endif
      
         if((trighit .or. tdchit) .and. ecell .gt. max) then
            max = ecell
            ixmax = icol
            iymax = irow
            ihitmax = ihit
         endif
      enddo

      ! if, upon looping through all hits, we have found a maximum that has a valid tdc or 
      ! trigger tdc value, then build a cluster from all the neighboring channels using the
      ! detector arrays (regardless of whether there is a hit: some cells may have 0) 
      if(ixmax.ge.2.and.iymax.ge.2.and.ixmax.le.BIGCAL_PROT_NX-1.and.
     $     iymax.le.BIGCAL_PROT_NY-1) then
         ncellclust = 0
         do irow = iymax - maxcelldiff,iymax + maxcelldiff
            do icol = ixmax - maxcelldiff,ixmax + maxcelldiff
               if(irow.ge.1.and.icol.ge.1.and.irow.le.BIGCAL_PROT_NY
     $              .and.icol.le.BIGCAL_PROT_NX) then
                  icell = icol + BIGCAL_PROT_NX*(irow - 1)
                  ncellclust = ncellclust + 1
                  if(ncellclust.gt.BIGCAL_CLSTR_NCELL_MAX) goto 100
                  ecell = BIGCAL_PROT_GOOD_DET(icell)
                  xcell = BIGCAL_PROT_XCENTER(icell)
                  ycell = BIGCAL_PROT_YCENTER(icell)
                  clstr_temp_ecell(ncellclust) = ecell
                  clstr_temp_ixcell(ncellclust) = icol
                  clstr_temp_iycell(ncellclust) = irow
                  clstr_temp_xcell(ncellclust) = xcell
                  clstr_temp_ycell(ncellclust) = ycell
               endif
            enddo
         enddo
 100     continue

         ! in the special case of iymax = 31 add closest cells in the first rows of RCS part
         if(iymax + maxcelldiff .gt. BIGCAL_PROT_NY) then
            nrows = iymax + maxcelldiff - BIGCAL_PROT_NY
            do irow=1,nrows
               minxdiff = 1000.
               xcell = BIGCAL_PROT_XGOOD(ihitmax)
               do icol = 1,BIGCAL_RCS_NX
                  icell = icol + BIGCAL_RCS_NX * (irow - 1)
                  if(abs(xcell - BIGCAL_RCS_XCENTER(icell)).lt.minxdiff)
     $                 then
                     minxdiff = abs(xcell - BIGCAL_RCS_XCENTER(icell))
                     ixmin = icol
                  endif
               enddo
               if(minxdiff .lt. 1000.) then
                  do icol = ixmin - maxcelldiff,ixmin + maxcelldiff
                     icell = icol + BIGCAL_RCS_NX * (irow - 1)
                     if(icol.ge.1.and.icol.le.BIGCAL_RCS_NX) then
                        ncellclust = ncellclust + 1
                        if(ncellclust.gt.BIGCAL_CLSTR_NCELL_MAX)goto 102
                        ecell = BIGCAL_RCS_GOOD_DET(icell)
                        xcell = BIGCAL_RCS_XCENTER(icell)
                        ycell = BIGCAL_RCS_YCENTER(icell)
                        clstr_temp_ecell(ncellclust) = ecell
                        clstr_temp_ixcell(ncellclust) = icol
                        clstr_temp_iycell(ncellclust) =
     $                       irow + BIGCAL_PROT_NY
                        clstr_temp_xcell(ncellclust) = xcell
                        clstr_temp_ycell(ncellclust) = ycell
                     endif
                  enddo
               endif
            enddo
         endif
         ! end special case iymax = 31
         ! check temporary cluster for absence of second max. and sum above 
         ! software threshold:
         ! start by sorting cluster cells in descending order of amplitude:

 102     continue

         clstr_temp_esum = 0.
         do ihit=1,ncellclust
            clstr_temp_esum = clstr_temp_esum + clstr_temp_ecell(ihit)
         enddo

         do ihit=1,ncellclust
            do jhit=ihit+1,ncellclust
               if(clstr_temp_ecell(jhit).gt.clstr_temp_ecell(ihit))then
                  ! switch hits i and j, remember to switch all five quantities
                  irtemp = clstr_temp_ecell(ihit)
                  clstr_temp_ecell(ihit) = clstr_temp_ecell(jhit)
                  clstr_temp_ecell(jhit) = irtemp
                  
                  irtemp = clstr_temp_xcell(ihit)
                  clstr_temp_xcell(ihit) = clstr_temp_xcell(jhit)
                  clstr_temp_xcell(jhit) = irtemp

                  irtemp = clstr_temp_ycell(ihit)
                  clstr_temp_ycell(ihit) = clstr_temp_ycell(jhit)
                  clstr_temp_ycell(jhit) = irtemp

                  itemp = clstr_temp_ixcell(ihit)
                  clstr_temp_ixcell(ihit) = clstr_temp_ixcell(jhit)
                  clstr_temp_ixcell(jhit) = itemp

                  itemp = clstr_temp_iycell(ihit)
                  clstr_temp_iycell(ihit) = clstr_temp_iycell(jhit)
                  clstr_temp_iycell(jhit) = itemp
               endif
            enddo
         enddo

         celldiffx = clstr_temp_ixcell(2) - clstr_temp_ixcell(1)
         celldiffy = clstr_temp_iycell(2) - clstr_temp_iycell(1)
         
         celldiffx = int(abs(float(celldiffx)))
         celldiffy = int(abs(float(celldiffy)))
         
         if(celldiffx .gt. 1.or.celldiffy .gt. 1) then
            second_max = .true.
         endif

         if(clstr_temp_esum.ge.b_cluster_cut.and. .not. second_max) then
            ! add a cluster to the array for protvino:
            found_cluster = .true.
            ncluster = ncluster + 1
            BIGCAL_PROT_CLSTR_IYMAX(ncluster) = clstr_temp_ixcell(1)
            BIGCAL_PROT_CLSTR_IXMAX(ncluster) = clstr_temp_iycell(1)
            BIGCAL_PROT_CLSTR_ETOT(ncluster) = clstr_temp_esum
            BIGCAL_PROT_CLSTR_NCELL(ncluster) = ncellclust

            xmom = 0.
            ymom = 0.
            ntdc = 0
            tavg = 0.

            do ihit=1,ncellclust
               BIGCAL_PROT_CLSTR_IYCELL(ncluster,ihit) = 
     $              clstr_temp_iycell(ihit)
               BIGCAL_PROT_CLSTR_IXCELL(ncluster,ihit) = 
     $              clstr_temp_ixcell(ihit)
               BIGCAL_PROT_CLSTR_ECELL(ncluster,ihit) = 
     $              clstr_temp_ecell(ihit)
               BIGCAL_PROT_CLSTR_XCELL(ncluster,ihit) = 
     $              clstr_temp_xcell(ihit)
               BIGCAL_PROT_CLSTR_YCELL(ncluster,ihit) = 
     $              clstr_temp_ycell(ihit)

               xmom = xmom + clstr_temp_ecell(ihit) / clstr_temp_esum * 
     $              (clstr_temp_xcell(ihit) - clstr_temp_xcell(1))
               ymom = ymom + clstr_temp_ecell(ihit) / clstr_temp_esum * 
     $              (clstr_temp_ycell(ihit) - clstr_temp_ycell(1))

               ! zero the hits belonging to this cluster so that they can't be 
               ! used more than once
               do jhit=1,BIGCAL_PROT_NGOOD
                  if(BIGCAL_PROT_IYGOOD(jhit).eq.clstr_temp_iycell(ihit)
     $                 .and.BIGCAL_PROT_IXGOOD(jhit) .eq.
     $                 clstr_temp_ixcell(ihit)) then
                     BIGCAL_PROT_ECELL(jhit) = 0.
                     BIGCAL_PROT_IYGOOD(jhit) = 0
                     BIGCAL_PROT_IXGOOD(jhit) = 0
                     BIGCAL_PROT_XGOOD(jhit) = 0.
                     BIGCAL_PROT_YGOOD(jhit) = 0.
                  endif
               enddo
               ! do the same for the RCS part in the case of clusters overlapping
               ! the Prot-RCS boundary:
               do jhit=1,BIGCAL_RCS_NGOOD
                  if(BIGCAL_RCS_IYGOOD(jhit)+BIGCAL_PROT_NY.eq.
     $                 clstr_temp_iycell(ihit).and.
     $                 BIGCAL_RCS_IXGOOD(jhit) .eq.
     $                 clstr_temp_ixcell(ihit)) then
                     BIGCAL_RCS_ECELL(jhit) = 0.
                     BIGCAL_RCS_IYGOOD(jhit) = 0
                     BIGCAL_RCS_IXGOOD(jhit) = 0
                     BIGCAL_RCS_XGOOD(jhit) = 0.
                     BIGCAL_RCS_YGOOD(jhit) = 0.
                  endif
               enddo
               irow = clstr_temp_iycell(ihit)
               icol = clstr_temp_ixcell(ihit)
               if(irow.le.BIGCAL_PROT_NY)then
                  icell = icol + BIGCAL_PROT_NX * (irow - 1)
                  irow8 = irow
                  icol8 = (icol - 1) / 8 + 1
                  igroup64 = (irow - 1) / 3 + 1
                  ihalf64 = (icol - 1) / 16 + 1
               else
                  icell = icol + BIGCAL_RCS_NX * (irow-1-BIGCAL_PROT_NY)
                  irow8 = irow
                  if(icol .lt. 16) icol8 = (icol - 1) / 8 + 1
                  if(icol .ge. 16) icol8 = icol / 8 + 1
                  igroup64 = (irow - 1) / 3 + 1
                  ihalf64 = icol / 16 + 1
               endif
               itdc = icol8 + (irow8 - 1) * BIGCAL_MAX_GROUPS
               ilogic = igroup64 + (ihalf64 - 1)*BIGCAL_LOGIC_GROUPS / 2
               if(abs(BIGCAL_TIME_DET(itdc) - trigtime).le.b_timing_cut) 
     $              then
                  ntdc = ntdc + 1
                  tavg = tavg + BIGCAL_TIME_DET(itdc)
               endif
               if(abs(BIGCAL_TRIG_TIME_DET(ilogic) - trigtime).le.
     $              b_timing_cut) then
                  ntdc = ntdc + 1
                  tavg = tavg + BIGCAL_TRIG_TIME_DET(ilogic)
               endif
            enddo
            clstr_time = tavg / ntdc
            BIGCAL_PROT_CLSTR_XMOM(ncluster) = xmom
            BIGCAL_PROT_CLSTR_YMOM(ncluster) = ymom
            BIGCAL_PROT_CLSTR_TIME(ncluster) = clstr_time
         endif
      endif

      if(found_cluster .and. ncluster .lt. BIGCAL_CLSTR_NCELL_MAX) 
     $     goto 101
 
      BIGCAL_PROT_NCLSTR = ncluster

      ncluster = 0

 201  continue
      found_cluster = .false.

      ! zero temporary cluster quantities:
      do icell=1,BIGCAL_CLSTR_NCELL_MAX
         clstr_temp_ecell(icell) = 0.
         clstr_temp_xcell(icell) = 0.
         clstr_temp_ycell(icell) = 0.
         clstr_temp_ixcell(icell) = 0
         clstr_temp_iycell(icell) = 0
      enddo

      max = 0.
      ixmax = 0
      iymax = 0

      ! first step is to find first hit with good timing and with maximum amplitude
      do ihit=1,BIGCAL_RCS_NGOOD
         tdchit = .false.
         trighit = .false.
         ecell = BIGCAL_RCS_ECELL(ihit)
         irow = BIGCAL_RCS_IYGOOD(ihit)
         icol = BIGCAL_RCS_IXGOOD(ihit)
         icell = icol + BIGCAL_RCS_NX * (irow - 1)
         xcell = BIGCAL_RCS_XGOOD(ihit)
         ycell = BIGCAL_RCS_YGOOD(ihit)
         ! determine which tdc channel (group of 8) we are in
         irow8 = irow + BIGCAL_PROT_NY
         if(icol .lt. 16) icol8 = (icol - 1) / 8 + 1
         if(icol .ge. 16) icol8 = icol / 8 + 1
         itdc = icol8 + (irow8 - 1)*BIGCAL_MAX_GROUPS
         ! determine which trigger group (group of 64) we are in
         ihalf64 = icol / 16 + 1
         igroup64 = (irow8 - 1) / 3 + 1
         ilogic = igroup64 + (ihalf64 - 1) * BIGCAL_LOGIC_GROUPS / 2

         if(abs(BIGCAL_TIME_DET(itdc) - trigtime).le.b_timing_cut) then
            tdchit = .true.
         endif
         
         if(abs(BIGCAL_TRIG_TIME_DET(ilogic) - trigtime).le.b_timing_cut 
     $        ) then
            trighit = .true.
         endif
      
         if((trighit .or. tdchit) .and. ecell .gt. max) then
            max = ecell
            ixmax = icol
            iymax = irow
            ihitmax = ihit
         endif
      enddo

      ! if, upon looping through all hits, we have found a maximum that has a valid tdc or 
      ! trigger tdc value, then build a cluster from all the neighboring channels using the
      ! detector arrays (regardless of whether there is a hit: some cells may have 0) 
      if(ixmax.ge.2.and.iymax.ge.2.and.ixmax.le.BIGCAL_RCS_NX-1.and.
     $     iymax.le.BIGCAL_RCS_NY-1) then
         ncellclust = 0
         do irow = iymax - maxcelldiff,iymax + maxcelldiff
            do icol = ixmax - maxcelldiff,ixmax + maxcelldiff
               if(irow.ge.1.and.icol.ge.1.and.irow.le.BIGCAL_RCS_NY
     $              .and.icol.le.BIGCAL_RCS_NX) then
                  icell = icol + BIGCAL_RCS_NX*(irow - 1)
                  ncellclust = ncellclust + 1
                  if(ncellclust.gt.BIGCAL_CLSTR_NCELL_MAX) goto 200
                  ecell = BIGCAL_RCS_GOOD_DET(icell)
                  xcell = BIGCAL_RCS_XCENTER(icell)
                  ycell = BIGCAL_RCS_YCENTER(icell)
                  clstr_temp_ecell(ncellclust) = ecell
                  clstr_temp_ixcell(ncellclust) = icol
                  clstr_temp_iycell(ncellclust) = irow + BIGCAL_PROT_NY
                  clstr_temp_xcell(ncellclust) = xcell
                  clstr_temp_ycell(ncellclust) = ycell
               endif
            enddo
         enddo
 200     continue

         ! in the special case of iymax = 2 add closest cells in the last rows of PROT part
         if(iymax - maxcelldiff .lt. 1) then
            nrows = maxcelldiff - iymax + 1
            do irow=1,nrows
               minxdiff = 1000.
               xcell = BIGCAL_RCS_XGOOD(ihitmax)
               do icol = 1,BIGCAL_PROT_NX
                  icell = icol + BIGCAL_PROT_NX*(BIGCAL_PROT_NY - irow)
                  if(abs(xcell-BIGCAL_PROT_XCENTER(icell)).lt.minxdiff)
     $                 then
                     minxdiff = abs(xcell - BIGCAL_PROT_XCENTER(icell))
                     ixmin = icol
                  endif
               enddo
               if(minxdiff .lt. 1000.) then
                  do icol = ixmin - maxcelldiff,ixmin + maxcelldiff
                     icell = icol+BIGCAL_PROT_NX*(BIGCAL_PROT_NY - irow)
                     if(icol.ge.1.and.icol.le.BIGCAL_PROT_NX) then
                        ncellclust = ncellclust + 1
                        if(ncellclust.gt.BIGCAL_CLSTR_NCELL_MAX)goto 202
                        ecell = BIGCAL_PROT_GOOD_DET(icell)
                        xcell = BIGCAL_PROT_XCENTER(icell)
                        ycell = BIGCAL_PROT_YCENTER(icell)
                        clstr_temp_ecell(ncellclust) = ecell
                        clstr_temp_ixcell(ncellclust) = icol
                        clstr_temp_iycell(ncellclust) =
     $                       BIGCAL_PROT_NY - irow + 1
                        clstr_temp_xcell(ncellclust) = xcell
                        clstr_temp_ycell(ncellclust) = ycell
                     endif
                  enddo
               endif
            enddo
         endif
         ! end special case iymax = 2
         ! check temporary cluster for absence of second max. and sum above 
         ! software threshold:
         ! start by sorting cluster cells in descending order of amplitude:

 202     continue

         clstr_temp_esum = 0.
         do ihit=1,ncellclust
            clstr_temp_esum = clstr_temp_esum + clstr_temp_ecell(ihit)
         enddo

         do ihit=1,ncellclust
            do jhit=ihit+1,ncellclust
               if(clstr_temp_ecell(jhit).gt.clstr_temp_ecell(ihit))then
                  ! switch hits i and j, remember to switch all five quantities
                  irtemp = clstr_temp_ecell(ihit)
                  clstr_temp_ecell(ihit) = clstr_temp_ecell(jhit)
                  clstr_temp_ecell(jhit) = irtemp
                  
                  irtemp = clstr_temp_xcell(ihit)
                  clstr_temp_xcell(ihit) = clstr_temp_xcell(jhit)
                  clstr_temp_xcell(jhit) = irtemp

                  irtemp = clstr_temp_ycell(ihit)
                  clstr_temp_ycell(ihit) = clstr_temp_ycell(jhit)
                  clstr_temp_ycell(jhit) = irtemp

                  itemp = clstr_temp_ixcell(ihit)
                  clstr_temp_ixcell(ihit) = clstr_temp_ixcell(jhit)
                  clstr_temp_ixcell(jhit) = itemp

                  itemp = clstr_temp_iycell(ihit)
                  clstr_temp_iycell(ihit) = clstr_temp_iycell(jhit)
                  clstr_temp_iycell(jhit) = itemp
               endif
            enddo
         enddo

         celldiffx = clstr_temp_ixcell(2) - clstr_temp_ixcell(1)
         celldiffy = clstr_temp_iycell(2) - clstr_temp_iycell(1)
         
         celldiffx = int(abs(float(celldiffx)))
         celldiffy = int(abs(float(celldiffy)))
         
         if(celldiffx .gt. 1.or.celldiffy .gt. 1) then
            second_max = .true.
         endif

         if(clstr_temp_esum.ge.b_cluster_cut.and. .not. second_max) then
            ! add a cluster to the array for rcs:
            found_cluster = .true.
            ncluster = ncluster + 1
            BIGCAL_RCS_CLSTR_IYMAX(ncluster) = clstr_temp_iycell(1)
            BIGCAL_RCS_CLSTR_IXMAX(ncluster) = clstr_temp_ixcell(1)
            BIGCAL_RCS_CLSTR_ETOT(ncluster) = clstr_temp_esum
            BIGCAL_RCS_CLSTR_NCELL(ncluster) = ncellclust

            xmom = 0.
            ymom = 0.
            ntdc = 0
            tavg = 0.

            do ihit=1,ncellclust
               BIGCAL_RCS_CLSTR_IYCELL(ncluster,ihit) = 
     $              clstr_temp_iycell(ihit)
               BIGCAL_RCS_CLSTR_IXCELL(ncluster,ihit) = 
     $              clstr_temp_ixcell(ihit)
               BIGCAL_RCS_CLSTR_ECELL(ncluster,ihit) = 
     $              clstr_temp_ecell(ihit)
               BIGCAL_RCS_CLSTR_XCELL(ncluster,ihit) = 
     $              clstr_temp_xcell(ihit)
               BIGCAL_RCS_CLSTR_YCELL(ncluster,ihit) = 
     $              clstr_temp_ycell(ihit)

               xmom = xmom + clstr_temp_ecell(ihit) / clstr_temp_esum * 
     $              (clstr_temp_xcell(ihit) - clstr_temp_xcell(1))
               ymom = ymom + clstr_temp_ecell(ihit) / clstr_temp_esum * 
     $              (clstr_temp_ycell(ihit) - clstr_temp_ycell(1))

               ! zero the hits belonging to this cluster so that they can't be 
               ! used more than once
               do jhit=1,BIGCAL_RCS_NGOOD
                  if(BIGCAL_RCS_IYGOOD(jhit).eq.clstr_temp_iycell(ihit)
     $                 - BIGCAL_PROT_NY.and.BIGCAL_RCS_IXGOOD(jhit).eq.
     $                 clstr_temp_ixcell(ihit)) then
                     BIGCAL_RCS_ECELL(jhit) = 0.
                     BIGCAL_RCS_IYGOOD(jhit) = 0
                     BIGCAL_RCS_IXGOOD(jhit) = 0
                     BIGCAL_RCS_XGOOD(jhit) = 0.
                     BIGCAL_RCS_YGOOD(jhit) = 0.
                  endif
               enddo
               ! do the same for the Protvino part in case of a cluster straddling the 
               ! two sections
               do jhit=1,BIGCAL_PROT_NGOOD
                  if(BIGCAL_PROT_IYGOOD(jhit).eq.clstr_temp_iycell(ihit)
     $                 .and. BIGCAL_PROT_IXGOOD(jhit).eq.
     $                 clstr_temp_ixcell(ihit))then
                     BIGCAL_PROT_ECELL(jhit) = 0.
                     BIGCAL_PROT_IYGOOD(jhit) = 0
                     BIGCAL_PROT_IXGOOD(jhit) = 0
                     BIGCAL_PROT_XGOOD(jhit) = 0.
                     BIGCAL_PROT_YGOOD(jhit) = 0.
                  endif
               enddo
               irow = clstr_temp_iycell(ihit)
               icol = clstr_temp_ixcell(ihit)
               if(irow.le.BIGCAL_PROT_NY)then
                  icell = icol + BIGCAL_PROT_NX * (irow - 1)
                  irow8 = irow
                  icol8 = (icol - 1) / 8 + 1
                  igroup64 = (irow - 1) / 3 + 1
                  ihalf64 = (icol - 1) / 16 + 1
               else
                  icell = icol + BIGCAL_RCS_NX * (irow-1-BIGCAL_PROT_NY)
                  irow8 = irow
                  if(icol .lt. 16) icol8 = (icol - 1) / 8 + 1
                  if(icol .ge. 16) icol8 = icol / 8 + 1
                  igroup64 = (irow - 1) / 3 + 1
                  ihalf64 = icol / 16 + 1
               endif
               itdc = icol8 + (irow8 - 1) * BIGCAL_MAX_GROUPS
               ilogic = igroup64 + (ihalf64 - 1)*BIGCAL_LOGIC_GROUPS / 2
               if(abs(BIGCAL_TIME_DET(itdc) - trigtime).le.b_timing_cut) 
     $              then
                  ntdc = ntdc + 1
                  tavg = tavg + BIGCAL_TIME_DET(itdc)
               endif
               if(abs(BIGCAL_TRIG_TIME_DET(ilogic) - trigtime).le.
     $              b_timing_cut) then
                  ntdc = ntdc + 1
                  tavg = tavg + BIGCAL_TRIG_TIME_DET(ilogic)
               endif
            enddo
            clstr_time = tavg / ntdc
            BIGCAL_RCS_CLSTR_XMOM(ncluster) = xmom
            BIGCAL_RCS_CLSTR_YMOM(ncluster) = ymom
            BIGCAL_RCS_CLSTR_TIME(ncluster) = clstr_time
         endif
      endif

      if(found_cluster .and. ncluster .lt. BIGCAL_CLSTR_NCELL_MAX) 
     $     goto 201
 
      BIGCAL_RCS_NCLSTR = ncluster
      
      !!!!!!!!!!! come back to middle section later !!!!!!!!!!!!!!!!!
      !!!!! code still needs to be written !!!!!!!!!!!!!!!!!!!!!!!!!!
      return 
      end
