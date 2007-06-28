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
      real*4 ecell,xcell,ycell,xcenter,ycenter
      integer*4 irow8,icol8
      integer*4 igroup64,ihalf64
      integer*4 maxcelldiff,celldiffx,celldiffy
      real*4 clstr_temp_ecell(BIGCAL_CLSTR_NCELL_MAX)
      integer*4 clstr_temp_ixcell(BIGCAL_CLSTR_NCELL_MAX)
      integer*4 clstr_temp_iycell(BIGCAL_CLSTR_NCELL_MAX)
      real*4 clstr_temp_xcell(BIGCAL_CLSTR_NCELL_MAX)
      real*4 clstr_temp_ycell(BIGCAL_CLSTR_NCELL_MAX)
      real*4 clstr_temp_esum

      logical goodhit(BIGCAL_CLSTR_NCELL_MAX)

      real*4 emax,xmax,ymax,minxdiff
      integer*4 ixmax,iymax,ihitmax,ixmin,iymin
      integer*4 ncellclust
      integer*4 ncluster
      integer*4 nbad
      
      integer*4 nrows

      integer*4 itemp,jtemp,ktemp
      real*4 irtemp,jrtemp,krtemp

      real*4 trigtime,hittime
      real*4 xmom,ymom,clstr_time
      real*4 tavg

      integer*4 ntdc

      logical tdchit,trighit
      logical second_max
      logical found_cluster

      !trigtime = BIGCAL_REF_TIME

      maxcelldiff = int(sqrt(float(BIGCAL_CLSTR_NCELL_MAX))) / 2

      if(bbypass_prot.ne.0.or.bbypass_rcs.ne.0) return 
      ! at a minimum, the glass ADC information is required to look for clusters
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
         goodhit(icell) = .false.
      enddo

      emax = 0.
      ixmax = 0
      iymax = 0

      ! find clusters. Only check timing if bypass switches are set for timing analysis
      do ihit=1,BIGCAL_PROT_NGOOD
         !tdchit = .false.
         !trighit = .false.
         ecell = BIGCAL_PROT_ECELL(ihit)
         irow = BIGCAL_PROT_IYGOOD(ihit)
         icol = BIGCAL_PROT_IXGOOD(ihit)
         icell = icol + BIGCAL_PROT_NX * (irow - 1)
         xcell = BIGCAL_PROT_XGOOD(ihit)
         ycell = BIGCAL_PROT_YGOOD(ihit)
         ! determine which tdc channel (group of 8) we are in
         !irow8 = irow
         !icol8 = (icol - 1) / 8 + 1
         !itdc = icol8 + (irow8 - 1)*BIGCAL_MAX_GROUPS
         ! determine which trigger group (group of 64) we are in
         !ihalf64 = (icol - 1) / 16 + 1
         !igroup64 = (irow - 1) / 3 + 1
         !ilogic = igroup64 + (ihalf64 - 1) * BIGCAL_LOGIC_GROUPS / 2

c$$$         if(bbypass_sum8.eq.0) then
c$$$            if(abs(BIGCAL_TIME_DET(itdc)-trigtime).le.b_timing_cut) then
c$$$               tdchit = .true.
c$$$            endif
c$$$         endif
c$$$         if(bbypass_sum64.eq.0) then
c$$$            if(abs(BIGCAL_TRIG_TIME_DET(ilogic)-trigtime).le.
c$$$     $           b_timing_cut) then
c$$$               trighit = .true.
c$$$            endif
c$$$         endif
         !if(bbypass_sum64.eq.0.or.bbypass_sum8.eq.0) then
         if(ecell .gt. emax) then
            emax = ecell
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
     $           .and.icol.le.BIGCAL_PROT_NX) then
              icell = icol + BIGCAL_PROT_NX*(irow - 1)

              ecell = BIGCAL_PROT_GOOD_DET(icell)
              xcell = BIGCAL_PROT_XCENTER(icell)
              ycell = BIGCAL_PROT_YCENTER(icell)
              
              if(ecell.ge.b_cell_cut_prot) then
                 ncellclust = ncellclust + 1
                 if(ncellclust.gt.BIGCAL_CLSTR_NCELL_MAX) goto 100
                 
                 clstr_temp_ecell(ncellclust) = ecell
                 clstr_temp_ixcell(ncellclust) = icol
                 clstr_temp_iycell(ncellclust) = irow
                 clstr_temp_xcell(ncellclust) = xcell
                 clstr_temp_ycell(ncellclust) = ycell
                 goodhit(ncellclust) = .true.
              endif
              
            endif
          enddo
        enddo
 100    continue
        
c     ! in the special case of iymax = 31 add closest cells in the first rows of RCS part
        if(iymax + maxcelldiff .gt. BIGCAL_PROT_NY) then
          nrows = iymax + maxcelldiff - BIGCAL_PROT_NY
          do irow=1,nrows
c$$$            minxdiff = 1000.
c$$$            xcell = BIGCAL_PROT_XGOOD(ihitmax)
c$$$            do icol = max(1,ixmax-5),min(ixmax+5,BIGCAL_RCS_NX)
c$$$              icell = icol + BIGCAL_RCS_NX * (irow - 1)
c$$$              if(abs(xcell - BIGCAL_RCS_XCENTER(icell)).lt.minxdiff)
c$$$     $             then
c$$$                minxdiff = abs(xcell - BIGCAL_RCS_XCENTER(icell))
c$$$                ixmin = icol
c$$$              endif
c$$$            enddo

             ixmin = bigcal_ixclose_prot(ixmax)

c            if(minxdiff .lt. 1000.) then
             do icol = max(ixmin-maxcelldiff,1),min(ixmin+maxcelldiff,30)
c$$$              do icol = ixmin - maxcelldiff,ixmin + maxcelldiff
                icell = icol + BIGCAL_RCS_NX * (irow - 1)
c                if(icol.ge.1.and.icol.le.BIGCAL_RCS_NX) then

                ecell = BIGCAL_RCS_GOOD_DET(icell)
                xcell = BIGCAL_RCS_XCENTER(icell)
                ycell = BIGCAL_RCS_YCENTER(icell)

                if(ecell.ge.b_cell_cut_rcs) then
                   ncellclust = ncellclust + 1
                   if(ncellclust.gt.BIGCAL_CLSTR_NCELL_MAX)goto 102
                      
                   clstr_temp_ecell(ncellclust) = ecell
                   clstr_temp_ixcell(ncellclust) = icol
                   clstr_temp_iycell(ncellclust) =
     $                  irow + BIGCAL_PROT_NY
                   clstr_temp_xcell(ncellclust) = xcell
                   clstr_temp_ycell(ncellclust) = ycell
                   goodhit(ncellclust)=.true.
                endif
                   
c                endif
             enddo
c           endif
          enddo
       endif
c     ! end special case iymax = 31
c     ! check temporary cluster for absence of second max. and sum above 
c     ! software threshold:
c     ! start by sorting cluster cells in descending order of amplitude:

 102    continue
        
        if(ncellclust.gt.BIGCAL_CLSTR_NCELL_MAX) then 
          ncellclust = BIGCAL_CLSTR_NCELL_MAX
        endif
        
        if(ncellclust.lt.bigcal_clstr_ncell_min) then
           bigcal_prot_bad_clstr_flag(ncluster) = 2
c$$$           call b_trig_check(2,ncellclust,clstr_temp_iycell,
c$$$     $          clstr_temp_ixcell,clstr_temp_ecell,abort,err)
           goto 134
        endif

        clstr_temp_esum = 0.
        do ihit=1,ncellclust
          clstr_temp_esum = clstr_temp_esum + clstr_temp_ecell(ihit)
        enddo
        
        if(clstr_temp_esum .lt.b_cluster_cut) then
           bigcal_prot_bad_clstr_flag(ncluster) = 3
c$$$           call b_trig_check(3,ncellclust,clstr_temp_iycell,
c$$$     $          clstr_temp_ixcell,clstr_temp_ecell,abort,err)
           goto 134
        endif

        do ihit=1,ncellclust
           do jhit=ihit+1,ncellclust
              if(clstr_temp_ecell(jhit).gt.clstr_temp_ecell(ihit))then
c     ! switch hits i and j, remember to switch all five quantities
c     ! at this point all goodhit are true, so no need to switch
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
        else
           second_max = .false.
        endif
        
        if(clstr_temp_iycell(1).ne.iymax.or.clstr_temp_ixcell(1).ne.ixmax) then
           second_max = .true.
        endif

        if(second_max) then
           bigcal_prot_bad_clstr_flag(ncluster) = 4
c$$$           call b_trig_check(4,ncellclust,clstr_temp_iycell,
c$$$     $          clstr_temp_ixcell,clstr_temp_ecell,abort,err)
           goto 134
        endif

        if(clstr_temp_esum.ge.b_cluster_cut.and. .not. second_max) then
c     !add a cluster to the array for protvino:
          if(ncellclust.ge.BIGCAL_CLSTR_NCELL_MIN) then
            found_cluster = .true.
            ncluster = ncluster + 1
            BIGCAL_PROT_CLSTR_IYMAX(ncluster) = clstr_temp_iycell(1)
            BIGCAL_PROT_CLSTR_IXMAX(ncluster) = clstr_temp_ixcell(1)
            BIGCAL_PROT_CLSTR_ETOT(ncluster) = clstr_temp_esum
            BIGCAL_PROT_CLSTR_NCELL(ncluster) = ncellclust
            
            xmom = 0.
            ymom = 0.
            
            do ihit=1,ncellclust
              BIGCAL_PROT_CLSTR_IYCELL(ncluster,ihit) = 
     $             clstr_temp_iycell(ihit)
              BIGCAL_PROT_CLSTR_IXCELL(ncluster,ihit) = 
     $             clstr_temp_ixcell(ihit)
              BIGCAL_PROT_CLSTR_ECELL(ncluster,ihit) = 
     $             clstr_temp_ecell(ihit)
              BIGCAL_PROT_CLSTR_XCELL(ncluster,ihit) = 
     $             clstr_temp_xcell(ihit)
              BIGCAL_PROT_CLSTR_YCELL(ncluster,ihit) = 
     $             clstr_temp_ycell(ihit)
              
              xmom = xmom+clstr_temp_ecell(ihit)/clstr_temp_esum 
     $             *(clstr_temp_xcell(ihit)-clstr_temp_xcell(1))
              ymom = ymom+clstr_temp_ecell(ihit)/clstr_temp_esum  
     $             *(clstr_temp_ycell(ihit)-clstr_temp_ycell(1))
              
c     ! zero the hits belonging to this cluster so that they can't be 
c     ! used more than once

              irow = clstr_temp_iycell(ihit)
              icol = clstr_temp_ixcell(ihit)

              if(irow.le.bigcal_prot_ny) then
                 icell = icol + bigcal_prot_nx*(irow-1)
                 bigcal_prot_good_det(icell) = 0.
              else
                 icell = icol + bigcal_rcs_nx*(irow-33)
                 bigcal_rcs_good_det(icell) = 0.
              endif

              do jhit=1,BIGCAL_PROT_NGOOD
                if(BIGCAL_PROT_IYGOOD(jhit).eq.
     $               clstr_temp_iycell(ihit).and.
     $               BIGCAL_PROT_IXGOOD(jhit).eq.
     $               clstr_temp_ixcell(ihit)) then
                  BIGCAL_PROT_ECELL(jhit) = 0.
                  BIGCAL_PROT_IYGOOD(jhit) = 0
                  BIGCAL_PROT_IXGOOD(jhit) = 0
                  BIGCAL_PROT_XGOOD(jhit) = 0.
                  BIGCAL_PROT_YGOOD(jhit) = 0.
                endif
              enddo
c     ! do the same for the RCS part in the case of clusters overlapping
c     ! the Prot-RCS boundary:
              do jhit=1,BIGCAL_RCS_NGOOD
                if(BIGCAL_RCS_IYGOOD(jhit)+BIGCAL_PROT_NY.eq.
     $               clstr_temp_iycell(ihit).and.
     $               BIGCAL_RCS_IXGOOD(jhit) .eq.
     $               clstr_temp_ixcell(ihit)) then
                  BIGCAL_RCS_ECELL(jhit) = 0.
                  BIGCAL_RCS_IYGOOD(jhit) = 0
                  BIGCAL_RCS_IXGOOD(jhit) = 0
                  BIGCAL_RCS_XGOOD(jhit) = 0.
                  BIGCAL_RCS_YGOOD(jhit) = 0.
                endif
              enddo
            enddo
            BIGCAL_PROT_CLSTR_XMOM(ncluster) = xmom
            BIGCAL_PROT_CLSTR_YMOM(ncluster) = ymom
            
            if(bdebug_print_clusters.ne.0) call b_print_cluster(1,ncluster,ABORT,err)
            
         endif
      endif

      else if(ixmax.eq.1.or.ixmax.eq.bigcal_prot_nx.or.iymax.eq.1.or.
     $     iymax.eq.bigcal_prot_ny) then ! max is at edge
         bigcal_prot_bad_clstr_flag(ncluster) = 1
c$$$         call b_trig_check(1,ncellclust,clstr_temp_iycell,
c$$$     $          clstr_temp_ixcell,clstr_temp_ecell,abort,err)
         !write(*,*) 'edge max! ncluster = ',ncluster
      endif
      
 134  continue

      if((.not.found_cluster).and.ncluster.eq.0) then
         bigcal_prot_no_clstr_why = bigcal_prot_bad_clstr_flag(0)
      endif

      if(found_cluster .and. ncluster .lt. BIGCAL_PROT_NCLSTR_MAX) then 
         goto 101
      endif

      BIGCAL_PROT_NCLSTR = ncluster
      
      ncluster = 0
      
 201  continue
      found_cluster = .false.

c     ! zero temporary cluster quantities:
      do icell=1,BIGCAL_CLSTR_NCELL_MAX
        clstr_temp_ecell(icell) = 0.
        clstr_temp_xcell(icell) = 0.
        clstr_temp_ycell(icell) = 0.
        clstr_temp_ixcell(icell) = 0
        clstr_temp_iycell(icell) = 0
        goodhit(icell) = .false.
      enddo
      
      emax = 0.
      ixmax = 0
      iymax = 0
      
c     ! find clusters. Only check timing if bypass switches are set for timing analysis
      do ihit=1,BIGCAL_RCS_NGOOD
c     !tdchit = .false.
c     !trighit = .false.
        ecell = BIGCAL_RCS_ECELL(ihit)
        irow = BIGCAL_RCS_IYGOOD(ihit)
        icol = BIGCAL_RCS_IXGOOD(ihit)
        icell = icol + BIGCAL_RCS_NX * (irow - 1)
        xcell = BIGCAL_RCS_XGOOD(ihit)
        ycell = BIGCAL_RCS_YGOOD(ihit)
c     ! determine which tdc channel (group of 8) we are in
c     !irow8 = irow
c     !icol8 = (icol - 1) / 8 + 1
c     !itdc = icol8 + (irow8 - 1)*BIGCAL_MAX_GROUPS
c     ! determine which trigger group (group of 64) we are in
c     !ihalf64 = (icol - 1) / 16 + 1
c     !igroup64 = (irow - 1) / 3 + 1
c     !ilogic = igroup64 + (ihalf64 - 1) * BIGCAL_LOGIC_GROUPS / 2

c$$$         if(bbypass_sum8.eq.0) then
c$$$            if(abs(BIGCAL_TIME_DET(itdc)-trigtime).le.b_timing_cut) then
c$$$               tdchit = .true.
c$$$            endif
c$$$         endif
c$$$  if(bbypass_sum64.eq.0) then
c$$$  if(abs(BIGCAL_TRIG_TIME_DET(ilogic)-trigtime).le.
c$$$  $           b_timing_cut) then
c$$$  trighit = .true.
c$$$  endif
c$$$  endif
c     !if(bbypass_sum64.eq.0.or.bbypass_sum8.eq.0) then
        if(ecell .gt. emax) then
          emax = ecell
          ixmax = icol
          iymax = irow
          ihitmax = ihit
        endif
      enddo

c     ! if, upon looping through all hits, we have found a maximum that has a valid tdc or 
c     ! trigger tdc value, then build a cluster from all the neighboring channels using the
c     ! detector arrays (regardless of whether there is a hit: some cells may have 0) 
      if(ixmax.ge.2.and.iymax.ge.2.and.ixmax.le.BIGCAL_RCS_NX-1.and.
     $     iymax.le.BIGCAL_RCS_NY-1) then
        ncellclust = 0
        do irow = iymax - maxcelldiff,iymax + maxcelldiff
          do icol = ixmax - maxcelldiff,ixmax + maxcelldiff
            if(irow.ge.1.and.icol.ge.1.and.irow.le.BIGCAL_RCS_NY
     $           .and.icol.le.BIGCAL_RCS_NX) then
              icell = icol + BIGCAL_RCS_NX*(irow - 1)
              
              ecell = BIGCAL_RCS_GOOD_DET(icell)
              xcell = BIGCAL_RCS_XCENTER(icell)
              ycell = BIGCAL_RCS_YCENTER(icell)
            
              if(ecell.ge.b_cell_cut_rcs) then
                 ncellclust = ncellclust + 1
                 if(ncellclust.gt.BIGCAL_CLSTR_NCELL_MAX) goto 200
              
                 clstr_temp_ecell(ncellclust) = ecell
                 clstr_temp_ixcell(ncellclust) = icol
                 clstr_temp_iycell(ncellclust) = irow + 
     $                BIGCAL_PROT_NY
                 clstr_temp_xcell(ncellclust) = xcell
                 clstr_temp_ycell(ncellclust) = ycell
                 goodhit(ncellclust) = .true.
              endif

            endif
          enddo
        enddo
 200    continue

c     ! in the special case of iymax = 34 add closest cells in the first rows of RCS part
        if(iymax - maxcelldiff .lt. 1) then
          nrows = maxcelldiff - iymax + 1
          do irow=1,nrows
c$$$            minxdiff = 1000.
c$$$            xcell = BIGCAL_RCS_XGOOD(ihitmax)
c$$$            do icol = max(1,ixmax-5),min(ixmax+5,BIGCAL_PROT_NX)
c$$$              icell = icol + BIGCAL_PROT_NX * (BIGCAL_PROT_NY-irow)
c$$$              if(abs(xcell-BIGCAL_PROT_XCENTER(icell)).lt.minxdiff)
c$$$     $             then
c$$$                minxdiff = abs(xcell - BIGCAL_PROT_XCENTER(icell))
c$$$                ixmin = icol
c$$$              endif
c$$$            enddo
c            if(minxdiff .lt. 1000.) then
             ixmin = bigcal_ixclose_rcs(ixmax)

             do icol = max(ixmin-maxcelldiff,1),min(ixmin+maxcelldiff,32)
                icell = icol + BIGCAL_PROT_NX*(BIGCAL_PROT_NY-irow)
c     if(icol.ge.1.and.icol.le.BIGCAL_PROT_NX) then
                
                ecell = BIGCAL_PROT_GOOD_DET(icell)
                xcell = BIGCAL_PROT_XCENTER(icell)
                ycell = BIGCAL_PROT_YCENTER(icell)

                if(ecell.ge.b_cell_cut_prot) then
                   ncellclust = ncellclust + 1
                   if(ncellclust.gt.BIGCAL_CLSTR_NCELL_MAX)goto 202
                   
                   clstr_temp_ecell(ncellclust) = ecell
                   clstr_temp_ixcell(ncellclust) = icol
                   clstr_temp_iycell(ncellclust) = BIGCAL_PROT_NY 
     $                  - irow + 1
                   clstr_temp_xcell(ncellclust) = xcell
                   clstr_temp_ycell(ncellclust) = ycell
                   goodhit(ncellclust)=.true.
                endif
                   
c                endif
             enddo
c     endif
          enddo
        endif
c     ! end special case iymax = 34
c     ! check temporary cluster for absence of second max. and sum above 
c     ! software threshold:
c     ! start by sorting cluster cells in descending order of amplitude:
        
 202    continue

        if(ncellclust.gt.BIGCAL_CLSTR_NCELL_MAX) then 
          ncellclust = BIGCAL_CLSTR_NCELL_MAX
        endif
        
        if(ncellclust.lt.bigcal_clstr_ncell_min) then
           bigcal_rcs_bad_clstr_flag(ncluster) = 2
           goto 234
        endif

        clstr_temp_esum = 0.
        do ihit=1,ncellclust
          clstr_temp_esum = clstr_temp_esum + clstr_temp_ecell(ihit)
        enddo
        
        if(clstr_temp_esum.lt.b_cluster_cut) then
           bigcal_rcs_bad_clstr_flag(ncluster) = 3
           goto 234
        endif

        do ihit=1,ncellclust
          do jhit=ihit+1,ncellclust
            if(clstr_temp_ecell(jhit).gt.clstr_temp_ecell(ihit))then
c     ! switch hits i and j, remember to switch all five quantities
c     ! at this point all goodhit are true, so no need to switch
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
        else 
          second_max = .false.
        endif

        if(clstr_temp_iycell(1)-bigcal_prot_ny.ne.iymax.or.
     $       clstr_temp_ixcell(1).ne.ixmax) then
           second_max = .true.
        endif

        if(second_max) then
           bigcal_rcs_bad_clstr_flag(ncluster) = 4
           goto 234
        endif
        
        if(clstr_temp_esum.ge.b_cluster_cut.and. .not. second_max) then
c     !add a cluster to the array for rcs:
          if(ncellclust.ge.BIGCAL_CLSTR_NCELL_MIN) then
            found_cluster = .true.
            ncluster = ncluster + 1
            BIGCAL_RCS_CLSTR_IYMAX(ncluster) = clstr_temp_iycell(1)
            BIGCAL_RCS_CLSTR_IXMAX(ncluster) = clstr_temp_ixcell(1)
            BIGCAL_RCS_CLSTR_ETOT(ncluster) = clstr_temp_esum
            BIGCAL_RCS_CLSTR_NCELL(ncluster) = ncellclust
            
            xmom = 0.
            ymom = 0.
            
            do ihit=1,ncellclust
              BIGCAL_RCS_CLSTR_IYCELL(ncluster,ihit) = 
     $             clstr_temp_iycell(ihit)
              BIGCAL_RCS_CLSTR_IXCELL(ncluster,ihit) = 
     $             clstr_temp_ixcell(ihit)
              BIGCAL_RCS_CLSTR_ECELL(ncluster,ihit) = 
     $             clstr_temp_ecell(ihit)
              BIGCAL_RCS_CLSTR_XCELL(ncluster,ihit) = 
     $             clstr_temp_xcell(ihit)
              BIGCAL_RCS_CLSTR_YCELL(ncluster,ihit) = 
     $             clstr_temp_ycell(ihit)
              
              xmom = xmom+clstr_temp_ecell(ihit)/clstr_temp_esum 
     $             *(clstr_temp_xcell(ihit)-clstr_temp_xcell(1))
              ymom = ymom+clstr_temp_ecell(ihit)/clstr_temp_esum  
     $             *(clstr_temp_ycell(ihit)-clstr_temp_ycell(1))
              

              icol = clstr_temp_ixcell(ihit)
              irow = clstr_temp_iycell(ihit)

              if(irow.le.bigcal_prot_ny) then
                 icell = icol + bigcal_prot_nx*(irow-1)
                 bigcal_prot_good_det(icell) = 0.
              else
                 icell = icol + bigcal_rcs_nx*(irow-33)
                 bigcal_rcs_good_det(icell) = 0.
              endif

c     ! zero the hits belonging to this cluster so that they can't be 
c     ! used more than once
              do jhit=1,BIGCAL_PROT_NGOOD
                if(BIGCAL_PROT_IYGOOD(jhit).eq.
     $               clstr_temp_iycell(ihit).and.
     $               BIGCAL_PROT_IXGOOD(jhit).eq.
     $               clstr_temp_ixcell(ihit)) then
                  BIGCAL_PROT_ECELL(jhit) = 0.
                  BIGCAL_PROT_IYGOOD(jhit) = 0
                  BIGCAL_PROT_IXGOOD(jhit) = 0
                  BIGCAL_PROT_XGOOD(jhit) = 0.
                  BIGCAL_PROT_YGOOD(jhit) = 0.
                endif
              enddo
c     ! do the same for the RCS part in the case of clusters overlapping
c     ! the Prot-RCS boundary:
              do jhit=1,BIGCAL_RCS_NGOOD
                if(BIGCAL_RCS_IYGOOD(jhit)+BIGCAL_PROT_NY.eq.
     $               clstr_temp_iycell(ihit).and.
     $               BIGCAL_RCS_IXGOOD(jhit) .eq.
     $               clstr_temp_ixcell(ihit)) then
                  BIGCAL_RCS_ECELL(jhit) = 0.
                  BIGCAL_RCS_IYGOOD(jhit) = 0
                  BIGCAL_RCS_IXGOOD(jhit) = 0
                  BIGCAL_RCS_XGOOD(jhit) = 0.
                  BIGCAL_RCS_YGOOD(jhit) = 0.
                endif
              enddo
            enddo
          
            BIGCAL_RCS_CLSTR_XMOM(ncluster) = xmom
            BIGCAL_RCS_CLSTR_YMOM(ncluster) = ymom

            if(bdebug_print_clusters.ne.0) call b_print_cluster(2,ncluster,ABORT,err)
            
          endif
        endif
      else if(ixmax.eq.1.or.ixmax.eq.bigcal_rcs_nx.or.iymax.eq.1.or.
     $       iymax.eq.bigcal_rcs_ny) then ! max at edge
         bigcal_rcs_bad_clstr_flag(ncluster) = 1
         !write(*,*) 'edge max! ncluster = ',ncluster
      endif

 234  continue
      
      if((.not.found_cluster).and.ncluster.eq.0) then
         bigcal_rcs_no_clstr_why = bigcal_rcs_bad_clstr_flag(0)
      endif

      if(found_cluster .and. ncluster .lt. BIGCAL_RCS_NCLSTR_MAX) then 
         goto 201
      endif
      BIGCAL_RCS_NCLSTR = ncluster
      
c     !!!!!!!!Come back to the middle section later!!!!!!!!!!!!!!1
      
c     first fill middle hit arrays:

      do irow=30,32
         do icol=1,32
            icell = icol + 32*(irow-1)
            bigcal_mid_ehit(irow,icol) = bigcal_prot_good_det(icell)
            bigcal_mid_xhit(irow,icol) = bigcal_prot_xcenter(icell)
            bigcal_mid_yhit(irow,icol) = bigcal_prot_ycenter(icell)
         enddo
      enddo
      
      do irow=33,35
         do icol=1,30
            icell = icol + 30*(irow-33)
            bigcal_mid_ehit(irow,icol) = bigcal_rcs_good_det(icell)
            bigcal_mid_xhit(irow,icol) = bigcal_rcs_xcenter(icell)
            bigcal_mid_yhit(irow,icol) = bigcal_rcs_ycenter(icell)
         enddo
      enddo  
      
c$$$      !write(*,*) 'mid_ehit = ',bigcal_mid_ehit
c$$$      write(*,*) 'mid_xhit = ',bigcal_mid_xhit
c$$$      write(*,*) 'mid_yhit = ',bigcal_mid_yhit

c     find maximum 
      
      ncluster = 0
      
 301  continue 
      
      found_cluster = .false.
c     zero temporary cluster quantities!!!!
      do icell=1,BIGCAL_CLSTR_NCELL_MAX
         clstr_temp_ecell(icell) = 0.
         clstr_temp_xcell(icell) = 0.
         clstr_temp_ycell(icell) = 0.
         clstr_temp_ixcell(icell) = 0
         clstr_temp_iycell(icell) = 0
         goodhit(icell) = .false.
      enddo
      
      ixmax = 0
      iymax = 0
      
      emax = 0.

      do irow = 30,35
         do icol = 1,32
            if(bigcal_mid_ehit(irow,icol).gt.emax) then
               if( icol.le.30 .or. irow .le. 32) then
                  emax = bigcal_mid_ehit(irow,icol)
                  ixmax = icol
                  iymax = irow
               endif
            endif
         enddo
      enddo
      
c$$$      if(iymax.ne.0) write(*,*) 'row = ',iymax,' col = ',ixmax
      
      if(iymax.eq.32) then
         if(ixmax.ge.2.and.ixmax.le.31) then
            
c$$$            minxdiff = 10000.
c$$$
c$$$            do icol = max(ixmax-5,1),min(ixmax+5,30)
c$$$               if(abs(bigcal_mid_xhit(33,icol)-bigcal_mid_xhit(32,ixmax))
c$$$     $              .lt.minxdiff) then
c$$$                  ixmin = icol
c$$$                  minxdiff = abs(bigcal_mid_xhit(33,icol) - 
c$$$     $                 bigcal_mid_xhit(32,ixmax))
c$$$               endif
c$$$            enddo

c     !write(*,*) 'colmindiff = ',ixmin
            
            ixmin = bigcal_ixclose_prot(ixmax)

            ncellclust = 0
            
c            if(minxdiff.lt.10000.) then
            do irow = 30,32
               do icol = max(ixmax-2,1),min(ixmax+2,32)
                  
                  ecell = bigcal_mid_ehit(irow,icol)
                  xcell = bigcal_mid_xhit(irow,icol)
                  ycell = bigcal_mid_yhit(irow,icol)
                  
                  if(ecell.ge.b_cell_cut_prot) then
                     
                     ncellclust = ncellclust + 1
                     if(ncellclust.gt.bigcal_clstr_ncell_max)goto 300
                     
                     clstr_temp_ecell(ncellclust) = ecell
                     clstr_temp_xcell(ncellclust) = xcell
                     clstr_temp_ycell(ncellclust) = ycell
                     clstr_temp_ixcell(ncellclust) = icol
                     clstr_temp_iycell(ncellclust) = irow
                  endif
               enddo
            enddo
            
            do irow = 33,34
               do icol = max(ixmin-2,1),min(ixmin+2,30)
                  
                  ecell = bigcal_mid_ehit(irow,icol)
                  xcell = bigcal_mid_xhit(irow,icol)
                  ycell = bigcal_mid_yhit(irow,icol)
                  
                  if(ecell.ge.b_cell_cut_rcs) then
                     ncellclust = ncellclust + 1
                     if(ncellclust.gt.bigcal_clstr_ncell_max)goto 300
                     
                     clstr_temp_ecell(ncellclust) = ecell
                     clstr_temp_xcell(ncellclust) = xcell
                     clstr_temp_ycell(ncellclust) = ycell
                     clstr_temp_ixcell(ncellclust) = icol
                     clstr_temp_iycell(ncellclust) = irow
                  endif
               enddo
            enddo
c     endif ! otherwise something doesn't make sense
 300        continue
          
            if(ncellclust.gt.bigcal_clstr_ncell_max) then
               ncellclust = bigcal_clstr_ncell_max
            endif

            if(ncellclust.lt.bigcal_clstr_ncell_min) then
               bigcal_mid_bad_clstr_flag(ncluster) = 2
               goto 334
            endif

c     sort in descending order of amplitude:
            
            clstr_temp_esum = 0.
            do ihit=1,ncellclust
               clstr_temp_esum = clstr_temp_esum + clstr_temp_ecell(ihit)
            enddo

            if(clstr_temp_esum.lt.b_cluster_cut) then
               bigcal_mid_bad_clstr_flag(ncluster) = 3
               goto 334
            endif

            do ihit=1,ncellclust
               do jhit=ihit+1,ncellclust
                  if(clstr_temp_ecell(jhit).gt.clstr_temp_ecell(ihit))then
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
            
c     !write(*,*) 'ecell = ',clstr_temp_ecell
c     !write(*,*) 'iycell = ',clstr_temp_iycell
c     !write(*,*) 'ixcell = ',clstr_temp_ixcell

            celldiffx = clstr_temp_ixcell(2) - clstr_temp_ixcell(1)
            celldiffy = clstr_temp_iycell(2) - clstr_temp_iycell(1)

            if( (clstr_temp_iycell(1).le.bigcal_prot_ny.and.
     $           celldiffy.gt.0) .or. (clstr_temp_iycell(1).gt.
     $           bigcal_prot_ny.and.celldiffy.lt.0) ) then 
               celldiffx = celldiffx + clstr_temp_ixcell(1) - ixmin
            endif
            
            celldiffx = int(abs(float(celldiffx)))
            celldiffy = int(abs(float(celldiffy)))
            
            if(celldiffx.gt.1.or.celldiffy.gt.1) then
               second_max = .true.
            else
               second_max = .false.
            endif
          
            if(clstr_temp_iycell(1).ne.iymax.or.clstr_temp_ixcell(1)
     $           .ne.ixmax) then
               second_max = .true.
            endif

            if(second_max) then
               bigcal_mid_bad_clstr_flag(ncluster) = 4
               goto 334
            endif

            if(clstr_temp_esum.ge.b_cluster_cut.and. .not. second_max
     $           .and.ncellclust.gt.bigcal_clstr_ncell_min)then
c     ADD A CLUSTER TO THE ARRAY FOR THE MIDDLE SECTION!
               found_cluster = .true.

c     !write(*,*) 'found good cluster in mid section!!!!'
            
               ncluster = ncluster + 1
               bigcal_mid_clstr_ncell(ncluster) = ncellclust
               bigcal_mid_clstr_iymax(ncluster) = clstr_temp_iycell(1)
               bigcal_mid_clstr_ixmax(ncluster) = clstr_temp_ixcell(1)
               bigcal_mid_clstr_etot(ncluster) = clstr_temp_esum
               
               xmom = 0.
               ymom = 0.
               
               xcenter = clstr_temp_xcell(1)
               ycenter = clstr_temp_ycell(1)
               
               do icell=1,ncellclust
                  icol = clstr_temp_ixcell(icell)
                  irow = clstr_temp_iycell(icell)
                  xcell = clstr_temp_xcell(icell)
                  ycell = clstr_temp_ycell(icell)
                  ecell = clstr_temp_ecell(icell)
c     zero the hits from this array so that they can't be used again:
                  bigcal_mid_ehit(irow,icol) = 0.
                  bigcal_mid_xhit(irow,icol) = 0.
                  bigcal_mid_yhit(irow,icol) = 0.
                  
                  xmom = xmom + (xcell - xcenter)* ecell / clstr_temp_esum
                  ymom = ymom + (ycell - ycenter)* ecell / clstr_temp_esum
                  
                  bigcal_mid_clstr_ixcell(ncluster,icell) = icol
                  bigcal_mid_clstr_iycell(ncluster,icell) = irow
                  bigcal_mid_clstr_xcell(ncluster,icell) = xcell
                  bigcal_mid_clstr_ycell(ncluster,icell) = ycell
                  bigcal_mid_clstr_ecell(ncluster,icell) = ecell
               enddo
            
               bigcal_mid_clstr_xmom(ncluster) = xmom
               bigcal_mid_clstr_ymom(ncluster) = ymom

            endif
         else if(ixmax.eq.1.or.ixmax.eq.bigcal_prot_nx) then
            bigcal_mid_bad_clstr_flag(ncluster) = 1
            
            !write(*,*) 'edge max! ncluster = ',ncluster
         endif
      else if(iymax.eq.33) then
         if(ixmax.ge.2.and.ixmax.le.29) then
c$$$            minxdiff = 10000.
c$$$            do icol = max(ixmax-5,1),min(ixmax+5,32)
c$$$               if(abs(bigcal_mid_xhit(32,icol)-bigcal_mid_xhit(33,ixmax))
c$$$     $              .lt.minxdiff) then
c$$$                  ixmin = icol
c$$$                  minxdiff = abs(bigcal_mid_xhit(32,icol) - 
c$$$     $                 bigcal_mid_xhit(33,ixmax))
c$$$               endif
c$$$            enddo
            
c     !write(*,*) 'colmindiff = ',ixmin
            
            ixmin = bigcal_ixclose_rcs(ixmax)

            ncellclust = 0
c            if(minxdiff.lt.10000.) then
            do irow = 31,32
               do icol = max(ixmin-2,1),min(ixmin+2,32)
                  
                  ecell = bigcal_mid_ehit(irow,icol)
                  xcell = bigcal_mid_xhit(irow,icol)
                  ycell = bigcal_mid_yhit(irow,icol)
                  
                  if(ecell.ge.b_cell_cut_prot) then
                     ncellclust = ncellclust + 1
                     if(ncellclust.gt.bigcal_clstr_ncell_max)goto 302
                     
                     clstr_temp_ecell(ncellclust) = ecell
                     clstr_temp_xcell(ncellclust) = xcell
                     clstr_temp_ycell(ncellclust) = ycell
                     clstr_temp_ixcell(ncellclust) = icol
                     clstr_temp_iycell(ncellclust) = irow
                  endif
               enddo
            enddo
            
            do irow = 33,35
               do icol = max(ixmax-2,1),min(ixmax+2,30)
                  ecell = bigcal_mid_ehit(irow,icol)
                  xcell = bigcal_mid_xhit(irow,icol)
                  ycell = bigcal_mid_yhit(irow,icol)
                  
                  if(ecell.ge.b_cell_cut_rcs) then
                     ncellclust = ncellclust + 1
                     if(ncellclust.gt.bigcal_clstr_ncell_max)goto 302
                     
                     clstr_temp_ecell(ncellclust) = ecell
                     clstr_temp_xcell(ncellclust) = xcell
                     clstr_temp_ycell(ncellclust) = ycell
                     clstr_temp_ixcell(ncellclust) = icol
                     clstr_temp_iycell(ncellclust) = irow
                  endif
               enddo
            enddo
c           endif
            
 302        continue
 
            if(ncellclust.gt.bigcal_clstr_ncell_max) then
               ncellclust = bigcal_clstr_ncell_max
            endif
c     sort in descending order of amplitude:
          
            if(ncellclust.lt.bigcal_clstr_ncell_min) then
               bigcal_mid_bad_clstr_flag(ncluster) = 2
               goto 334
            endif

            clstr_temp_esum = 0.
            do ihit=1,ncellclust
               clstr_temp_esum = clstr_temp_esum + clstr_temp_ecell(ihit)
            enddo

            if(clstr_temp_esum.lt.b_cluster_cut) then
               bigcal_mid_bad_clstr_flag(ncluster) = 3
               goto 334
            endif
            
            do ihit=1,ncellclust
               do jhit=ihit+1,ncellclust
                  if(clstr_temp_ecell(jhit).gt.clstr_temp_ecell(ihit))then
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
            
c     !write(*,*) 'ecell = ',clstr_temp_ecell
c     !write(*,*) 'iycell = ',clstr_temp_iycell
c     !write(*,*) 'ixcell = ',clstr_temp_ixcell

            celldiffx = clstr_temp_ixcell(2) - clstr_temp_ixcell(1)
            celldiffy = clstr_temp_iycell(2) - clstr_temp_iycell(1)
            
            if( (clstr_temp_iycell(1).le.bigcal_prot_ny.and.
     $           celldiffy.gt.0) .or. (clstr_temp_iycell(1).gt.
     $           bigcal_prot_ny.and.celldiffy.lt.0) ) then 
               celldiffx = celldiffx + clstr_temp_ixcell(1) - ixmin
            endif
            
            celldiffx = int(abs(float(celldiffx)))
            celldiffy = int(abs(float(celldiffy)))

            if(celldiffx.gt.1.or.celldiffy.gt.1) then
               second_max = .true.
            else
               second_max = .false.
            endif
            
            if(clstr_temp_iycell(1).ne.iymax.or.clstr_temp_ixcell(1).ne.
     $           ixmax) then
               second_max = .true.
            endif

            if(second_max) then
               bigcal_mid_bad_clstr_flag(ncluster) = 4
               goto 334 
            endif

            if(clstr_temp_esum.ge.b_cluster_cut.and. .not. second_max
     $           .and.ncellclust.gt.bigcal_clstr_ncell_min)then
c     ADD A CLUSTER TO THE ARRAY FOR THE MIDDLE SECTION!
               
c     !write(*,*) 'found good cluster in mid section!!!!'
               
               found_cluster = .true.
               ncluster = ncluster + 1
               bigcal_mid_clstr_ncell(ncluster) = ncellclust
               bigcal_mid_clstr_iymax(ncluster) = clstr_temp_iycell(1)
               bigcal_mid_clstr_ixmax(ncluster) = clstr_temp_ixcell(1)
               bigcal_mid_clstr_etot(ncluster) = clstr_temp_esum
               
               xmom = 0.
               ymom = 0.
               
               xcenter = clstr_temp_xcell(1)
               ycenter = clstr_temp_ycell(1)
               
               do icell=1,ncellclust
                  icol = clstr_temp_ixcell(icell)
                  irow = clstr_temp_iycell(icell)
c     zero the hits from this array so that they can't be used again:
                  bigcal_mid_ehit(irow,icol) = 0.
                  bigcal_mid_xhit(irow,icol) = 0.
                  bigcal_mid_yhit(irow,icol) = 0.
                  
                  xcell = clstr_temp_xcell(icell)
                  ycell = clstr_temp_ycell(icell)
                  ecell = clstr_temp_ecell(icell)
                  
                  xmom = xmom + (xcell-xcenter) * ecell / clstr_temp_esum
                  ymom = ymom + (ycell-ycenter) * ecell / clstr_temp_esum
                  
                  bigcal_mid_clstr_ixcell(ncluster,icell) = icol
                  bigcal_mid_clstr_iycell(ncluster,icell) = irow
                  bigcal_mid_clstr_xcell(ncluster,icell) = xcell
                  bigcal_mid_clstr_ycell(ncluster,icell) = ycell
                  bigcal_mid_clstr_ecell(ncluster,icell) = ecell
               enddo
            
               bigcal_mid_clstr_xmom(ncluster) = xmom
               bigcal_mid_clstr_ymom(ncluster) = ymom
            
               if(bdebug_print_clusters.ne.0) call b_print_cluster(3,ncluster,ABORT,err)
               
            endif
         else if(ixmax.eq.1.or.ixmax.eq.bigcal_rcs_nx) then
            bigcal_mid_bad_clstr_flag(ncluster) = 1
            !write(*,*) 'edge max! ncluster = ',ncluster
         endif
      endif
      
 334  continue

      if((.not.found_cluster).and.ncluster.eq.0) then
         bigcal_mid_no_clstr_why = bigcal_mid_bad_clstr_flag(0)
      endif

      if(found_cluster .and. ncluster.lt.bigcal_mid_nclstr_max) then
         goto 301
      endif
      
      bigcal_mid_nclstr = ncluster

      if(bigcal_prot_nclstr.eq.0.and.bigcal_rcs_nclstr.eq.0.and.
     $     bigcal_mid_nclstr.eq.0) then
         bigcal_all_no_clstr_why = max(bigcal_prot_no_clstr_why,
     $        bigcal_rcs_no_clstr_why,bigcal_mid_no_clstr_why)
      else 
         bigcal_all_no_clstr_why = 0
      endif
      
      return 
      end
