      subroutine b_calc_cluster_time(ABORT,err)
      
      implicit none
      save
      
      character*19 here
      parameter(here='b_calc_cluster_time')

      include 'bigcal_data_structures.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'bigcal_bypass_switches.cmn'
    
      logical abort
      character*(*) err

      integer i,j,k,irow,icol,icell,ihit,jhit
      integer jcell,kcell
      integer irow8,icol8,itdc,jtdc,ktdc
      integer jrow8,jcol8,jgroup64,jhalf64
      integer igroup64,ihalf64,ilogic,jlogic,klogic
      integer ntdc,nlogic,n8,n64
      real tsum8,tsum64
      real reftime
      real mintdiff,tdiff(BIGCAL_CLSTR_NCELL_MAX)
      integer imindiff

      integer irefbest,nagreej,maxnagree
      integer nagree(BIGCAL_TDC_MAXHITS)

      logical goodtime(BIGCAL_CLSTR_NCELL_MAX)
      logical tdchit(BIGCAL_CLSTR_NCELL_MAX)

      real hittime(BIGCAL_CLSTR_NCELL_MAX)
      
c     define some temporary arrays to hold hit times in a manner more 
c     convenient for the analysis we are doing here: 

      integer ntdchit(BIGCAL_CLSTR_NCELL_MAX)
      real hittimes(BIGCAL_CLSTR_NCELL_MAX,BIGCAL_TDC_MAXHITS)
      integer tdcnumber(BIGCAL_CLSTR_NCELL_MAX,BIGCAL_TDC_MAXHITS)

      integer ntrighit(BIGCAL_CLSTR_NCELL_MAX)
      real logictimes(BIGCAL_CLSTR_NCELL_MAX,BIGCAL_TRIG_MAXHITS)
      integer logicnumber(BIGCAL_CLSTR_NCELL_MAX,BIGCAL_TRIG_MAXHITS)

      integer triglevelsum ! sum of all sum64 levels
      integer tdclevelsum  ! sum of all sum8 levels
      
c     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     !!                                               !!
c     !!          PROTVINO CLUSTERS BEGIN HERE         !!
c     !!                                               !!
c     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      if(BIGCAL_PROT_NCLSTR.gt.0) then
         do i=1,BIGCAL_PROT_NCLSTR
            do j=1,BIGCAL_PROT_CLSTR_NCELL(i)
               irow = BIGCAL_PROT_CLSTR_IYCELL(i,j)
               icol = BIGCAL_PROT_CLSTR_IXCELL(i,j)
               irow8 = irow
               igroup64 = (irow8-1)/3 + 1
               if(irow8.le.BIGCAL_PROT_NY) then
                  icol8 = (icol-1)/8 + 1
                  ihalf64 = (icol-1)/16 + 1
               else
                  if(icol.lt.16) then
                     icol8 = (icol-1)/8+1
                  else
                     icol8 = icol/8 + 1
                  endif
                  ihalf64 = icol/16 + 1
               endif
               itdc = icol8 + (irow8-1)*BIGCAL_MAX_GROUPS
               ilogic = igroup64 + (ihalf64-1)*BIGCAL_LOGIC_GROUPS/2
               ntdchit(j) = BIGCAL_TIME_DET_NHIT(itdc)
               ntrighit(j) = BIGCAL_TRIG_TIME_NHIT(ilogic)
               ntdc=0
               
               do ihit=1,BIGCAL_TIME_NGOOD
                  jrow8 = BIGCAL_TIME_IROW(ihit)
                  jcol8 = BIGCAL_TIME_IGROUP(ihit)
                  jtdc = jcol8 + (jrow8-1)*BIGCAL_MAX_GROUPS
                  if(jtdc.eq.itdc) then
                     ntdc=ntdc+1
                     hittimes(j,ntdc) = BIGCAL_HIT_TIME(ihit)
                     tdcnumber(j,ntdc) = jtdc
                  endif
               enddo
               
               if(ntdc.ne.ntdchit(j))then
                  ntdchit(j) = ntdc
               endif

               nlogic = 0

               do ihit=1,BIGCAL_TRIG_NGOOD
                  jgroup64 = BIGCAL_TRIG_GOOD_IGROUP(ihit)
                  jhalf64 = BIGCAL_TRIG_GOOD_IHALF(ihit)
                  jlogic = jgroup64 + (jhalf64-1)*BIGCAL_LOGIC_GROUPS/2
                  if(jlogic.eq.ilogic) then
                     nlogic = nlogic + 1
                     logictimes(j,nlogic) = BIGCAL_TRIG_TIME_GOOD(ihit)
                     logicnumber(j,nlogic) = jlogic
                  endif
               enddo

               if(nlogic.ne.ntrighit(j))then
                  ntrighit(j) = nlogic
               endif
            enddo
            
            if(ntdchit(1).gt.0)then
               do j=1,ntdchit(1)
                  reftime = hittimes(1,j)
                  nagreej = 0
                  
                  do icell=2,BIGCAL_PROT_CLSTR_NCELL(i)
c     find hit with minimum time difference from reference:
                     mintdiff = 1000. ! units are ns
                     imindiff = 0
                     if(ntdchit(icell).gt.0)then
                        do k=1,ntdchit(icell)
                           if(abs(hittimes(icell,k)-reftime).lt.
     $                          mintdiff) then
                              mintdiff = abs(hittimes(icell,k)-reftime)
                              imindiff = k
                           endif
                        enddo
                        if(mintdiff.le.b_timing_cut.and.
     $                       imindiff.gt.0)then
                           nagreej = nagreej + 1
                        endif
                     endif
                  enddo
                  nagree(j) = nagreej
               enddo
               
               maxnagree = 0
               
               do j=1,ntdchit(1)
                  if(nagree(j).gt.maxnagree) then
                     maxnagree = nagree(j)
                     irefbest = j
                  endif
               enddo
               if(maxnagree.gt.0)then
                  reftime = hittimes(1,irefbest)
                  BIGCAL_PROT_CLSTR_TC8(i,1) = reftime
                  BIGCAL_PROT_CLSTR_L8(i,1) = 1
                  do icell=2,BIGCAL_PROT_CLSTR_NCELL(i)
                     mintdiff = 1000.
                     imindiff = 0
                     if(ntdchit(icell).gt.0)then
                        do k=1,ntdchit(icell)
                           if(abs(hittimes(icell,k)-reftime).lt.mintdiff
     $                          )then
                              mintdiff = abs(hittimes(icell,k)-reftime)
                              imindiff = k
                           endif
                        enddo
                       
                        if(mintdiff.le.b_timing_cut.and.
     $                       imindiff.gt.0)then
                           ! this is a good hit for the cluster and 
                           ! we should use it
                           BIGCAL_PROT_CLSTR_L8(i,icell) = 1
                           BIGCAL_PROT_CLSTR_TC8(i,icell) = 
     $                          hittimes(icell,imindiff)
                        else
                           BIGCAL_PROT_CLSTR_L8(i,icell) = -1
                           if(imindiff.gt.0) then
                              BIGCAL_PROT_CLSTR_TC8(i,icell) = 
     $                             hittimes(icell,imindiff)
                           else
                              BIGCAL_PROT_CLSTR_TC8(i,icell) = 
     $                             hittimes(icell,1)
                           endif
                        endif
                     else
                        BIGCAL_PROT_CLSTR_L8(i,icell) = 0
                        BIGCAL_PROT_CLSTR_TC8(i,icell) = 0.
                     endif
                  enddo
               else ! no other hits in the cluster have "agreeing" times
                  ! note that this situation is virtually impossible for 
                  ! good electron showers
                  ! use the time of the first hit if there is a hit, or 0.
                  ! if not. If hit, set "level" to -1 unless the only hit is 
                  ! in the central cell, in which case use 0
                  BIGCAL_PROT_CLSTR_L8(i,1) = 0
                  BIGCAL_PROT_CLSTR_TC8(i,1) = hittimes(1,1)
                  do icell=2,BIGCAL_PROT_CLSTR_NCELL(i)
                     if(ntdchit(icell).gt.0) then
                        BIGCAL_PROT_CLSTR_L8(i,icell) = -1
                        BIGCAL_PROT_CLSTR_TC8(i,icell) = 
     $                       hittimes(icell,1)
                        BIGCAL_PROT_CLSTR_L8(i,1) = -1
                     else 
                        BIGCAL_PROT_CLSTR_L8(i,icell) = 0
                        BIGCAL_PROT_CLSTR_TC8(i,icell) = 0.
                     endif
                  enddo
               endif
            else
               do icell=1,BIGCAL_PROT_CLSTR_NCELL(i)
                  if(ntdchit(icell).eq.0) then
                     BIGCAL_PROT_CLSTR_L8(i,icell) = 0
                     BIGCAL_PROT_CLSTR_TC8(i,icell) = 0.
                  else
                     BIGCAL_PROT_CLSTR_L8(i,icell) = -1
                     BIGCAL_PROT_CLSTR_TC8(i,icell) = 
     $                    hittimes(icell,1)
                  endif
               enddo
            endif
c     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     !!                                                               !!
c     !!                    ANALYZE TRIG HITS                          !!
c     !!                                                               !!
c     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if(ntrighit(1).gt.0)then
               do j=1,ntrighit(1)
                  reftime = logictimes(1,j)
                  nagreej = 0
                  
                  do icell=2,BIGCAL_PROT_CLSTR_NCELL(i)
c     find hit with minimum time difference from reference:
                     mintdiff = 1000. ! units are ns
                     imindiff = 0
                     if(ntrighit(icell).gt.0)then
                        do k=1,ntrighit(icell)
                           if(abs(logictimes(icell,k)-reftime).lt.
     $                          mintdiff) then
                              mintdiff=abs(logictimes(icell,k)-reftime)
                              imindiff = k
                           endif
                        enddo
                        if(mintdiff.le.b_timing_cut.and.
     $                       imindiff.gt.0)then
                           nagreej = nagreej + 1
                        endif
                     endif
                  enddo
                  nagree(j) = nagreej
               enddo
               
               maxnagree = 0
               
               do j=1,ntrighit(1)
                  if(nagree(j).gt.maxnagree) then
                     maxnagree = nagree(j)
                     irefbest = j
                  endif
               enddo
               if(maxnagree.gt.0)then
                  reftime = logictimes(1,irefbest)
                  BIGCAL_PROT_CLSTR_TC64(i,1) = reftime
                  BIGCAL_PROT_CLSTR_L64(i,1) = 1
                  do icell=2,BIGCAL_PROT_CLSTR_NCELL(i)
                     mintdiff = 1000.
                     imindiff = 0
                     if(ntrighit(icell).gt.0)then
                        do k=1,ntrighit(icell)
                           if(abs(logictimes(icell,k)-reftime)
     $                          .lt.mintdiff)then
                              mintdiff=abs(logictimes(icell,k)-reftime)
                              imindiff = k
                           endif
                        enddo
                       
                        if(mintdiff.le.b_timing_cut.and.
     $                       imindiff.gt.0)then
                           ! this is a good hit for the cluster and 
                           ! we should use it
                           BIGCAL_PROT_CLSTR_L64(i,icell) = 1
                           BIGCAL_PROT_CLSTR_TC64(i,icell) = 
     $                          logictimes(icell,imindiff)
                        else
                           BIGCAL_PROT_CLSTR_L64(i,icell) = -1
                           if(imindiff.gt.0) then
                              BIGCAL_PROT_CLSTR_TC64(i,icell) = 
     $                             logictimes(icell,imindiff)
                           else
                              BIGCAL_PROT_CLSTR_TC64(i,icell) = 
     $                             logictimes(icell,1)
                           endif
                        endif
                     else
                        BIGCAL_PROT_CLSTR_L64(i,icell) = 0
                        BIGCAL_PROT_CLSTR_TC64(i,icell) = 0.
                     endif
                  enddo
               else ! no other hits in the cluster have "agreeing" times
                  ! note that this situation is virtually impossible for 
                  ! good electron showers
                  ! use the time of the first hit if there is a hit, or 0.
                  ! if not. If hit, set "level" to -1 unless the only hit is 
                  ! in the central cell, in which case use 0
                  BIGCAL_PROT_CLSTR_L64(i,1) = 0
                  BIGCAL_PROT_CLSTR_TC64(i,1) = logictimes(1,1)
                  do icell=2,BIGCAL_PROT_CLSTR_NCELL(i)
                     if(ntrighit(icell).gt.0) then
                        BIGCAL_PROT_CLSTR_L64(i,icell) = -1
                        BIGCAL_PROT_CLSTR_TC64(i,icell) = 
     $                       logictimes(icell,1)
                        BIGCAL_PROT_CLSTR_L64(i,1) = -1
                     else 
                        BIGCAL_PROT_CLSTR_L64(i,icell) = 0
                        BIGCAL_PROT_CLSTR_TC64(i,icell) = 0.
                     endif
                  enddo
               endif
            else
               do icell=1,BIGCAL_PROT_CLSTR_NCELL(i)
                  if(ntrighit(icell).eq.0) then
                     BIGCAL_PROT_CLSTR_L64(i,icell) = 0
                     BIGCAL_PROT_CLSTR_TC64(i,icell) = 0.
                  else
                     BIGCAL_PROT_CLSTR_L64(i,icell) = -1
                     BIGCAL_PROT_CLSTR_TC64(i,icell) = 
     $                    logictimes(icell,1)
                  endif
               enddo
            endif
            
            
            tdclevelsum = 0
            triglevelsum = 0
            tsum8 = 0.
            tsum64 = 0.
            n8 = 0
            n64 = 0
            do icell=1,BIGCAL_PROT_CLSTR_NCELL(i)
               tdclevelsum = tdclevelsum + 
     $              BIGCAL_PROT_CLSTR_L8(i,icell)
               triglevelsum = triglevelsum + 
     $              BIGCAL_PROT_CLSTR_L64(i,icell)
               if(BIGCAL_PROT_CLSTR_L8(i,icell).ge.0)then
                  n8 = n8 + 1
                  tsum8 = tsum8 + BIGCAL_PROT_CLSTR_TC8(i,icell)
               endif
               if(BIGCAL_PROT_CLSTR_L64(i,icell).ge.0)then
                  n64 = n64 + 1
                  tsum64 = tsum64 + BIGCAL_PROT_CLSTR_TC64(i,icell)
               endif
            enddo
            BIGCAL_PROT_CLSTR_L8SUM(i) = tdclevelsum
            BIGCAL_PROT_CLSTR_L64SUM(i) = triglevelsum
            BIGCAL_PROT_CLSTR_TIME8(i) = tsum8 / float(n8)
            BIGCAL_PROT_CLSTR_TIME64(i) = tsum64 / float(n64)
         enddo
      endif
      
      
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!                                                          !!
      !!                  RCS CLUSTERS BEGIN HERE                 !!
      !!                                                          !!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if(BIGCAL_RCS_NCLSTR.gt.0) then
         do i=1,BIGCAL_RCS_NCLSTR
            do j=1,BIGCAL_RCS_CLSTR_NCELL(i)
               irow = BIGCAL_RCS_CLSTR_IYCELL(i,j)
               icol = BIGCAL_RCS_CLSTR_IXCELL(i,j)
               irow8 = irow
               igroup64 = (irow8-1)/3 + 1
               if(irow8.le.BIGCAL_PROT_NY) then
                  icol8 = (icol-1)/8 + 1
                  ihalf64 = (icol-1)/16 + 1
               else
                  if(icol.lt.16) then
                     icol8 = (icol-1)/8+1
                  else
                     icol8 = icol/8 + 1
                  endif
                  ihalf64 = icol/16 + 1
               endif
               itdc = icol8 + (irow8-1)*BIGCAL_MAX_GROUPS
               ilogic = igroup64 + (ihalf64-1)*BIGCAL_LOGIC_GROUPS/2
               ntdchit(j) = BIGCAL_TIME_DET_NHIT(itdc)
               ntrighit(j) = BIGCAL_TRIG_TIME_NHIT(ilogic)
               ntdc=0
               
               do ihit=1,BIGCAL_TIME_NGOOD
                  jrow8 = BIGCAL_TIME_IROW(ihit)
                  jcol8 = BIGCAL_TIME_IGROUP(ihit)
                  jtdc = jcol8 + (jrow8-1)*BIGCAL_MAX_GROUPS
                  if(jtdc.eq.itdc) then
                     ntdc=ntdc+1
                     hittimes(j,ntdc) = BIGCAL_HIT_TIME(ihit)
                     tdcnumber(j,ntdc) = jtdc
                  endif
               enddo
               
               if(ntdc.ne.ntdchit(j))then
                  ntdchit(j) = ntdc
               endif

               nlogic = 0

               do ihit=1,BIGCAL_TRIG_NGOOD
                  jgroup64 = BIGCAL_TRIG_GOOD_IGROUP(ihit)
                  jhalf64 = BIGCAL_TRIG_GOOD_IHALF(ihit)
                  jlogic = jgroup64 + (jhalf64-1)*BIGCAL_LOGIC_GROUPS/2
                  if(jlogic.eq.ilogic) then
                     nlogic = nlogic + 1
                     logictimes(j,nlogic) = BIGCAL_TRIG_TIME_GOOD(ihit)
                     logicnumber(j,nlogic) = jlogic
                  endif
               enddo

               if(nlogic.ne.ntrighit(j))then
                  ntrighit(j) = nlogic
               endif
            enddo
            
            if(ntdchit(1).gt.0)then
               do j=1,ntdchit(1)
                  reftime = hittimes(1,j)
                  nagreej = 0
                  
                  do icell=2,BIGCAL_RCS_CLSTR_NCELL(i)
c     find hit with minimum time difference from reference:
                     mintdiff = 1000. ! units are ns
                     imindiff = 0
                     if(ntdchit(icell).gt.0)then
                        do k=1,ntdchit(icell)
                           if(abs(hittimes(icell,k)-reftime).lt.
     $                          mintdiff) then
                              mintdiff = abs(hittimes(icell,k)-reftime)
                              imindiff = k
                           endif
                        enddo
                        if(mintdiff.le.b_timing_cut.and.
     $                       imindiff.gt.0)then
                           nagreej = nagreej + 1
                        endif
                     endif
                  enddo
                  nagree(j) = nagreej
               enddo
               
               maxnagree = 0
               
               do j=1,ntdchit(1)
                  if(nagree(j).gt.maxnagree) then
                     maxnagree = nagree(j)
                     irefbest = j
                  endif
               enddo
               if(maxnagree.gt.0)then
                  reftime = hittimes(1,irefbest)
                  BIGCAL_RCS_CLSTR_TC8(i,1) = reftime
                  BIGCAL_RCS_CLSTR_L8(i,1) = 1
                  do icell=2,BIGCAL_RCS_CLSTR_NCELL(i)
                     mintdiff = 1000.
                     imindiff = 0
                     if(ntdchit(icell).gt.0)then
                        do k=1,ntdchit(icell)
                           if(abs(hittimes(icell,k)-reftime).lt.mintdiff
     $                          )then
                              mintdiff = abs(hittimes(icell,k)-reftime)
                              imindiff = k
                           endif
                        enddo
                       
                        if(mintdiff.le.b_timing_cut.and.
     $                       imindiff.gt.0)then
                           ! this is a good hit for the cluster and 
                           ! we should use it
                           BIGCAL_RCS_CLSTR_L8(i,icell) = 1
                           BIGCAL_RCS_CLSTR_TC8(i,icell) = 
     $                          hittimes(icell,imindiff)
                        else
                           BIGCAL_RCS_CLSTR_L8(i,icell) = -1
                           if(imindiff.gt.0) then
                              BIGCAL_RCS_CLSTR_TC8(i,icell) = 
     $                             hittimes(icell,imindiff)
                           else
                              BIGCAL_RCS_CLSTR_TC8(i,icell) = 
     $                             hittimes(icell,1)
                           endif
                        endif
                     else
                        BIGCAL_RCS_CLSTR_L8(i,icell) = 0
                        BIGCAL_RCS_CLSTR_TC8(i,icell) = 0.
                     endif
                  enddo
               else ! no other hits in the cluster have "agreeing" times
                  ! note that this situation is virtually impossible for 
                  ! good electron showers
                  ! use the time of the first hit if there is a hit, or 0.
                  ! if not. If hit, set "level" to -1 unless the only hit is 
                  ! in the central cell, in which case use 0
                  BIGCAL_RCS_CLSTR_L8(i,1) = 0
                  BIGCAL_RCS_CLSTR_TC8(i,1) = hittimes(1,1)
                  do icell=2,BIGCAL_RCS_CLSTR_NCELL(i)
                     if(ntdchit(icell).gt.0) then
                        BIGCAL_RCS_CLSTR_L8(i,icell) = -1
                        BIGCAL_RCS_CLSTR_TC8(i,icell) = 
     $                       hittimes(icell,1)
                        BIGCAL_RCS_CLSTR_L8(i,1) = -1
                     else 
                        BIGCAL_RCS_CLSTR_L8(i,icell) = 0
                        BIGCAL_RCS_CLSTR_TC8(i,icell) = 0.
                     endif
                  enddo
               endif
            else
               do icell=1,BIGCAL_RCS_CLSTR_NCELL(i)
                  if(ntdchit(icell).eq.0) then
                     BIGCAL_RCS_CLSTR_L8(i,icell) = 0
                     BIGCAL_RCS_CLSTR_TC8(i,icell) = 0.
                  else
                     BIGCAL_RCS_CLSTR_L8(i,icell) = -1
                     BIGCAL_RCS_CLSTR_TC8(i,icell) = 
     $                    hittimes(icell,1)
                  endif
               enddo
            endif
c     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     !!                                                               !!
c     !!                    ANALYZE TRIG HITS                          !!
c     !!                                                               !!
c     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if(ntrighit(1).gt.0)then
               do j=1,ntrighit(1)
                  reftime = logictimes(1,j)
                  nagreej = 0
                  
                  do icell=2,BIGCAL_RCS_CLSTR_NCELL(i)
c     find hit with minimum time difference from reference:
                     mintdiff = 1000. ! units are ns
                     imindiff = 0
                     if(ntrighit(icell).gt.0)then
                        do k=1,ntrighit(icell)
                           if(abs(logictimes(icell,k)-reftime).lt.
     $                          mintdiff) then
                              mintdiff=abs(logictimes(icell,k)-reftime)
                              imindiff = k
                           endif
                        enddo
                        if(mintdiff.le.b_timing_cut.and.
     $                       imindiff.gt.0)then
                           nagreej = nagreej + 1
                        endif
                     endif
                  enddo
                  nagree(j) = nagreej
               enddo
               
               maxnagree = 0
               
               do j=1,ntrighit(1)
                  if(nagree(j).gt.maxnagree) then
                     maxnagree = nagree(j)
                     irefbest = j
                  endif
               enddo
               if(maxnagree.gt.0)then
                  reftime = logictimes(1,irefbest)
                  BIGCAL_RCS_CLSTR_TC64(i,1) = reftime
                  BIGCAL_RCS_CLSTR_L64(i,1) = 1
                  do icell=2,BIGCAL_RCS_CLSTR_NCELL(i)
                     mintdiff = 1000.
                     imindiff = 0
                     if(ntrighit(icell).gt.0)then
                        do k=1,ntrighit(icell)
                           if(abs(logictimes(icell,k)-reftime)
     $                          .lt.mintdiff)then
                              mintdiff=abs(logictimes(icell,k)-reftime)
                              imindiff = k
                           endif
                        enddo
                       
                        if(mintdiff.le.b_timing_cut.and.
     $                       imindiff.gt.0)then
                           ! this is a good hit for the cluster and 
                           ! we should use it
                           BIGCAL_RCS_CLSTR_L64(i,icell) = 1
                           BIGCAL_RCS_CLSTR_TC64(i,icell) = 
     $                          logictimes(icell,imindiff)
                        else
                           BIGCAL_RCS_CLSTR_L64(i,icell) = -1
                           if(imindiff.gt.0) then
                              BIGCAL_RCS_CLSTR_TC64(i,icell) = 
     $                             logictimes(icell,imindiff)
                           else
                              BIGCAL_RCS_CLSTR_TC64(i,icell) = 
     $                             logictimes(icell,1)
                           endif
                        endif
                     else
                        BIGCAL_RCS_CLSTR_L64(i,icell) = 0
                        BIGCAL_RCS_CLSTR_TC64(i,icell) = 0.
                     endif
                  enddo
               else ! no other hits in the cluster have "agreeing" times
                  ! note that this situation is virtually impossible for 
                  ! good electron showers
                  ! use the time of the first hit if there is a hit, or 0.
                  ! if not. If hit, set "level" to -1 unless the only hit is 
                  ! in the central cell, in which case use 0
                  BIGCAL_RCS_CLSTR_L64(i,1) = 0
                  BIGCAL_RCS_CLSTR_TC64(i,1) = logictimes(1,1)
                  do icell=2,BIGCAL_RCS_CLSTR_NCELL(i)
                     if(ntrighit(icell).gt.0) then
                        BIGCAL_RCS_CLSTR_L64(i,icell) = -1
                        BIGCAL_RCS_CLSTR_TC64(i,icell) = 
     $                       logictimes(icell,1)
                        BIGCAL_RCS_CLSTR_L64(i,1) = -1
                     else 
                        BIGCAL_RCS_CLSTR_L64(i,icell) = 0
                        BIGCAL_RCS_CLSTR_TC64(i,icell) = 0.
                     endif
                  enddo
               endif
            else
               do icell=1,BIGCAL_RCS_CLSTR_NCELL(i)
                  if(ntrighit(icell).eq.0) then
                     BIGCAL_RCS_CLSTR_L64(i,icell) = 0
                     BIGCAL_RCS_CLSTR_TC64(i,icell) = 0.
                  else
                     BIGCAL_RCS_CLSTR_L64(i,icell) = -1
                     BIGCAL_RCS_CLSTR_TC64(i,icell) = 
     $                    logictimes(icell,1)
                  endif
               enddo
            endif
            
            
            tdclevelsum = 0
            triglevelsum = 0
            tsum8 = 0.
            tsum64 = 0.
            n8 = 0
            n64 = 0


            do icell=1,BIGCAL_RCS_CLSTR_NCELL(i)
               tdclevelsum = tdclevelsum + 
     $              BIGCAL_RCS_CLSTR_L8(i,icell)
               triglevelsum = triglevelsum + 
     $              BIGCAL_RCS_CLSTR_L64(i,icell)
               if(BIGCAL_RCS_CLSTR_L8(i,icell).ge.0)then
                  n8 = n8 + 1
                  tsum8 = tsum8 + BIGCAL_RCS_CLSTR_TC8(i,icell)
               endif
               if(BIGCAL_PROT_CLSTR_L64(i,icell).ge.0)then
                  n64 = n64 + 1
                  tsum64 = tsum64 + BIGCAL_RCS_CLSTR_TC64(i,icell)
               endif
            enddo
            BIGCAL_RCS_CLSTR_L8SUM(i) = tdclevelsum
            BIGCAL_RCS_CLSTR_L64SUM(i) = triglevelsum
            BIGCAL_RCS_CLSTR_TIME8(i) = tsum8 / float(n8)
            BIGCAL_RCS_CLSTR_TIME64(i) = tsum64 / float(n64)
         enddo
      endif

c     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     !!                                                          !!
c     !!                  MID CLUSTERS BEGIN HERE                 !!
c     !!                                                          !!
c     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

      if(BIGCAL_MID_NCLSTR.gt.0) then
         do i=1,BIGCAL_MID_NCLSTR
            do j=1,BIGCAL_MID_CLSTR_NCELL(i)
               irow = BIGCAL_MID_CLSTR_IYCELL(i,j)
               icol = BIGCAL_MID_CLSTR_IXCELL(i,j)
               irow8 = irow
               igroup64 = (irow8-1)/3 + 1
               if(irow8.le.BIGCAL_PROT_NY) then
                  icol8 = (icol-1)/8 + 1
                  ihalf64 = (icol-1)/16 + 1
               else
                  if(icol.lt.16) then
                     icol8 = (icol-1)/8+1
                  else
                     icol8 = icol/8 + 1
                  endif
                  ihalf64 = icol/16 + 1
               endif
               itdc = icol8 + (irow8-1)*BIGCAL_MAX_GROUPS
               ilogic = igroup64 + (ihalf64-1)*BIGCAL_LOGIC_GROUPS/2
               ntdchit(j) = BIGCAL_TIME_DET_NHIT(itdc)
               ntrighit(j) = BIGCAL_TRIG_TIME_NHIT(ilogic)
               ntdc=0
               
               do ihit=1,BIGCAL_TIME_NGOOD
                  jrow8 = BIGCAL_TIME_IROW(ihit)
                  jcol8 = BIGCAL_TIME_IGROUP(ihit)
                  jtdc = jcol8 + (jrow8-1)*BIGCAL_MAX_GROUPS
                  if(jtdc.eq.itdc) then
                     ntdc=ntdc+1
                     hittimes(j,ntdc) = BIGCAL_HIT_TIME(ihit)
                     tdcnumber(j,ntdc) = jtdc
                  endif
               enddo
               
               if(ntdc.ne.ntdchit(j))then
                  ntdchit(j) = ntdc
               endif

               nlogic = 0

               do ihit=1,BIGCAL_TRIG_NGOOD
                  jgroup64 = BIGCAL_TRIG_GOOD_IGROUP(ihit)
                  jhalf64 = BIGCAL_TRIG_GOOD_IHALF(ihit)
                  jlogic = jgroup64 + (jhalf64-1)*BIGCAL_LOGIC_GROUPS/2
                  if(jlogic.eq.ilogic) then
                     nlogic = nlogic + 1
                     logictimes(j,nlogic) = BIGCAL_TRIG_TIME_GOOD(ihit)
                     logicnumber(j,nlogic) = jlogic
                  endif
               enddo

               if(nlogic.ne.ntrighit(j))then
                  ntrighit(j) = nlogic
               endif
            enddo
            
            if(ntdchit(1).gt.0)then
               do j=1,ntdchit(1)
                  reftime = hittimes(1,j)
                  nagreej = 0
                  
                  do icell=2,BIGCAL_MID_CLSTR_NCELL(i)
c     find hit with minimum time difference from reference:
                     mintdiff = 1000. ! units are ns
                     imindiff = 0
                     if(ntdchit(icell).gt.0)then
                        do k=1,ntdchit(icell)
                           if(abs(hittimes(icell,k)-reftime).lt.
     $                          mintdiff) then
                              mintdiff = abs(hittimes(icell,k)-reftime)
                              imindiff = k
                           endif
                        enddo
                        if(mintdiff.le.b_timing_cut.and.
     $                       imindiff.gt.0)then
                           nagreej = nagreej + 1
                        endif
                     endif
                  enddo
                  nagree(j) = nagreej
               enddo
               
               maxnagree = 0
               
               do j=1,ntdchit(1)
                  if(nagree(j).gt.maxnagree) then
                     maxnagree = nagree(j)
                     irefbest = j
                  endif
               enddo
               if(maxnagree.gt.0)then
                  reftime = hittimes(1,irefbest)
                  BIGCAL_MID_CLSTR_TC8(i,1) = reftime
                  BIGCAL_MID_CLSTR_L8(i,1) = 1
                  do icell=2,BIGCAL_MID_CLSTR_NCELL(i)
                     mintdiff = 1000.
                     imindiff = 0
                     if(ntdchit(icell).gt.0)then
                        do k=1,ntdchit(icell)
                           if(abs(hittimes(icell,k)-reftime).lt.mintdiff
     $                          )then
                              mintdiff = abs(hittimes(icell,k)-reftime)
                              imindiff = k
                           endif
                        enddo
                       
                        if(mintdiff.le.b_timing_cut.and.
     $                       imindiff.gt.0)then
                           ! this is a good hit for the cluster and 
                           ! we should use it
                           BIGCAL_MID_CLSTR_L8(i,icell) = 1
                           BIGCAL_MID_CLSTR_TC8(i,icell) = 
     $                          hittimes(icell,imindiff)
                        else
                           BIGCAL_MID_CLSTR_L8(i,icell) = -1
                           if(imindiff.gt.0) then
                              BIGCAL_MID_CLSTR_TC8(i,icell) = 
     $                             hittimes(icell,imindiff)
                           else
                              BIGCAL_MID_CLSTR_TC8(i,icell) = 
     $                             hittimes(icell,1)
                           endif
                        endif
                     else
                        BIGCAL_MID_CLSTR_L8(i,icell) = 0
                        BIGCAL_MID_CLSTR_TC8(i,icell) = 0.
                     endif
                  enddo
               else ! no other hits in the cluster have "agreeing" times
                  ! note that this situation is virtually impossible for 
                  ! good electron showers
                  ! use the time of the first hit if there is a hit, or 0.
                  ! if not. If hit, set "level" to -1 unless the only hit is 
                  ! in the central cell, in which case use 0
                  BIGCAL_MID_CLSTR_L8(i,1) = 0
                  BIGCAL_MID_CLSTR_TC8(i,1) = hittimes(1,1)
                  do icell=2,BIGCAL_MID_CLSTR_NCELL(i)
                     if(ntdchit(icell).gt.0) then
                        BIGCAL_MID_CLSTR_L8(i,icell) = -1
                        BIGCAL_MID_CLSTR_TC8(i,icell) = 
     $                       hittimes(icell,1)
                        BIGCAL_MID_CLSTR_L8(i,1) = -1
                     else 
                        BIGCAL_MID_CLSTR_L8(i,icell) = 0
                        BIGCAL_MID_CLSTR_TC8(i,icell) = 0.
                     endif
                  enddo
               endif
            else
               do icell=1,BIGCAL_MID_CLSTR_NCELL(i)
                  if(ntdchit(icell).eq.0) then
                     BIGCAL_MID_CLSTR_L8(i,icell) = 0
                     BIGCAL_MID_CLSTR_TC8(i,icell) = 0.
                  else
                     BIGCAL_MID_CLSTR_L8(i,icell) = -1
                     BIGCAL_MID_CLSTR_TC8(i,icell) = 
     $                    hittimes(icell,1)
                  endif
               enddo
            endif
c     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     !!                                                               !!
c     !!                    ANALYZE TRIG HITS                          !!
c     !!                                                               !!
c     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if(ntrighit(1).gt.0)then
               do j=1,ntrighit(1)
                  reftime = logictimes(1,j)
                  nagreej = 0
                  
                  do icell=2,BIGCAL_MID_CLSTR_NCELL(i)
c     find hit with minimum time difference from reference:
                     mintdiff = 1000. ! units are ns
                     imindiff = 0
                     if(ntrighit(icell).gt.0)then
                        do k=1,ntrighit(icell)
                           if(abs(logictimes(icell,k)-reftime).lt.
     $                          mintdiff) then
                              mintdiff=abs(logictimes(icell,k)-reftime)
                              imindiff = k
                           endif
                        enddo
                        if(mintdiff.le.b_timing_cut.and.
     $                       imindiff.gt.0)then
                           nagreej = nagreej + 1
                        endif
                     endif
                  enddo
                  nagree(j) = nagreej
               enddo
               
               maxnagree = 0
               
               do j=1,ntrighit(1)
                  if(nagree(j).gt.maxnagree) then
                     maxnagree = nagree(j)
                     irefbest = j
                  endif
               enddo
               if(maxnagree.gt.0)then
                  reftime = logictimes(1,irefbest)
                  BIGCAL_MID_CLSTR_TC64(i,1) = reftime
                  BIGCAL_MID_CLSTR_L64(i,1) = 1
                  do icell=2,BIGCAL_MID_CLSTR_NCELL(i)
                     mintdiff = 1000.
                     imindiff = 0
                     if(ntrighit(icell).gt.0)then
                        do k=1,ntrighit(icell)
                           if(abs(logictimes(icell,k)-reftime)
     $                          .lt.mintdiff)then
                              mintdiff=abs(logictimes(icell,k)-reftime)
                              imindiff = k
                           endif
                        enddo
                       
                        if(mintdiff.le.b_timing_cut.and.
     $                       imindiff.gt.0)then
                           ! this is a good hit for the cluster and 
                           ! we should use it
                           BIGCAL_MID_CLSTR_L64(i,icell) = 1
                           BIGCAL_MID_CLSTR_TC64(i,icell) = 
     $                          logictimes(icell,imindiff)
                        else
                           BIGCAL_MID_CLSTR_L64(i,icell) = -1
                           if(imindiff.gt.0) then
                              BIGCAL_MID_CLSTR_TC64(i,icell) = 
     $                             logictimes(icell,imindiff)
                           else
                              BIGCAL_MID_CLSTR_TC64(i,icell) = 
     $                             logictimes(icell,1)
                           endif
                        endif
                     else
                        BIGCAL_MID_CLSTR_L64(i,icell) = 0
                        BIGCAL_MID_CLSTR_TC64(i,icell) = 0.
                     endif
                  enddo
               else ! no other hits in the cluster have "agreeing" times
                  ! note that this situation is virtually impossible for 
                  ! good electron showers
                  ! use the time of the first hit if there is a hit, or 0.
                  ! if not. If hit, set "level" to -1 unless the only hit is 
                  ! in the central cell, in which case use 0
                  BIGCAL_MID_CLSTR_L64(i,1) = 0
                  BIGCAL_MID_CLSTR_TC64(i,1) = logictimes(1,1)
                  do icell=2,BIGCAL_MID_CLSTR_NCELL(i)
                     if(ntrighit(icell).gt.0) then
                        BIGCAL_MID_CLSTR_L64(i,icell) = -1
                        BIGCAL_MID_CLSTR_TC64(i,icell) = 
     $                       logictimes(icell,1)
                        BIGCAL_MID_CLSTR_L64(i,1) = -1
                     else 
                        BIGCAL_MID_CLSTR_L64(i,icell) = 0
                        BIGCAL_MID_CLSTR_TC64(i,icell) = 0.
                     endif
                  enddo
               endif
            else
               do icell=1,BIGCAL_MID_CLSTR_NCELL(i)
                  if(ntrighit(icell).eq.0) then
                     BIGCAL_MID_CLSTR_L64(i,icell) = 0
                     BIGCAL_MID_CLSTR_TC64(i,icell) = 0.
                  else
                     BIGCAL_MID_CLSTR_L64(i,icell) = -1
                     BIGCAL_MID_CLSTR_TC64(i,icell) = 
     $                    logictimes(icell,1)
                  endif
               enddo
            endif
            
            
            tdclevelsum = 0
            triglevelsum = 0
            tsum8 = 0.
            tsum64 = 0.
            n8 = 0
            n64 = 0

            do icell=1,BIGCAL_MID_CLSTR_NCELL(i)
               tdclevelsum = tdclevelsum + 
     $              BIGCAL_MID_CLSTR_L8(i,icell)
               triglevelsum = triglevelsum + 
     $              BIGCAL_MID_CLSTR_L64(i,icell)
               if(BIGCAL_MID_CLSTR_L8(i,icell).ge.0)then
                  n8 = n8 + 1
                  tsum8 = tsum8 + BIGCAL_MID_CLSTR_TC8(i,icell)
               endif
               if(BIGCAL_MID_CLSTR_L64(i,icell).ge.0)then
                  n64 = n64 + 1
                  tsum64 = tsum64 + BIGCAL_MID_CLSTR_TC64(i,icell)
               endif
            enddo
            BIGCAL_MID_CLSTR_L8SUM(i) = tdclevelsum
            BIGCAL_MID_CLSTR_L64SUM(i) = triglevelsum
            BIGCAL_MID_CLSTR_TIME8(i) = tsum8 / float(n8)
            BIGCAL_MID_CLSTR_TIME64(i) = tsum64 / float(n64)
         enddo
      endif

      return 
      end
