      subroutine b_find_clusters(ABORT,err)

      implicit none
      save
      
      character*16 here
      parameter (here= 'b_find_clusters')
      
      logical ABORT
      character*(*) err
      
      include 'bigcal_data_structures.cmn'

      integer*4 ihit,jhit,ixmax,iymax,ixcell,iycell
      integer*4 icellclst
      integer*4 ixmax2,iymax2
      real*4 max,max2
      real*4 xmax,ymax
      logical second_max
      integer*4 clstr_ix_temp(BIGCAL_CLSTR_NCELL_MAX)
      integer*4 clstr_iy_temp(BIGCAL_CLSTR_NCELL_MAX)
      real*4 clstr_ecell_temp(BIGCAL_CLSTR_NCELL_MAX)
      real*4 clstr_xcell_temp(BIGCAL_CLSTR_NCELL_MAX)
      real*4 clstr_ycell_temp(BIGCAL_CLSTR_NCELL_MAX)
      integer*4 nneighbor
      integer*4 igroup,irow
      integer*4 itrig,ihalf,ig64
      real*4 hittime
      real*4 trigtime
      integer*4 nxtimesny
      integer*4 maxcelldiff
      logical tdchit
      logical trighit
      integer*4 ncluster_prot,ncluster_rcs,ncluster_mid
      integer*4 itemp
      real*4 rtemp
c     first step, loop through ADC hit arrays and find hit with max. 
c     amplitude
      
      ncluster_prot = 0
      ncluster_rcs = 0
      ncluster_mid = 0

      maxcelldiff = int(sqrt(float(BIGCAL_CLSTR_NCELL_MAX))) / 2

      second_max = .false.
      ixmax = -1
      iymax = -1
      max = -1.

      nxtimesny = max(BIGCAL_PROT_NX,BIGCAL_RCS_NX)*(BIGCAL_PROT_NY + 
     $     BIGCAL_RCS_NY)

      nneighbor = 0

      trigtime = BIGCAL_REF_TIME

      do ihit=1,BIGCAL_PROT_NGOOD
         tdchit = .false.
         trighit = .false.
         ixcell = BIGCAL_PROT_IXGOOD(ihit)
         iycell = BIGCAL_PROT_IYGOOD(ihit)
         igroup = (ixcell - 1) / 8 + 1
         irow = iycell
         ihalf = (ixcell - 1) / 16 + 1
         itrig = (iycell - 1) / 3 + 1
         ig64 = igroup + (ihalf - 1) * BIGCAL_LOGIC_GROUPS / 2
         do jhit=1,BIGCAL_TIME_NGOOD
            if(BIGCAL_TIME_IROW(jhit).eq.irow .and.
     $           BIGCAL_TIME_IGROUP(jhit).eq.igroup .and.
     $           abs(BIGCAL_HIT_TIME(jhit) - trigtime).le.b_timing_cut) 
     $           then
               
               tdchit = .true.
            endif
         enddo
         do jhit=1,BIGCAL_TRIG_NGOOD
            if(BIGCAL_TRIG_GOOD_IGROUP(jhit).eq.itrig .and.
     $           BIGCAL_TRIG_GOOD_IHALF(jhit).eq.ihalf .and.
     $       abs(BIGCAL_TRIG_TIME_GOOD(jhit)-trigtime).le.b_timing_cut) 
     $           then
               trighit = .true.
            endif
         enddo
         if(BIGCAL_PROT_ECELL(ihit).gt.max.and.(tdchit.or.trighit))then  
            ixmax = BIGCAL_PROT_IXGOOD(ihit)
            iymax = BIGCAL_PROT_IYGOOD(ihit)
            max = BIGCAL_PROT_ECELL(ihit)
         endif
      enddo

      ! "zero" temporary cluster arrays:
      do ihit = 1,BIGCAL_CLSTR_NCELL_MAX
         clstr_ix_temp(ihit) = 0
         clstr_iy_temp(ihit) = 0
         clstr_ecell_temp(ihit) = 0.
         clstr_xcell_temp(ihit) = 0.
         clstr_ycell_temp(ihit) = 0.
      enddo

      if(max.ge.0. .and. ixmax .ge. 2 .and. iymax .ge. 2 .and. ixmax 
     $     .le. BIGCAL_PROT_NX-1 .and. iymax .le. BIGCAL_PROT_NY-1) then
         do ihit=1,BIGCAL_PROT_NGOOD
            ixcell = BIGCAL_PROT_IXGOOD(ihit)
            iycell = BIGCAL_PROT_IYGOOD(ihit)
            
            if(-maxcelldiff .le. ixcell - ixmax .and. ixcell - ixmax 
     $           .le. maxcelldiff .and. -maxcelldiff .le. iycell - iymax 
     $           .and. iycell - iymax .le. maxcelldiff) then
               
               nneighbor = nneighbor + 1
               if(nneighbor .le. BIGCAL_CLSTR_NCELL_MAX) then
                  clstr_ix_temp(nneighbor) = ixcell
                  clstr_iy_temp(nneighbor) = iycell
                  clstr_ecell_temp(nneighbor) = BIGCAL_PROT_ECELL(ihit)
                  clstr_xcell_temp(nneighbor) = BIGCAL_PROT_XGOOD(ihit)
                  clstr_ycell_temp(nneighbor) = BIGCAL_PROT_YGOOD(ihit)
               else 
                  goto 100! shouldn't happen
               endif
            endif
         enddo
 100     continue
         ! now check temporary cluster for absence of second max: 
         ! sort cluster in descending order of amplitude
         do ihit = 1, BIGCAL_CLSTR_NCELL_MAX
            do jhit = ihit+1, BIGCAL_CLSTR_NCELL_MAX
               
            enddo
         enddo
         

         

      endif
      

      
      
      
      
      return 
      end
