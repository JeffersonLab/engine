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

c     the main purpose of this routine is to fill the timing related variables in the 
c     cluster array. Maybe later we will use this routine to actually "trim" hits with bad timing
c     off of the clusters. For starters, we will calculate the mean time for a cluster based on
c     the values of all unique TDC channels, separately for sums of 8 and sums of 64. We will 
c     also calculate the rms value. If a unique TDC channel has more than one hit, then we 
c     choose the value closest to bigcal_window_center as the best value!!!!!!!!!!!!!!!!!

      integer iclust,icell
      integer i8,n8,i64,n64
      integer irow,icol,irow8,icol8,irow64,icol64
      integer icell8,icell64,ihit8,ihit64
      integer jcell8,jcell64,jhit8,jhit64
      
      real meant8,rmst8,mintdiff,meant64,rmst64,tdiff,thit
      real t8sum2,t64sum2,t8sum,t64sum

      integer ihitmin

      logical overlap_row

      abort=.false.
      err = ' '

      do iclust=1,bigcal_all_nclstr
         n8 = 0
         n64 = 0

         meant8 = 0.
         rmst8 = 0.
         meant64 = 0.
         rmst64 = 0.
         t8sum2 = 0.
         t64sum2 = 0.
         t8sum = 0.
         t64sum = 0.

         do icell=1,bigcal_all_clstr_ncell(iclust)
            irow = bigcal_all_clstr_iycell(iclust,icell)
            icol = bigcal_all_clstr_ixcell(iclust,icell)
            
            irow8 = irow
            if(irow8.le.32) then
               icol8 = (icol-1)/8 + 1
            else 
               if(icol.lt.16) then
                  icol8 = (icol-1)/8 + 1
               else 
                  icol8 = icol/8 + 1
               endif
            endif
            
            icell8 = icol8 + 4*(irow8-1)

            if(bigcal_tdc_det_ngood(icell8).gt.0) then
               if(n8 .eq. 0) then ! first channel with a tdc hit
                  n8 = n8 + 1

                  bigcal_all_clstr_nhit8(iclust,n8) = 
     $                 bigcal_tdc_det_ngood(icell8)
                  bigcal_all_clstr_irow8(iclust,n8) = irow8
                  bigcal_all_clstr_icol8(iclust,n8) = icol8

                  ihitmin = 0

                  do ihit8=1,bigcal_tdc_det_ngood(icell8)
                     thit = bigcal_tdc_good_det(icell8,ihit8)
                     bigcal_all_clstr_tcell8(iclust,n8,ihit8) = thit
                     
                     tdiff = abs(thit - bigcal_window_center)
                     
                     if(ihit8.eq.1) then 
                        mintdiff = tdiff
                        ihitmin = ihit8
                     else 
                        if(tdiff.lt.mintdiff) then
                           mintdiff = tdiff
                           ihitmin = ihit8
                        endif
                     endif
                  enddo

                  thit = bigcal_tdc_good_det(icell8,ihitmin)

                  t8sum = t8sum + thit
                  t8sum2 = t8sum2 + thit**2
                  
               else ! make sure this is a unique TDC channel!!!!
                  do jcell8=1,n8
                     if(irow8.eq.bigcal_all_clstr_irow8(iclust,jcell8)
     $             .and.icol8.eq.bigcal_all_clstr_icol8(iclust,jcell8))
     $                    then ! not unique, exit the if-block
                        goto 101
                     endif
                  enddo
c     if we make it to here without jumping out of this if-block, then 
c     the tdc channel is unique!!!!
                  n8 = n8 + 1
                  
                  if(n8.gt.10) then
                     write(*,*) 'problem, more than 10 unique TDC'//
     $                    ' channels found for cluster ',iclust,
     $                    '(irow,icol) = (',irow,icol,'), (nx,ny)=(',
     $                    bigcal_all_clstr_ncellx(iclust),
     $                    bigcal_all_clstr_ncelly(iclust),
     $                    '), unexpected!'
                     goto 101
                  endif

                  bigcal_all_clstr_nhit8(iclust,n8) =
     $                 bigcal_tdc_det_ngood(icell8)
                  bigcal_all_clstr_irow8(iclust,n8) = irow8
                  bigcal_all_clstr_icol8(iclust,n8) = icol8
                  
                  do ihit8 = 1,bigcal_tdc_det_ngood(icell8)
                     thit = bigcal_tdc_good_det(icell8,ihit8)
                     bigcal_all_clstr_tcell8(iclust,n8,ihit8) = thit

                     tdiff = abs(thit - bigcal_window_center)
                     
                     if(ihit8.eq.1) then 
                        mintdiff = tdiff
                        ihitmin = ihit8
                     else 
                        if(tdiff.lt.mintdiff) then
                           mintdiff = tdiff
                           ihitmin = ihit8
                        endif
                     endif
                  enddo
                  
                  thit = bigcal_tdc_good_det(icell8,ihitmin)

                  t8sum = t8sum + thit
                  t8sum2 = t8sum2 + thit**2

               endif
            endif
 101        continue

            irow64 = (irow-1)/3 + 1
           
            if(irow.le.32) then
               icol64 = (icol-1)/16 + 1
            else 
               icol64 = icol/16 + 1
            endif

            icell64 = icol64 + 2*(irow64-1)
            if(mod(irow-1,3).eq.0 .and.irow.gt.0) then
               overlap_row = .true.
            else 
               overlap_row = .false.
            endif

            goto 104

 103        overlap_row = .false.
 
 104        continue

            if(bigcal_ttrig_det_ngood(icell64).gt.0) then
               if(n64.eq.0) then ! first trig. tdc channel with a hit
                  n64 = n64 + 1
                  
                  bigcal_all_clstr_nhit64(iclust,n64) = 
     $                 bigcal_ttrig_det_ngood(icell64)
                  bigcal_all_clstr_irow64(iclust,n64) = irow64
                  bigcal_all_clstr_icol64(iclust,n64) = icol64
                  bigcal_all_clstr_A64(iclust,n64) = 
     $                 bigcal_atrig_good_det(icell64)
                  bigcal_all_clstr_sum64(iclust,n64) = 
     $                 bigcal_atrig_sum64(icell64)
                  
                  do ihit64=1,bigcal_ttrig_det_ngood(icell64)
                     thit = bigcal_ttrig_good_det(icell64,ihit64)
                     bigcal_all_clstr_tcell64(iclust,n64,ihit64) = thit

                     tdiff = abs(thit - bigcal_window_center)

                     if(ihit64.eq.1) then
                        mintdiff = tdiff
                        ihitmin = ihit64
                     else 
                        if(tdiff.lt.mintdiff) then
                           mintdiff = tdiff
                           ihitmin = ihit64
                        endif
                     endif
                  enddo

                  thit = bigcal_ttrig_good_det(icell64,ihitmin)

                  t64sum = t64sum + thit
                  t64sum2 = t64sum2 + thit**2
               else             ! must check if unique trig. tdc channel
                  do jcell64=1,n64
                     if(irow64.eq.bigcal_all_clstr_irow64(iclust,jcell64)
     $                    .and.icol64.eq.bigcal_all_clstr_icol64(iclust,jcell64))
     $                    then
                        goto 102
                     endif
                  enddo
c     if we make it to this point without jumping out of the if block, then
c     the trig. tdc channel is unique!!!!!!!!!!!!!!!!!!!!!!!!!!
                  n64 = n64 + 1
                  
                  if(n64.gt.6) then
                     write(*,*) 'problem, more than 6 unique TTDC'//
     $                    ' channels found for cluster ',iclust,
     $                    '(irow,icol) = (',irow,icol,'), (nx,ny)=(',
     $                    bigcal_all_clstr_ncellx(iclust),
     $                    bigcal_all_clstr_ncelly(iclust),
     $                    '), unexpected!'
                     goto 102
                  endif

                  bigcal_all_clstr_nhit64(iclust,n64) = 
     $                 bigcal_ttrig_det_ngood(icell64)
                  bigcal_all_clstr_irow64(iclust,n64) = irow64
                  bigcal_all_clstr_icol64(iclust,n64) = icol64
                  bigcal_all_clstr_A64(iclust,n64) = 
     $                 bigcal_atrig_good_det(icell64)
                  bigcal_all_clstr_sum64(iclust,n64) = 
     $                 bigcal_atrig_sum64(icell64)

                  do ihit64=1,bigcal_ttrig_det_ngood(icell64)
                    thit = bigcal_ttrig_good_det(icell64,ihit64)
                    bigcal_all_clstr_tcell64(iclust,n64,ihit64) = thit

                    tdiff = abs(thit - bigcal_window_center)

                     if(ihit64.eq.1) then
                        mintdiff = tdiff
                        ihitmin = ihit64
                     else 
                        if(tdiff.lt.mintdiff) then
                           mintdiff = tdiff
                           ihitmin = ihit64
                        endif
                     endif
                  enddo
                  
                  thit = bigcal_ttrig_good_det(icell64,ihitmin)
                  
                  t64sum = t64sum + thit
                  t64sum2 = t64sum2 + thit**2

               endif
            endif

 102        continue

            if(overlap_row) then ! also check previous overlapping sum64 channel
               icell64 = icell64 - 2
               goto 103
            endif
         enddo
         
         bigcal_all_clstr_ncell8(iclust) = n8
         bigcal_all_clstr_ncell64(iclust) = n64

         meant8 = t8sum / n8
         rmst8 = sqrt(t8sum2 / n8 - meant8**2)

         if(n8.eq.0) then
            meant8 = 0.
            rmst8 = 0.
         endif

         meant64 = t64sum / n64
         rmst64 = sqrt(t64sum2 / n64 - meant64**2)

         if(n64.eq.0) then
            meant64 = 0.
            rmst64 = 0.
         endif

         bigcal_all_clstr_t8mean(iclust) = meant8
         bigcal_all_clstr_t64mean(iclust) = meant64
         bigcal_all_clstr_t8rms(iclust) = rmst8
         bigcal_all_clstr_t64rms(iclust) = rmst64
 
      enddo   
    
      return 
      end
