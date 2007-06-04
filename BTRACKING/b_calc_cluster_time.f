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

      integer iclust,jclust,kclust
      integer icell,jcell,kcell,ncell
      integer ihit,jhit,khit,nhit
      integer irow,icol
      integer irow8,icol8,isum8
      integer irow64,icol64,isum64
      integer ixlo8,iylo8,ixhi8,iyhi8
      integer ixlo64,iylo64,ixhi64,iyhi64
      integer nx64,ny64,nx8,ny8,nhit8,nhit64
      integer i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

      logical goodhit8,goodhit64,firstcombo

      real currenthits8(10)
      real currenthits64(6)

      real tmean,trms,bestrms,minrms,tsum2,bestmean

c     what is our approach? First we count the number of unique 
c     sum8 and sum64 channels in each cluster. Then, for each unique channel,
c     we record the time for each hit in that channel (up to 8). Then, to calculate the
c     best time for each cluster, we find the combination of hits which best agrees with
c     the channel containing the maximum. If the channel with the maximum has more than one
c     hit either in the sum8 or sum64 channel, then we find the ...
c     **********************************************************************************
c     **********************************************************************************
c     **//////////////////////////////////////////////////////////////////////////////**
c     **//////////////////////////////PROTVINO////////////////////////////////////////**
c     **//////////////////////////////////////////////////////////////////////////////**
c     **********************************************************************************
c     **********************************************************************************

      if(bigcal_prot_nclstr.gt.0) then
        do iclust=1,bigcal_prot_nclstr
          ixlo8 = 5
          ixhi8 = 0
          iylo8 = 57
          iyhi8 = 0
          
          ixlo64 = 3
          ixhi64 = 0
          iylo64 = 20
          iyhi64 = 0
          
          do icell=1,bigcal_prot_clstr_ncell(iclust)
            irow = bigcal_prot_clstr_iycell(iclust,icell)
            icol = bigcal_prot_clstr_ixcell(iclust,icell)
            irow8 = irow
            irow64 = (irow-1) / 3 + 1
            if(irow.le.32) then
              icol8 = (icol-1)/8 + 1
              icol64 = (icol-1)/16 + 1
            else
              if(icol.lt.16) then
                icol8 = (icol-1)/8 + 1
                icol64 = 1
              else
                icol8 = icol/8 + 1
                icol64 = 2
              endif
            endif ! all of irowcol864 are now set

            isum8 = icol8 + 4*(irow8-1)
            isum64 = icol64 + 2*(irow64-1)

            goodhit8 = bigcal_tdc_det_ngood(isum8).gt.0
            goodhit64 = bigcal_ttrig_det_ngood(isum64).gt.0

            if(irow8.gt.iyhi8.and.goodhit8) then
              iyhi8 = irow8
            endif
            
            if(irow8.lt.iylo8.and.goodhit8) then
              iylo8 = irow8
            endif

            if(icol8.gt.ixhi8.and.goodhit8) then
              ixhi8 = icol8
            endif
            
            if(icol8.lt.ixlo8.and.goodhit8) then
              ixlo8 = icol8
            endif

            if(icol64.gt.ixhi64.and.goodhit64) then
              ixhi64 = icol64
            endif

            if(icol64.lt.ixlo64.and.goodhit64) then
              ixlo64 = icol64
            endif
  

            if( mod(irow-1,3).eq.0 .and. irow.gt.1 ) then
              if(irow64-1.lt.iylo64.and.bigcal_ttrig_det_ngood(isum64-2) 
     $             .gt.0)then
                iylo64 = irow64 - 1
              endif
              if( irow64.gt.iyhi64.and.goodhit64) then
                iyhi64 = irow64
              endif
            else
              if( irow64.lt.iylo64.and.goodhit64) then
                iylo64 = irow64
              endif
              if( irow64.gt.iyhi64.and.goodhit64) then
                iyhi64 = irow64
              endif
            endif
          enddo

          nx8 = ixhi8 - ixlo8 + 1
          ny8 = iyhi8 - iylo8 + 1
          
          if(nx8.ge.1.and.nx8.le.2.and.ny8.ge.1.and.ny8.le.5)then ! bounds are sensible, continue:
            nhit8 = 0
            do icol=ixlo8,ixhi8
              do irow=iylo8,iyhi8
                nhit8 = nhit8 + 1
                isum8 = icol + 4*(irow-1)
                do ihit=1,bigcal_tdc_det_ngood(isum8)
                  bigcal_prot_clstr_tcell8(iclust,nhit8,ihit) = 
     $                 bigcal_tdc_good_det(isum8,ihit)
                enddo
                bigcal_prot_clstr_nhit8(iclust,nhit8) = 
     $               bigcal_tdc_det_ngood(isum8)
              enddo
            enddo
            bigcal_prot_clstr_ncell8(iclust) = nhit8
          endif
          nx64 = ixhi64 - ixlo64 + 1
          ny64 = iyhi64 - iylo64 + 1
          if(nx64.ge.1.and.nx64.le.2.and.ny64.ge.1.and.ny64.le.3)then ! bounds are sensible, continue:
            nhit64 = 0
            do icol=ixlo64,ixhi64
              do irow=iylo64,iyhi64
                nhit64 = nhit64 + 1
                isum64 = icol + 2*(irow-1)
                do ihit=1,bigcal_ttrig_det_ngood(isum64)
                  bigcal_prot_clstr_tcell64(iclust,nhit64,ihit) = 
     $                 bigcal_ttrig_good_det(isum64,ihit)
                enddo
                bigcal_prot_clstr_nhit64(iclust,nhit64) = 
     $               bigcal_ttrig_det_ngood(isum64)
              enddo
            enddo
            bigcal_prot_clstr_ncell64(iclust) = nhit64
          endif
c     subsequent analysis to calculate best time for the sums of 8 and sums of 64 - do tomorrow
c     fill array of all possible hit combinations:
c     for sum8: a maximum of 10 sum8 cells with a hit: 

          do ihit=1,10
            currenthits8(ihit) = 0.
          enddo
          
          do ihit=1,6
            currenthits64(ihit) = 0.
          enddo

          bestrms = b_timing_cut
          firstcombo = .true.

          do i1 = 1,bigcal_prot_clstr_nhit8(iclust,1)
            currenthits8(1) = bigcal_prot_clstr_tcell8(iclust,1,i1)
            if(bigcal_prot_clstr_ncell8(iclust).gt.1)then
              do i2 = 1,bigcal_prot_clstr_nhit8(iclust,2)
                currenthits8(2) = bigcal_prot_clstr_tcell8(iclust,2,i2)
                if(bigcal_prot_clstr_ncell8(iclust).gt.2) then
                  do i3 = 1,bigcal_prot_clstr_nhit8(iclust,3)
                    currenthits8(3) = bigcal_prot_clstr_tcell8(iclust,3,i3)
                    if(bigcal_prot_clstr_ncell8(iclust).gt.3) then
                      do i4 = 1,bigcal_prot_clstr_nhit8(iclust,4)
                        currenthits8(4) = 
     $                       bigcal_prot_clstr_tcell8(iclust,4,i4)
                        if(bigcal_prot_clstr_ncell8(iclust).gt.4) then
                          do i5=1,bigcal_prot_clstr_nhit8(iclust,5)
                            currenthits8(5) = 
     $                           bigcal_prot_clstr_tcell8(iclust,5,i5)
                            if(bigcal_prot_clstr_ncell8(iclust).gt.5) then
                              do i6=1,bigcal_prot_clstr_nhit8(iclust,6)
                                currenthits8(6) = 
     $                               bigcal_prot_clstr_tcell8(iclust,6,i6)
                                if(bigcal_prot_clstr_ncell8(iclust).gt.6) then
                                  do i7=1,bigcal_prot_clstr_nhit8(iclust,7)
                                    currenthits8(7) = 
     $                                   bigcal_prot_clstr_tcell8(iclust,7,i7)
                                    if(bigcal_prot_clstr_ncell8(iclust).gt.7)then
                                      do i8=1,bigcal_prot_clstr_nhit8(iclust,8)
                                        currenthits8(8) = 
     $                                       bigcal_prot_clstr_tcell8(iclust,8,i8)
                                        if(bigcal_prot_clstr_ncell8(iclust).gt.8) then
                                          do i9=1,bigcal_prot_clstr_nhit8(iclust,9)
                                            currenthits8(9) = 
     $                                           bigcal_prot_clstr_tcell8(iclust,9,i9)
                                            if(bigcal_prot_clstr_ncell8(iclust).gt.9)
     $                                           then
                                              do i10=1,bigcal_prot_clstr_nhit8(iclust,10)
                                                currenthits8(10) =
     $                                               bigcal_prot_clstr_tcell8(iclust,10,i10)
                                                
                                                tmean = 0.
                                                trms = 0.
                                                tsum2 = 0.
                                                do ihit=1,10
                                                  tmean = tmean + currenthits8(ihit)
                                                  tsum2 = tsum2 + currenthits8(ihit)**2
                                                enddo
                                                tmean = 0.1 * tmean
                                                tsum2 = 0.1 * tsum2
                                                trms = sqrt(tsum2 - tmean**2)
                                                if(firstcombo) then
                                                  firstcombo=.false.
                                                  minrms = trms
                                                  bestmean = tmean
                                                else if(trms.lt.minrms) then
                                                  minrms = trms
                                                  bestmean = tmean
                                                endif
                                              enddo
                                            else
                                              tmean = 0.
                                              trms = 0.
                                              tsum2 = 0.
                                              do ihit=1,9
                                                tmean = tmean + currenthits8(ihit)
                                                tsum2 = tsum2 + currenthits8(ihit)**2
                                              enddo
                                              tmean = tmean / 9.
                                              tsum2 = tsum2 / 9.
                                              trms = sqrt(tsum2 - tmean**2)
                                              if(firstcombo) then
                                                firstcombo=.false.
                                                minrms = trms
                                                bestmean = tmean
                                              else if(trms.lt.minrms) then
                                                minrms = trms
                                                bestmean = tmean
                                              endif
                                            endif
                                          enddo
                                        else
                                          tmean = 0.
                                          trms = 0.
                                          tsum2 = 0.
                                          do ihit=1,8
                                            tmean = tmean + currenthits8(ihit)
                                            tsum2 = tsum2 + currenthits8(ihit)**2
                                          enddo
                                          tmean = tmean / 8.
                                          tsum2 = tsum2 / 8.
                                          trms = sqrt(tsum2 - tmean**2)
                                          if(firstcombo) then
                                            firstcombo=.false.
                                            minrms = trms
                                            bestmean = tmean
                                          else if(trms.lt.minrms) then
                                            minrms = trms
                                            bestmean = tmean
                                          endif
                                        endif
                                      enddo
                                    else
                                      tmean = 0.
                                      trms = 0.
                                      tsum2 = 0.
                                      do ihit=1,7
                                        tmean = tmean + currenthits8(ihit)
                                        tsum2 = tsum2 + currenthits8(ihit)**2
                                      enddo
                                      tmean = tmean / 7.
                                      tsum2 = tsum2 / 7.
                                      trms = sqrt(tsum2 - tmean**2)
                                      if(firstcombo) then
                                        firstcombo=.false.
                                        minrms = trms
                                        bestmean = tmean
                                      else if(trms.lt.minrms) then
                                        minrms = trms
                                        bestmean = tmean
                                      endif
                                    endif
                                  enddo
                                else
                                  tmean = 0.
                                  trms = 0.
                                  tsum2 = 0.
                                  do ihit=1,6
                                    tmean = tmean + currenthits8(ihit)
                                    tsum2 = tsum2 + currenthits8(ihit)**2
                                  enddo
                                  tmean = tmean / 6.
                                  tsum2 = tsum2 / 6.
                                  trms = sqrt(tsum2 - tmean**2)
                                  if(firstcombo) then
                                    firstcombo=.false.
                                    minrms = trms
                                    bestmean = tmean
                                  else if(trms.lt.minrms) then
                                    minrms = trms
                                    bestmean = tmean
                                  endif
                                endif
                              enddo
                            else
                              tmean = 0.
                              trms = 0.
                              tsum2 = 0.
                              do ihit=1,5
                                tmean = tmean + currenthits8(ihit)
                                tsum2 = tsum2 + currenthits8(ihit)**2
                              enddo
                              tmean = tmean / 5.
                              tsum2 = tsum2 / 5.
                              trms = sqrt(tsum2 - tmean**2)
                              if(firstcombo) then
                                firstcombo=.false.
                                minrms = trms
                                bestmean = tmean
                              else if(trms.lt.minrms) then
                                minrms = trms
                                bestmean = tmean
                              endif
                            endif
                          enddo
                        else
                          tmean = 0.
                          trms = 0.
                          tsum2 = 0.
                          do ihit=1,4
                            tmean = tmean + currenthits8(ihit)
                            tsum2 = tsum2 + currenthits8(ihit)**2
                          enddo
                          tmean = tmean / 4.
                          tsum2 = tsum2 / 4.
                          trms = sqrt(tsum2 - tmean**2)
                          if(firstcombo) then
                            firstcombo=.false.
                            minrms = trms
                            bestmean = tmean
                          else if(trms.lt.minrms) then
                            minrms = trms
                            bestmean = tmean
                          endif
                        endif
                      enddo
                    else
                      tmean = 0.
                      trms = 0.
                      tsum2 = 0.
                      do ihit=1,3
                        tmean = tmean + currenthits8(ihit)
                        tsum2 = tsum2 + currenthits8(ihit)**2
                      enddo
                      tmean = tmean / 3.
                      tsum2 = tsum2 / 3.
                      trms = sqrt(tsum2 - tmean**2)
                      if(firstcombo) then
                        firstcombo=.false.
                        minrms = trms
                        bestmean = tmean
                      else if(trms.lt.minrms) then
                        minrms = trms
                        bestmean = tmean
                      endif
                    endif
                  enddo
                else
                  tmean = 0.
                  trms = 0.
                  tsum2 = 0.
                  do ihit=1,2
                    tmean = tmean + currenthits8(ihit)
                    tsum2 = tsum2 + currenthits8(ihit)**2
                  enddo
                  tmean = tmean / 2.
                  tsum2 = tsum2 / 2.
                  trms = sqrt(tsum2 - tmean**2)
                  if(firstcombo) then
                    firstcombo=.false.
                    minrms = trms
                    bestmean = tmean
                  else if(trms.lt.minrms) then
                    minrms = trms
                    bestmean = tmean
                  endif
                endif
              enddo
            else ! only one sum8 cell associated with this cluster, use first hit
              tmean = 0.
              trms = 0.
              tsum2 = 0.
              do ihit=1,1
                tmean = tmean + currenthits8(ihit)
                tsum2 = tsum2 + currenthits8(ihit)**2
              enddo
              tmean = tmean / 1.
              tsum2 = tsum2 / 1.
              trms = sqrt(tsum2 - tmean**2)
              if(firstcombo) then
                firstcombo=.false.
                minrms = trms
                bestmean = tmean
              else if(trms.lt.minrms) then
                minrms = trms
                bestmean = tmean
              endif
            endif
          enddo
          
c     ! at this point we have calculated the average time for the "best" 
c     ! combination of tdc hits in all sum8 channels associated with this cell.
c     ! set "best" time and logical whether the rms of the best combination is 
c     ! smaller than b_timing_cut:
          
          bigcal_prot_clstr_t8best(iclust) = bestmean
          if(minrms.le.b_timing_cut) then
            bigcal_prot_clstr_good8(iclust) = .true.
          else
            bigcal_prot_clstr_good8(iclust) = .false.
          endif

c     ! now repeat this calculation for the sum64 channels: 
          
          firstcombo = .true.

          do i1 = 1,bigcal_prot_clstr_nhit64(iclust,1)
            currenthits64(1) = bigcal_prot_clstr_tcell64(iclust,1,i1)
            if(bigcal_prot_clstr_ncell64(iclust).gt.1)then
              do i2 = 1,bigcal_prot_clstr_nhit64(iclust,2)
                currenthits64(2) = bigcal_prot_clstr_tcell64(iclust,2,i2)
                if(bigcal_prot_clstr_ncell64(iclust).gt.2) then
                  do i3 = 1,bigcal_prot_clstr_nhit64(iclust,3)
                    currenthits64(3) = bigcal_prot_clstr_tcell64(iclust,3,i3)
                    if(bigcal_prot_clstr_ncell64(iclust).gt.3) then
                      do i4 = 1,bigcal_prot_clstr_nhit64(iclust,4)
                        currenthits64(4) = 
     $                       bigcal_prot_clstr_tcell64(iclust,4,i4)
                        if(bigcal_prot_clstr_ncell64(iclust).gt.4) then
                          do i5=1,bigcal_prot_clstr_nhit64(iclust,5)
                            currenthits64(5) = 
     $                           bigcal_prot_clstr_tcell64(iclust,5,i5)
                            if(bigcal_prot_clstr_ncell64(iclust).gt.5) then
                              do i6=1,bigcal_prot_clstr_nhit64(iclust,6)
                                currenthits64(6) = 
     $                               bigcal_prot_clstr_tcell64(iclust,6,i6)
                                
                                tmean = 0.
                                trms = 0.
                                tsum2 = 0.
                                do ihit=1,6
                                  tmean = tmean + currenthits64(ihit)
                                  tsum2 = tsum2 + currenthits64(ihit)**2
                                enddo
                                tmean = tmean / 6.
                                tsum2 = tsum2 / 6.
                                trms = sqrt(tsum2 - tmean**2)
                                if(firstcombo) then
                                  firstcombo=.false.
                                  minrms = trms
                                  bestmean = tmean
                                else if(trms.lt.minrms) then
                                  minrms = trms
                                  bestmean = tmean
                                endif
                              enddo
                            else
                              tmean = 0.
                              trms = 0.
                              tsum2 = 0.
                              do ihit=1,5
                                tmean = tmean + currenthits64(ihit)
                                tsum2 = tsum2 + currenthits64(ihit)**2
                              enddo
                              tmean = tmean / 5.
                              tsum2 = tsum2 / 5.
                              trms = sqrt(tsum2 - tmean**2)
                              if(firstcombo) then
                                firstcombo=.false.
                                minrms = trms
                                bestmean = tmean
                              else if(trms.lt.minrms) then
                                minrms = trms
                                bestmean = tmean
                              endif
                            endif
                          enddo
                        else
                          tmean = 0.
                          trms = 0.
                          tsum2 = 0.
                          do ihit=1,4
                            tmean = tmean + currenthits64(ihit)
                            tsum2 = tsum2 + currenthits64(ihit)**2
                          enddo
                          tmean = tmean / 4.
                          tsum2 = tsum2 / 4.
                          trms = sqrt(tsum2 - tmean**2)
                          if(firstcombo) then
                            firstcombo=.false.
                            minrms = trms
                            bestmean = tmean
                          else if(trms.lt.minrms) then
                            minrms = trms
                            bestmean = tmean
                          endif
                        endif
                      enddo
                    else
                      tmean = 0.
                      trms = 0.
                      tsum2 = 0.
                      do ihit=1,3
                        tmean = tmean + currenthits64(ihit)
                        tsum2 = tsum2 + currenthits64(ihit)**2
                      enddo
                      tmean = tmean / 3.
                      tsum2 = tsum2 / 3.
                      trms = sqrt(tsum2 - tmean**2)
                      if(firstcombo) then
                        firstcombo=.false.
                        minrms = trms
                        bestmean = tmean
                      else if(trms.lt.minrms) then
                        minrms = trms
                        bestmean = tmean
                      endif
                    endif
                  enddo
                else
                  tmean = 0.
                  trms = 0.
                  tsum2 = 0.
                  do ihit=1,2
                    tmean = tmean + currenthits64(ihit)
                    tsum2 = tsum2 + currenthits64(ihit)**2
                  enddo
                  tmean = tmean / 2.
                  tsum2 = tsum2 / 2.
                  trms = sqrt(tsum2 - tmean**2)
                  if(firstcombo) then
                    firstcombo=.false.
                    minrms = trms
                    bestmean = tmean
                  else if(trms.lt.minrms) then
                    minrms = trms
                    bestmean = tmean
                  endif
                endif
              enddo
            else
              tmean = 0.
              trms = 0.
              tsum2 = 0.
              do ihit=1,1
                tmean = tmean + currenthits64(ihit)
                tsum2 = tsum2 + currenthits64(ihit)**2
              enddo
              tmean = tmean / 1.
              tsum2 = tsum2 / 1.
              trms = sqrt(tsum2 - tmean**2)
              if(firstcombo) then
                firstcombo=.false.
                minrms = trms
                bestmean = tmean
              else if(trms.lt.minrms) then
                minrms = trms
                bestmean = tmean
              endif
            endif
          enddo
          
          bigcal_prot_clstr_t64best(iclust) = bestmean
          if(minrms .le. b_timing_cut) then
            bigcal_prot_clstr_good64(iclust) = .true.
          else
            bigcal_prot_clstr_good64(iclust) = .false.
          endif
        enddo
      endif

c     **********************************************************************************
c     **********************************************************************************
c     **//////////////////////////////////////////////////////////////////////////////**
c     **/////////////////////////////////RCS//////////////////////////////////////////**
c     **//////////////////////////////////////////////////////////////////////////////**
c     **********************************************************************************
c     **********************************************************************************

      if(bigcal_rcs_nclstr.gt.0) then
        do iclust=1,bigcal_rcs_nclstr
          ixlo8 = 5
          ixhi8 = 0
          iylo8 = 57
          iyhi8 = 0
          
          ixlo64 = 3
          ixhi64 = 0
          iylo64 = 20
          iyhi64 = 0
          
          do icell=1,bigcal_rcs_clstr_ncell(iclust)
            irow = bigcal_rcs_clstr_iycell(iclust,icell)
            icol = bigcal_rcs_clstr_ixcell(iclust,icell)
            irow8 = irow
            irow64 = (irow-1) / 3 + 1
            if(irow.le.32) then
              icol8 = (icol-1)/8 + 1
              icol64 = (icol-1)/16 + 1
            else
              if(icol.lt.16) then
                icol8 = (icol-1)/8 + 1
                icol64 = 1
              else
                icol8 = icol/8 + 1
                icol64 = 2
              endif
            endif ! all of irowcol864 are now set

            isum8 = icol8 + 4*(irow8-1)
            isum64 = icol64 + 2*(irow64-1)

            goodhit8 = bigcal_tdc_det_ngood(isum8).gt.0
            goodhit64 = bigcal_ttrig_det_ngood(isum64).gt.0

            if(irow8.gt.iyhi8.and.goodhit8) then
              iyhi8 = irow8
            endif
            
            if(irow8.lt.iylo8.and.goodhit8) then
              iylo8 = irow8
            endif

            if(icol8.gt.ixhi8.and.goodhit8) then
              ixhi8 = icol8
            endif
            
            if(icol8.lt.ixlo8.and.goodhit8) then
              ixlo8 = icol8
            endif

            if(icol64.gt.ixhi64.and.goodhit64) then
              ixhi64 = icol64
            endif

            if(icol64.lt.ixlo64.and.goodhit64) then
              ixlo64 = icol64
            endif
  

            if( mod(irow-1,3).eq.0 .and. irow.gt.1 ) then
              if(irow64-1.lt.iylo64.and.bigcal_ttrig_det_ngood(isum64-2) 
     $             .gt.0)then
                iylo64 = irow64 - 1
              endif
              if( irow64.gt.iyhi64.and.goodhit64) then
                iyhi64 = irow64
              endif
            else
              if( irow64.lt.iylo64.and.goodhit64) then
                iylo64 = irow64
              endif
              if( irow64.gt.iyhi64.and.goodhit64) then
                iyhi64 = irow64
              endif
            endif
          enddo

          nx8 = ixhi8 - ixlo8 + 1
          ny8 = iyhi8 - iylo8 + 1
          
          if(nx8.ge.1.and.nx8.le.2.and.ny8.ge.1.and.ny8.le.5)then ! bounds are sensible, continue:
            nhit8 = 0
            do icol=ixlo8,ixhi8
              do irow=iylo8,iyhi8
                nhit8 = nhit8 + 1
                isum8 = icol + 4*(irow-1)
                do ihit=1,bigcal_tdc_det_ngood(isum8)
                  bigcal_rcs_clstr_tcell8(iclust,nhit8,ihit) = 
     $                 bigcal_tdc_good_det(isum8,ihit)
                enddo
                bigcal_rcs_clstr_nhit8(iclust,nhit8) = 
     $               bigcal_tdc_det_ngood(isum8)
              enddo
            enddo
            bigcal_rcs_clstr_ncell8(iclust) = nhit8
          endif
          nx64 = ixhi64 - ixlo64 + 1
          ny64 = iyhi64 - iylo64 + 1
          if(nx64.ge.1.and.nx64.le.2.and.ny64.ge.1.and.ny64.le.3)then ! bounds are sensible, continue:
            nhit64 = 0
            do icol=ixlo64,ixhi64
              do irow=iylo64,iyhi64
                nhit64 = nhit64 + 1
                isum64 = icol + 2*(irow-1)
                do ihit=1,bigcal_ttrig_det_ngood(isum64)
                  bigcal_rcs_clstr_tcell64(iclust,nhit64,ihit) = 
     $                 bigcal_ttrig_good_det(isum64,ihit)
                enddo
                bigcal_rcs_clstr_nhit64(iclust,nhit64) = 
     $               bigcal_ttrig_det_ngood(isum64)
              enddo
            enddo
            bigcal_rcs_clstr_ncell64(iclust) = nhit64
          endif
c     subsequent analysis to calculate best time for the sums of 8 and sums of 64 - do tomorrow
c     fill array of all possible hit combinations:
c     for sum8: a maximum of 10 sum8 cells with a hit: 

          do ihit=1,10
            currenthits8(ihit) = 0.
          enddo
          
          do ihit=1,6
            currenthits64(ihit) = 0.
          enddo

          bestrms = b_timing_cut
          firstcombo = .true.

          do i1 = 1,bigcal_rcs_clstr_nhit8(iclust,1)
            currenthits8(1) = bigcal_rcs_clstr_tcell8(iclust,1,i1)
            if(bigcal_rcs_clstr_ncell8(iclust).gt.1)then
              do i2 = 1,bigcal_rcs_clstr_nhit8(iclust,2)
                currenthits8(2) = bigcal_rcs_clstr_tcell8(iclust,2,i2)
                if(bigcal_rcs_clstr_ncell8(iclust).gt.2) then
                  do i3 = 1,bigcal_rcs_clstr_nhit8(iclust,3)
                    currenthits8(3) = bigcal_rcs_clstr_tcell8(iclust,3,i3)
                    if(bigcal_rcs_clstr_ncell8(iclust).gt.3) then
                      do i4 = 1,bigcal_rcs_clstr_nhit8(iclust,4)
                        currenthits8(4) = 
     $                       bigcal_rcs_clstr_tcell8(iclust,4,i4)
                        if(bigcal_rcs_clstr_ncell8(iclust).gt.4) then
                          do i5=1,bigcal_rcs_clstr_nhit8(iclust,5)
                            currenthits8(5) = 
     $                           bigcal_rcs_clstr_tcell8(iclust,5,i5)
                            if(bigcal_rcs_clstr_ncell8(iclust).gt.5) then
                              do i6=1,bigcal_rcs_clstr_nhit8(iclust,6)
                                currenthits8(6) = 
     $                               bigcal_rcs_clstr_tcell8(iclust,6,i6)
                                if(bigcal_rcs_clstr_ncell8(iclust).gt.6) then
                                  do i7=1,bigcal_rcs_clstr_nhit8(iclust,7)
                                    currenthits8(7) = 
     $                                   bigcal_rcs_clstr_tcell8(iclust,7,i7)
                                    if(bigcal_rcs_clstr_ncell8(iclust).gt.7)then
                                      do i8=1,bigcal_rcs_clstr_nhit8(iclust,8)
                                        currenthits8(8) = 
     $                                       bigcal_rcs_clstr_tcell8(iclust,8,i8)
                                        if(bigcal_rcs_clstr_ncell8(iclust).gt.8) then
                                          do i9=1,bigcal_rcs_clstr_nhit8(iclust,9)
                                            currenthits8(9) = 
     $                                           bigcal_rcs_clstr_tcell8(iclust,9,i9)
                                            if(bigcal_rcs_clstr_ncell8(iclust).gt.9)
     $                                           then
                                              do i10=1,bigcal_rcs_clstr_nhit8(iclust,10)
                                                currenthits8(10) =
     $                                               bigcal_rcs_clstr_tcell8(iclust,10,i10)
                                                
                                                tmean = 0.
                                                trms = 0.
                                                tsum2 = 0.
                                                do ihit=1,10
                                                  tmean = tmean + currenthits8(ihit)
                                                  tsum2 = tsum2 + currenthits8(ihit)**2
                                                enddo
                                                tmean = 0.1 * tmean
                                                tsum2 = 0.1 * tsum2
                                                trms = sqrt(tsum2 - tmean**2)
                                                if(firstcombo) then
                                                  firstcombo=.false.
                                                  minrms = trms
                                                  bestmean = tmean
                                                else if(trms.lt.minrms) then
                                                  minrms = trms
                                                  bestmean = tmean
                                                endif
                                              enddo
                                            else
                                              tmean = 0.
                                              trms = 0.
                                              tsum2 = 0.
                                              do ihit=1,9
                                                tmean = tmean + currenthits8(ihit)
                                                tsum2 = tsum2 + currenthits8(ihit)**2
                                              enddo
                                              tmean = tmean / 9.
                                              tsum2 = tsum2 / 9.
                                              trms = sqrt(tsum2 - tmean**2)
                                              if(firstcombo) then
                                                firstcombo=.false.
                                                minrms = trms
                                                bestmean = tmean
                                              else if(trms.lt.minrms) then
                                                minrms = trms
                                                bestmean = tmean
                                              endif
                                            endif
                                          enddo
                                        else
                                          tmean = 0.
                                          trms = 0.
                                          tsum2 = 0.
                                          do ihit=1,8
                                            tmean = tmean + currenthits8(ihit)
                                            tsum2 = tsum2 + currenthits8(ihit)**2
                                          enddo
                                          tmean = tmean / 8.
                                          tsum2 = tsum2 / 8.
                                          trms = sqrt(tsum2 - tmean**2)
                                          if(firstcombo) then
                                            firstcombo=.false.
                                            minrms = trms
                                            bestmean = tmean
                                          else if(trms.lt.minrms) then
                                            minrms = trms
                                            bestmean = tmean
                                          endif
                                        endif
                                      enddo
                                    else
                                      tmean = 0.
                                      trms = 0.
                                      tsum2 = 0.
                                      do ihit=1,7
                                        tmean = tmean + currenthits8(ihit)
                                        tsum2 = tsum2 + currenthits8(ihit)**2
                                      enddo
                                      tmean = tmean / 7.
                                      tsum2 = tsum2 / 7.
                                      trms = sqrt(tsum2 - tmean**2)
                                      if(firstcombo) then
                                        firstcombo=.false.
                                        minrms = trms
                                        bestmean = tmean
                                      else if(trms.lt.minrms) then
                                        minrms = trms
                                        bestmean = tmean
                                      endif
                                    endif
                                  enddo
                                else
                                  tmean = 0.
                                  trms = 0.
                                  tsum2 = 0.
                                  do ihit=1,6
                                    tmean = tmean + currenthits8(ihit)
                                    tsum2 = tsum2 + currenthits8(ihit)**2
                                  enddo
                                  tmean = tmean / 6.
                                  tsum2 = tsum2 / 6.
                                  trms = sqrt(tsum2 - tmean**2)
                                  if(firstcombo) then
                                    firstcombo=.false.
                                    minrms = trms
                                    bestmean = tmean
                                  else if(trms.lt.minrms) then
                                    minrms = trms
                                    bestmean = tmean
                                  endif
                                endif
                              enddo
                            else
                              tmean = 0.
                              trms = 0.
                              tsum2 = 0.
                              do ihit=1,5
                                tmean = tmean + currenthits8(ihit)
                                tsum2 = tsum2 + currenthits8(ihit)**2
                              enddo
                              tmean = tmean / 5.
                              tsum2 = tsum2 / 5.
                              trms = sqrt(tsum2 - tmean**2)
                              if(firstcombo) then
                                firstcombo=.false.
                                minrms = trms
                                bestmean = tmean
                              else if(trms.lt.minrms) then
                                minrms = trms
                                bestmean = tmean
                              endif
                            endif
                          enddo
                        else
                          tmean = 0.
                          trms = 0.
                          tsum2 = 0.
                          do ihit=1,4
                            tmean = tmean + currenthits8(ihit)
                            tsum2 = tsum2 + currenthits8(ihit)**2
                          enddo
                          tmean = tmean / 4.
                          tsum2 = tsum2 / 4.
                          trms = sqrt(tsum2 - tmean**2)
                          if(firstcombo) then
                            firstcombo=.false.
                            minrms = trms
                            bestmean = tmean
                          else if(trms.lt.minrms) then
                            minrms = trms
                            bestmean = tmean
                          endif
                        endif
                      enddo
                    else
                      tmean = 0.
                      trms = 0.
                      tsum2 = 0.
                      do ihit=1,3
                        tmean = tmean + currenthits8(ihit)
                        tsum2 = tsum2 + currenthits8(ihit)**2
                      enddo
                      tmean = tmean / 3.
                      tsum2 = tsum2 / 3.
                      trms = sqrt(tsum2 - tmean**2)
                      if(firstcombo) then
                        firstcombo=.false.
                        minrms = trms
                        bestmean = tmean
                      else if(trms.lt.minrms) then
                        minrms = trms
                        bestmean = tmean
                      endif
                    endif
                  enddo
                else
                  tmean = 0.
                  trms = 0.
                  tsum2 = 0.
                  do ihit=1,2
                    tmean = tmean + currenthits8(ihit)
                    tsum2 = tsum2 + currenthits8(ihit)**2
                  enddo
                  tmean = tmean / 2.
                  tsum2 = tsum2 / 2.
                  trms = sqrt(tsum2 - tmean**2)
                  if(firstcombo) then
                    firstcombo=.false.
                    minrms = trms
                    bestmean = tmean
                  else if(trms.lt.minrms) then
                    minrms = trms
                    bestmean = tmean
                  endif
                endif
              enddo
            else ! only one sum8 cell associated with this cluster, use first hit
              tmean = 0.
              trms = 0.
              tsum2 = 0.
              do ihit=1,1
                tmean = tmean + currenthits8(ihit)
                tsum2 = tsum2 + currenthits8(ihit)**2
              enddo
              tmean = tmean / 1.
              tsum2 = tsum2 / 1.
              trms = sqrt(tsum2 - tmean**2)
              if(firstcombo) then
                firstcombo=.false.
                minrms = trms
                bestmean = tmean
              else if(trms.lt.minrms) then
                minrms = trms
                bestmean = tmean
              endif
            endif
          enddo
          
c     ! at this point we have calculated the average time for the "best" 
c     ! combination of tdc hits in all sum8 channels associated with this cell.
c     ! set "best" time and logical whether the rms of the best combination is 
c     ! smaller than b_timing_cut:
          
          bigcal_rcs_clstr_t8best(iclust) = bestmean
          if(minrms.le.b_timing_cut) then
            bigcal_rcs_clstr_good8(iclust) = .true.
          else
            bigcal_rcs_clstr_good8(iclust) = .false.
          endif

c     ! now repeat this calculation for the sum64 channels: 
          
          firstcombo = .true.

          do i1 = 1,bigcal_rcs_clstr_nhit64(iclust,1)
            currenthits64(1) = bigcal_rcs_clstr_tcell64(iclust,1,i1)
            if(bigcal_rcs_clstr_ncell64(iclust).gt.1)then
              do i2 = 1,bigcal_rcs_clstr_nhit64(iclust,2)
                currenthits64(2) = bigcal_rcs_clstr_tcell64(iclust,2,i2)
                if(bigcal_rcs_clstr_ncell64(iclust).gt.2) then
                  do i3 = 1,bigcal_rcs_clstr_nhit64(iclust,3)
                    currenthits64(3) = bigcal_rcs_clstr_tcell64(iclust,3,i3)
                    if(bigcal_rcs_clstr_ncell64(iclust).gt.3) then
                      do i4 = 1,bigcal_rcs_clstr_nhit64(iclust,4)
                        currenthits64(4) = 
     $                       bigcal_rcs_clstr_tcell64(iclust,4,i4)
                        if(bigcal_rcs_clstr_ncell64(iclust).gt.4) then
                          do i5=1,bigcal_rcs_clstr_nhit64(iclust,5)
                            currenthits64(5) = 
     $                           bigcal_rcs_clstr_tcell64(iclust,5,i5)
                            if(bigcal_rcs_clstr_ncell64(iclust).gt.5) then
                              do i6=1,bigcal_rcs_clstr_nhit64(iclust,6)
                                currenthits64(6) = 
     $                               bigcal_rcs_clstr_tcell64(iclust,6,i6)
                                
                                tmean = 0.
                                trms = 0.
                                tsum2 = 0.
                                do ihit=1,6
                                  tmean = tmean + currenthits64(ihit)
                                  tsum2 = tsum2 + currenthits64(ihit)**2
                                enddo
                                tmean = tmean / 6.
                                tsum2 = tsum2 / 6.
                                trms = sqrt(tsum2 - tmean**2)
                                if(firstcombo) then
                                  firstcombo=.false.
                                  minrms = trms
                                  bestmean = tmean
                                else if(trms.lt.minrms) then
                                  minrms = trms
                                  bestmean = tmean
                                endif
                              enddo
                            else
                              tmean = 0.
                              trms = 0.
                              tsum2 = 0.
                              do ihit=1,5
                                tmean = tmean + currenthits64(ihit)
                                tsum2 = tsum2 + currenthits64(ihit)**2
                              enddo
                              tmean = tmean / 5.
                              tsum2 = tsum2 / 5.
                              trms = sqrt(tsum2 - tmean**2)
                              if(firstcombo) then
                                firstcombo=.false.
                                minrms = trms
                                bestmean = tmean
                              else if(trms.lt.minrms) then
                                minrms = trms
                                bestmean = tmean
                              endif
                            endif
                          enddo
                        else
                          tmean = 0.
                          trms = 0.
                          tsum2 = 0.
                          do ihit=1,4
                            tmean = tmean + currenthits64(ihit)
                            tsum2 = tsum2 + currenthits64(ihit)**2
                          enddo
                          tmean = tmean / 4.
                          tsum2 = tsum2 / 4.
                          trms = sqrt(tsum2 - tmean**2)
                          if(firstcombo) then
                            firstcombo=.false.
                            minrms = trms
                            bestmean = tmean
                          else if(trms.lt.minrms) then
                            minrms = trms
                            bestmean = tmean
                          endif
                        endif
                      enddo
                    else
                      tmean = 0.
                      trms = 0.
                      tsum2 = 0.
                      do ihit=1,3
                        tmean = tmean + currenthits64(ihit)
                        tsum2 = tsum2 + currenthits64(ihit)**2
                      enddo
                      tmean = tmean / 3.
                      tsum2 = tsum2 / 3.
                      trms = sqrt(tsum2 - tmean**2)
                      if(firstcombo) then
                        firstcombo=.false.
                        minrms = trms
                        bestmean = tmean
                      else if(trms.lt.minrms) then
                        minrms = trms
                        bestmean = tmean
                      endif
                    endif
                  enddo
                else
                  tmean = 0.
                  trms = 0.
                  tsum2 = 0.
                  do ihit=1,2
                    tmean = tmean + currenthits64(ihit)
                    tsum2 = tsum2 + currenthits64(ihit)**2
                  enddo
                  tmean = tmean / 2.
                  tsum2 = tsum2 / 2.
                  trms = sqrt(tsum2 - tmean**2)
                  if(firstcombo) then
                    firstcombo=.false.
                    minrms = trms
                    bestmean = tmean
                  else if(trms.lt.minrms) then
                    minrms = trms
                    bestmean = tmean
                  endif
                endif
              enddo
            else
              tmean = 0.
              trms = 0.
              tsum2 = 0.
              do ihit=1,1
                tmean = tmean + currenthits64(ihit)
                tsum2 = tsum2 + currenthits64(ihit)**2
              enddo
              tmean = tmean / 1.
              tsum2 = tsum2 / 1.
              trms = sqrt(tsum2 - tmean**2)
              if(firstcombo) then
                firstcombo=.false.
                minrms = trms
                bestmean = tmean
              else if(trms.lt.minrms) then
                minrms = trms
                bestmean = tmean
              endif
            endif
          enddo
          
          bigcal_rcs_clstr_t64best(iclust) = bestmean
          if(minrms .le. b_timing_cut) then
            bigcal_rcs_clstr_good64(iclust) = .true.
          else
            bigcal_rcs_clstr_good64(iclust) = .false.
          endif
        enddo
      endif

c     **********************************************************************************
c     **********************************************************************************
c     **//////////////////////////////////////////////////////////////////////////////**
c     **/////////////////////////////////MID//////////////////////////////////////////**
c     **//////////////////////////////////////////////////////////////////////////////**
c     **********************************************************************************
c     **********************************************************************************      

      if(bigcal_mid_nclstr.gt.0) then
        do iclust=1,bigcal_mid_nclstr
          ixlo8 = 5
          ixhi8 = 0
          iylo8 = 57
          iyhi8 = 0
          
          ixlo64 = 3
          ixhi64 = 0
          iylo64 = 20
          iyhi64 = 0
          
          do icell=1,bigcal_mid_clstr_ncell(iclust)
            irow = bigcal_mid_clstr_iycell(iclust,icell)
            icol = bigcal_mid_clstr_ixcell(iclust,icell)
            irow8 = irow
            irow64 = (irow-1) / 3 + 1
            if(irow.le.32) then
              icol8 = (icol-1)/8 + 1
              icol64 = (icol-1)/16 + 1
            else
              if(icol.lt.16) then
                icol8 = (icol-1)/8 + 1
                icol64 = 1
              else
                icol8 = icol/8 + 1
                icol64 = 2
              endif
            endif ! all of irowcol864 are now set

            isum8 = icol8 + 4*(irow8-1)
            isum64 = icol64 + 2*(irow64-1)

            goodhit8 = bigcal_tdc_det_ngood(isum8).gt.0
            goodhit64 = bigcal_ttrig_det_ngood(isum64).gt.0

            if(irow8.gt.iyhi8.and.goodhit8) then
              iyhi8 = irow8
            endif
            
            if(irow8.lt.iylo8.and.goodhit8) then
              iylo8 = irow8
            endif

            if(icol8.gt.ixhi8.and.goodhit8) then
              ixhi8 = icol8
            endif
            
            if(icol8.lt.ixlo8.and.goodhit8) then
              ixlo8 = icol8
            endif

            if(icol64.gt.ixhi64.and.goodhit64) then
              ixhi64 = icol64
            endif

            if(icol64.lt.ixlo64.and.goodhit64) then
              ixlo64 = icol64
            endif
  

            if( mod(irow-1,3).eq.0 .and. irow.gt.1 ) then
              if(irow64-1.lt.iylo64.and.bigcal_ttrig_det_ngood(isum64-2) 
     $             .gt.0)then
                iylo64 = irow64 - 1
              endif
              if( irow64.gt.iyhi64.and.goodhit64) then
                iyhi64 = irow64
              endif
            else
              if( irow64.lt.iylo64.and.goodhit64) then
                iylo64 = irow64
              endif
              if( irow64.gt.iyhi64.and.goodhit64) then
                iyhi64 = irow64
              endif
            endif
          enddo

          nx8 = ixhi8 - ixlo8 + 1
          ny8 = iyhi8 - iylo8 + 1
          
          if(nx8.ge.1.and.nx8.le.2.and.ny8.ge.1.and.ny8.le.5)then ! bounds are sensible, continue:
            nhit8 = 0
            do icol=ixlo8,ixhi8
              do irow=iylo8,iyhi8
                nhit8 = nhit8 + 1
                isum8 = icol + 4*(irow-1)
                do ihit=1,bigcal_tdc_det_ngood(isum8)
                  bigcal_mid_clstr_tcell8(iclust,nhit8,ihit) = 
     $                 bigcal_tdc_good_det(isum8,ihit)
                enddo
                bigcal_mid_clstr_nhit8(iclust,nhit8) = 
     $               bigcal_tdc_det_ngood(isum8)
              enddo
            enddo
            bigcal_mid_clstr_ncell8(iclust) = nhit8
          endif
          nx64 = ixhi64 - ixlo64 + 1
          ny64 = iyhi64 - iylo64 + 1
          if(nx64.ge.1.and.nx64.le.2.and.ny64.ge.1.and.ny64.le.3)then ! bounds are sensible, continue:
            nhit64 = 0
            do icol=ixlo64,ixhi64
              do irow=iylo64,iyhi64
                nhit64 = nhit64 + 1
                isum64 = icol + 2*(irow-1)
                do ihit=1,bigcal_ttrig_det_ngood(isum64)
                  bigcal_mid_clstr_tcell64(iclust,nhit64,ihit) = 
     $                 bigcal_ttrig_good_det(isum64,ihit)
                enddo
                bigcal_mid_clstr_nhit64(iclust,nhit64) = 
     $               bigcal_ttrig_det_ngood(isum64)
              enddo
            enddo
            bigcal_mid_clstr_ncell64(iclust) = nhit64
          endif
c     subsequent analysis to calculate best time for the sums of 8 and sums of 64 - do tomorrow
c     fill array of all possible hit combinations:
c     for sum8: a maximum of 10 sum8 cells with a hit: 

          do ihit=1,10
            currenthits8(ihit) = 0.
          enddo
          
          do ihit=1,6
            currenthits64(ihit) = 0.
          enddo

          bestrms = b_timing_cut
          firstcombo = .true.

          do i1 = 1,bigcal_mid_clstr_nhit8(iclust,1)
            currenthits8(1) = bigcal_mid_clstr_tcell8(iclust,1,i1)
            if(bigcal_mid_clstr_ncell8(iclust).gt.1)then
              do i2 = 1,bigcal_mid_clstr_nhit8(iclust,2)
                currenthits8(2) = bigcal_mid_clstr_tcell8(iclust,2,i2)
                if(bigcal_mid_clstr_ncell8(iclust).gt.2) then
                  do i3 = 1,bigcal_mid_clstr_nhit8(iclust,3)
                    currenthits8(3) = bigcal_mid_clstr_tcell8(iclust,3,i3)
                    if(bigcal_mid_clstr_ncell8(iclust).gt.3) then
                      do i4 = 1,bigcal_mid_clstr_nhit8(iclust,4)
                        currenthits8(4) = 
     $                       bigcal_mid_clstr_tcell8(iclust,4,i4)
                        if(bigcal_mid_clstr_ncell8(iclust).gt.4) then
                          do i5=1,bigcal_mid_clstr_nhit8(iclust,5)
                            currenthits8(5) = 
     $                           bigcal_mid_clstr_tcell8(iclust,5,i5)
                            if(bigcal_mid_clstr_ncell8(iclust).gt.5) then
                              do i6=1,bigcal_mid_clstr_nhit8(iclust,6)
                                currenthits8(6) = 
     $                               bigcal_mid_clstr_tcell8(iclust,6,i6)
                                if(bigcal_mid_clstr_ncell8(iclust).gt.6) then
                                  do i7=1,bigcal_mid_clstr_nhit8(iclust,7)
                                    currenthits8(7) = 
     $                                   bigcal_mid_clstr_tcell8(iclust,7,i7)
                                    if(bigcal_mid_clstr_ncell8(iclust).gt.7)then
                                      do i8=1,bigcal_mid_clstr_nhit8(iclust,8)
                                        currenthits8(8) = 
     $                                       bigcal_mid_clstr_tcell8(iclust,8,i8)
                                        if(bigcal_mid_clstr_ncell8(iclust).gt.8) then
                                          do i9=1,bigcal_mid_clstr_nhit8(iclust,9)
                                            currenthits8(9) = 
     $                                           bigcal_mid_clstr_tcell8(iclust,9,i9)
                                            if(bigcal_mid_clstr_ncell8(iclust).gt.9)
     $                                           then
                                              do i10=1,bigcal_mid_clstr_nhit8(iclust,10)
                                                currenthits8(10) =
     $                                               bigcal_mid_clstr_tcell8(iclust,10,i10)
                                                
                                                tmean = 0.
                                                trms = 0.
                                                tsum2 = 0.
                                                do ihit=1,10
                                                  tmean = tmean + currenthits8(ihit)
                                                  tsum2 = tsum2 + currenthits8(ihit)**2
                                                enddo
                                                tmean = 0.1 * tmean
                                                tsum2 = 0.1 * tsum2
                                                trms = sqrt(tsum2 - tmean**2)
                                                if(firstcombo) then
                                                  firstcombo=.false.
                                                  minrms = trms
                                                  bestmean = tmean
                                                else if(trms.lt.minrms) then
                                                  minrms = trms
                                                  bestmean = tmean
                                                endif
                                              enddo
                                            else
                                              tmean = 0.
                                              trms = 0.
                                              tsum2 = 0.
                                              do ihit=1,9
                                                tmean = tmean + currenthits8(ihit)
                                                tsum2 = tsum2 + currenthits8(ihit)**2
                                              enddo
                                              tmean = tmean / 9.
                                              tsum2 = tsum2 / 9.
                                              trms = sqrt(tsum2 - tmean**2)
                                              if(firstcombo) then
                                                firstcombo=.false.
                                                minrms = trms
                                                bestmean = tmean
                                              else if(trms.lt.minrms) then
                                                minrms = trms
                                                bestmean = tmean
                                              endif
                                            endif
                                          enddo
                                        else
                                          tmean = 0.
                                          trms = 0.
                                          tsum2 = 0.
                                          do ihit=1,8
                                            tmean = tmean + currenthits8(ihit)
                                            tsum2 = tsum2 + currenthits8(ihit)**2
                                          enddo
                                          tmean = tmean / 8.
                                          tsum2 = tsum2 / 8.
                                          trms = sqrt(tsum2 - tmean**2)
                                          if(firstcombo) then
                                            firstcombo=.false.
                                            minrms = trms
                                            bestmean = tmean
                                          else if(trms.lt.minrms) then
                                            minrms = trms
                                            bestmean = tmean
                                          endif
                                        endif
                                      enddo
                                    else
                                      tmean = 0.
                                      trms = 0.
                                      tsum2 = 0.
                                      do ihit=1,7
                                        tmean = tmean + currenthits8(ihit)
                                        tsum2 = tsum2 + currenthits8(ihit)**2
                                      enddo
                                      tmean = tmean / 7.
                                      tsum2 = tsum2 / 7.
                                      trms = sqrt(tsum2 - tmean**2)
                                      if(firstcombo) then
                                        firstcombo=.false.
                                        minrms = trms
                                        bestmean = tmean
                                      else if(trms.lt.minrms) then
                                        minrms = trms
                                        bestmean = tmean
                                      endif
                                    endif
                                  enddo
                                else
                                  tmean = 0.
                                  trms = 0.
                                  tsum2 = 0.
                                  do ihit=1,6
                                    tmean = tmean + currenthits8(ihit)
                                    tsum2 = tsum2 + currenthits8(ihit)**2
                                  enddo
                                  tmean = tmean / 6.
                                  tsum2 = tsum2 / 6.
                                  trms = sqrt(tsum2 - tmean**2)
                                  if(firstcombo) then
                                    firstcombo=.false.
                                    minrms = trms
                                    bestmean = tmean
                                  else if(trms.lt.minrms) then
                                    minrms = trms
                                    bestmean = tmean
                                  endif
                                endif
                              enddo
                            else
                              tmean = 0.
                              trms = 0.
                              tsum2 = 0.
                              do ihit=1,5
                                tmean = tmean + currenthits8(ihit)
                                tsum2 = tsum2 + currenthits8(ihit)**2
                              enddo
                              tmean = tmean / 5.
                              tsum2 = tsum2 / 5.
                              trms = sqrt(tsum2 - tmean**2)
                              if(firstcombo) then
                                firstcombo=.false.
                                minrms = trms
                                bestmean = tmean
                              else if(trms.lt.minrms) then
                                minrms = trms
                                bestmean = tmean
                              endif
                            endif
                          enddo
                        else
                          tmean = 0.
                          trms = 0.
                          tsum2 = 0.
                          do ihit=1,4
                            tmean = tmean + currenthits8(ihit)
                            tsum2 = tsum2 + currenthits8(ihit)**2
                          enddo
                          tmean = tmean / 4.
                          tsum2 = tsum2 / 4.
                          trms = sqrt(tsum2 - tmean**2)
                          if(firstcombo) then
                            firstcombo=.false.
                            minrms = trms
                            bestmean = tmean
                          else if(trms.lt.minrms) then
                            minrms = trms
                            bestmean = tmean
                          endif
                        endif
                      enddo
                    else
                      tmean = 0.
                      trms = 0.
                      tsum2 = 0.
                      do ihit=1,3
                        tmean = tmean + currenthits8(ihit)
                        tsum2 = tsum2 + currenthits8(ihit)**2
                      enddo
                      tmean = tmean / 3.
                      tsum2 = tsum2 / 3.
                      trms = sqrt(tsum2 - tmean**2)
                      if(firstcombo) then
                        firstcombo=.false.
                        minrms = trms
                        bestmean = tmean
                      else if(trms.lt.minrms) then
                        minrms = trms
                        bestmean = tmean
                      endif
                    endif
                  enddo
                else
                  tmean = 0.
                  trms = 0.
                  tsum2 = 0.
                  do ihit=1,2
                    tmean = tmean + currenthits8(ihit)
                    tsum2 = tsum2 + currenthits8(ihit)**2
                  enddo
                  tmean = tmean / 2.
                  tsum2 = tsum2 / 2.
                  trms = sqrt(tsum2 - tmean**2)
                  if(firstcombo) then
                    firstcombo=.false.
                    minrms = trms
                    bestmean = tmean
                  else if(trms.lt.minrms) then
                    minrms = trms
                    bestmean = tmean
                  endif
                endif
              enddo
            else ! only one sum8 cell associated with this cluster, use first hit
              tmean = 0.
              trms = 0.
              tsum2 = 0.
              do ihit=1,1
                tmean = tmean + currenthits8(ihit)
                tsum2 = tsum2 + currenthits8(ihit)**2
              enddo
              tmean = tmean / 1.
              tsum2 = tsum2 / 1.
              trms = sqrt(tsum2 - tmean**2)
              if(firstcombo) then
                firstcombo=.false.
                minrms = trms
                bestmean = tmean
              else if(trms.lt.minrms) then
                minrms = trms
                bestmean = tmean
              endif
            endif
          enddo
          
c     ! at this point we have calculated the average time for the "best" 
c     ! combination of tdc hits in all sum8 channels associated with this cell.
c     ! set "best" time and logical whether the rms of the best combination is 
c     ! smaller than b_timing_cut:
          
          bigcal_mid_clstr_t8best(iclust) = bestmean
          if(minrms.le.b_timing_cut) then
            bigcal_mid_clstr_good8(iclust) = .true.
          else
            bigcal_mid_clstr_good8(iclust) = .false.
          endif

c     ! now repeat this calculation for the sum64 channels: 
          
          firstcombo = .true.

          do i1 = 1,bigcal_mid_clstr_nhit64(iclust,1)
            currenthits64(1) = bigcal_mid_clstr_tcell64(iclust,1,i1)
            if(bigcal_mid_clstr_ncell64(iclust).gt.1)then
              do i2 = 1,bigcal_mid_clstr_nhit64(iclust,2)
                currenthits64(2) = bigcal_mid_clstr_tcell64(iclust,2,i2)
                if(bigcal_mid_clstr_ncell64(iclust).gt.2) then
                  do i3 = 1,bigcal_mid_clstr_nhit64(iclust,3)
                    currenthits64(3) = bigcal_mid_clstr_tcell64(iclust,3,i3)
                    if(bigcal_mid_clstr_ncell64(iclust).gt.3) then
                      do i4 = 1,bigcal_mid_clstr_nhit64(iclust,4)
                        currenthits64(4) = 
     $                       bigcal_mid_clstr_tcell64(iclust,4,i4)
                        if(bigcal_mid_clstr_ncell64(iclust).gt.4) then
                          do i5=1,bigcal_mid_clstr_nhit64(iclust,5)
                            currenthits64(5) = 
     $                           bigcal_mid_clstr_tcell64(iclust,5,i5)
                            if(bigcal_mid_clstr_ncell64(iclust).gt.5) then
                              do i6=1,bigcal_mid_clstr_nhit64(iclust,6)
                                currenthits64(6) = 
     $                               bigcal_mid_clstr_tcell64(iclust,6,i6)
                                
                                tmean = 0.
                                trms = 0.
                                tsum2 = 0.
                                do ihit=1,6
                                  tmean = tmean + currenthits64(ihit)
                                  tsum2 = tsum2 + currenthits64(ihit)**2
                                enddo
                                tmean = tmean / 6.
                                tsum2 = tsum2 / 6.
                                trms = sqrt(tsum2 - tmean**2)
                                if(firstcombo) then
                                  firstcombo=.false.
                                  minrms = trms
                                  bestmean = tmean
                                else if(trms.lt.minrms) then
                                  minrms = trms
                                  bestmean = tmean
                                endif
                              enddo
                            else
                              tmean = 0.
                              trms = 0.
                              tsum2 = 0.
                              do ihit=1,5
                                tmean = tmean + currenthits64(ihit)
                                tsum2 = tsum2 + currenthits64(ihit)**2
                              enddo
                              tmean = tmean / 5.
                              tsum2 = tsum2 / 5.
                              trms = sqrt(tsum2 - tmean**2)
                              if(firstcombo) then
                                firstcombo=.false.
                                minrms = trms
                                bestmean = tmean
                              else if(trms.lt.minrms) then
                                minrms = trms
                                bestmean = tmean
                              endif
                            endif
                          enddo
                        else
                          tmean = 0.
                          trms = 0.
                          tsum2 = 0.
                          do ihit=1,4
                            tmean = tmean + currenthits64(ihit)
                            tsum2 = tsum2 + currenthits64(ihit)**2
                          enddo
                          tmean = tmean / 4.
                          tsum2 = tsum2 / 4.
                          trms = sqrt(tsum2 - tmean**2)
                          if(firstcombo) then
                            firstcombo=.false.
                            minrms = trms
                            bestmean = tmean
                          else if(trms.lt.minrms) then
                            minrms = trms
                            bestmean = tmean
                          endif
                        endif
                      enddo
                    else
                      tmean = 0.
                      trms = 0.
                      tsum2 = 0.
                      do ihit=1,3
                        tmean = tmean + currenthits64(ihit)
                        tsum2 = tsum2 + currenthits64(ihit)**2
                      enddo
                      tmean = tmean / 3.
                      tsum2 = tsum2 / 3.
                      trms = sqrt(tsum2 - tmean**2)
                      if(firstcombo) then
                        firstcombo=.false.
                        minrms = trms
                        bestmean = tmean
                      else if(trms.lt.minrms) then
                        minrms = trms
                        bestmean = tmean
                      endif
                    endif
                  enddo
                else
                  tmean = 0.
                  trms = 0.
                  tsum2 = 0.
                  do ihit=1,2
                    tmean = tmean + currenthits64(ihit)
                    tsum2 = tsum2 + currenthits64(ihit)**2
                  enddo
                  tmean = tmean / 2.
                  tsum2 = tsum2 / 2.
                  trms = sqrt(tsum2 - tmean**2)
                  if(firstcombo) then
                    firstcombo=.false.
                    minrms = trms
                    bestmean = tmean
                  else if(trms.lt.minrms) then
                    minrms = trms
                    bestmean = tmean
                  endif
                endif
              enddo
            else
              tmean = 0.
              trms = 0.
              tsum2 = 0.
              do ihit=1,1
                tmean = tmean + currenthits64(ihit)
                tsum2 = tsum2 + currenthits64(ihit)**2
              enddo
              tmean = tmean / 1.
              tsum2 = tsum2 / 1.
              trms = sqrt(tsum2 - tmean**2)
              if(firstcombo) then
                firstcombo=.false.
                minrms = trms
                bestmean = tmean
              else if(trms.lt.minrms) then
                minrms = trms
                bestmean = tmean
              endif
            endif
          enddo
          
          bigcal_mid_clstr_t64best(iclust) = bestmean
          if(minrms .le. b_timing_cut) then
            bigcal_mid_clstr_good64(iclust) = .true.
          else
            bigcal_mid_clstr_good64(iclust) = .false.
          endif
        enddo
      endif

      return 
      end
