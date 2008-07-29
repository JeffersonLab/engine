      subroutine h_prune_stubs(ABORT,err)

      implicit none
      save
    
      include 'hms_data_structures.cmn'
      include 'hms_tracking.cmn'
      include 'hms_geometry.cmn'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'
      
      integer isp,jsp,ngood(2) ! keep at least 1 stub each from DC1 and DC2
      logical keep(hmax_space_points)

      logical firsttry

      integer ichbr,jchbr

      real*4 sigmax0(2),sigmay0(2),sigmaxp0(2)
      real*4 sigmax,sigmay,sigmaxp
      real*4 xdiffscin,ydiffscin
      real*4 dxstub,dystub,dxpstub
      real*4 chi2,minchi2
      integer best1,best2

      integer*4 h_chamnum
      external h_chamnum

      logical abort
      character*(*) err

      abort=.false.
      err=' '
      
      do isp=1,hnspace_points_tot
         keep(isp) = .true.
      enddo

c     initialize resolution parameters for stub "chi2" selection

      sigmaxp0(1) = hdc_sigma(1) / (hdc_zpos(6)-hdc_zpos(1))
      sigmax0(1)  = sqrt( (hdc_sigma(1) )**2 + (sigmaxp*hdc_1_zpos)**2 )
      sigmay0(1)  = sqrt( (hdc_sigma(2) )**2 + (.04*hdc_1_zpos)**2 )

      sigmaxp0(2) = hdc_sigma(7) / (hdc_zpos(12)-hdc_zpos(7))
      sigmax0(2)  = sqrt( (hdc_sigma(7) )**2 + (sigmaxp*hdc_2_zpos)**2 )
      sigmay0(2)  = sqrt( (hdc_sigma(8) )**2 + (.04*hdc_2_zpos)**2 )

      sigmax = sqrt( (sigmax0(1))**2 + (sigmax0(2))**2)
      sigmay = sqrt( (sigmay0(1))**2 + (sigmay0(2))**2)
      sigmaxp = sqrt( (sigmaxp0(1))**2 + (sigmaxp0(2))**2 )

c     if user has defined something reasonable, go with it

      if( h_stub_sigmax .ge. .01 * sigmax .or. h_stub_sigmax .le. 100.*sigmax )
     $     sigmax = h_stub_sigmax
      if( h_stub_sigmay .ge. .01 * sigmay .or. h_stub_sigmay .le. 100.*sigmay )
     $     sigmay = h_stub_sigmay
      if( h_stub_sigmaxp .ge. .01 * sigmaxp .or. h_stub_sigmaxp .le. 100.*sigmaxp)
     $     sigmaxp = h_stub_sigmaxp
      
      

c     The purpose of this routine is to trim down the number of space points used as candidates for 
c     tracks

c     first prune test: prune stub x coordinate (keep at least one space point per chamber!)

      

      if(h_stub_prune_flags(1).ne.0) then

         ngood(1) = 0
         ngood(2) = 0

         do isp=1,hnspace_points_tot
            ichbr = h_chamnum(isp)

            if(keep(isp).and.abs(hbeststub(isp,1)).le.
     $           h_stub_prune_x(ichbr)) then
               ngood(ichbr) = ngood(ichbr) + 1
            endif
         enddo
         
         do ichbr=1,2
            if(ngood(ichbr).gt.0) then
               do isp=1,hnspace_points_tot
                  jchbr = h_chamnum(isp)
                  if(jchbr.eq.ichbr.and.abs(hbeststub(isp,1)).gt.
     $                 h_stub_prune_x(ichbr)) then
                     keep(isp) = .false.
                  endif
               enddo
            endif
         enddo    
      endif

c     second prune test: stub y coordinate (keep at least one space point per chamber!)

      if(h_stub_prune_flags(2).ne.0) then
         
         ngood(1) = 0
         ngood(2) = 0
         
         do isp=1,hnspace_points_tot
            ichbr = h_chamnum(isp)

            if(keep(isp).and.abs(hbeststub(isp,2)).le.
     $           h_stub_prune_y(ichbr))then
               ngood(ichbr) = ngood(ichbr) + 1
            endif
         enddo
         
         do ichbr=1,2
            if(ngood(ichbr).gt.0) then
               do isp=1,hnspace_points_tot
                  jchbr = h_chamnum(isp)
                  if(jchbr.eq.ichbr.and.abs(hbeststub(isp,2)).gt.
     $                 h_stub_prune_y(ichbr)) then
                     keep(isp) = .false.
                  endif
               enddo
            endif
         enddo
      endif

c     third prune test: stub x' (keep at least one space point per chamber!)
      
      if(h_stub_prune_flags(3).ne.0) then
         ngood(1) = 0
         ngood(2) = 0
         
         do isp=1,hnspace_points_tot
            ichbr = h_chamnum(isp)
            if(keep(isp).and.abs(hbeststub(isp,3)).le.
     $           h_stub_prune_xp(ichbr)) then
               ngood(ichbr) = ngood(ichbr) + 1
            endif
         enddo
         
         do ichbr=1,2
            if(ngood(ichbr).gt.0) then
               do isp=1,hnspace_points_tot
                  jchbr = h_chamnum(isp)
                  if(jchbr.eq.ichbr.and.abs(hbeststub(isp,3)).gt.
     $                 h_stub_prune_xp(ichbr)) then
                     keep(isp) = .false.
                  endif
               enddo
            endif
         enddo
      endif

c     fourth prune test: stub x - xscin (keep at least one space point per chamber!)
c     This test is highly GEp-III specialized! (no S2, S0, etc)

      if(h_stub_prune_flags(4).ne.0.and.htwo_good_starttime_hits) then
         ngood(1) = 0
         ngood(2) = 0
         
         do isp=1,hnspace_points_tot
            ichbr = h_chamnum(isp)

            xdiffscin = hbeststub(isp,1) + 
     $           hbeststub(isp,3)*hS1_crude_ztrack(1) - 
     $           hS1_crude_xtrack

            if(keep(isp).and.abs(xdiffscin).le.
     $           h_stub_prune_dxscin(ichbr)) then
               ngood(ichbr) = ngood(ichbr) + 1
            endif
         enddo
         
         do ichbr=1,2
            if(ngood(ichbr).gt.0) then
               do isp=1,hnspace_points_tot
                  jchbr = h_chamnum(isp)

                  xdiffscin = hbeststub(isp,1) + 
     $                 hbeststub(isp,3)*hS1_crude_ztrack(1) - 
     $                 hS1_crude_xtrack

                  if(jchbr.eq.ichbr.and.abs(xdiffscin).gt.
     $                 h_stub_prune_dxscin(ichbr)) then
                     keep(isp) = .false.
                  endif
               enddo
            endif
         enddo
      endif

c     fifth prune test: stub y - yscin ( keep at least one space point per chamber!)
      
      if(h_stub_prune_flags(5).ne.0.and.htwo_good_starttime_hits) then
         ngood(1) = 0
         ngood(2) = 0
         
         do isp=1,hnspace_points_tot
            ichbr = h_chamnum(isp)
            ydiffscin = hbeststub(isp,2) + 
     $           hbeststub(isp,4)*hS1_crude_ztrack(2) - 
     $           hS1_crude_ytrack
            if(keep(isp).and.abs(ydiffscin).le.
     $           h_stub_prune_dyscin(ichbr)) then
               ngood(ichbr) = ngood(ichbr) + 1
            endif
         enddo
         
         do ichbr=1,2
            if(ngood(ichbr).gt.0) then
               do isp=1,hnspace_points_tot
                  jchbr = h_chamnum(isp)
                  ydiffscin = hbeststub(isp,2) + 
     $                 hbeststub(isp,4)*hS1_crude_ztrack(2) - 
     $                 hS1_crude_ytrack
                  if(jchbr.eq.ichbr.and.abs(ydiffscin).gt.
     $                 h_stub_prune_dyscin(ichbr)) then
                     keep(isp) = .false.
                  endif
               enddo
            endif
         enddo
      endif
      
c     sixth prune test: stub chi2 (keep at least one space point per chamber!)

      if(h_stub_prune_flags(6).ne.0) then
         ngood(1) = 0
         ngood(2) = 0
         
         do isp=1,hnspace_points_tot
            ichbr = h_chamnum(isp)
            if(keep(isp).and.abs(h_stub_chi2perdf(isp)).le.
     $           h_stub_prune_chi2) then
               ngood(ichbr) = ngood(ichbr) + 1
            endif
         enddo
         
         do ichbr=1,2
            if(ngood(ichbr).gt.0) then
               do isp=1,hnspace_points_tot
                  jchbr = h_chamnum(isp)
                  if(jchbr.eq.ichbr.and.abs(h_stub_chi2perdf(isp)).gt.
     $                 h_stub_prune_chi2) then
                     keep(isp) = .false.
                  endif
               enddo
            endif
         enddo
      endif

c     seventh prune test: pick one and only one space point per chamber
c     based on the two stubs which best agree with each other.
c     this will result in one and only one track in the second call to 
c     h_link_stubs

      best1 = 0
      best2 = 0

      if(h_stub_prune_flags(7).ne.0) then

         firsttry = .true.

         do isp=1,hnspace_points_tot-1
            do jsp=isp+1,hnspace_points_tot
               if(h_chamnum(isp).ne.h_chamnum(jsp).and.keep(isp).and.
     $              keep(jsp)) then
                  dxstub = hbeststub(isp,1)-hbeststub(jsp,1)
                  dystub = hbeststub(isp,2)-hbeststub(jsp,2)
                  dxpstub = hbeststub(isp,3)-hbeststub(jsp,3)
                  
                  chi2 = 0. 
                  chi2 = chi2 + (dxstub/sigmax)**2
                  chi2 = chi2 + (dystub/sigmay)**2
                  chi2 = chi2 + (dxpstub/sigmaxp)**2
                  
                  if(chi2.lt.minchi2.or.firsttry) then
                     minchi2 = chi2
                     firsttry = .false.
                     if(h_chamnum(isp).eq.1) then
                        best1 = isp
                        best2 = jsp
                     else
                        best1 = jsp
                        best2 = isp
                     endif
                  endif
               endif
            enddo
         enddo
      endif
            
c     now set the overall "keep" flag for each space point:
      
      do isp=1,hnspace_points_tot
         h_keep_stub(isp) = .false.
         
         if(keep(isp)) then
            if((best1.eq.0.and.best2.eq.0).or.isp.eq.best1.or.isp.eq.best2)
     $           h_keep_stub(isp) = .true.
         endif
      enddo

      return
      end
