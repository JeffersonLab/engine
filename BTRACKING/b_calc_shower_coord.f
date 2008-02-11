      subroutine b_calc_shower_coord(ABORT,err)

      implicit none
      save
      
      character*19 here
      parameter(here='b_calc_shower_coord')

      logical ABORT
      character*(*) err
      
      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_geometry.cmn'
      include 'bigcal_shower_parms.cmn'
      include 'bigcal_bypass_switches.cmn'

      integer i,j,irow,icol,icell,ibin,xsector,ysector,foundbin
      real xmom,ymom,xcenter,ycenter,xdiff,ydiff
      real xpar(6),ypar(6)
      real mlo,mhi,binwidth,frac,frachi,fraclo

c     all clusters are already sorted in order of decreasing amplitude, so cell 1 is the maximum

      do i=1,bigcal_all_nclstr

         if(b_recon_using_map.ne.0) then

            xcenter = bigcal_all_clstr_xcell(i,1)
            ycenter = bigcal_all_clstr_ycell(i,1)
            
            xmom = bigcal_all_clstr_xmom(i) 
            ymom = bigcal_all_clstr_ymom(i)
            
            irow = bigcal_all_clstr_iycell(i,1)
            icol = bigcal_all_clstr_ixcell(i,1)
            
            if(irow.le.32) then
               xmom = xmom / bigcal_prot_size_x
               ymom = ymom / bigcal_prot_size_y

c	       write(*,*) 'xmom,ymom=',xmom,ymom  		

               xsector = (icol-1)/8 + 1
               ysector = (irow-1)/8 + 1

c	       write(*,*) 'xsector,ysector=',xsector,ysector

               !first do x
               binwidth = (bigcal_pxmap_mmax(xsector) - bigcal_pxmap_mmin(xsector)) /
     $              float(bigcal_pxmap_nbin(xsector))
               mlo = bigcal_pxmap_mmin(xsector)

c	       write(*,*) 'binwidth=',binwidth

               if(xmom.lt.bigcal_pxmap_mmin(xsector).or.xmom.gt.
     $              bigcal_pxmap_mmax(xsector)) then
                  xdiff = xmom * bigcal_prot_size_x
               endif

               do ibin=1,bigcal_pxmap_nbin(xsector)
                  mhi = mlo + binwidth

                  if(mlo.le.xmom.and.xmom.le.mhi) then ! cluster is in this bin
c                     write(*,*) 'mlo,mhi,xmom=',mlo,mhi,xmom
		     if(ibin.gt.1) then
                        fraclo = bigcal_pxmap_frac(xsector,ibin-1)
                     else
                        fraclo = 0.
                     endif
                     frachi = bigcal_pxmap_frac(xsector,ibin)
c     linearly interpolate frac within this bin
                     frac = fraclo + (frachi - fraclo)/(mhi - mlo) * (xmom - mlo)
                     
c		     write(*,*) 'fraclo,frachi,frac=',fraclo,frachi,frac
                     xdiff = (-.5 + frac)*bigcal_prot_size_x
c                     write(*,*) 'xdiff=',xdiff
                  endif
                  mlo = mhi
               enddo
               
               !then do y
               binwidth = (bigcal_pymap_mmax(ysector) - bigcal_pymap_mmin(ysector)) /
     $              float(bigcal_pymap_nbin(ysector))
               mlo = bigcal_pymap_mmin(ysector)

               if(ymom.lt.bigcal_pymap_mmin(ysector).or.ymom.gt.
     $              bigcal_pymap_mmax(ysector)) then
                  ydiff = ymom * bigcal_prot_size_y
               endif

               do ibin=1,bigcal_pymap_nbin(ysector)
                  mhi = mlo + binwidth
                  
                  if(mlo.le.ymom.and.ymom.le.mhi) then ! cluster is in this bin
                     if(ibin.gt.1) then
                        fraclo = bigcal_pymap_frac(ysector,ibin-1)
                     else
                        fraclo = 0.
                     endif
                     frachi = bigcal_pymap_frac(ysector,ibin)
c     linearly interpolate frac within this bin
                     frac = fraclo + (frachi - fraclo)/(mhi - mlo) * (ymom - mlo)
                     
                     ydiff = (-.5 + frac)*bigcal_prot_size_y
                     
                  endif
                  mlo = mhi
               enddo 
            else

               xmom = xmom / bigcal_rcs_size_x
               ymom = ymom / bigcal_rcs_size_y

               xsector = (icol-1)/8 + 1
               ysector = (irow-33)/8 + 1
               
               !first do x
               binwidth = (bigcal_rxmap_mmax(xsector) - bigcal_rxmap_mmin(xsector)) /
     $              float(bigcal_rxmap_nbin(xsector))
               mlo = bigcal_rxmap_mmin(xsector)

               if(xmom.lt.bigcal_rxmap_mmin(xsector).or.xmom.gt.
     $              bigcal_rxmap_mmax(xsector)) then
                  xdiff = xmom * bigcal_rcs_size_x
               endif

               do ibin=1,bigcal_rxmap_nbin(xsector)
                  mhi = mlo + binwidth
                  
                  if(mlo.le.xmom.and.xmom.le.mhi) then ! cluster is in this bin
                     if(ibin.gt.1) then
                        fraclo = bigcal_rxmap_frac(xsector,ibin-1)
                     else
                        fraclo = 0.
                     endif
                     frachi = bigcal_rxmap_frac(xsector,ibin)
c     linearly interpolate frac within this bin
                     frac = fraclo + (frachi - fraclo)/(mhi - mlo) * (xmom - mlo)
                     
                     xdiff = (-.5 + frac)*bigcal_rcs_size_x
                     
                  endif
                  mlo = mhi
               enddo
               
               !then do y
               binwidth = (bigcal_rymap_mmax(ysector) - bigcal_rymap_mmin(ysector)) /
     $              float(bigcal_rymap_nbin(ysector))
               mlo = bigcal_rymap_mmin(ysector)

               if(ymom.lt.bigcal_rymap_mmin(ysector).or.ymom.gt.
     $              bigcal_rymap_mmax(ysector)) then
                  ydiff = ymom * bigcal_rcs_size_y
               endif

               do ibin=1,bigcal_rymap_nbin(ysector)
                  mhi = mlo + binwidth
                  
                  if(mlo.le.ymom.and.ymom.le.mhi) then ! cluster is in this bin
                     if(ibin.gt.1) then
                        fraclo = bigcal_rymap_frac(ysector,ibin-1)
                     else
                        fraclo = 0.
                     endif
                     frachi = bigcal_rymap_frac(ysector,ibin)
c     linearly interpolate frac within this bin
                     frac = fraclo + (frachi - fraclo)/(mhi - mlo) * (ymom - mlo)
                     
                     ydiff = (-.5 + frac)*bigcal_rcs_size_y
                     
                  endif
                  mlo = mhi
               enddo 

            endif

            bigcal_all_clstr_x(i) = xcenter + xdiff
            bigcal_all_clstr_y(i) = ycenter + ydiff

         else

            xcenter = bigcal_all_clstr_xcell(i,1)
            ycenter = bigcal_all_clstr_ycell(i,1)
            
            xmom = bigcal_all_clstr_xmom(i)
            ymom = bigcal_all_clstr_ymom(i)
            
            irow = bigcal_all_clstr_iycell(i,1)
            icol = bigcal_all_clstr_ixcell(i,1)
            
            if(irow.eq.1) irow = 2
            if(irow.eq.56) irow = 55
            
            if(irow.le.32) then
               if(icol.eq.1) icol = 2
               if(icol.eq.32) icol = 31
               
               do j=1,6
                  xpar(j) = bigcal_prot_xpar(icol,j)
                  ypar(j) = bigcal_prot_ypar(irow,j)
               enddo
            else 
               if(icol.eq.1) icol = 2
               if(icol.eq.30) icol = 29
               
               do j=1,6
                  xpar(j) = bigcal_rcs_xpar(icol,j)
                  ypar(j) = bigcal_rcs_ypar(irow-32,j)
               enddo
            endif
            
            xdiff = xpar(1) * atan(xpar(2)*xmom**4 + xpar(3)*xmom**3 + 
     $           xpar(4)*xmom**2 + xpar(5)*xmom + xpar(6))
            ydiff = ypar(1) * atan(ypar(2)*ymom**4 + ypar(3)*ymom**3 + 
     $           ypar(4)*ymom**2 + ypar(5)*ymom + ypar(6))
            
            bigcal_all_clstr_x(i) = xcenter + xdiff
            bigcal_all_clstr_y(i) = ycenter + ydiff
         endif
      enddo

      return 
      end
