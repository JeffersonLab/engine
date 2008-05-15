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

      integer i,j,irow,icol,icell,ibin,xsector,ysector,foundbin,section
      real xmom,ymom,xcenter,ycenter,xdiff,ydiff,xshift,yshift
      real xpar(6),ypar(6)
      real mlo,mhi,binwidth,frac,frachi,fraclo,sizex,sizey

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

               sizex = bigcal_prot_size_x
               sizey = bigcal_prot_size_y

            else
               xmom = xmom / bigcal_rcs_size_x
               ymom = ymom / bigcal_rcs_size_y

               sizex = bigcal_rcs_size_x
               sizey = bigcal_rcs_size_y

            endif

c	       write(*,*) 'xmom,ymom=',xmom,ymom  		

            xsector = (icol-1)/8 + 1
            ysector = (irow-1)/8 + 1

            section = xsector + 4*(ysector-1)
            
c     write(*,*) 'xsector,ysector=',xsector,ysector
            
               !first do x
            binwidth = (bigcal_xmap_mmax(section) - bigcal_xmap_mmin(section)) /
     $           float(bigcal_xmap_nbin(section))
            mlo = bigcal_xmap_mmin(section)
            
c     write(*,*) 'binwidth=',binwidth
            
            if(xmom.lt.bigcal_xmap_mmin(section).or.xmom.gt.
     $           bigcal_xmap_mmax(section)) then
               xdiff = xmom * sizex
            endif
            
            do ibin=1,bigcal_xmap_nbin(section)
               mhi = mlo + binwidth
               
               if(mlo.le.xmom.and.xmom.le.mhi) then ! cluster is in this bin
c     write(*,*) 'mlo,mhi,xmom=',mlo,mhi,xmom
                  if(ibin.gt.1) then
                     fraclo = bigcal_xmap_xfrac(section,ibin-1)
                  else
                     fraclo = 0.
                  endif
                  frachi = bigcal_xmap_xfrac(section,ibin)
c     linearly interpolate frac within this bin
                  frac = fraclo + (frachi - fraclo)/(mhi - mlo) * (xmom - mlo)
                  
c     write(*,*) 'fraclo,frachi,frac=',fraclo,frachi,frac
                  xdiff = (-.5 + frac)*sizex
c     write(*,*) 'xdiff=',xdiff

c     jump out of the loop once we find the right bin
                  goto 101
               endif
               mlo = mhi
            enddo
            
 101        continue
                                !then do y
            binwidth = (bigcal_ymap_mmax(section) - bigcal_ymap_mmin(section)) /
     $           float(bigcal_ymap_nbin(section))
            mlo = bigcal_ymap_mmin(section)
            
            if(ymom.lt.bigcal_ymap_mmin(section).or.ymom.gt.
     $           bigcal_ymap_mmax(section)) then
               ydiff = ymom * sizey
            endif
            
            do ibin=1,bigcal_ymap_nbin(section)
               mhi = mlo + binwidth
               
               if(mlo.le.ymom.and.ymom.le.mhi) then ! cluster is in this bin
                  if(ibin.gt.1) then
                     fraclo = bigcal_ymap_yfrac(section,ibin-1)
                  else
                     fraclo = 0.
                  endif
                  frachi = bigcal_ymap_yfrac(section,ibin)
c     linearly interpolate frac within this bin
                  frac = fraclo + (frachi - fraclo)/(mhi - mlo) * (ymom - mlo)
                  
                  ydiff = (-.5 + frac)*sizey
c     exit the do loop when we find the right bin.
                  goto 102

               endif
               mlo = mhi
            enddo 
 
 102        continue

            xshift = bigcal_shower_map_shift(1) + (xcenter + xdiff)*
     $           bigcal_shower_map_slope(1)
            yshift = bigcal_shower_map_shift(2) + (ycenter + ydiff)*
     $           bigcal_shower_map_slope(2)

            bigcal_all_clstr_x(i) = xcenter + xdiff - xshift
            bigcal_all_clstr_y(i) = ycenter + ydiff - yshift

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
