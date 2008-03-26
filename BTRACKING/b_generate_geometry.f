      subroutine b_generate_geometry

      implicit none
      save
      
      include 'bigcal_data_structures.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'bigcal_geometry.cmn'

      integer ix,iy,icell,ixmin

      real xshift,yshift,xsize,ysize,xcenter,ycenter,xcell,diff
      real mindiff
      real sumx,sumy,ylow

      if(b_use_size_measurements.ne.0) then
         sumx = 0.
         sumy = 0.
         do iy=1,bigcal_prot_ny
            sumx = sumx + bigcal_psizex(iy)
            sumy = sumy + bigcal_psizey(iy)
         enddo
         
         bigcal_prot_size_x = sumx / float(bigcal_prot_ny)
         bigcal_prot_size_y = sumy / float(bigcal_prot_ny)
         
         write(*,*) 'Average horizontal block size, protvino = ',bigcal_prot_size_x
         write(*,*) 'Average vertical block size, protvino = ',bigcal_prot_size_y

         sumx = 0.
         sumy = 0.
         do iy=1,bigcal_rcs_ny
            sumx = sumx + bigcal_rsizex(iy)
            sumy = sumy + bigcal_rsizey(iy)
         enddo
         
         bigcal_rcs_size_x = sumx / float(bigcal_rcs_ny)
         bigcal_rcs_size_y = sumy / float(bigcal_rcs_ny)

         write(*,*) 'Average horizontal block size, rcs = ',bigcal_rcs_size_x
         write(*,*) 'Average vertical block size, rcs = ',bigcal_rcs_size_y
      endif

      xshift = BIGCAL_PROT_SHIFT_X
      yshift = BIGCAL_PROT_SHIFT_Y + bigcal_height

      xsize = BIGCAL_PROT_SIZE_X
      ysize = BIGCAL_PROT_SIZE_Y
      
      do ix=1,BIGCAL_PROT_NX

         ylow = yshift

         do iy=1,BIGCAL_PROT_NY

            if(b_use_size_measurements.ne.0) then
               xsize = bigcal_psizex(iy)
               ysize = bigcal_psizey(iy)
            endif

            icell = ix + (iy-1)*BIGCAL_PROT_NX

            xcenter = xshift - ( bigcal_prot_nx - ix + .5 )*xsize
            ycenter = yshift + (iy - .5)*ysize

            if(b_use_size_measurements.ne.0) then
               ycenter = ylow + .5 * ysize
               ylow = ylow + ysize
            endif

            BIGCAL_PROT_XCENTER(icell) = xcenter
            BIGCAL_PROT_YCENTER(icell) = ycenter
            bigcal_all_xcenter(icell) = xcenter
            bigcal_all_ycenter(icell) = ycenter

c            write(*,*) 'row,col,xblock,yblock=',iy,ix,xcenter,ycenter

         enddo
      enddo

      xshift = BIGCAL_RCS_SHIFT_X
      yshift = BIGCAL_RCS_SHIFT_Y + bigcal_height

      xsize = BIGCAL_RCS_SIZE_X
      ysize = BIGCAL_RCS_SIZE_Y

      do ix=1,BIGCAL_RCS_NX

         ylow = yshift

         do iy=1,BIGCAL_RCS_NY

            if(b_use_size_measurements.ne.0) then
               xsize = bigcal_rsizex(iy)
               ysize = bigcal_rsizey(iy)
            endif

            icell = ix + (iy-1)*BIGCAL_RCS_NX
            xcenter = xshift - ( bigcal_rcs_nx - ix + .5 )*xsize
            ycenter = yshift + (iy - .5)*ysize

            if(b_use_size_measurements.ne.0) then
               ycenter = ylow + .5 * ysize
               ylow = ylow + ysize
            endif

            BIGCAL_RCS_XCENTER(icell) = xcenter
            BIGCAL_RCS_YCENTER(icell) = ycenter

            bigcal_all_xcenter(icell + bigcal_prot_maxhits) = xcenter
            bigcal_all_ycenter(icell + bigcal_prot_maxhits) = ycenter

c            write(*,*) 'row,col,xblock,yblock=',iy,ix,xcenter,ycenter
            
         enddo
      enddo

      do ix=1,bigcal_prot_nx

         mindiff = 1000000.
         ixmin = 0

         xcenter = bigcal_prot_xcenter(ix + 31*32)
         do icell=1,30
            xcell = bigcal_rcs_xcenter(icell)
            diff = xcenter - xcell
            if(abs(diff).lt.mindiff) then
               mindiff = diff
               ixmin = icell
            endif
         enddo

         if(mindiff.lt.1000000..and.ixmin.ge.1.and.ixmin.le.30) then
            bigcal_ixclose_prot(ix) = ixmin
         else
            write(*,*) 'warning: could not find ixclose_prot, ix = ',ix
            write(*,*) 'something probably wrong with geometry database'
            bigcal_ixclose_prot(ix) = min(max(ix,1),30)
         endif     
      enddo

      do ix=1,bigcal_rcs_nx
         
         mindiff = 1000000.
         ixmin = 0

         xcenter = bigcal_rcs_xcenter(ix)
         do icell=1,32
            xcell = bigcal_prot_xcenter(icell + 31*32)
            diff = xcenter - xcell
            if(abs(diff).lt.mindiff) then
               mindiff = diff
               ixmin = icell
            endif
         enddo
         if(mindiff.lt.1000000..and.ixmin.ge.1.and.ixmin.le.32) then
            bigcal_ixclose_rcs(ix) = ixmin
         else
            write(*,*) 'warning: could not find ixclose_rcs, ix = ',ix
            write(*,*) 'something probably wrong with geometry database'
            bigcal_ixclose_rcs(ix) = min(max(ix,1),32)
         endif
      enddo 

      return
      end
