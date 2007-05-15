      subroutine b_generate_geometry

      implicit none
      save
      
      include 'bigcal_data_structures.cmn'
      include 'bigcal_geometry.cmn'

      integer ix,iy,icell

      real xshift,yshift,xsize,ysize,xcenter,ycenter

      xshift = BIGCAL_PROT_SHIFT_X
      yshift = BIGCAL_PROT_SHIFT_Y

      xsize = BIGCAL_PROT_SIZE_X
      ysize = BIGCAL_PROT_SIZE_Y
      

      do ix=1,BIGCAL_PROT_NX
         do iy=1,BIGCAL_PROT_NY
            icell = ix + (iy-1)*BIGCAL_PROT_NX
            xcenter = xshift + (ix - .5)*xsize
            ycenter = yshift + (iy - .5)*ysize

            BIGCAL_PROT_XCENTER(icell) = xcenter
            BIGCAL_PROT_YCENTER(icell) = ycenter
         enddo
      enddo

      xshift = BIGCAL_RCS_SHIFT_X
      yshift = BIGCAL_RCS_SHIFT_Y

      xsize = BIGCAL_RCS_SIZE_X
      ysize = BIGCAL_RCS_SIZE_Y

      do ix=1,BIGCAL_RCS_NX
         do iy=1,BIGCAL_RCS_NY
            icell = ix + (iy-1)*BIGCAL_RCS_NX
            xcenter = xshift + (ix - .5)*xsize
            ycenter = yshift + (iy - .5)*ysize

            BIGCAL_RCS_XCENTER(icell) = xcenter
            BIGCAL_RCS_YCENTER(icell) = ycenter
         enddo
      enddo

      return
      end
