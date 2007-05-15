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

      integer i,j,irow,icol,icell
      real xmom,ymom,xcenter,ycenter,xdiff,ydiff
      real xpar(6),ypar(6)

      if(BIGCAL_PROT_NCLSTR.gt.0) then
         do i=1,BIGCAL_PROT_NCLSTR
            xmom = BIGCAL_PROT_CLSTR_XMOM(i)
            ymom = BIGCAL_PROT_CLSTR_YMOM(i)

            irow = BIGCAL_PROT_CLSTR_IYMAX(i)
            icol = BIGCAL_PROT_CLSTR_IXMAX(i)
            
            icell = icol + (irow - 1)*BIGCAL_PROT_NX

            xcenter = BIGCAL_PROT_XCENTER(icell)
            ycenter = BIGCAL_PROT_YCENTER(icell)
            
            do j=1,6
               xpar(j) = BIGCAL_PROT_XPAR(icol,j)
               ypar(j) = BIGCAL_PROT_YPAR(irow,j)
            enddo
            
            xdiff = xpar(1)*atan(xpar(2) * xmom**4 + xpar(3) * xmom**3 + 
     $           xpar(4) * xmom**2 + xpar(5) * xmom + xpar(6))
            ydiff = ypar(1)*atan(ypar(2) * ymom**4 + ypar(3) * ymom**3 + 
     $           ypar(4) * ymom**2 + ypar(5) * ymom + ypar(6))
            
            BIGCAL_PROT_CLSTR_X(i) = xcenter + xdiff
            BIGCAL_PROT_CLSTR_Y(i) = ycenter + ydiff
         enddo
      endif

      if(BIGCAL_RCS_NCLSTR.gt.0) then
         do i=1,BIGCAL_RCS_NCLSTR
            xmom = BIGCAL_RCS_CLSTR_XMOM(i)
            ymom = BIGCAL_RCS_CLSTR_YMOM(i)

            irow = BIGCAL_RCS_CLSTR_IYMAX(i) - BIGCAL_PROT_NY
            icol = BIGCAL_RCS_CLSTR_IXMAX(i)
            
            icell = icol + (irow - 1) * BIGCAL_RCS_NX

            xcenter = BIGCAL_RCS_XCENTER(icell)
            ycenter = BIGCAL_RCS_YCENTER(icell)
            
            do j=1,6
               xpar(j) = BIGCAL_RCS_XPAR(icol,j)
               ypar(j) = BIGCAL_RCS_YPAR(irow,j)
            enddo
            
            xdiff = xpar(1)*atan(xpar(2) * xmom**4 + xpar(3) * xmom**3 + 
     $           xpar(4) * xmom**2 + xpar(5) * xmom + xpar(6))
            ydiff = ypar(1)*atan(ypar(2) * ymom**4 + ypar(3) * ymom**3 + 
     $           ypar(4) * ymom**2 + ypar(5) * ymom + ypar(6))
            
            BIGCAL_RCS_CLSTR_X(i) = xcenter + xdiff
            BIGCAL_RCS_CLSTR_Y(i) = ycenter + ydiff
         enddo
      endif

      if(BIGCAL_MID_NCLSTR.gt.0) then
         do i=1,BIGCAL_MID_NCLSTR
            xmom = BIGCAL_MID_CLSTR_XMOM(i)
            ymom = BIGCAL_MID_CLSTR_YMOM(i)
            
            irow = BIGCAL_MID_CLSTR_IYMAX(i) 
            icol = BIGCAL_MID_CLSTR_IXMAX(i)
            if(irow.le.BIGCAL_PROT_NY) then
               icell = icol + (irow-1)*BIGCAL_PROT_NX
               xcenter = BIGCAL_PROT_XCENTER(icell)
               ycenter = BIGCAL_PROT_YCENTER(icell)
               do j=1,6
                  xpar(j) = BIGCAL_PROT_XPAR(icol,j)
                  ypar(j) = BIGCAL_PROT_YPAR(irow,j)
               enddo
            else 
               irow = irow - BIGCAL_PROT_NY
               icell = icol + (irow - 1)*BIGCAL_RCS_NX
               xcenter = BIGCAL_RCS_XCENTER(icell)
               ycenter = BIGCAL_RCS_YCENTER(icell)
               do j=1,6
                  xpar(j) = BIGCAL_RCS_XPAR(icol,j)
                  ypar(j) = BIGCAL_RCS_YPAR(irow,j)
               enddo
            endif
            
            xdiff = xpar(1)*atan(xpar(2) * xmom**4 + xpar(3) * xmom**3 + 
     $           xpar(4) * xmom**2 + xpar(5) * xmom + xpar(6))
            ydiff = ypar(1)*atan(ypar(2) * ymom**4 + ypar(3) * ymom**3 + 
     $           ypar(4) * ymom**2 + ypar(5) * ymom + ypar(6))

            BIGCAL_MID_CLSTR_X(i) = xcenter + xdiff
            BIGCAL_MID_CLSTR_Y(i) = ycenter + ydiff
         enddo
      endif

      return 
      end
