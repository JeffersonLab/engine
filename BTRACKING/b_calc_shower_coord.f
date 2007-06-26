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

      integer i,j,irow,icol,icell
      real xmom,ymom,xcenter,ycenter,xdiff,ydiff
      real xpar(6),ypar(6)

      if(BIGCAL_PROT_NCLSTR.gt.0) then
         do i=1,BIGCAL_PROT_NCLSTR
            xmom = BIGCAL_PROT_CLSTR_XMOM(i)
            ymom = BIGCAL_PROT_CLSTR_YMOM(i)

c$$$            irow = BIGCAL_PROT_CLSTR_IYMAX(i)
c$$$            icol = BIGCAL_PROT_CLSTR_IXMAX(i)
c$$$            
c$$$            icell = icol + (irow - 1)*BIGCAL_PROT_NX
c$$$
c$$$            xcenter = BIGCAL_PROT_XCENTER(icell)
c$$$            ycenter = BIGCAL_PROT_YCENTER(icell)

            irow = bigcal_prot_clstr_iycell(i,1)
            icol = bigcal_prot_clstr_ixcell(i,1)

            xcenter = bigcal_prot_clstr_xcell(i,1)
            ycenter = bigcal_prot_clstr_ycell(i,1)
            
            do j=1,6
               xpar(j) = BIGCAL_PROT_XPAR(icol,j)
               !write(*,'(A5,I2,A4,F8.4)') 'xpar(',j,') = ',xpar(j)
               ypar(j) = BIGCAL_PROT_YPAR(irow,j)
               !write(*,'(A5,I2,A4,F8.4)') 'ypar(',j,') = ',ypar(j)
            enddo

c$$$            if(bdebug_print_clusters) then 
c$$$               write(*,*) 'xpar = ',xpar
c$$$               write(*,*) 'ypar = ',ypar
c$$$            endif
            
            xdiff = xpar(1)*atan(xpar(2) * xmom**4 + xpar(3) * xmom**3 + 
     $           xpar(4) * xmom**2 + xpar(5) * xmom + xpar(6))
            ydiff = ypar(1)*atan(ypar(2) * ymom**4 + ypar(3) * ymom**3 + 
     $           ypar(4) * ymom**2 + ypar(5) * ymom + ypar(6))
            
            if(abs(xdiff) .le. bigcal_prot_size_x*(1. + 
     $           (xcenter/bigcal_r_tgt)**2/2. ) ) then
               BIGCAL_PROT_CLSTR_X(i) = xcenter + xdiff
              
            else   ! fit probably bad, just use moment 
               BIGCAL_PROT_CLSTR_X(i) = xcenter + xmom
               
            endif
            
            if(abs(ydiff) .le. bigcal_prot_size_y*(1. + 
     $           (ycenter/bigcal_r_tgt)**2/2. ) ) then
               BIGCAL_PROT_CLSTR_Y(i) = ycenter + ydiff
            else   ! fit probably bad, just use moment
               BIGCAL_PROT_CLSTR_Y(i) = ycenter + ymom
            endif
         enddo
      endif

      if(BIGCAL_RCS_NCLSTR.gt.0) then
         do i=1,BIGCAL_RCS_NCLSTR
            xmom = BIGCAL_RCS_CLSTR_XMOM(i)
            ymom = BIGCAL_RCS_CLSTR_YMOM(i)

c$$$            irow = BIGCAL_RCS_CLSTR_IYMAX(i) - BIGCAL_PROT_NY
c$$$            icol = BIGCAL_RCS_CLSTR_IXMAX(i)
c$$$            
c$$$            icell = icol + (irow - 1) * BIGCAL_RCS_NX
c$$$
c$$$            xcenter = BIGCAL_RCS_XCENTER(icell)
c$$$            ycenter = BIGCAL_RCS_YCENTER(icell)

            irow = bigcal_rcs_clstr_iycell(i,1)
            icol = bigcal_rcs_clstr_ixcell(i,1)

            xcenter = bigcal_rcs_clstr_xcell(i,1)
            ycenter = bigcal_rcs_clstr_ycell(i,1)
            
            do j=1,6
               xpar(j) = BIGCAL_RCS_XPAR(icol,j)
               ypar(j) = BIGCAL_RCS_YPAR(irow-bigcal_prot_ny,j)
            enddo
            
            xdiff = xpar(1)*atan(xpar(2) * xmom**4 + xpar(3) * xmom**3 + 
     $           xpar(4) * xmom**2 + xpar(5) * xmom + xpar(6))
            ydiff = ypar(1)*atan(ypar(2) * ymom**4 + ypar(3) * ymom**3 + 
     $           ypar(4) * ymom**2 + ypar(5) * ymom + ypar(6))
            

            if(abs(xdiff) .le. bigcal_rcs_size_x*(1. + 
     $           (xcenter/bigcal_r_tgt)**2/2. ) ) then
               BIGCAL_RCS_CLSTR_X(i) = xcenter + xdiff
              
            else   ! fit probably bad, just use moment 
               BIGCAL_RCS_CLSTR_X(i) = xcenter + xmom
               
            endif
            
            if(abs(ydiff) .le. bigcal_rcs_size_y*(1. + 
     $           (ycenter/bigcal_r_tgt)**2/2. ) ) then
               BIGCAL_RCS_CLSTR_Y(i) = ycenter + ydiff
            else   ! fit probably bad, just use moment
               BIGCAL_RCS_CLSTR_Y(i) = ycenter + ymom
            endif

            
         enddo
      endif

      if(BIGCAL_MID_NCLSTR.gt.0) then
         do i=1,BIGCAL_MID_NCLSTR
            xmom = BIGCAL_MID_CLSTR_XMOM(i)
            ymom = BIGCAL_MID_CLSTR_YMOM(i)
            
c$$$            irow = BIGCAL_MID_CLSTR_IYMAX(i) 
c$$$            icol = BIGCAL_MID_CLSTR_IXMAX(i)

            irow = bigcal_mid_clstr_iycell(i,1)
            icol = bigcal_mid_clstr_ixcell(i,1)

            xcenter = bigcal_mid_clstr_xcell(i,1)
            ycenter = bigcal_mid_clstr_ycell(i,1)

            if(irow.le.BIGCAL_PROT_NY) then
c$$$               icell = icol + (irow-1)*BIGCAL_PROT_NX
c$$$               xcenter = BIGCAL_PROT_XCENTER(icell)
c$$$               ycenter = BIGCAL_PROT_YCENTER(icell)
               do j=1,6
                  xpar(j) = BIGCAL_PROT_XPAR(icol,j)
                  ypar(j) = BIGCAL_PROT_YPAR(irow,j)
               enddo
            else 
c               irow = irow - BIGCAL_PROT_NY
c$$$               icell = icol + (irow - 1 - bigcal_prot_ny)*BIGCAL_RCS_NX
c$$$               xcenter = BIGCAL_RCS_XCENTER(icell)
c$$$               ycenter = BIGCAL_RCS_YCENTER(icell)
               do j=1,6
                  xpar(j) = BIGCAL_RCS_XPAR(icol,j)
                  ypar(j) = BIGCAL_RCS_YPAR(irow-bigcal_prot_ny,j)
               enddo
            endif
            
            

            xdiff = xpar(1)*atan(xpar(2) * xmom**4 + xpar(3) * xmom**3 + 
     $           xpar(4) * xmom**2 + xpar(5) * xmom + xpar(6))
            ydiff = ypar(1)*atan(ypar(2) * ymom**4 + ypar(3) * ymom**3 + 
     $           ypar(4) * ymom**2 + ypar(5) * ymom + ypar(6))

            if(abs(xdiff) .le. bigcal_rcs_size_x*(1. + 
     $           (xcenter/bigcal_r_tgt)**2/2. ) ) then
               BIGCAL_MID_CLSTR_X(i) = xcenter + xdiff
              
            else   ! fit probably bad, just use moment 
               BIGCAL_MID_CLSTR_X(i) = xcenter + xmom
               
            endif
            
            if(abs(ydiff) .le. bigcal_rcs_size_y*(1. + 
     $           (ycenter/bigcal_r_tgt)**2/2. ) ) then
               BIGCAL_MID_CLSTR_Y(i) = ycenter + ydiff
            else   ! fit probably bad, just use moment
               BIGCAL_MID_CLSTR_Y(i) = ycenter + ymom
            endif

            
         enddo
      endif

      return 
      end
