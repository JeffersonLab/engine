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

c     all clusters are already sorted in order of decreasing amplitude, so cell 1 is the maximum

      do i=1,bigcal_all_nclstr
         xcenter = bigcal_all_clstr_xcell(i,1)
         ycenter = bigcal_all_clstr_ycell(i,1)

         xmom = bigcal_all_clstr_xmom(i)
         ymom = bigcal_all_clstr_ymom(i)

         irow = bigcal_all_clstr_iycell(i,1)
         icol = bigcal_all_clstr_ixcell(i,1)

         if(irow.le.32) then
            do j=1,6
               xpar(j) = bigcal_prot_xpar(icol,j)
               ypar(j) = bigcal_prot_ypar(irow,j)
            enddo
         else 
            do j=1,6
               xpar(j) = bigcal_rcs_xpar(icol,j)
               ypar(j) = bigcal_rcs_ypar(irow-32,j)
            enddo
         endif
         
         xdiff = xpar(1) * atan(xpar(2)*xmom**4 + xpar(3)*xmom**3 + 
     $        xpar(4)*xmom**2 + xpar(5)*xmom + xpar(6))
         ydiff = ypar(1) * atan(ypar(2)*ymom**4 + ypar(3)*ymom**3 + 
     $        ypar(4)*ymom**2 + ypar(5)*ymom + ypar(6))
         
         bigcal_all_clstr_x(i) = xcenter + xdiff
         bigcal_all_clstr_y(i) = ycenter + ydiff
         
      enddo
      
      return 
      end
