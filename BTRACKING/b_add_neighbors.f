      subroutine b_add_neighbors(cell_index,ncell,ncellmax,ix,iy,x,y,E,abort,err)

      implicit none
      save

      logical abort
      character*(*) err

      character*15 here
      parameter(here='b_add_neighbors')

      integer cell_index,ncell,ncellmax
      integer ix(ncellmax)
      integer iy(ncellmax)
      real x(ncellmax)
      real y(ncellmax)
      real E(ncellmax)

      integer ix0,iy0
      integer irow,icol,icell

c      logical found_any

      include 'bigcal_data_structures.cmn'
      include 'bigcal_geometry.cmn'

      abort = .false.
      err=' '

      !found_any = .false.
      
      ix0 = ix(cell_index)
      iy0 = iy(cell_index)
      
c$$$      write(*,*) 'searching for neighbors around cell (ix,iy,E) = ',
c$$$     $     ix0,iy0,E(cell_index)
      
c     check that center cell coordinates are in range!!
      if(ix0.lt.1 .or. iy0.lt.1 .or. iy0 .gt. 56 .or. ix0.gt.32 .or.
     $     (iy0.gt.33.and.ix0.gt.30) ) goto 101
      
      if(iy0 .le. 32) then
c     Check cell to the immediate left, if it exists
         if(ncell.lt.ncellmax) then
            irow = iy0
            icol = ix0 - 1
            icell = icol + 32*(irow-1)
            if(icol .ge. 1) then
               if(bigcal_all_good_det(icell).gt.b_cell_cut_prot) then
                  ncell = ncell + 1
                  ix(ncell) = icol
                  iy(ncell) = irow
                  x(ncell) = bigcal_all_xcenter(icell)
                  y(ncell) = bigcal_all_ycenter(icell)
                  E(ncell) = bigcal_all_good_det(icell)
                  bigcal_all_good_det(icell) = 0.
                  !found_any = .true.
               endif
            endif
         endif
c     Check cell to the immediate right, if it exists
         if(ncell.lt.ncellmax) then
            irow = iy0
            icol = ix0 + 1
            icell = icol + 32*(irow-1)

            if(icol.le.32) then
               if(bigcal_all_good_det(icell).gt.b_cell_cut_prot) then
                  ncell = ncell + 1
                  ix(ncell) = icol
                  iy(ncell) = irow
                  x(ncell) = bigcal_all_xcenter(icell)
                  y(ncell) = bigcal_all_ycenter(icell)
                  E(ncell) = bigcal_all_good_det(icell)
                  bigcal_all_good_det(icell) = 0.
                  !found_any = .true.
               endif
            endif
         endif
c     Check one cell down, if it exists
         if(ncell.lt.ncellmax) then
            irow = iy0-1
            icol = ix0
            icell = icol + 32*(irow-1)
            if(irow.ge.1) then
               if(bigcal_all_good_det(icell).gt.b_cell_cut_prot) then
                  ncell = ncell + 1
                  ix(ncell) = icol
                  iy(ncell) = irow
                  x(ncell) = bigcal_all_xcenter(icell)
                  y(ncell) = bigcal_all_ycenter(icell)
                  E(ncell) = bigcal_all_good_det(icell)
                  bigcal_all_good_det(icell) = 0.
                  !found_any = .true.
               endif
            endif
         endif
c     Check one cell up: if going up one row puts us in RCS section, 
c     then check closest column according to ixclose_prot!!!!
         if(ncell.lt.ncellmax) then
            irow = iy0 + 1
            icol = ix0
            if(irow.le.32) then
               icell = icol + 32*(irow-1)
               if(bigcal_all_good_det(icell).gt.b_cell_cut_prot) then
                  ncell = ncell + 1
                  ix(ncell) = icol
                  iy(ncell) = irow
                  x(ncell) = bigcal_all_xcenter(icell)
                  y(ncell) = bigcal_all_ycenter(icell)
                  E(ncell) = bigcal_all_good_det(icell)
                  bigcal_all_good_det(icell) = 0.
                  !found_any = .true.
               endif
            else 
               icell = bigcal_prot_maxhits + bigcal_ixclose_prot(icol)
               if(bigcal_all_good_det(icell).gt.b_cell_cut_rcs) then
                  ncell = ncell + 1
                  ix(ncell) = bigcal_ixclose_prot(icol)
                  iy(ncell) = irow
                  x(ncell) = bigcal_all_xcenter(icell)
                  y(ncell) = bigcal_all_ycenter(icell)
                  E(ncell) = bigcal_all_good_det(icell)
                  bigcal_all_good_det(icell) = 0.
                  !found_any = .true.
               endif
            endif
         endif
      else ! center cell is in RCS section
c     check one cell to the left, if it exists:
         if(ncell.lt.ncellmax) then
            irow = iy0
            icol = ix0 - 1
            icell = bigcal_prot_maxhits + icol + 30*(irow-33)
            if(icol.ge.1) then
               if(bigcal_all_good_det(icell).gt.b_cell_cut_rcs) then
                  ncell = ncell + 1
                  ix(ncell) = icol
                  iy(ncell) = irow
                  x(ncell) = bigcal_all_xcenter(icell)
                  y(ncell) = bigcal_all_ycenter(icell)
                  E(ncell) = bigcal_all_good_det(icell)
                  bigcal_all_good_det(icell) = 0.
                  !found_any = .true.
               endif
            endif
         endif
c     check one cell to the right, if it exists:
         if(ncell.lt.ncellmax) then
            irow = iy0
            icol = ix0 + 1
            icell = bigcal_prot_maxhits + icol + 30*(irow-33)
            if(icol.le.30) then
               if(bigcal_all_good_det(icell).gt.b_cell_cut_rcs) then
                  ncell = ncell + 1
                  ix(ncell) = icol
                  iy(ncell) = irow
                  x(ncell) = bigcal_all_xcenter(icell)
                  y(ncell) = bigcal_all_ycenter(icell)
                  E(ncell) = bigcal_all_good_det(icell)
                  bigcal_all_good_det(icell) = 0.
                  !found_any = .true.
               endif
            endif
         endif
c     check one cell up, if it exists:
         if(ncell.lt.ncellmax) then
            irow = iy0 + 1
            icol = ix0
            icell = bigcal_prot_maxhits + icol + 30*(irow-33)
            if(irow.le.56) then
               if(bigcal_all_good_det(icell).gt.b_cell_cut_rcs) then
                  ncell = ncell + 1
                  ix(ncell) = icol
                  iy(ncell) = irow
                  x(ncell) = bigcal_all_xcenter(icell)
                  y(ncell) = bigcal_all_ycenter(icell)
                  E(ncell) = bigcal_all_good_det(icell)
                  bigcal_all_good_det(icell) = 0.
                  !found_any = .true.
               endif
            endif
         endif
c     check one cell down, if it exists: if going one cell down puts us in protvino 
c     section, then check closest column according to ixclose_rcs!!!!
         if(ncell.lt.ncellmax) then
            irow = iy0 - 1
            icol = ix0
            if(irow.ge.33) then
               icell = bigcal_prot_maxhits + icol + 30*(irow-33)
               if(bigcal_all_good_det(icell).gt.b_cell_cut_rcs) then
                  ncell = ncell + 1
                  ix(ncell) = icol
                  iy(ncell) = irow
                  x(ncell) = bigcal_all_xcenter(icell)
                  y(ncell) = bigcal_all_ycenter(icell)
                  E(ncell) = bigcal_all_good_det(icell)
                  bigcal_all_good_det(icell) = 0.
                  !found_any = .true.
               endif
            else
               icell = bigcal_ixclose_rcs(icol) + 32*(irow-1)
               if(bigcal_all_good_det(icell).gt.b_cell_cut_prot) then
                  ncell = ncell + 1
                  ix(ncell) = bigcal_ixclose_rcs(icol)
                  iy(ncell) = irow
                  x(ncell) = bigcal_all_xcenter(icell)
                  y(ncell) = bigcal_all_ycenter(icell)
                  E(ncell) = bigcal_all_good_det(icell)
                  bigcal_all_good_det(icell) = 0.
                  !found_any = .true.
               endif
            endif
         endif
      endif

 101  continue

c      add_neighbors = found_any

      return
      end
