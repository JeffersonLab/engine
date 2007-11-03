      subroutine b_rebuild_cluster(clust)

      implicit none
      save

      logical badi, badj
      integer clust,icell,jcell,i,j
      integer rowi,rowj,coli,colj,row,col
      real ecell,xcell,ycell,xmom,ymom
      real ei,ej,ai,aj,xi,xj,yi,yj
      real xcenter,ycenter,xdiff,ydiff
      real esum,asum
      real xpar(6),ypar(6)

      include 'bigcal_data_structures.cmn'
      include 'bigcal_shower_parms.cmn'

      logical abort
      character*80 err

c     PURPOSE:
c     this routine assumes that the cell array for cluster iclust has 
c     already been filled, but maybe one or more of the energies has changed
c     as a result of a guess using HMS info and elastic kinematics when a 
c     channel is in the "bad channels" list.
c     So re-sort the cluster in order of decreasing energy, and re-calculate
c     cluster quantities related to the energies in the cells. 
c     this routine gets called from the gep_check_bigcal routine which is a 
c     subroutine of gep_physics. If the HMS predicts that electron should have
c     hit BigCal at a certain place, then gep_check_bigcal looks for 
c     channels in the vicinity of the expected electron that are in the 
c     bad channel list. If it finds any, then it will guess what the energy
c     should have been in those channels based on a fit to the shower shape
c     using the HMS-predicted energy and position of the expected electron.
c     If there are any clusters in the vicinity containing the same channels
c     from the bad channel list, then this routine reevaluates those clusters
c     based on the energy that was "guessed" for the channels in question. 

      if(bigcal_all_nclstr.lt.clust) return

      esum = 0.
      asum = 0.

      do icell = 1,bigcal_all_clstr_ncell(clust)
         ei = bigcal_all_clstr_ecell(clust,icell)
         ai = bigcal_all_clstr_acell(clust,icell)
         rowi = bigcal_all_clstr_iycell(clust,icell)
         coli = bigcal_all_clstr_ixcell(clust,icell)
         xi = bigcal_all_clstr_xcell(clust,icell)
         yi = bigcal_all_clstr_ycell(clust,icell)

         badi = bigcal_clstr_bad_chan(clust,icell)

         do jcell = icell+1,bigcal_all_clstr_ncell(clust)
            ej = bigcal_all_clstr_ecell(clust,jcell)
            aj = bigcal_all_clstr_acell(clust,jcell)
            xj = bigcal_all_clstr_xcell(clust,jcell)
            yj = bigcal_all_clstr_ycell(clust,jcell)
            rowj = bigcal_all_clstr_iycell(clust,jcell)
            colj = bigcal_all_clstr_ixcell(clust,jcell)
            badj = bigcal_clstr_bad_chan(clust,jcell)
            if(ej.gt.ei) then ! switch everything:
               bigcal_all_clstr_ecell(clust,icell) = ej
               bigcal_all_clstr_acell(clust,icell) = aj
               bigcal_all_clstr_xcell(clust,icell) = xj
               bigcal_all_clstr_ycell(clust,icell) = yj
               bigcal_all_clstr_ixcell(clust,icell) = colj
               bigcal_all_clstr_iycell(clust,icell) = rowj

               bigcal_clstr_bad_chan(clust,icell) = badj

               bigcal_all_clstr_ecell(clust,jcell) = ei
               bigcal_all_clstr_acell(clust,jcell) = ai
               bigcal_all_clstr_xcell(clust,jcell) = xi
               bigcal_all_clstr_ycell(clust,jcell) = yi
               bigcal_all_clstr_ixcell(clust,jcell) = coli
               bigcal_all_clstr_iycell(clust,jcell) = rowi
               
               bigcal_clstr_bad_chan(clust,jcell) = badi
            endif
         enddo
      enddo

      do icell=1,bigcal_all_clstr_ncell(clust)
         esum = esum + bigcal_all_clstr_ecell(clust,icell)
         asum = asum + bigcal_all_clstr_acell(clust,icell)
      enddo

      bigcal_all_clstr_iymax(clust) = bigcal_all_clstr_iycell(clust,1)
      bigcal_all_clstr_ixmax(clust) = bigcal_all_clstr_ixcell(clust,1)
      bigcal_all_clstr_etot(clust) = esum
      bigcal_all_clstr_atot(clust) = asum
      
      xcenter = bigcal_all_clstr_xcell(clust,1)
      ycenter = bigcal_all_clstr_ycell(clust,1)

      xmom = 0.
      ymom = 0.

      do icell=1,bigcal_all_clstr_ncell(clust)
         xdiff = bigcal_all_clstr_xcell(clust,icell) - xcenter
         ydiff = bigcal_all_clstr_ycell(clust,icell) - ycenter
         
         ecell = bigcal_all_clstr_ecell(clust,icell)

         xmom = xmom + xdiff*ecell/esum
         ymom = ymom + ydiff*ecell/esum

      enddo

      bigcal_all_clstr_xmom(clust) = xmom
      bigcal_all_clstr_ymom(clust) = ymom

c     also re-calculate shower coordinates:

      row = bigcal_all_clstr_iymax(clust)
      col = bigcal_all_clstr_ixmax(clust)

      if(row.le.32) then
         do i=1,6
            xpar(i) = bigcal_prot_xpar(col,i)
            ypar(i) = bigcal_prot_ypar(row,i)
         enddo
      else
         do i=1,6
            xpar(i) = bigcal_rcs_xpar(col,i)
            ypar(i) = bigcal_rcs_ypar(row,i)
         enddo
      endif

      bigcal_all_clstr_x(clust) = xcenter + xpar(1)*atan(
     $     xpar(2)*xmom**4 + xpar(3)*xmom**3 + xpar(4)*xmom**2 + 
     $     xpar(5)*xmom    + xpar(6))
      bigcal_all_clstr_y(clust) = ycenter + ypar(1)*atan(
     $     ypar(2)*ymom**4 + ypar(3)*ymom**3 + ypar(4)*ymom**2 + 
     $     ypar(5)*ymom    + ypar(6))

c$$$      write(*,*) 'REBUILT CLUSTER#',clust,' after guessing energies '//
c$$$     $     'for channels in the bad list:'
c$$$      call b_print_cluster(clust,abort,err)

      return 
      end
      
