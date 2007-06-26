      subroutine b_print_cluster(isection,iclust,ABORT,err)

      implicit none
      save
      
      integer isection,iclust,icell
      logical ABORT
      character*(*) err

      include 'bigcal_data_structures.cmn'
      include 'bigcal_bypass_switches.cmn'

      character*15 here
      parameter(here='b_print_cluster')

      if(isection.eq.1) then ! prot.
         write(*,101) 'Protvino Cluster #',iclust
         write(*,102) '(row,col) = (',bigcal_prot_clstr_iymax(iclust),
     $        ', ',bigcal_prot_clstr_ixmax(iclust),')'
         write(*,105) 'Etot = ',bigcal_prot_clstr_etot(iclust)
         write(*,103) 'xcellcenter = ',bigcal_prot_clstr_xcell(iclust,1)
         write(*,103) 'ycellcenter = ',bigcal_prot_clstr_ycell(iclust,1)
         write(*,104) 'xmoment = ',bigcal_prot_clstr_xmom(iclust)
         write(*,104) 'ymoment = ',bigcal_prot_clstr_ymom(iclust)
      else if(isection.eq.2) then !rcs
         write(*,101) 'RCS Cluster #',iclust
         write(*,102) '(row,col) = (',bigcal_rcs_clstr_iymax(iclust),
     $        ', ',bigcal_rcs_clstr_ixmax(iclust),')'
         write(*,105) 'Etot = ',bigcal_rcs_clstr_etot(iclust)
         write(*,103) 'xcellcenter = ',bigcal_rcs_clstr_xcell(iclust,1)
         write(*,103) 'ycellcenter = ',bigcal_rcs_clstr_ycell(iclust,1)
         write(*,104) 'xmoment = ',bigcal_rcs_clstr_xmom(iclust)
         write(*,104) 'ymoment = ',bigcal_rcs_clstr_ymom(iclust)
         
      else if(isection.eq.3) then !mid
         write(*,101) 'Mid. Cluster #',iclust
         write(*,102) '(row,col) = (',bigcal_mid_clstr_iymax(iclust),
     $        ', ',bigcal_mid_clstr_ixmax(iclust),')'
         write(*,105) 'Etot = ',bigcal_mid_clstr_etot(iclust)
         write(*,103) 'xcellcenter = ',bigcal_mid_clstr_xcell(iclust,1)
         write(*,103) 'ycellcenter = ',bigcal_mid_clstr_ycell(iclust,1)
         write(*,104) 'xmoment = ',bigcal_mid_clstr_xmom(iclust)
         write(*,104) 'ymoment = ',bigcal_mid_clstr_ymom(iclust)
      endif

 101  format(A18,I10)
 102  format(A13,I2,A2,I2,A1)
 105  format(A7,F10.3)
 103  format(A14,F10.3)
 104  format(A10,F10.3)
      return
      end
