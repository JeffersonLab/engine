      subroutine b_print_cluster(iclust,ABORT,err)

      implicit none
      save
      
      integer iclust,icell
      logical ABORT
      character*(*) err

      include 'bigcal_data_structures.cmn'
      include 'bigcal_bypass_switches.cmn'

      character*15 here
      parameter(here='b_print_cluster')

      if(iclust.ge.1.and.iclust.le.bigcal_all_nclstr) then ! prot.
         write(*,101) 'BigCal Cluster #',iclust
         write(*,102) '(row,col) = (',bigcal_all_clstr_iymax(iclust),
     $        ', ',bigcal_all_clstr_ixmax(iclust),')'
         write(*,105) 'Etot = ',bigcal_all_clstr_etot(iclust)
         write(*,103) 'xcellcenter = ',bigcal_all_clstr_xcell(iclust,1)
         write(*,103) 'ycellcenter = ',bigcal_all_clstr_ycell(iclust,1)
         write(*,104) 'xmoment = ',bigcal_all_clstr_xmom(iclust)
         write(*,104) 'ymoment = ',bigcal_all_clstr_ymom(iclust)
         write(*,106) 'ncell = ',bigcal_all_clstr_ncell(iclust)
         
         do icell=1,bigcal_all_clstr_ncell(iclust)
            write(*,107) 'Cell #',icell,', (row,col,E,bad?) = (',
     $           bigcal_all_clstr_iycell(iclust,icell),', ',
     $           bigcal_all_clstr_ixcell(iclust,icell),', ',
     $           bigcal_all_clstr_ecell(iclust,icell),', ',
     $           bigcal_clstr_bad_chan(iclust,icell),')'
         enddo
      endif

 101  format(A18,I10)
 102  format(A13,I2,A2,I2,A1)
 105  format(A7,F10.3)
 103  format(A14,F10.3)
 104  format(A10,F10.3)
 106  format(A8,I2)
 107  format(A6,I2,A25,I2,A2,I2,A2,F12.5,A2,L2,A1)

      return
      end
