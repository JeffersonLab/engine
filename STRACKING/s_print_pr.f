      subroutine s_print_pr
*     subroutine to dump output of S_PATTERN_RECOGNITION
*     All the results are contained in sos_tracking.inc
*     d.f. geesaman          5 September 1993
* $Log: s_print_pr.f,v $
* Revision 1.2  1995/05/22 19:45:45  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/02/21  16:37:52  cdaq
* Initial revision
*
      implicit none
      include "sos_data_structures.cmn"
      include "sos_tracking.cmn"
      include "sos_geometry.cmn"
*     local variables
      integer*4 i,j
      write(sluno,'('' SOS PATTERN RECOGNITION RESULTS'')')
      write(sluno,'('' chamber='',i3,'' number of hits='',i3)')
     &         (i,sncham_hits(i),i=1,sdc_num_chambers)
      write(sluno,'('' Total number of space points found='',i3)')
     &         snspace_points_tot
      write(sluno,'('' chamber number'',i2,''  number of points='',i3)')
     &     (i,snspace_points(i),i=1,sdc_num_chambers)
      write(sluno,'('' Space point requirements'')')
      write(sluno,'('' chamber='',i3,''   min_hit='',i4,''  min_combos='',i3)')
     &      (i,smin_hit(i),smin_combos(i),i=1,sdc_num_chambers)  
      if(snspace_points_tot.ge.1) then
         write(sluno,'('' point       x         y     number  number  hits'')')
         write(sluno,'('' number                       hits   combos'')')
1001  format(3x,i3,f10.4,f10.4,3x,i3,6x,i3,5x,11i3)
         do i=1,snspace_points_tot
         write(sluno,1001) i, sspace_points(i,1),sspace_points(i,2),
     &   sspace_point_hits(i,1), sspace_point_hits(i,2),
     &   (sspace_point_hits(i,j+2),j=1,sspace_point_hits(i,1))
         enddo
      endif
      return
      end
