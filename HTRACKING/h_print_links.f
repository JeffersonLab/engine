      subroutine h_print_links
*     prints the output of link matching
*     d.f. geesaman           7 Sept 1993
* $Log: h_print_links.f,v $
* Revision 1.2  1995/05/22 19:39:17  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/02/19  06:16:24  cdaq
* Initial revision
*
*
      implicit none
      include "hms_data_structures.cmn"
      include "hms_tracking.cmn"
      integer*4 itrack,ihit
      write(hluno,'(''  NUMBER OF TRACKS FROM HMS LINKED STUBS='',i4)') 
     &                      HNTRACKS_FP
        if(HNTRACKS_FP.gt.0) then
        write(hluno,'(''  Track   HITS'')')
          do itrack=1,HNTRACKS_FP
           write(hluno,1000) itrack,(HNTRACK_HITS(itrack,ihit),
     &        ihit=2,HNTRACK_HITS(itrack,1)+1) 
1000  format(2x,i3,2x,24i3)
          enddo
        endif
      return
      end
