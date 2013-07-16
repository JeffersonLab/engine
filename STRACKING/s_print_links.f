      subroutine s_print_links
*     prints the output of link matching
*     d.f. geesaman           7 Sept 1993
* $Log: s_print_links.f,v $
* Revision 1.2  1995/05/22 19:45:45  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/02/21  16:15:59  cdaq
* Initial revision
*
*
      implicit none
      include "sos_data_structures.cmn"
      include "sos_tracking.cmn"
      integer*4 itrack,ihit
      write(sluno,
     &  '(''  NUMBER OF TRACKS FROM SOS LINKED STUBS='',i4)') SNTRACKS_FP
        if(SNTRACKS_FP.gt.0) then
        write(sluno,'(''  Track   HITS'')')
          do itrack=1,SNTRACKS_FP
           write(sluno,1000) itrack,(SNTRACK_HITS(itrack,ihit),
     &        ihit=2,SNTRACK_HITS(itrack,1)+1) 
1000  format(2x,i3,2x,24i3)
          enddo
        endif
      return
      end
