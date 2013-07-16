      subroutine s_one_ev_persp_view
*
* $Log: s_one_ev_persp_view.f,v $
* Revision 1.2  1996/01/17 16:38:44  cdaq
* (DVW) Tweak args in gdraw calls
*
* Revision 1.1  1995/09/18 14:44:08  cdaq
* Initial revision
*

      implicit none

      include 'sos_data_structures.cmn'
      include 'sos_tracking.cmn'
      include 'sos_geometry.cmn'
      include 'sos_calorimeter.cmn'
      include 'gen_event_info.cmn'
      include 'sos_one_ev.par'
      include 'gen_one_ev_gctrak.cmn'
      include 'gen_one_ev_gckine.cmn'
      include 'gen_one_ev_gcvolu.cmn'

      character*5 scinname
      integer iscin


      call gdopen (8)
      call gdrawt (5.,2.,'PERSPECTIVE VIEW',.5,0.,2,0)
      call gdrawt (5.,1.,'SOS',.5,0.,2,0)
      call gdraw ('SHUT', 45., 115., 90., 1.8, 8.0, 0.06, 0.06)
      call s_one_ev_track
      call gdhits ('*   ', '*   ', 0, 850, 0.1)
      call gdclos (8)
*     
*     blow up the wire chambers, and make the hodoscopes invisible
*     
      call gdopen (9)
      call gsatt ('HDX1','SEEN',0)
      call gsatt ('HDY1','SEEN',0)
      do iscin=1,LOWER_HODO_X_PADDLES
         write(scinname,'(a,a)') 'H1X',char(64 + iscin)
         call gsatt (scinname,'SEEN',0)
      enddo
      do iscin=1,LOWER_HODO_Y_PADDLES
         write(scinname,'(a,a)') 'H1Y',char(64 + iscin)
         call gsatt (scinname,'SEEN',0)
      enddo
      call gdraw ('SHUT', 45., 115., 90., 12.8, 5.1, 0.15, 0.15)
      call s_one_ev_track
      call gdhits ('*   ', '*   ', 0, 850, 0.3)
      call gdclos (9)
      call gsatt ('HDX1','SEEN',1)
      call gsatt ('HDY1','SEEN',1)
      call gdclos (9)
*     Now make them visible again for the next pass...
      do iscin=1,LOWER_HODO_X_PADDLES
         write(scinname,'(a,a)') 'H1X',char(64 + iscin)
         call gsatt (scinname,'SEEN',1)
      enddo
      do iscin=1,LOWER_HODO_Y_PADDLES
         write(scinname,'(a,a)') 'H1Y',char(64 + iscin)
         call gsatt (scinname,'SEEN',1)
      enddo
      call gdshow (9)
      call gdshow (8)
      call gdshow (9)
      call gdelet (8)
      call gdelet (9)
      
      end
