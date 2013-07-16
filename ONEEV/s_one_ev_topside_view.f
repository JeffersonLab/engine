      subroutine s_one_ev_topside_view
*
* $Log: s_one_ev_topside_view.f,v $
* Revision 1.1  1996/01/17 16:37:05  cdaq
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

      call gdopen (7)
      call gsatt ('HDX1','SEEN',0)
      call gsatt ('HDX2','SEEN',0)
      call gdrawt (4.4,2.,'TOP VIEW',.5,0.,2,0)
      call gdrawt (4.4,1.,'SOS',.5,0.,2,0)
      call gdraw ('SHUT', 270., 0., 90., 4.4,4.0, 0.045, 0.080)
      call s_one_ev_track
      call gdhits ('*   ', '*   ', 0, 850, 0.1)
      call gdhits ('HOD1', 'HDY1', 0, 850, 0.1)
      call gdhits ('HOD2', 'HDY2', 0, 850, 0.1)
      call gdclos (7)
*     
*     
*     Other side view
*     
      call gdopen (6)
      call gsatt ('HDY1','SEEN',0)
      call gsatt ('HDY2','SEEN',0)
      call gdrawt (14.75,2.,'SIDE VIEW',.5,0.,2,0)
      call gdraw ('SHUT', 90., 90., 90., 14.75,4.0,0.045, 0.080)
      call s_one_ev_track
      call gdhits ('*   ', '*   ', 0, 850, 0.1)
      call gdclos (6)
      call gdshow (7)
      call gdshow (6)
      call gdshow (7)
      call gdelet (6)
      call gdelet (7)
*     
      end
