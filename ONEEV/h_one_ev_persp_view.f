      subroutine h_one_ev_persp_view
*
* $Log: h_one_ev_persp_view.f,v $
* Revision 1.1  1995/09/18 14:44:02  cdaq
* Initial revision
*

      implicit none

      include 'hms_data_structures.cmn'
      include 'hms_tracking.cmn'
      include 'hms_geometry.cmn'
      include 'hms_calorimeter.cmn'
      include 'gen_event_info.cmn'
      include 'hms_one_ev.par'
      include 'gen_one_ev_gctrak.cmn'
      include 'gen_one_ev_gckine.cmn'
      include 'gen_one_ev_gcvolu.cmn'

      character*5 scinname
      integer iscin
      call gdopen (8)
      call gdrawt (5.,2.,'PERSPECTIVE VIEW',.5,0.,2,0)
      call gdrawt (5.,1.,'HMS',.5,0.,2,0)
      call gdraw ('HHUT', 45., 115., 90., 3.5, 9.0, 0.05, 0.05)
      call h_one_ev_track
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
      call gdraw ('HHUT', 45., 115., 90., 14.0, 6.1, 0.08, 0.08)
      call h_one_ev_track
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
      
