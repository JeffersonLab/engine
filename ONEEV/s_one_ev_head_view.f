      subroutine s_one_ev_head_view
*
* $Log:

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
      character*4 blockname
      character*5 layername
      integer ilayer
      integer irow

      
      call gdopen (5)
*     first, get all the background junk out of the picture...
      call gsatt ('HDX1','SEEN',0)
      call gsatt ('HDX2','SEEN',0)
      call gsatt ('HDY1','SEEN',0)
      call gsatt ('HDY2','SEEN',0)
      call gsatt ('SHOW','SEEN',0)
      do iscin=1,LOWER_HODO_X_PADDLES
         write(scinname,'(a,a)') 'H1X',char(64 + iscin)
         call gsatt (scinname,'SEEN',0)
      enddo
      do iscin=1,LOWER_HODO_Y_PADDLES
         write(scinname,'(a,a)') 'H1Y',char(64 + iscin)
         call gsatt (scinname,'SEEN',0)
      enddo
      do iscin=1,UPPER_HODO_X_PADDLES
         write(scinname,'(a,a)') 'H2X',char(64 + iscin)
         call gsatt (scinname,'SEEN',0)
      enddo
      do iscin=1,UPPER_HODO_Y_PADDLES
         write(scinname,'(a,a)') 'H2Y',char(64 + iscin)
         call gsatt (scinname,'SEEN',0)
      enddo
      do ilayer =1,SMAX_CAL_COLUMNS
         write(layername,'(a,i1)') 'LAY',ilayer
         call gsatt (layername,'SEEN',0)
         do irow = 1,SMAX_CAL_ROWS
            write(blockname,'(a,i1,a)') 'BL',ilayer,char(64 + irow)
            call gsatt (blockname,'SEEN',0)
         enddo
      enddo         
      call gdhits ('*   ', '*   ', 0, 850, 0.3)
      call gdrawt (3.,2.,'HEAD ON VIEW',.5,0.,2,0)
      call gdrawt (3.,1.,'SOS',.5,0.,2,0)
      call gdraw ('SHUT', 0., 0., 90., 10.0, 8.5,0.25,0.25)
      call s_one_ev_track
      call gdclos (5)
      call gdshow (5)
      call gdshow (5)
      
*     It's already been stored, so now make everything visible again for 
*     the next pass
      call gsatt ('HDX1','SEEN',1)
      call gsatt ('HDY1','SEEN',1)
      call gsatt ('HDX2','SEEN',1)
      call gsatt ('HDY2','SEEN',1)
      do iscin=1,LOWER_HODO_X_PADDLES
         write(scinname,'(a,a)') 'H1X',char(64 + iscin)
         call gsatt (scinname,'SEEN',1)
      enddo
      do iscin=1,LOWER_HODO_Y_PADDLES
         write(scinname,'(a,a)') 'H1Y',char(64 + iscin)
         call gsatt (scinname,'SEEN',1)
      enddo
      do iscin=1,UPPER_HODO_X_PADDLES
         write(scinname,'(a,a)') 'H2X',char(64 + iscin)
         call gsatt (scinname,'SEEN',1)
      enddo
      do iscin=1,UPPER_HODO_Y_PADDLES
         write(scinname,'(a,a)') 'H2Y',char(64 + iscin)
         call gsatt (scinname,'SEEN',1)
      enddo
      do ilayer =1,SMAX_CAL_COLUMNS
         write(layername,'(a,i1)') 'LAY',ilayer
         call gsatt (layername,'SEEN',1)
         do irow = 1,SMAX_CAL_ROWS
            write(blockname,'(a,i1,a)') 'BL',ilayer,char(64 + irow)
            call gsatt (blockname,'SEEN',1)
         enddo
      enddo
      call gdelet (5)
*     
      end
