      subroutine s_one_ev_det_reset
*
* This routine will reset the hit indicators for the detector elements
* Hall C
*
* July 1995  Derek van Westrum (vanwestr@cebaf.gov)
*
* $Log: s_one_ev_det_reset.f,v $
* Revision 1.2  1996/01/17 16:40:27  cdaq
* (SAW) Change an include file name
*
* Revision 1.1  1995/07/31 15:24:52  cdaq
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
      character*5 blockname
      integer istrip
      integer ihodo
      integer ilayer
      integer iblock
      character*4 wire
      integer ichamber
      integer isector
      integer iwire
*
*     First, clear the lower xpaddles
*
      do istrip=1,LOWER_HODO_X_PADDLES
         write (scinname,'(a,a)') 'H1X',char(64 + istrip)
         call gsatt (scinname,'COLO',1)
         call gsatt (scinname,'FILL',0)
      enddo
*
*       next, clear the upper x hodoscopes:
*
      do istrip=1,UPPER_HODO_X_PADDLES
	 write (scinname,'(a,a)') 'H2X',char(64 + istrip)
	 call gsatt (scinname,'COLO',1)
         call gsatt (scinname,'FILL',0)
      enddo
*
*       now clear the lower Y hodoscopes:
*
      do istrip=1,LOWER_HODO_Y_PADDLES
	 write (scinname,'(a,a)') 'H1Y',char(64 + istrip)
	 call gsatt (scinname,'COLO',1)
         call gsatt (scinname,'FILL',0)
      enddo
*
*       finally, clear the upper y hodoscopes:
*
      do istrip=1,UPPER_HODO_Y_PADDLES
	 write (scinname,'(a,a)') 'H2Y',char(64 + istrip)
	 call gsatt (scinname,'COLO',1)
         call gsatt (scinname,'FILL',0)
      enddo
*
*  Now clear the shower counter blocks.
*
      do ilayer=1,smax_cal_columns
	 do iblock=1,smax_cal_rows
	  write (blockname,'(a,i1,a)') 'BL',ilayer,CHAR(64 + iblock)
	  call gsatt (blockname,'COLO',1)
          call gsatt (blockname,'FILL',0)
       enddo
      enddo
*
*  Now clear the wire chambers...
*
*  first the U wires...
      do ichamber=1,2
        do isector=1,4
          do iwire = 1,24
             write (wire,'(a,a,a,a)') char(64 + ichamber),'U',
     $             char(64 + isector),char(64 + iwire)
	     call gsatt (wire,'COLO',1)
	     call gsatt (wire,'SEEN',0)
             call gsatt (wire,'FILL',0)
           enddo
         enddo
       enddo
*  then the X wires
      do ichamber=1,2
        do isector=1,8
          do iwire = 1,16
             write (wire,'(a,a,a,a)') char(64 + ichamber),'X',
     $             char(64 + isector),char(64 + iwire)
	     call gsatt (wire,'COLO',1)
	     call gsatt (wire,'SEEN',0)
             call gsatt (wire,'FILL',0)
           enddo
         enddo
       enddo
*  then the V wires
      do ichamber=1,2
        do isector=1,4
          do iwire = 1,24
             write (wire,'(a,a,a,a)') char(64 + ichamber),'V',
     $             char(64 + isector),char(64 + iwire)
	     call gsatt (wire,'COLO',1)
	     call gsatt (wire,'SEEN',0)
             call gsatt (wire,'FILL',0)
           enddo
         enddo
       enddo

      end
