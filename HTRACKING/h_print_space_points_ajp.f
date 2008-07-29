      subroutine h_print_space_points_ajp(abort,err)
      
      implicit none
      save
      
      character*24 here
      parameter (here='h_print_space_points_ajp')

      logical abort
      character*(*) err
      
      integer isp,ichbr,ihit,nhit,plane,wire,i
      real xstub,ystub,xpstub

      external h_chamnum
      integer*4 h_chamnum

      include 'hms_data_structures.cmn'
      include 'hms_tracking.cmn'
      include 'hms_geometry.cmn'
      include 'gen_event_info.cmn'

      abort=.false.
      err=' '

      write(hluno,100) 'HMS space point details and stub linking, '//
     $     'event #',gen_event_id_number

 100  format(A72,I12)

      write(hluno,'('' chamber='',i3,'' number of hits='',i3)')
     &         (i,hncham_hits(i),i=1,hdc_num_chambers)
      write(hluno,'('' Total number of space points found='',i3)')
     &         hnspace_points_tot
      write(hluno,'('' chamber number'',i2,''  number of points='',i3)')
     &     (i,hnspace_points(i),i=1,hdc_num_chambers)
      write(hluno,101) 'hstubtest=',hstubtest

 101  format(A20,I3)
      
      do isp=1,hnspace_points_tot
         write(hluno,102) 'Space point #',isp,', chamber ',h_chamnum(isp)
 102     format(2(A15,I5))
         write(hluno,103) 'N hits = ',hspace_point_hits(isp,1)
         write(hluno,103) 'N combos = ',hspace_point_hits(isp,2)

 103     format(A12,I5)
         
         write(hluno,105) '(x,y,xp,chi2)=(',hbeststub(isp,1),',',hbeststub(isp,2),
     $        ',',hbeststub(isp,3),',',h_stub_chi2perdf(isp),')'
 105     format(A15,4(F12.3,A2))

         write(hluno,104) 'hit = ','plane = ','wire = ','wpos = ',
     $        'ddist = '
 104     format(5A10)
         do ihit=1,hspace_point_hits(isp,1)
            plane = hdc_plane_num(hspace_point_hits(isp,ihit+2))
            wire = hdc_wire_num(hspace_point_hits(isp,ihit+2))
            write(hluno,106) hspace_point_hits(isp,ihit+2),plane,wire,
     $           hdc_wire_center(hspace_point_hits(isp,ihit+2)),
     $           hdc_drift_dis(hspace_point_hits(isp,ihit+2))
 106        format(3I10,2F10.4)
         enddo
      enddo

      return
      end
