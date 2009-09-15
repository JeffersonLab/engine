      SUBROUTINE H_TRACK(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods :  Finds and fits tracks in HMS focal plane 
*-
*-      Required Input BANKS     HMS_DECODED_DC
*-
*-      Output BANKS             HMS_FOCAL_PLANE
*-                               HMS_DECODED_DC hit coordinates
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 19-JAN-1994   D. F. Geesaman
* $Log$
* Revision 1.5.26.1  2009/09/15 20:37:39  jones
* Add code to track with single stub
*
* Revision 1.5  1996/09/04 13:37:02  saw
* (JRA) Initialize hstubmin variables
*
* Revision 1.4  1995/10/11 12:19:50  cdaq
* (JRA) Only call tracking routines when it is warranted
*
* Revision 1.3  1995/05/22 19:39:30  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/04/13  17:07:57  cdaq
* (DFG) Added histograming call (h_fill_dc_fp_hist)
*
* Revision 1.1  1994/02/19  06:20:31  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*7 here
      parameter (here= 'H_TRACK')
*
      logical ABORT
      character*(*) err
      integer*4 ierr
      character*5  line_err
      integer*4 i,j,tot_hit,num_trk,k,m,n,num_extra
      integer*4 itrk1,exhits,p
      integer*4 hit(25),hits,goodhit(12),nhit
      integer*4 extra_index(6)
      real*4 hchi2_min
      logical hit_in_space_point
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_scin_tof.cmn'
      include 'hms_scin_parms.cmn'
      include "hms_geometry.cmn"

*
*--------------------------------------------------------
*
*
      ABORT = .false.
      err = ' '


c      hchi2_min=10000
             
      if (hdc_tot_hits.ne.0) then
         call H_PATTERN_RECOGNITION(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif
     
*
         if (hnspace_points_tot.ne.0) then
            call H_LEFT_RIGHT(ABORT,err)
            if(ABORT) then
               call G_add_path(here,err)
               return
            endif
*
            hstubminx = 999999.
            hstubminy = 999999.
            hstubminxp = 999999.
            hstubminyp = 999999.
            call H_LINK_STUBS(ABORT,err)
            if(ABORT) then
               call G_add_path(here,err)
               return
            endif


            if (hntracks_fp.ne.0) then ! both chambers havs space points
c               write(*,*) 'use two stubs '
               hstubs=2
               call H_TRACK_FIT(ABORT,err,ierr)

               if(ABORT) then
                  call G_add_path(here,err)
                  return
               endif

*     Check for internal error in H_TRACK_FIT
               if(ierr.ne.0) then
                  line_err=' '
                  call CSETDI(ierr,line_err,1,5)
                  err='ERROR IN H_TRACK_FIT' // line_err
                  call G_add_path(here,err)
                  call G_LOG_MESSAGE(err)
               endif                    
*     histogram focal plane tracks
*
c               call h_fill_dc_fp_hist(ABORT,err)
c               if(ABORT) then
c                  call g_add_path(here,err)
c                  return
c               endif
*
         
            else                !only 1 chamber has a stub, hntracks_fp = 0

****************USE ONLY 1 CHAMBER STUB ******************
               hstubs=1
               if (hdc_tot_hits .lt. 12) then
c                  write(*,*) 'using one stub'
c                  write(*,*) ' hdc_tot_hits = ',hdc_tot_hits
c                  write(*,*) ' hit  , plane, wire, wire center'

                  do i=1,hdc_tot_hits
c                     write(*,*) i,hdc_plane_num(i),hdc_wire_num(i),hdc_wire_coord(i)
c     >                    ,(hplane_coeff(j,hdc_plane_num(i)),j=3,6),hdc_sigma(hdc_plane_num(i))
                  enddo
c                  write(*,*) ' chamber hits =',hncham_hits(1),hncham_hits(2)
c                  write(*,*) ' hnspace_points_tot = ',hnspace_points_tot

                  
                  if(hnspace_points_tot.eq.1) then
******************fit space point hits **********************************************************************

                     hntracks_fp=1
                     if (hspace_point_hits(hntracks_fp,1) .gt. 4) then 
                     hntrack_hits(hntracks_fp,1)= hspace_point_hits(1,1)                   
                     do i=3,hspace_point_hits(1,1)
                        hntrack_hits(hntracks_fp,i-1)= hspace_point_hits(1,i)
                     enddo
                     
                     call H_TRACK_FIT(ABORT,err,ierr)
c                     write(*,*) 'fit space point ',hntrack_hits(hntracks_fp,1)  
c                     write(*,*) hx_fp(1),hy_fp(1),hxp_fp(1),hyp_fp(1),hchi2_fp(1)

                     do i=2,hntrack_hits(hntracks_fp,1)+1
                        j=hdc_plane_num(hntrack_hits(hntracks_fp,i))
                     enddo
                  endif
*************** use projections from the stub *******************************************************************
**********
                     num_extra = 0

                     if (hspace_point_hits(hntracks_fp,1) .gt. 4) then
                     do J=1,hdc_tot_hits
                        hit_in_space_point = .false.
                        do i=1,hspace_point_hits(hntracks_fp,1)
                           if ( j .eq. hspace_point_hits(hntracks_fp,i+2)) hit_in_space_point = .true.
                           if ( hdc_plane_num(j).eq.hdc_plane_num(hspace_point_hits(hntracks_fp,i+2))) 
     >                          hit_in_space_point = .true.
                        enddo
                        if ( .not. hit_in_space_point ) then
                           if((hdc_track_coord(hntracks_fp,hdc_plane_num(j)) - HDC_WIRE_CENTER(j)).lt.0) then
                              HDC_WIRE_COORD(j)= HDC_WIRE_CENTER(j) - HDC_DRIFT_DIS(j)
                           else
                              HDC_WIRE_COORD(j)= HDC_WIRE_CENTER(j) + HDC_DRIFT_DIS(j)
                           endif
c                           write(*,*) 'hdc_track_coord,HDC_WIRE_COORD'
c                           write(*,*) hdc_track_coord(hntracks_fp,hdc_plane_num(j)),HDC_WIRE_COORD(j)
                           if(abs(hdc_track_coord(hntracks_fp,hdc_plane_num(j))-HDC_WIRE_COORD(j)).lt.2) then 

c                              write(*,*) hdc_track_coord(hntracks_fp,hdc_plane_num(j)),HDC_WIRE_COORD(j)
                                     
                              num_extra = num_extra + 1
                              extra_index(num_extra) = j
                           endif

                        endif
                     enddo
                     endif
                    
c                     write(*,*) 'num_extra',num_extra

***********************fit the stub with 0 and 1 extra hits and select the minimum chi2*********************
                     if(num_extra+ hspace_point_hits(hntracks_fp,1).gt.4) then 
                     if(num_extra.eq.1) then ! use only stub fit
                       
                           hntracks_fp=hntracks_fp+1
                                     
                           do i=3,hspace_point_hits(1,1)+2
                              hntrack_hits(hntracks_fp,i-1)= hspace_point_hits(1,i)
                           enddo

                              hntrack_hits(hntracks_fp,i-1)=extra_index(num_extra)
                              hntrack_hits(hntracks_fp,1)=  hspace_point_hits(1,1)+1
c                              write(*,*) 'fit total hits',hntrack_hits(hntracks_fp,1)
                         
                           call H_TRACK_FIT(ABORT,err,ierr)
c                           write(*,*) 'fit  stub hits + extra one ',hntrack_hits(hntracks_fp,1)  
c                           write(*,*) hx_fp(hntracks_fp),hy_fp(hntracks_fp),hxp_fp(hntracks_fp)
c     >                          ,hyp_fp(hntracks_fp),hchi2_fp(hntracks_fp)

                           do i=2,hntrack_hits(hntracks_fp,1)+1
                              j=hdc_plane_num(hntrack_hits(hntracks_fp,i))
c                              write(*,*) hntrack_hits(hntracks_fp,i),j,hdc_single_residual(hntracks_fp,j)
                           enddo
                       
c                        do itrk1=1,hntracks_fp

c                           if(hchi2_fp(itrk1).lt.hchi2_min) then
c                              hchi2_min=hchi2_fp(itrk1)
c                           endif
c                        enddo
c                        write(*,*) ' hchi2_min',hchi2_min
                       
c                     do itrk1=1,hntracks_fp

c                        if(hchi2_fp(itrk1).eq.hchi2_min) then
c                           write(*,*) 'hntracks_fp',itrk1
c                           write(*,*) 'hit index',hntrack_hits(itrk1,k-1),hntrack_hits(itrk1,k)
c                           write(*,*) 'dc planes',hdc_plane_num(hntrack_hits(itrk1,k-1)),
c     >                          hdc_plane_num(hntrack_hits(itrk1,k))

c                           call H_TRACK_FIT(ABORT,err,ierr)
c                           write(*,*) hx_fp(itrk1),hy_fp(itrk1),hxp_fp(itrk1),
c     <                          hyp_fp(itrk1),hchi2_fp(itrk1)

c                           do i=2,hntrack_hits(itrk1,1)+1
c                              j=hdc_plane_num(hntrack_hits(itrk1,i))
c                              write(*,*) hntrack_hits(itrk1,i),j,hdc_single_residual(itrk1,j)
c                           enddo
c                        endif
c                     enddo
                        
                     else
***************************fit tracks using  all extra hits excluding one at each time ************************
                   
                    do m=1,num_extra 
                       hntracks_fp=hntracks_fp+1
                       
                       do k=3,hspace_point_hits(1,1)+2
                          hntrack_hits(hntracks_fp,k-1)=  hspace_point_hits(1,k)
                       enddo
                       hits=0
                       do n=1,num_extra
                          if(n.ne.m) then
                           
                             hits=hits+1

                             hntrack_hits(hntracks_fp,hits-1+k-1)=extra_index(n) 
                          endif
                       enddo  
                         
                       hntrack_hits(hntracks_fp,1)=  hspace_point_hits(1,1)+num_extra-1

                       call H_TRACK_FIT(ABORT,err,ierr)
c                       write(*,*) 'fit excluding extra hit',m,'total',hntrack_hits(hntracks_fp,1)  
c                       write(*,*) hx_fp(hntracks_fp),hy_fp(hntracks_fp),hxp_fp(hntracks_fp),
c     <                      hyp_fp(hntracks_fp),hchi2_fp(hntracks_fp)

                       do i=2,hntrack_hits(hntracks_fp,1)+1
                          j=hdc_plane_num(hntrack_hits(hntracks_fp,i))
c                          write(*,*) hntrack_hits(hntracks_fp,i),j,hdc_single_residual(hntracks_fp,j)
                       enddo
                    
                    enddo
                  
                    
**************find the track from above  with minimum hchi2 value *****************************                     
c                     do itrk1=1,hntracks_fp

c                        if(hchi2_fp(itrk1).lt.hchi2_min) then
c                           hchi2_min=hchi2_fp(itrk1)
c                        endif
c                     enddo
c                     write(*,*) ' hchi2_min',hchi2_min
                     
**************use the track with the minimum hchi2 value with the stub hits **********************

c                     do itrk1=1,hntracks_fp

c                        if(hchi2_fp(itrk1).eq.hchi2_min) then
c                           write(*,*) 'hntracks_fp',itrk1
c                           write(*,*) 'hit index',hntrack_hits(itrk1,k-1)
c                           write(*,*) 'dc planes',hdc_plane_num(hntrack_hits(itrk1,k-1))                

c                           call H_TRACK_FIT(ABORT,err,ierr)
c                           write(*,*) hx_fp(itrk1),hy_fp(itrk1),hxp_fp(itrk1),
c     <                          hyp_fp(itrk1),hchi2_fp(itrk1)

c                           do i=2,hntrack_hits(itrk1,1)+1
c                              j=hdc_plane_num(hntrack_hits(itrk1,i))
c                              write(*,*) hntrack_hits(itrk1,i),j,hdc_single_residual(itrk1,j)
c                           enddo
c                        endif
c                     enddo
c                     endif
                 
**************************USE SCINTILLATOR HITS *****************************************************
cc                  write(*,*)' hscin_tot_hits = ',hscin_tot_hits
                 
c                     do i=1,hscin_tot_hits
                     
c                        if (htwo_good_times(i)) then
cc                        write(*,*) i,hscin_plane_num(i),hscin_counter_num(i)
cc     >                       ,hhodo_center(hscin_plane_num(i),hscin_counter_num(i)),hscin_zpos(i)
                        
c                           scingoodhit=scingoodhit+1
c                           hit(scingoodhit)=i
                  
c                        endif
cc                     write(*,*) scingoodhit, hit(scingoodhit)
c                     enddo

cc                  write(*,*) 'scin good hits =',scingoodhit
c                     tot_hit=hdc_tot_hits+scingoodhit
cc                  write(*,*) 'tot_hit',tot_hit

*******************************
                 endif          ! (num_extra+ hspace_point_hits(hntracks,1)) .gt. 4) 
              endif             ! ( number of extra)
           endif                ! (hnspace_points_tot.eq.1)
        endif                   !(hdc_tot_hits .lt. 12)
              
      endif                     !(hntracks_fp.ne.0)

******************************USE ALL THE HITS WHICH DOESN'T MAKE ANY STUBS *****************************
            else                !(hnspace_points_tot.eq.0) ,hits<min_hits(4)
               hstubs=0
               if (hdc_tot_hits .lt. 12) then
c                  write(*,*) 'no any stub'
c                  write(*,*) ' hdc_tot_hits = ',hdc_tot_hits
c                  write(*,*) ' hit  , plane, wire, wire center'

                  do i=1,hdc_tot_hits
c                     write(*,*) i,hdc_plane_num(i),hdc_wire_num(i),hdc_wire_coord(i)
c     >                    ,(hplane_coeff(j,hdc_plane_num(i)),j=3,6),hdc_sigma(hdc_plane_num(i))
                  enddo
c                  write(*,*) ' chamber hits =',hncham_hits(1),hncham_hits(2)
c                  write(*,*) ' hnspace_points_tot = ',hnspace_points_tot
                  if((hncham_hits(1).ne.0).and.(hncham_hits(2).ne.0) .and.(hdc_tot_hits .gt. 4)) then
                     hntracks_fp=1
                     hntrack_hits(hntracks_fp,1)=hdc_tot_hits                 
                     do i=1,hdc_tot_hits
                        hntrack_hits(hntracks_fp,i+1)= i
                     enddo

                     call H_TRACK_FIT(ABORT,err,ierr)
c                     write(*,*) 'num of hits used to fit',hdc_tot_hits
c                     write(*,*) 'hntracks_fp=',hntracks_fp   
c                     write(*,*) hx_fp(1),hy_fp(1),hxp_fp(1),hyp_fp(1),hchi2_fp(1)

                     do i=2,hntrack_hits(hntracks_fp,1)+1
                        j=hdc_plane_num(hntrack_hits(hntracks_fp,i))
c                        write(*,*) hntrack_hits(hntracks_fp,i),j,hdc_single_residual(hntracks_fp,j)
                     enddo
                  endif
               endif
*******************************
            endif               !(hnspace_points_tot.ne.0) **************

*     histogram focal plane tracks
*
            call h_fill_dc_fp_hist(ABORT,err)
            if(ABORT) then
               call g_add_path(here,err)
               return
            endif

         endif                  !(hdc_tot_hits.ne.0)

         return
         end




