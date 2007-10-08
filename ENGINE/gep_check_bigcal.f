      subroutine gep_check_bigcal(X_H,Y_H,E_H)

      implicit none
      save

      character*16 here
      parameter(here='gep_check_bigcal')

      real X_H,Y_H,E_H

      include 'gep_data_structures.cmn'
      include 'bigcal_data_structures.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'bigcal_geometry.cmn'
      include 'bigcal_shower_parms.cmn'

      integer rowexpect,colexpect,row,col,colcenter,maxcol,cell
      integer nsearch_y,nsearch_x
      integer i,j,k
      integer nbad_near

c      integer nguessclust,nrealclust
      integer nclustbefore,nclustafter

      integer rowbad(10),colbad(10),cellbad(10),imax_bad
      real eguess_bad(10)
      real maxebad

      real xdiff_expect,ydiff_expect
c      real xmom_expect,ymom_expect
c      real xroot_predict,yroot_predict
c      integer bestroot
      
      real yedge,xedge ! distance from edge of section

      real csxp,csyp,csxr,csyr

      real diffsq,mindiffsq
      
      integer bestclstr,cellexpect

c$$$      integer ncellclust
c$$$      integer cluster_temp_irow(bigcal_clstr_ncell_max)
c$$$      integer cluster_temp_icol(bigcal_clstr_ncell_max)
c$$$      real cluster_temp_ecell(bigcal_clstr_ncell_max)
c$$$      real cluster_temp_xcell(bigcal_clstr_ncell_max)
c$$$      real cluster_temp_ycell(bigcal_clstr_ncell_max)
      
c      logical fixed_any

      logical abort
      character*80 err

      csxp = bigcal_prot_size_x
      csyp = bigcal_prot_size_y
      csxr = bigcal_rcs_size_x
      csyr = bigcal_rcs_size_y

c      fixed_any = .false.

c      write(*,*) 'checking for bad BigCal channels'//
c     $     'near the expected electron'

      yedge = Y_H - bigcal_prot_shift_y

      if(yedge.le.32.*csyp) then
         rowexpect = int(yedge/csyp) + 1
      else
         yedge = yedge - 32.*csyp
         rowexpect = int(yedge/csyr) + 33
      endif

      if(rowexpect.lt.1) rowexpect = 1
      if(rowexpect.gt.56) rowexpect = 56

      if(rowexpect.le.32) then
         xedge = X_H - bigcal_prot_shift_x
         colexpect = int(xedge/csxp) + 1
         if(colexpect.lt.1) colexpect = 1
         if(colexpect.gt.32) colexpect = 32

         cellexpect = colexpect + 32*(rowexpect-1)

         nsearch_y = nint(3.*gep_sigma_ydiff/csyp) + 2
         nsearch_x = nint(3.*gep_sigma_xdiff/csxp) + 2

      else
         xedge = X_H - bigcal_rcs_shift_x
         colexpect = int(xedge/csxr) + 1
         if(colexpect.lt.1) colexpect = 1
         if(colexpect.gt.30) colexpect = 30

         cellexpect = colexpect + 30*(rowexpect-33) + 1024

         nsearch_y = nint(3.*gep_sigma_ydiff/csyr) + 2
         nsearch_x = nint(3.*gep_sigma_xdiff/csxr) + 2

      endif

      xdiff_expect = bigcal_all_xcenter(cellexpect) - X_H
      ydiff_expect = bigcal_all_ycenter(cellexpect) - Y_H
      
      nbad_near = 0

      do row=max(rowexpect-nsearch_y,1),min(rowexpect+nsearch_y,56)
         if(rowexpect.le.32) then
            if(row.le.32) then
               colcenter = colexpect
               maxcol = 32
            else
               colcenter = bigcal_ixclose_prot(colexpect)
               maxcol = 30
            endif
         else 
            if(row.gt.32) then
               colcenter = colexpect
               maxcol = 30
            else
               colcenter = bigcal_ixclose_rcs(colexpect)
               maxcol = 32
            endif
         endif
         
         do col=max(colcenter-nsearch_x,1),min(colcenter+nsearch_x,maxcol)
            if(row.le.32) then
               cell = col + 32*(row-1)
            else
               cell = col + 30*(row-33) + 1024
            endif

            if(bigcal_bad_chan_list(cell)) then
               nbad_near = nbad_near + 1

               if(nbad_near.gt.10) goto 100 ! probably time to give up well before this ever happens
               
               rowbad(nbad_near) = row
               colbad(nbad_near) = col
               cellbad(nbad_near) = cell

            endif
         enddo
      enddo

c     if yes, then check if there are any clusters already in the vicinity:

      if(nbad_near.gt.0) then

*     !write(*,*) '(X_H,Y_H)=(',X_H,', ',Y_H,')'
*         !write(*,*) '(rowexpect,colexpect)=(',rowexpect,', ',
*     $        colexpect,')'

c     guess the energy in each bad channel:

*         !write(*,*) '(xdiffexpect,ydiffexpect)=(',xdiff_expect,', ',
*     $        ydiff_expect,')'
*         !write(*,*) '(nsearch_x,nsearch_y)=(',nsearch_x,', ',nsearch_y,
*     $        ')'

         call b_guess_ecell(nbad_near, 10, rowbad, colbad, cellbad,eguess_bad,E_H,X_H,Y_H)

         bestclstr = 0
         
         do i=1,bigcal_all_nclstr
            if(rowexpect-nsearch_y .le. bigcal_all_clstr_iymax(i) .and.
     $           bigcal_all_clstr_iymax(i).le.rowexpect+nsearch_y ) then
               if(rowexpect .le. 32) then
                  if(bigcal_all_clstr_iymax(i).le.32) then
                     if(colexpect-nsearch_x.le.bigcal_all_clstr_ixmax(i).and.
     $                    bigcal_all_clstr_ixmax(i).le.colexpect + nsearch_x) then
                        diffsq = float(bigcal_all_clstr_ixmax(i)-colexpect)**2 +
     $                       float(bigcal_all_clstr_iymax(i)-rowexpect)**2
                        if(i.eq.1.or.diffsq.lt.mindiffsq) then
                           mindiffsq = diffsq
                           bestclstr = i
                        endif
                     endif
                  else 
                     if(bigcal_ixclose_prot(colexpect)-nsearch_x.le.bigcal_all_clstr_ixmax(i)
     $                    .and. bigcal_all_clstr_ixmax(i).le.bigcal_ixclose_prot(colexpect)
     $                    + nsearch_x) then
                        diffsq = float(bigcal_all_clstr_ixmax(i)-bigcal_ixclose_prot(colexpect))**2
     $                       + float(bigcal_all_clstr_iymax(i)-rowexpect)**2
                        if(i.eq.1.or.diffsq.lt.mindiffsq) then
                           mindiffsq = diffsq
                           bestclstr = i
                        endif
                     endif
                  endif
               else
                  if(bigcal_all_clstr_iymax(i).le.32) then
                     if(bigcal_ixclose_rcs(colexpect)-nsearch_x.le.bigcal_all_clstr_ixmax(i)
     $                    .and. bigcal_all_clstr_ixmax(i).le.bigcal_ixclose_rcs(colexpect) +
     $                    nsearch_x) then
                        diffsq = float(bigcal_all_clstr_ixmax(i)-bigcal_ixclose_rcs(colexpect))**2
     $                       + float(bigcal_all_clstr_iymax(i)-rowexpect)**2
                        if(i.eq.1.or.diffsq.lt.mindiffsq) then
                           mindiffsq = diffsq
                           bestclstr = i
                        endif
                     endif
                  else
                     if(colexpect - nsearch_x.le.bigcal_all_clstr_ixmax(i).and.
     $                    bigcal_all_clstr_ixmax(i).le.colexpect + nsearch_x) then
                        diffsq = float(bigcal_all_clstr_ixmax(i)-colexpect)**2
     $                       + float(bigcal_all_clstr_iymax(i)-rowexpect)**2
                        if(i.eq.1.or.diffsq.lt.mindiffsq) then
                           mindiffsq = diffsq
                           bestclstr = i
                        endif
                     endif
                  endif
               endif
            endif
         enddo
c     if there is a cluster near the expected position, try and "fill in the blanks" of any bad 
c     cells in the cluster. If we found a cluster despite a bad channel, then chances are it 
c     already contains a significant fraction of the energy of the cluster. If there is no cluster,
c     then chances are the maximum should have been in one of the bad channels.

         if(bestclstr.gt.0) then
            if(bigcal_all_clstr_nbadlist(bestclstr).gt.0) then ! try to fill in the blanks:
               do i=1,bigcal_all_clstr_ncell(bestclstr)
                  if(bigcal_clstr_bad_chan(bestclstr,i)) then
                     do j=1,nbad_near
                        if(rowbad(j).eq.bigcal_all_clstr_iycell(bestclstr,i) 
     $                       .and. colbad(j).eq.bigcal_all_clstr_ixcell(bestclstr,i)
     $                       ) then
                           bigcal_all_clstr_ecell(bestclstr,i) = eguess_bad(j)
c                           bigcal_clstr_bad_chan(bestclstr,i) = .false.
                        endif
                     enddo
                  endif
               enddo
c     now re-sort the cluster and recalculate important quantities:
               call b_rebuild_cluster(bestclstr)
c               fixed_any = .true.
            endif
         else 
*     no cluster was found: try to build a cluster around the bad cell with biggest "guessed" amplitude 
*     and see if we find any nearby hits. It OUGHT to be the case that the "good hit" array still contains 
*     the surrounding hits, because the good hit array only gets zeroed if we find a cluster, HOWEVER, it is
*     also quite possible that the good "detector" array hits around our maximum have been zeroed, so it is 
*     useful here to re-initialize them from the "protvino" and "rcs" detector arrays which don't get zeroed
*     during the cluster finding. In retrospect it was good to keep those arrays around because they serve to
*     remember the values of each channel that might get zeroed in the cluster finding which uses the 
*     "all detector" array.
            do i=1,nbad_near
               if(eguess_bad(i).gt.maxebad .or. i.eq.1) then
                  maxebad = eguess_bad(i)
                  imax_bad = i
               endif
            enddo

            if(maxebad.gt.b_min_emax) then
*     initialize the signals in the max. cell and surrounding cells:
               do row=max(rowbad(imax_bad)-nsearch_y,1),min(rowbad(imax_bad)+nsearch_y,56)
                  if(rowexpect.le.32) then
                     if(row.le.32) then
                        colcenter = colbad(imax_bad)
                        maxcol = 32
                     else
                        colcenter = bigcal_ixclose_prot(colbad(imax_bad))
                        maxcol = 30
                     endif
                  else 
                     if(row.gt.32) then
                        colcenter = colbad(imax_bad)
                        maxcol = 30
                     else
                        colcenter = bigcal_ixclose_rcs(colbad(imax_bad))
                        maxcol = 32
                     endif
                  endif
                  
                  do col=max(colcenter-nsearch_x,1),min(colcenter+nsearch_x,maxcol)
                     if(row.le.32) then
                        cell = col + 32*(row-1)
                        bigcal_all_good_det(cell) = bigcal_prot_good_det(cell)
                     else
                        cell = col + 30*(row-33) + 1024
                        bigcal_all_good_det(cell) = bigcal_rcs_good_det(cell-1024)
                     endif
                  enddo
               enddo

               bigcal_all_ngood = 1
               bigcal_all_iygood(1) = rowbad(imax_bad)
               bigcal_all_ixgood(1) = colbad(imax_bad)
               bigcal_all_ecell(1) = maxebad
               
               nclustbefore = bigcal_all_nclstr

*               !write(*,*) 'No cluster near expected electron: '//
*     $              'building new cluster around expected max!'

               call b_find_clusters(bigcal_all_nclstr,bigcal_nmaxima,abort,err)

               nclustafter = bigcal_all_nclstr
               call b_calc_shower_coord(abort,err)
c     for a new cluster we also need to calculate the time if possible.
               if(bbypass_calc_cluster_time.eq.0) then
                  call b_calc_cluster_time(abort,err)
               endif
c     require at least half of the blocks in any new cluster to be real hits
               if(nclustafter.eq.nclustbefore + 1) then
                  if(bigcal_all_clstr_ncell(nclustafter) - 
     $                 bigcal_all_clstr_nbadlist(nclustafter).lt.
     $                 bigcal_all_clstr_ncell(nclustafter)/2) then ! don't include this cluster
                     bigcal_all_nclstr = bigcal_all_nclstr - 1
                  else
c      !write(*,*)'built new cluster around expected maximum!'
                  endif
               endif
            endif
         endif
      endif

 100  continue

      return 
      end

      
