*=======================================================================
      subroutine s_clusters_cal(abort,errmsg)
*=======================================================================
*-
*-      Purpose: Finds clusters in the calorimeter and computes
*-               for each cluster it's size(number of hit blocks),
*-               position, energy deposition in the calorimeter
*-               columns and the total energy deposition.
*-               The energy depositions are not corrected yet for
*-               impact point coordinate dependence.
*-               A cluster is defined as a set of adjacent hit blocks
*-               which share a common edge or a corner. Any two hits
*-               from different clusters are separated by at least one 
*-               block which has not fired.
*-
*-      Input Banks: SOS_SPARSIFIED_CAL, SOS_DECODED_CAL
*-
*-      Output Bank: SOS_CLUSTERS_CAL
*-
*-      Created: 15 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                Change name of print routine
*-               11 Apr 1994      DFG Check if E_t =0 before division
* $Log: s_clusters_cal.f,v $
* Revision 1.4  1999/02/03 21:13:44  saw
* Code for new Shower counter tubes
*
* Revision 1.3  1995/05/22 19:45:34  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/11/22  21:08:54  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.1  1994/04/13  18:09:34  cdaq
* Initial revision
*
*-----------------------------------------------------------------------
*
*
      implicit none
      save
*
      logical abort
      character*(*) errmsg
      character*14 here
      parameter (here='S_CLUSTERS_CAL')
*
      integer*4 nc                     !Cluster number
      integer*4 ihit,jhit,khit,nh      !Internal loop counters.
      integer*4 hits_tagged            !Current number of tagged hits.
      integer*4 irow,icol,jrow,jcol,col!Row and column indecies
      integer*4 d_row,d_col            !Distance between rows(columns)
      logical tagged
*
      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
*
*
      snclusters_cal=0
      if(scal_num_hits.le.0) go to 100   !Return
*
      do ihit=1,smax_cal_blocks
        scluster_hit(ihit)=0
      enddo
*
      nc                 = 1
      scluster_hit(1)    =-1
      scluster_size(1)   = 1
      hits_tagged        = 1
      tagged             =.true.
*
*      Find the clusters.
*
*
*-----Loop untill all the hits are tagged
      do while(hits_tagged.le.scal_num_hits)
*
*--------Loop untill there are no more hits
*--------in the current cluster to be tagged
        do while(tagged)
          tagged=.false.
*
*-----------Loop over all the hits
          do ihit=1,scal_num_hits
*
*--------------and find a hit("seed") which belongs to the
*--------------current cluster, but it's neighbors are not tagged
            if(scluster_hit(ihit).lt.0) then
              irow  =scal_rows(ihit)
              icol  =scal_cols(ihit)
*
*-----------------Loop over all the hits
              do jhit=1,scal_num_hits
*
*--------------------and find hits which are not tagged yet
                if(scluster_hit(jhit).eq.0) then
                  jrow =scal_rows(jhit)
                  jcol =scal_cols(jhit)
                  d_row=iabs(jrow-irow)
                  d_col=iabs(jcol-icol)
*
*-----------------------Are these hits a neighbor to "seed"?
                  if(d_row.le.1.and.d_col.le.1) then
*
*--------------------------Assign them to the same current cluster
                    scluster_hit(jhit)=scluster_hit(ihit)
                    scluster_size(nc) =scluster_size(nc)+1
                    hits_tagged       =hits_tagged+1
                    tagged            =.true.
*
                  endif                 !End ... if neighbor of "seed"
*
                endif                   !End ... if not scanned yet
*
              enddo                     !End loop over all hits
*
*-----------------All the neighbors of "seed" were scanned
              scluster_hit(ihit)=-scluster_hit(ihit)
*
            endif                       !End ... if "seed"
*
          enddo                         !End loop over all hits
*
        enddo                           !All the hits of the current cluster were tagged
*
*--------Initialize to start the search for the next cluster
        nc            =nc+1
        hits_tagged   =hits_tagged+1
        tagged        =.true.
*
*--------Find a hit which is not tagged
        khit=1
        do while(scluster_hit(khit).ne.0 .AND. KHIT.LT.SMAX_CAL_BLOCKS)
          khit=khit+1
        enddo
*
*--------This will be the new "seed"
        IF (NC.GT.SNCLUSTERS_MAX) NC=SNCLUSTERS_MAX !AVOID OUT/BOUNDS
        scluster_hit(khit)=-nc
        scluster_size(nc) = 1
*
      enddo                             !End. Now all the hits are assigned to some cluster
*
*-----Number of clusters found
      snclusters_cal=nc-1
*
*     For each cluster found, compute the center of gravity in X
*     projection, the energy deposited in succesive calorimeter columns
*     and the total energy deposition
*
      do nc=1,snclusters_max
        scluster_e1_pos(nc)=0.      
        scluster_e1_neg(nc)=0.
        scluster_e2_pos(nc)=0.      
        scluster_e2_neg(nc)=0.     
*
        scluster_e1(nc)=0.
        scluster_e2(nc)=0.
        scluster_e3(nc)=0.
        scluster_e4(nc)=0.
        scluster_et(nc)=0.
        scluster_xc(nc)=0.
      enddo
*
*
      do nh=1,scal_num_hits
        nc =MAX(1,scluster_hit(nh))     !AVOIDS OUT/BOUNDS ERRORS

        col=scal_cols(nh)
*
        scluster_xc(nc)=scluster_xc(nc)+sblock_xc(nh)*sblock_de(nh)
*
        if(col.eq.1) then
          if(scal_num_neg_columns.ge.1) then
            scluster_e1_pos(nc)=scluster_e1_pos(nc)+sblock_de_pos(nh)
            scluster_e1_neg(nc)=scluster_e1_neg(nc)+sblock_de_neg(nh)
            scluster_e1(nc)=scluster_e1_pos(nc)+scluster_e1_neg(nc)
          else
            scluster_e1(nc)=scluster_e1(nc)+sblock_de(nh)
          endif
        else if (col.eq.2) then
          if(scal_num_neg_columns.ge.2) then
            scluster_e2_pos(nc)=scluster_e2_pos(nc)+sblock_de_pos(nh)
            scluster_e2_neg(nc)=scluster_e2_neg(nc)+sblock_de_neg(nh)
            scluster_e2(nc)=scluster_e2_pos(nc)+scluster_e2_neg(nc)
          else
            scluster_e2(nc)=scluster_e2(nc)+sblock_de(nh)
          endif
        else if(col.eq.3) then
          scluster_e3(nc)=scluster_e3(nc)+sblock_de(nh)
        else if(col.eq.4) then
          scluster_e4(nc)=scluster_e4(nc)+sblock_de(nh)
        endif
        scluster_et(nc)=scluster_et(nc)+sblock_de(nh)  ! Is sblock_de de_pos+de_neg?
*
      enddo
*
      do nc=1,snclusters_cal
*     MAKE SURE SCLUSTERS_ET .NE. ZERO SO NO DIVIDE BY ZERO
        if(scluster_et(nc) .gt. 0.) then
          scluster_xc(nc)=scluster_xc(nc)/scluster_et(nc)
        else
          scluster_xc(nc) = -1.
        endif
      enddo
*
 100  continue
      if(sdbg_clusters_cal.gt.0) call s_prt_cal_clusters
*
      return
      end
