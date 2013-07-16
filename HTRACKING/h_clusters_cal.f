*=======================================================================
      subroutine h_clusters_cal(abort,errmsg)
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
*-      Input Banks: HMS_SPARSIFIED_CAL, HMS_DECODED_CAL
*-
*-      Output Bank: HMS_CLUSTERS_CAL
*-
*-      Created: 15 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                Change name of print routine
*                10 Apr 1994      DFG Protect for Et=0 division
* $Log: h_clusters_cal.f,v $
* Revision 1.5  1999/02/03 21:13:23  saw
* Code for new Shower counter tubes
*
* Revision 1.4  1995/05/22 19:39:07  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1994/08/02  20:00:12  cdaq
* (JRA) Catch some out of bounds problems
*
* Revision 1.2  1994/04/13  05:31:37  cdaq
* *** empty log message ***
*
* Revision 1.1  1994/04/12  21:30:02  cdaq
* Initial revision
*
*-
*-----------------------------------------------------------------------
*
*
      implicit none
      save
*
      logical abort
      character*(*) errmsg
      character*14 here
      parameter (here='H_CLUSTERS_CAL')
*
      integer*4 nc                     !Cluster number
      integer*4 ihit,jhit,khit,nh      !Internal loop counters.
      integer*4 hits_tagged            !Current number of tagged hits.
      integer*4 irow,icol,jrow,jcol,col!Row and column indecies
      integer*4 d_row,d_col            !Distance between rows(columns)
      logical tagged
*
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
*
*
      hnclusters_cal=0
      if(hcal_num_hits.le.0) go to 100   !Return
*
      do ihit=1,hmax_cal_blocks
        hcluster_hit(ihit)=0
      enddo
*
      nc                 = 1
      hcluster_hit(1)    =-1
      hcluster_size(1)   = 1
      hits_tagged        = 1
      tagged             =.true.
*
*     Find the clusters.
*
*
*-----Loop untill all the hits are tagged
      do while(hits_tagged.le.hcal_num_hits)
*
*-------Loop untill there are no more hits
*-------in the current cluster to be tagged
        do while(tagged)
          tagged=.false.
*
*---------Loop over all the hits
          do ihit=1,hcal_num_hits
*     
*-----------and find a hit("seed") which belongs to the
*-----------current cluster, but it's neighbors are not tagged
            if(hcluster_hit(ihit).lt.0) then
              irow  =hcal_rows(ihit)
              icol  =hcal_cols(ihit)
*
*-------------Loop over all the hits
              do jhit=1,hcal_num_hits
*
*---------------and find hits which are not tagged yet
                if(hcluster_hit(jhit).eq.0) then
                  jrow =hcal_rows(jhit)
                  jcol =hcal_cols(jhit)
                  d_row=iabs(jrow-irow)
                  d_col=iabs(jcol-icol)
*
*-----------------Are these hits a neighbor to "seed"?
                  if(d_row.le.1.and.d_col.le.1) then
*
*-------------------Assign them to the same current cluster
                    hcluster_hit(jhit)=hcluster_hit(ihit)
                    hcluster_size(nc) =hcluster_size(nc)+1
                    hits_tagged       =hits_tagged+1
                    tagged            =.true.
*
                  endif                 !End ... if neighbor of "seed"
*
                endif                   !End ... if not scanned yet
*
              enddo                     !End loop over all hits
*
*-------------All the neighbors of "seed" were scanned
              hcluster_hit(ihit)=-hcluster_hit(ihit)
*
            endif                       !End ... if "seed"
*
          enddo                         !End loop over all hits
*
        enddo                           !All the hits of the current cluster were tagged
*
*-------Initialize to start the search for the next cluster
        nc            =nc+1
        hits_tagged   =hits_tagged+1
        tagged        =.true.
*     
*-------Find a hit which is not tagged
        khit=1
        do while(hcluster_hit(khit).ne.0 .AND. KHIT.LT.HMAX_CAL_BLOCKS)
          khit=khit+1
        enddo
*
*-------This will be the new "seed"
	IF (NC.GT.HNCLUSTERS_MAX) NC=HNCLUSTERS_MAX !AVOID OUT/BOUNDS.
        hcluster_hit(khit)=-nc
        hcluster_size(nc) = 1
* 
      enddo                             !End. Now all the hits are assigned to some cluster
*
*-----Number of clusters found
      hnclusters_cal=nc-1
*
*     For each cluster found, compute the center of gravity in X
*     projection, the energy deposited in succesive calorimeter columns
*     and the total energy deposition
*     
      do nc=1,hnclusters_max
        hcluster_e1_pos(nc)=0.      
        hcluster_e1_neg(nc)=0.
        hcluster_e2_pos(nc)=0.      
        hcluster_e2_neg(nc)=0.     
*
        hcluster_e1(nc)=0.
        hcluster_e2(nc)=0.
        hcluster_e3(nc)=0.
        hcluster_e4(nc)=0.
        hcluster_et(nc)=0.
        hcluster_xc(nc)=0.
      enddo
*
*
      do nh=1,hcal_num_hits
        nc = MAX(1,hcluster_hit(nh) )   !THIS DOES NOT HELP THE ANALYSIS,
                                        !BUT IT AVOIDS SUBSCRIPT OUT/BOUNDS ERRS.
        col=hcal_cols(nh)
*     
        hcluster_xc(nc)=hcluster_xc(nc)+hblock_xc(nh)*hblock_de(nh)
*
        if(col.eq.1) then
          if(hcal_num_neg_columns.ge.1) then
            hcluster_e1_pos(nc)=hcluster_e1_pos(nc)+hblock_de_pos(nh)
            hcluster_e1_neg(nc)=hcluster_e1_neg(nc)+hblock_de_neg(nh)
            hcluster_e1(nc)=hcluster_e1_pos(nc)+hcluster_e1_neg(nc)
          else
            hcluster_e1(nc)=hcluster_e1(nc)+hblock_de(nh)
          endif
        else if (col.eq.2) then
          if(hcal_num_neg_columns.ge.2) then
            hcluster_e2_pos(nc)=hcluster_e2_pos(nc)+hblock_de_pos(nh)
            hcluster_e2_neg(nc)=hcluster_e2_neg(nc)+hblock_de_neg(nh)
            hcluster_e2(nc)=hcluster_e2_pos(nc)+hcluster_e2_neg(nc)
          else
            hcluster_e2(nc)=hcluster_e2(nc)+hblock_de(nh)
          endif
        else if(col.eq.3) then
          hcluster_e3(nc)=hcluster_e3(nc)+hblock_de(nh)
        else if(col.eq.4) then
          hcluster_e4(nc)=hcluster_e4(nc)+hblock_de(nh)
        endif
        hcluster_et(nc)=hcluster_et(nc)+hblock_de(nh)  ! Is hblock_de de_pos+de_neg?
*
      enddo
*
      do nc=1,hnclusters_cal
*     make sure hcluster_et .ne. zero so no divide by zero        
        if(hcluster_et(nc).gt.0.) then
          hcluster_xc(nc)=hcluster_xc(nc)/hcluster_et(nc)
        else
          hcluster_xc(nc)= -1.0         ! Set fraction negative for bad et
        endif
      enddo
*
 100  continue
      if(hdbg_clusters_cal.gt.0) call h_prt_cal_clusters
*
      return
      end
