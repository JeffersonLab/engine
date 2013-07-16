*=======================================================================
      subroutine h_prt_cal_clusters
*=======================================================================
*-
*-      Dumps the calorimeter cluster data
*-
*-      Created: 20 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                change name and lun
* $Log: h_prt_cal_clusters.f,v $
* Revision 1.3  1999/01/21 21:40:14  saw
* Extra shower counter tube modifications
*
* Revision 1.2  1995/05/22 19:39:19  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/04/13  15:40:35  cdaq
* Initial revision
*
*-----------------------------------------------------------------------
*
*
      implicit none
      save
*
      integer*4 nh      !Hit number
      integer*4 nc      !Cluster number
*
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
      include 'hms_tracking.cmn'
*
*
      write(hlun_dbg_cal,10) hnclusters_cal
   10 format(///'      HMS Calorimeter Cluster Data', /,
     &          '      Total Number of Clusters:',i3,//,
     &          ' Hit #   Cluster #')
*
      if(hcal_num_hits.le.0) return
*
*-----Print the link pointer to cluster number
      do nh=1,hcal_num_hits
         write(hlun_dbg_cal,20) nh,hcluster_hit(nh)
   20    format(i5,7x,i5)
      enddo
*
      if(hnclusters_cal.le.0) return
*
*-----Print the cluster parameters
      write(hlun_dbg_cal,30)
   30 format(/,
     &' Cluster',/,
     &' #(size) XC[cm] E1[GeV] E2[GeV] E3[GeV] E4[GeV] ET[GeV] E1_POS[GeV] E1_NEG[GeV] E2_POS[GeV] E2_NEG[GeV ')
*
      if(hnclusters_cal.le.0) return
*
      do nc=1,hnclusters_cal
         write(hlun_dbg_cal,40)
     &   nc,
     &   hcluster_size(nc),
     &   hcluster_xc(nc),
     &   hcluster_e1(nc),
     &   hcluster_e2(nc),
     &   hcluster_e3(nc),
     &   hcluster_e4(nc),
     &   hcluster_et(nc),
     &   hcluster_e1_pos(nc),
     &   hcluster_e1_neg(nc),
     &   hcluster_e2_pos(nc),
     &   hcluster_e2_neg(nc)
   40    format(i3,'(',i3,')',4x,f6.2,9(1x,f8.4))
      enddo
*
      return
      end
