*=======================================================================
      subroutine s_prt_cal_clusters
*=======================================================================
*-
*-      Dumps the calorimeter cluster data
*-
*-      Created: 20 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                change name and lun
* $Log$
* Revision 1.1  1994/04/13 18:19:30  cdaq
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
      include 'gen_data_structures.cmn'
      include 'sos_calorimeter.cmn'
      include 'sos_tracking.cmn'
*
*
      write(slun_dbg_cal,10) snclusters_cal
   10 format(///'      SOS Calorimeter Cluster Data', /,
     &          '      Total Number of Clusters:',i3,//,
     &          ' Hit #   Cluster #')
*
      if(scal_num_hits.le.0) return
*
*-----Print the link pointer to cluster number
      do nh=1,scal_num_hits
         write(slun_dbg_cal,20) nh,scluster_hit(nh)
   20    format(i5,7x,i5)
      enddo
*
      if(snclusters_cal.le.0) return
*
*-----Print the cluster parameters
      write(slun_dbg_cal,30)
   30 format(/,
     &' Cluster',/,
     &' #(size)    XC[cm]  E1[GeV]  E2[GeV]  E3[GeV]  E4[GeV]  ET[GeV]')
*
      if(snclusters_cal.le.0) return
*
      do nc=1,snclusters_cal
         write(slun_dbg_cal,40)
     &   nc,
     &   scluster_size(nc),
     &   scluster_xc(nc),
     &   scluster_e1(nc),
     &   scluster_e2(nc),
     &   scluster_e3(nc),
     &   scluster_e4(nc),
     &   scluster_et(nc)
   40    format(i3,'(',i3,')',4x,f6.2,5(1x,f8.4))
      enddo
*
      return
      end
