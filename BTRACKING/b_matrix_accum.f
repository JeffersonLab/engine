      subroutine b_matrix_accum(abort,err)
      
      implicit none
      save

      character*14 here
      parameter(here='b_matrix_accum')

      logical abort
      character*(*) err
      
      include 'bigcal_data_structures.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'gen_run_info.cmn'
      include 'gen_event_info.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'b_ntuple.cmn'
      include 'gep_data_structures.cmn'
      include 'gen_data_structures.cmn'
      include 'hms_data_structures.cmn'

      integer irow,icol,jrow,jcol,icell,jcell
      integer best,ihit,jhit
      real Ee,ei,ej

      real PI
      parameter(PI=3.14159265359)

      real Mp
      parameter(Mp=.938272)
      abort=.false.
      err=' '

c     
c$$$      mintdiff = 0.
c$$$
c$$$      if(ntrigb.gt.0) then
c$$$         do i=1,ntrigb
c$$$            if(i.eq.1.or.abs(gep_btime(i)-gep_btime_elastic).lt.mintdiff) then
c$$$               btrigt = gep_btime(i)
c$$$               mintdiff = abs(gep_btime(i)-gep_btime_elastic)
c$$$            endif
c$$$         enddo
c$$$      else
c$$$         btrigt = gep_btime_elastic
c$$$      endif
c$$$
c$$$      breftime = bigcal_end_time - btrigt

      
      if(gen_bigcal_mc.ne.0.and.gen_bigcal_mc.ne.3) then
c     for monte carlo-based calibration we are happy with using only events where there is one cluster
c     corresponding to one electron
         if(nvtrk_mc.eq.1.and.pid_mc(1).eq.3.and.bigcal_all_nclstr.eq.1)
     $        then
c     check rough position agreement (cell positions only! reconstructed positions may be bad)
            if(abs(bigcal_all_clstr_xcell(1,1)-xgeant(1)).le.10..and.
     $           abs(bigcal_all_clstr_ycell(1,1)-ygeant(1)).le.10) then
               Ee = egeant(1)

               bigcal_nmatr_event = bigcal_nmatr_event + 1

               do ihit=1,bigcal_all_clstr_ncell(1)
                  irow = bigcal_all_clstr_iycell(1,ihit)
                  icol = bigcal_all_clstr_ixcell(1,ihit)
                  if(irow.le.bigcal_prot_ny) then
                     icell = icol + bigcal_prot_nx*(irow-1)
                  else
                     icell = icol + bigcal_rcs_nx*(irow-1-bigcal_prot_ny)
     $                    + bigcal_prot_maxhits
                  endif
                  
                  ei = bigcal_all_clstr_ecell(1,ihit)

                  bigcal_vector(icell) = bigcal_vector(icell) + ei / Ee                  
                  
                  do jhit=1,bigcal_all_clstr_ncell(1)
                     jrow = bigcal_all_clstr_iycell(1,jhit)
                     jcol = bigcal_all_clstr_ixcell(1,jhit)
                     if(jrow.le.bigcal_prot_ny) then
                        jcell = jcol + bigcal_prot_nx*(jrow-1)
                     else 
                        jcell = jcol + bigcal_rcs_nx*(jrow-1-bigcal_prot_ny)
     $                       + bigcal_prot_maxhits
                     endif
                     
                     ej = bigcal_all_clstr_ecell(1,jhit)
                     
                     bigcal_matrix(icell,jcell) = bigcal_matrix(icell,jcell) 
     $                    + ei*ej / (Ee**2)
                  enddo
               enddo
            endif              
         endif
         ! this was called from GEp reconstruction after selection of best track!!!
c$$$      else if(bigcal_all_nclstr.ge.1.and.hsnum_fptrack.gt.0.and.
c$$$     $        gen_event_trigtype(5).eq.1) then
c         Ee = gebeam - gep_Q2_H / (2.*.938272) ! E' = E - Q^2/2Mp, Q^2 as measured by HMS
      else if(bigcal_all_nclstr.ge.1.and.hsnum_fptrack.gt.0) then
         Ee = gep_E_electron
         best = bigcal_itrack_best
         
c     check event selection cuts for calibration: dx, dy, and ctime:
c     also check elastic cut: This is crucial!!!
c 

c     write(*,*) 'track time =',bigcal_track_time(best)
c$$$         write(*,*) 'dx,dy,dt,dpel,dth,dph=',bigcal_all_clstr_x(best)-gep_bx_expect_H,
c$$$     $        bigcal_all_clstr_y(best)-gep_by_expect_H,bigcal_track_time(best)-bigcal_window_center,
c$$$     $        (gep_p_proton-gep_pel_htheta)/hpcentral,bigcal_track_thetarad(best)-gep_etheta_expect_h,
c$$$     $        bigcal_track_phirad(best)-gep_ephi_expect_h

         if(abs(bigcal_all_clstr_x(best)-gep_bx_expect_H).lt.
     $        gep_bcalib_cut_dx.and.abs(bigcal_all_clstr_y(best) - 
     $        gep_by_expect_H).lt.gep_bcalib_cut_dy.and.abs(
     $        gep_ctime_hms-gep_ctime_cal).lt.gep_bcalib_cut_ctime.and.
     $        abs(gep_pel_htheta-gep_p_proton)/hpcentral.lt.gep_bcalib_cut_elastic
     $        .and.abs(bigcal_track_thetarad(best)-gep_etheta_expect_H).lt.
     $        gep_bcalib_cut_theta.and.abs(bigcal_track_phirad(best)-
     $        gep_ephi_expect_H+PI/2.).lt.gep_bcalib_cut_phi.and.
     $        Ee.ge.gep_bcalib_cut_ehms(1).and.Ee.le.
     $        gep_bcalib_cut_ehms(2)) then

c            write(*,*) 'writing event to calib. matrix'

            bigcal_nmatr_event = bigcal_nmatr_event + 1

            do ihit=1,bigcal_all_clstr_ncell(best)
               irow = bigcal_all_clstr_iycell(best,ihit)
               icol = bigcal_all_clstr_ixcell(best,ihit)
               if(irow.le.bigcal_prot_ny) then
                  icell = icol + bigcal_prot_nx*(irow-1)
               else
                  icell = icol + bigcal_rcs_nx*(irow-1-bigcal_prot_ny)
     $                 + bigcal_prot_maxhits
               endif
            
               ei = bigcal_all_clstr_ecell(best,ihit)
               
               bigcal_vector(icell) = bigcal_vector(icell) + ei / Ee                  
               
               do jhit=1,bigcal_all_clstr_ncell(best)
                  jrow = bigcal_all_clstr_iycell(best,jhit)
                  jcol = bigcal_all_clstr_ixcell(best,jhit)
                  if(jrow.le.bigcal_prot_ny) then
                     jcell = jcol + bigcal_prot_nx*(jrow-1)
                  else 
                     jcell = jcol + bigcal_rcs_nx*(jrow-1-bigcal_prot_ny)
     $                    + bigcal_prot_maxhits
                  endif
                  
                  ej = bigcal_all_clstr_ecell(best,jhit)
                  
                  bigcal_matrix(icell,jcell) = bigcal_matrix(icell,jcell) 
     $                 + ei*ej / (Ee**2)
               enddo
            enddo
         endif
      endif
      
      return 
      end
