      subroutine bigcal_mc_reconstruction(iflag,ABORT,err)

      implicit none
      save

      integer iflag,jflag,io_unit,i,j,k
      logical ABORT
      character*(*) err

      character*24 here
      parameter(here='bigcal_mc_reconstruction')

      real xhat,yhat,zhat,pxh,pyh,pzh,xvh,yvh,zvh
      real tintercept,xintercept,yintercept,zintercept
      real gbthetarad
      parameter(gbthetarad=1.1868239)
      real gbrcm
      parameter(gbrcm=440.0)

      include 'bigcal_data_structures.cmn'
      include 'gen_run_info.cmn'
      include 'gen_filenames.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'b_ntuple.cmn'
c      include 'gen_constants.par'

      abort=.false.
      err=' '

      if(iflag.eq.1) then !bigcal mc .dat file
         io_unit = g_data_source_in_hndl

         jflag = 0
         
         read(io_unit,end=101,err=102) iev_mc
         read(io_unit,end=101,err=102) xv_mc,yv_mc,zv_mc
         read(io_unit,end=101,err=102) nvtrk_mc

c$$$         xvertex_g = xv_mc
c$$$         yvertex_g = yv_mc
c$$$         zvertex_g = zv_mc
         if(nvtrk_mc.le.25) then 
            ntrk_g = nvtrk_mc
         else
            ntrk_g = 25
         endif
         
         jflag = 1
         
         do i=1,nvtrk_mc
            read(io_unit,end=101,err=102) pid_mc(i),px_mc(i),py_mc(i),
     $           pz_mc(i)
            if(i.le.25) then
               pid_g(i) = pid_mc(i)
c$$$               pxgeant(i) = px_mc(i)
c$$$               pygeant(i) = py_mc(i)
c$$$               pzgeant(i) = pz_mc(i)
               pgeant(i) = sqrt((px_mc(i))**2 + (py_mc(i))**2 + 
     $              (pz_mc(i))**2 )
               if(pid_g(i).eq.2.or.pid_g(i).eq.3) then !electron-positron
                  egeant(i) = sqrt((.510999e-3)**2 + (pgeant(i))**2)
               else if(pid_g(i).eq.1) then ! photon
                  egeant(i) = pgeant(i)
               else if(pid_g(i).eq.13.or.pid_g(i).eq.14) then ! nucleon
                  egeant(i) = sqrt(.938272**2 + (pgeant(i))**2)
               else if(pid_g(i).eq.5.or.pid_g(i).eq.6) then ! muon
                  egeant(i) = sqrt(.105658**2 + (pgeant(i))**2)
               else if(pid_g(i).eq.8.or.pid_g(i).eq.9) then ! pion
                  egeant(i) = sqrt(.13957**2 + (pgeant(i))**2)
               else ! assume electron:
                  egeant(i) = sqrt((.510999e-3)**2 + (pgeant(i))**2)
               endif

               ! now calculate trajectory and intersection point:
c$$$               xhat = pxgeant(i) / pgeant(i)
c$$$               yhat = pygeant(i) / pgeant(i)
c$$$               zhat = pzgeant(i) / pgeant(i)

               ! in the GEANT coordinate system, the calo is always at 440 cm and 68 degrees
               ! need to rotate and translate coordinates appropriately into hall system:
               
               pxh = px_mc(i)*cos(bigcal_theta_rad-gbthetarad)
     $              + pz_mc(i)*sin(bigcal_theta_rad-gbthetarad)
               pyh = py_mc(i)
               pzh = -px_mc(i)*sin(bigcal_theta_rad-gbthetarad)
     $              + pz_mc(i)*cos(bigcal_theta_rad-gbthetarad)
               
               pxgeant(i) = pxh
               pygeant(i) = pyh
               pzgeant(i) = pzh

               xhat = pxgeant(i)/pgeant(i)
               yhat = pygeant(i)/pgeant(i)
               zhat = pzgeant(i)/pgeant(i)

               gthetarad(i) = acos(zhat)
               gphirad(i) = atan2(yhat,xhat)

               xvh = xv_mc+(bigcal_r_tgt-gbrcm)*sin(gbthetarad)
               yvh = yv_mc
               zvh = zv_mc+(bigcal_r_tgt-gbrcm)*cos(gbthetarad)

               xvertex_g = xvh
               yvertex_g = yvh
               zvertex_g = zvh

               ! calculate intersection point of trajectory with calo:

               tintercept = (bigcal_r_tgt - xvertex_g*bigcal_sintheta - 
     $              zvertex_g*bigcal_costheta)/(xhat*bigcal_sintheta + 
     $              zhat*bigcal_costheta)

               xintercept = xvertex_g + tintercept*xhat
               yintercept = yvertex_g + tintercept*yhat
               zintercept = zvertex_g + tintercept*zhat

               ! rotate into calo-centered coordinate system:
               
               xgeant(i) = xintercept*bigcal_costheta - 
     $              zintercept*bigcal_sintheta
               ygeant(i) = yintercept

               ! and that's it!!!!!

            endif            
         enddo

         jflag = 2

         read(io_unit,end=101,err=102) isum_mc
         read(io_unit,end=101,err=102) esum_mc

         jflag = 3

         bigcal_prot_nhit = isum_mc(1)
         bigcal_rcs_nhit = isum_mc(2)
c     here's the key part: fill the raw hit arrays!!!
         do i=1,isum_mc(1)
            read(io_unit,end=101,err=102) ix_mc,iy_mc,npe_mc
            bigcal_prot_ix(i) = ix_mc
            bigcal_prot_iy(i) = iy_mc
            bigcal_prot_adc_raw(i) = nint(npe_mc)
         enddo

         jflag = 4

         do i=1,isum_mc(2)
            read(io_unit,end=101,err=102) ix_mc,iy_mc,npe_mc
            bigcal_rcs_ix(i) = ix_mc
            bigcal_rcs_iy(i) = iy_mc + 32
            bigcal_rcs_adc_raw(i) = nint(npe_mc)
         enddo
         
         jflag = 5

         read(io_unit,end=101,err=102) idesum_mc
         read(io_unit,end=101,err=102) allde_mc
         
         jflag = 6

         do i=1,2
            do j=1,idesum_mc(i)
               read(io_unit,end=101,err=102) ix_mc,iy_mc,dedx_mc
            enddo
         enddo

         jflag = 7

c     override bypass flags from parameter files: 
c$$$         bbypass_prot = 0
c$$$         bbypass_rcs = 0
c$$$         bbypass_sum8 = 1
c$$$         bbypass_sum64 = 1
c$$$         bbypass_find_clusters = 0
c$$$         bbypass_calc_cluster_time = 1
c$$$         bbypass_calc_shower_coord = 0
c$$$         bbypass_calc_physics = 0
c     BAD PUCKETT! DON'T hardwire things into the code, mmm'kay?

         !write(*,*) 'entering b_reconstruction'

         call b_reconstruction(abort,err)
         
         !write(*,*) 'finished b_reconstruction'

         if(abort) then
            call g_add_path(here,err)
            return
         endif

         return

 101     write(*,*) 'end of file',g_data_source_filename,'reached'
         if(jflag.ne.0) then
            abort=.true.
            err='unexpected EOF'
            call g_add_path(here,err)
            return
         endif
         EOF_MC_DAT = .true.
         return

 102     abort=.true.
         err='problem reading '//g_data_source_filename
         call g_add_path(here,err)
         return

      else if(iflag.eq.2) then !wei's bigcal mc ntuple
      

      endif

      return
      end
