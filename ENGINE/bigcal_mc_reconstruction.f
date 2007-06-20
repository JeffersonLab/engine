      subroutine bigcal_mc_reconstruction(iflag,ABORT,err)

      implicit none
      save

      integer iflag,jflag,io_unit,i,j,k
      logical ABORT
      character*(*) err

      character*24 here
      parameter(here='bigcal_mc_reconstruction')

      include 'bigcal_data_structures.cmn'
      include 'gen_run_info.cmn'
      include 'gen_filenames.cmn'
      include 'bigcal_bypass_switches.cmn'

      abort=.false.
      err=' '

      if(iflag.eq.1) then !bigcal mc .dat file
         io_unit = g_data_source_in_hndl

         jflag = 0
         
         read(io_unit,end=101,err=102) iev_mc
         read(io_unit,end=101,err=102) xv_mc,yv_mc,zv_mc
         read(io_unit,end=101,err=102) nvtrk_mc

         jflag = 1
         
         do i=1,nvtrk_mc
            read(io_unit,end=101,err=102) pid_mc(i),px_mc(i),py_mc(i),
     $           pz_mc(i)
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
         bbypass_prot = 0
         bbypass_rcs = 0
         bbypass_sum8 = 1
         bbypass_sum64 = 1
         bbypass_find_clusters = 0
         bbypass_calc_cluster_time = 1
         bbypass_calc_shower_coord = 0
         bbypass_calc_physics = 0
         call b_reconstruction(abort,err)

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
