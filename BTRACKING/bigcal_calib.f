      subroutine bigcal_calib(abort,err)

      implicit none
      save

      character*12 here
      parameter(here='bigcal_calib')

      include 'bigcal_data_structures.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'gen_run_info.cmn'
      include 'bigcal_filenames.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_hist_id.cmn'

      integer nempty,nsmalldiag,N,irow,icol,jrow,jcol,icell,jcell
      integer ihit,jhit,i,j
      integer iochan,iflag_matr
      
      real*4 c_old,g_old

      character*80 filename

      logical abort
      character*(*) err

      abort = .false.
      err = ' '

c     first check whether we have enough events to do a decent calibration:
      if(bigcal_nmatr_event.lt.bigcal_min_calib_events) then ! save matrix to a file to start next run.
         write(*,*) 'not enough events to calibrate, saving matrix'//
     $        ' for next run'
         filename = b_calib_matrix_filename
         call g_IO_control(iochan,'ANY',abort,err)
         if(abort) then
            call g_add_path(here,err)
            return
         endif
         
         open(unit=iochan,file=filename,status='unknown',
     $        form='unformatted',err=34)
         
         write(iochan) bigcal_nmatr_event
         write(iochan) bigcal_vector
         write(iochan) bigcal_matrix

         call g_IO_control(iochan,'FREE',abort,err)
         if(abort) then
            call g_add_path(here,err)
            return
         endif
         close(iochan)
         return

 34      write(*,*) 'problem opening '//filename
         return 
      else ! have enough events, do calibration
c     before solving matrix, determine nempty, nsmall, and nsmalldiag per Kravtsov:

         write(*,*) 'Ready to do calibration: nevent=',bigcal_nmatr_event

         do i=1,N
            bigcal_matr_iempty(i) = 0
            bigcal_matr_ismalld(i) = 0
         enddo

         nempty = 0
         nsmalldiag = 0

         N = bigcal_all_maxhits

         do i=1,N
            if(bigcal_vector(i).eq.0.) then
               nempty = nempty + 1
               bigcal_matr_iempty(nempty) = i 
               bigcal_vector(i) = 1.
               bigcal_matrix(i,i) = 1.
            endif
            
            if(bigcal_matrix(i,i).lt.0.1*bigcal_vector(i)) then
               nsmalldiag = nsmalldiag + 1
               bigcal_matr_ismalld(nsmalldiag) = i
               bigcal_vector(i) = 1.
               do j=1,N
                  bigcal_matrix(i,j) = 0.
                  bigcal_matrix(j,i) = 0.
               enddo
               bigcal_matrix(i,i) = 1.
            endif
         enddo
         bigcal_matr_nempty = nempty
         bigcal_matr_nsmalldiag = nsmalldiag

         do i=1,nempty
            if(bid_bcal_empty.gt.0) call hf1(bid_bcal_empty,
     $           float(bigcal_matr_iempty(i)),1.)
         enddo
         do i=1,nsmalldiag
            if(bid_bcal_small.gt.0) call hf1(bid_bcal_small,
     $           float(bigcal_matr_ismalld(i)),1.)
         enddo
c     call cernlib routine to solve the system of equations:
c     replaces "bigcal_vector" with the solution vector of coefficients
         
         call rseqn(N,bigcal_matrix,N,iflag_matr,1,bigcal_vector)

         bigcal_matr_iflag = iflag_matr
         if(iflag_matr.eq.-1) then
            write(*,*) '%BIGCAL CALIB: matrix not positive-definite'
            return
         else 
            write(*,*) '%BIGCAL CALIB: calibration successful'
         endif

c     now write out a CTP parm file with the new calibration coefficients:
         
         if(b_calib_parm_filename.ne.' ') then
            filename = b_calib_parm_filename

            write(*,*) filename

            call g_sub_run_number(filename,gen_run_number)
         
            write(*,*) 'writing new calib. coeffs. to '//filename

            call g_IO_control(iochan,'ANY',abort,err)
            if(abort) then
               call g_add_path(here,err)
               return
            endif

            open(unit=iochan,file=filename,status='unknown',
     $           form='formatted',err=35)
         
c     amplitudes were already multiplied by cfac and gain_cor, therefore, to get the new calibration coefficients,
c     we have to separate out the effect of such:
c     Eguess = cfac_old*gain_cor_old * (ADC - PED)
c     set new gain_cor to 1
c     Eactual = Ccalculated * Eguess = cfac_new * (ADC - PED)
c     --> cfac_new = cfac_old*gain_cor_old * Ccalculated
c     --> gain_cor_new = 1
            
            do i=1,bigcal_prot_maxhits
               c_old = bigcal_prot_cfac(i)
               g_old = bigcal_prot_gain_cor(i)
               
               bigcal_prot_gain_cor(i) = 1.
               bigcal_prot_cfac(i) = bigcal_vector(i) * c_old * g_old
               if(bid_bcal_cfac_old.gt.0) call hf1(bid_bcal_cfac_old,
     $              float(i),c_old*g_old)
               if(bid_bcal_cfac_new.gt.0) call hf1(bid_bcal_cfac_new,
     $              float(i),bigcal_vector(i))
               if(bid_bcal_oldxnew.gt.0) call hf1(bid_bcal_oldxnew,
     $              float(i),bigcal_prot_cfac(i))
            enddo
            
            do i=1,bigcal_rcs_maxhits
               c_old = bigcal_rcs_cfac(i)
               g_old = bigcal_rcs_gain_cor(i)
               
               bigcal_rcs_gain_cor(i) = 1.
               bigcal_rcs_cfac(i) = bigcal_vector(i+bigcal_prot_maxhits) * 
     $              c_old * g_old
               if(bid_bcal_cfac_old.gt.0) call hf1(bid_bcal_cfac_old,
     $              float(i+bigcal_prot_maxhits),c_old*g_old)
               if(bid_bcal_cfac_new.gt.0) call hf1(bid_bcal_cfac_new,
     $              float(i+bigcal_prot_maxhits),bigcal_vector(i+bigcal_prot_maxhits))
               if(bid_bcal_oldxnew.gt.0) call hf1(bid_bcal_oldxnew,
     $              float(i+bigcal_prot_maxhits),bigcal_rcs_cfac(i))
            enddo
            
            write(iochan,*) 'bigcal_prot_cfac = '
            
            do i=1,(bigcal_prot_maxhits/8)
               if(i.lt.bigcal_prot_maxhits/8) then
                  write(iochan,101) (bigcal_prot_cfac(j+8*(i-1)),
     $                 ', ',j=1,8)
               else 
                  write(iochan,102) (bigcal_prot_cfac(j+8*(i-1)),
     $                 ', ',j=1,7),bigcal_prot_cfac(8+8*(i-1))
               endif
            enddo
            
            write(iochan,*) 'bigcal_prot_gain_cor = '
            
            do i=1,(bigcal_prot_maxhits/8)
               if(i.lt.bigcal_prot_maxhits/8) then
                  write(iochan,101) (bigcal_prot_gain_cor(j+8*(i-1)),
     $                 ', ',j=1,8)
               else 
                  write(iochan,102) (bigcal_prot_gain_cor(j+8*(i-1)),
     $                 ', ',j=1,7),bigcal_prot_gain_cor(8+8*(i-1))
               endif
            enddo
            
            write(iochan,*) 'bigcal_rcs_cfac = '
            
            do i=1,(bigcal_rcs_maxhits/8)
               if(i.lt.bigcal_rcs_maxhits/8) then
                  write(iochan,101) (bigcal_rcs_cfac(j+8*(i-1)),
     $                 ', ',j=1,8)
               else 
                  write(iochan,102) (bigcal_rcs_cfac(j+8*(i-1)),
     $                 ', ',j=1,7),bigcal_rcs_cfac(8+8*(i-1))
               endif
            enddo
            
            write(iochan,*) 'bigcal_rcs_gain_cor = '
            
            do i=1,(bigcal_rcs_maxhits/8)
               if(i.lt.bigcal_rcs_maxhits/8) then
                  write(iochan,101) (bigcal_rcs_gain_cor(j+8*(i-1)),
     $                 ', ',j=1,8)
               else 
                  write(iochan,102) (bigcal_rcs_gain_cor(j+8*(i-1)),
     $                 ', ',j=1,7),bigcal_rcs_gain_cor(8+8*(i-1))
               endif
            enddo
            
 101        format(8(F12.5,A2))
 102        format(7(F12.5,A2),F12.5)
c     shut down the file:
            call g_IO_control(iochan,'FREE',abort,err)
            if(abort) then
               call g_add_path(here,err)
               return
            endif
            close(iochan)
         endif
c     reset calibration matrix to zero and rewrite the original file without the 
c     '_%d.param' extension. This way, if we are replaying runs using a script, the
c     next run will find the calibration coefficients reset:
         bigcal_nmatr_event = 0
         do i=1,bigcal_all_maxhits
            bigcal_vector(i) = 0.
            do j=1,bigcal_all_maxhits
               bigcal_matrix(i,j) = 0.
            enddo
         enddo 
         
         filename = b_calib_matrix_filename
         call g_IO_control(iochan,'ANY',abort,err)
         if(abort) then
            call g_add_path(here,err)
            return
         endif
         
         open(unit=iochan,file=filename,status='unknown',
     $        form='unformatted',err=35)
         
         write(iochan) bigcal_nmatr_event
         write(iochan) bigcal_vector
         write(iochan) bigcal_matrix
         
         call g_IO_control(iochan,'FREE',abort,err)
         if(abort) then
            call g_add_path(here,err)
            return
         endif
         close(iochan)
         return
         
 35      write(*,*) 'problem opening '//filename
         return
         
      endif
      
      return 
      end
