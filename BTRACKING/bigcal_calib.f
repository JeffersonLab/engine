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

      integer nempty,nsmalldiag,N,irow,icol,jrow,jcol,icell,jcell,ismall,iempty
      integer ihit,jhit,i,j,k
      integer iochan,iflag_matr
      
      real*4 c_old,g_old,c_new,g_new
      real*4 newavg,redavg
      integer*4 nnewavg,nredavg
      integer*4 Nred,ired,jred
      real*4 bigcal_reduced_matrix(bigcal_all_maxhits,bigcal_all_maxhits)
      real*4 bigcal_reduced_vector(bigcal_all_maxhits)
      integer*4 bigcal_ivect(bigcal_all_maxhits)
      integer*4 bigcal_imatr(bigcal_all_maxhits,bigcal_all_maxhits)
      integer*4 bigcal_jmatr(bigcal_all_maxhits,bigcal_all_maxhits)
      
      logical fillhist

      character*80 filename

      logical abort
      character*(*) err

c     build reduced matrix if parameters are set:

c      write(*,*) 'building reduced matrix Nred=',Nred

c      if(Nred.gt.0.and.Nred.le.bigcal_all_maxhits) then
      Nred = 0
      ired = 0
      jred = 0
      if(bigcal_calib_iylo.ge.1.and.bigcal_calib_iylo.le.56.and.
     $     bigcal_calib_iyhi.ge.1.and.bigcal_calib_iyhi.le.56.and.
     $     bigcal_calib_iyhi.gt.bigcal_calib_iylo) then
         if(bigcal_calib_ixlo(1).ge.1.and.bigcal_calib_ixlo(1).le.32
     $        .and.bigcal_calib_ixhi(1).ge.1.and.bigcal_calib_ixhi(1)
     $        .le.32.and.bigcal_calib_ixhi(1).gt.bigcal_calib_ixlo(1)) then
            if(bigcal_calib_ixlo(2).ge.1.and.bigcal_calib_ixlo(2).le.30
     $           .and.bigcal_calib_ixhi(2).ge.1.and.bigcal_calib_ixhi(2)
     $           .le.30.and.bigcal_calib_ixhi(2).gt.bigcal_calib_ixlo(2)) then
c               ired=0
               do i=1,bigcal_all_maxhits
                  if(i.le.1024) then
                     irow = (i-1)/32 + 1
                     icol = mod(i-1,32) + 1
                  else
                     j=i-1024
                     irow = (j-1)/30 + 33
                     icol = mod(j-1,30) + 1
                  endif
                  
                  if(irow.ge.bigcal_calib_iylo.and.irow.le.bigcal_calib_iyhi) then
                     if(irow.le.32) then
                        if(icol.ge.bigcal_calib_ixlo(1).and.icol.le.bigcal_calib_ixhi(1)) then
                           ired = ired + 1
c                           write(*,*) 'i,irow,icol,ired=',i,irow,icol,ired
                           bigcal_reduced_vector(ired) = bigcal_vector(i)
                           bigcal_ivect(ired) = i

                           jred = 0
                           do j=1,bigcal_all_maxhits
                              if(j.le.1024) then
                                 jrow = (j-1)/32 + 1
                                 jcol = mod(j-1,32) + 1
                              else
                                 k = j - 1024
                                 jrow = (k-1)/30 + 33
                                 jcol = mod(k-1,30) + 1
                              endif
                              
                              if(jrow.ge.bigcal_calib_iylo.and.jrow.le.bigcal_calib_iyhi) then
                                 if(jrow.le.32) then
                                    if(jcol.ge.bigcal_calib_ixlo(1).and.jcol.le.
     $                                   bigcal_calib_ixhi(1)) then
                                       jred = jred + 1
c                                       write(*,*) 'j,jrow,jcol,jred=',j,jrow,jcol,jred
                                       bigcal_reduced_matrix(ired,jred) = bigcal_matrix(i,j)
                                       bigcal_imatr(ired,jred) = i
                                       bigcal_jmatr(ired,jred) = j
                                    endif
                                 else
                                    if(jcol.ge.bigcal_calib_ixlo(2).and.jcol.le.
     $                                   bigcal_calib_ixhi(2)) then
                                       jred = jred + 1
c                                       write(*,*) 'j,jrow,jcol,jred=',j,jrow,jcol,jred
                                       bigcal_reduced_matrix(ired,jred) = bigcal_matrix(i,j)
                                       bigcal_imatr(ired,jred) = i
                                       bigcal_jmatr(ired,jred) = j
                                    endif
                                 endif
                              endif

                           enddo
c                           write(*,*) 'ired,jredFINAL=',ired,jred
                        endif
                     else
                        if(icol.ge.bigcal_calib_ixlo(2).and.icol.le.bigcal_calib_ixhi(2)) then
                           ired = ired + 1
c                           write(*,*) 'i,irow,icol,ired=',i,irow,icol,ired
                           bigcal_reduced_vector(ired) = bigcal_vector(i)
                           bigcal_ivect(ired) = i

                           jred = 0
                           do j=1,bigcal_all_maxhits
                              if(j.le.1024) then
                                 jrow = (j-1)/32 + 1
                                 jcol = mod(j-1,32) + 1
                              else
                                 k = j - 1024
                                 jrow = (k-1)/30 + 33
                                 jcol = mod(k-1,30) + 1
                              endif
                              
                              if(jrow.ge.bigcal_calib_iylo.and.jrow.le.bigcal_calib_iyhi) then
                                 if(jrow.le.32) then
                                    if(jcol.ge.bigcal_calib_ixlo(1).and.jcol.le.
     $                                   bigcal_calib_ixhi(1)) then
                                       jred = jred + 1
c                                       write(*,*) 'j,jrow,jcol,jred=',j,jrow,jcol,jred
                                       bigcal_reduced_matrix(ired,jred) = bigcal_matrix(i,j)
                                       bigcal_imatr(ired,jred) = i
                                       bigcal_jmatr(ired,jred) = j
                                    endif
                                 else
                                    if(jcol.ge.bigcal_calib_ixlo(2).and.jcol.le.
     $                                   bigcal_calib_ixhi(2)) then
                                       jred = jred + 1
c                                       write(*,*) 'j,jrow,jcol,jred=',j,jrow,jcol,jred
                                       bigcal_reduced_matrix(ired,jred) = bigcal_matrix(i,j)
                                       bigcal_imatr(ired,jred) = i
                                       bigcal_jmatr(ired,jred) = j
                                    endif
                                 endif
                              endif
                           enddo
c                           write(*,*) 'ired,jredFINAL=',ired,jred
                           
                        endif
                     endif
                  endif
               enddo
c               write(*,*) 'iredFINAL=',ired
               Nred = ired
            endif
         endif
      endif

      write(*,*) 'built reduced matrix Nred=',Nred

      abort = .false.
      err = ' '

c     first check whether we have enough events to do a decent calibration:
      if(bigcal_nmatr_event.lt.bigcal_min_calib_events) then ! save matrix to a file to start next run.
         write(*,*) 'not enough events to calibrate, saving matrix'//
     $        ' for next run'
         write(*,*) 'number of events in calib. matrix = ',bigcal_nmatr_event
         if(b_calib_matrix_filename.ne.' ') then
            filename = b_calib_matrix_filename
            call g_IO_control(iochan,'ANY',abort,err)
            if(abort) then
               call g_add_path(here,err)
               return
            endif
            
            open(unit=iochan,file=filename,status='unknown',
     $           form='unformatted',err=34)
            
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

 34         write(*,*) 'problem opening '//filename
            return 
         else
            write(*,*) 'WARNING: insufficient events '//
     $           'to calibrate BigCal, but '
            write(*,*) 'b_calib_matrix_filename undefined'
            write(*,*) 'next run calibration analysis will '//
     $           'start over at zero events!'
         endif
      else                      ! have enough events, do calibration
c     before solving matrix, determine nempty, nsmall, and nsmalldiag per Kravtsov:

         write(*,*) 'Ready to do calibration: nevent=',bigcal_nmatr_event

         if(Nred.gt.0..and.Nred.le.bigcal_all_maxhits) then
            write(*,*) 'using reduced calibration matrix:'
            write(*,366) '(xlop,xhip,xlor,xhir,ylo,yhi)=(',bigcal_calib_ixlo(1),
     $           ',',bigcal_calib_ixhi(1),',',bigcal_calib_ixlo(2),',',bigcal_calib_ixhi(2),
     $           ',',bigcal_calib_iylo,',',bigcal_calib_iyhi

 366        format(A33,5(I3,A2),I3)
            
         endif
         do i=1,N
            bigcal_matr_iempty(i) = 0
            bigcal_matr_ismalld(i) = 0
         enddo

         nempty = 0
         nsmalldiag = 0

         N = bigcal_all_maxhits

c         newavg = 0.

         ired = 1

         do i=1,N
c     newavg = newavg + bigcal_vector(i)
            if(bigcal_vector(i).eq.0.) then
               nempty = nempty + 1
               bigcal_matr_iempty(nempty) = i 
               bigcal_vector(i) = 1.
               bigcal_matrix(i,i) = 1.
               
               if(bigcal_ivect(ired).eq.i) then
                  bigcal_reduced_vector(ired) = 1.
                  bigcal_reduced_matrix(ired,ired) = 1.
               endif
            endif
            
            if(bigcal_matrix(i,i).lt.0.1*bigcal_vector(i)) then
               nsmalldiag = nsmalldiag + 1
               bigcal_matr_ismalld(nsmalldiag) = i
               bigcal_vector(i) = 1.
               if(bigcal_ivect(ired).eq.i) then
                  bigcal_reduced_vector(ired) = 1.
                  do jred=1,Nred
                     bigcal_reduced_matrix(ired,jred) = 0.
                     bigcal_reduced_matrix(jred,ired) = 0.
                  enddo
                  bigcal_reduced_matrix(ired,ired) = 1.
               endif
               do j=1,N
                  bigcal_matrix(i,j) = 0.
                  bigcal_matrix(j,i) = 0.
               enddo
               bigcal_matrix(i,i) = 1.
            endif
            
            if(bigcal_ivect(ired).eq.i) ired = ired + 1
         enddo

c         newavg = newavg / N

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

         if(Nred.gt.0.and.Nred.le.bigcal_all_maxhits) then
            call rseqn(Nred,bigcal_reduced_matrix,N,iflag_matr,1,bigcal_reduced_vector)
            
            if(iflag_matr.ne.-1) then
               ired = 1
               do i=1,N
                  if(i.eq.bigcal_ivect(ired)) then
                     bigcal_vector(i) = bigcal_reduced_vector(ired)
                     ired = ired + 1
                  else
                     bigcal_vector(i) = 1.
                  endif
               enddo   
            endif
         endif

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
            
            newavg = 0.
            redavg = 0.

            nnewavg = 0
            nredavg = 0
            
            ired = 1
            ismall = 1
            iempty = 1

            do i=1,bigcal_prot_maxhits
               
               fillhist = .true.
               
               if(bigcal_matr_ismalld(ismall).eq.i) then
                  fillhist = .false.
                  ismall = ismall + 1
               endif

               if(bigcal_matr_iempty(iempty).eq.i) then
                  fillhist = .false.
                  iempty = iempty + 1
               endif

               irow = (i-1)/32 + 1
               icol = mod(i-1,32) + 1

               c_old = bigcal_prot_cfac(i)
               g_old = bigcal_prot_gain_cor(i)
               
               g_new = 1.
               c_new = bigcal_vector(i) * c_old * g_old

               bigcal_prot_gain_cor(i) = 1.
               bigcal_prot_cfac(i) = bigcal_vector(i) * c_old * g_old

               if(bid_bcal_cfac_old.gt.0) call hf1(bid_bcal_cfac_old,
     $              float(i),c_old*g_old)
               
c$$$               if(bid_bcal_oldxnew.gt.0) call hf1(bid_bcal_oldxnew,
c$$$     $              float(i),bigcal_prot_cfac(i))

c$$$               if(.not.(Nred.gt.0.and.Nred.le.bigcal_all_maxhits)) then ! using full matrix
c$$$                  if(bid_bcal_cfac_new.gt.0) call hf1(bid_bcal_cfac_new,
c$$$     $                 float(i),bigcal_vector(i))
c$$$                  if(bid_bcal_cfac_dist.gt.0) call hf1(bid_bcal_cfac_dist,
c$$$     $                 bigcal_vector(i),1.)
c$$$               endif

               newavg = newavg + c_new
               nnewavg = nnewavg + 1
               if(i.eq.bigcal_ivect(ired)) then
                  ired = ired + 1
                  if(irow.ne.bigcal_calib_iylo.and.irow.ne.bigcal_calib_iyhi.and.
     $                 icol.ne.bigcal_calib_ixlo(1).and.icol.ne.bigcal_calib_ixhi(1)) then
                     if(bid_bcal_cfac_new.gt.0.and.fillhist) call hf1(bid_bcal_cfac_new,
     $                    float(i),bigcal_vector(i))
                     if(bid_bcal_cfac_dist.gt.0.and.fillhist) call hf1(bid_bcal_cfac_dist,
     $                    bigcal_vector(i),1.)
                  
                     redavg = redavg + c_new
                     nredavg = nredavg + 1
                  endif
               endif
            enddo
            
            do i=1,bigcal_rcs_maxhits

               irow = (i-1)/30 + 33
               icol = mod(i-1,30) + 1

               fillhist = .true.
               if(bigcal_matr_ismalld(ismall).eq.i+1024) then
                  fillhist = .false.
                  ismall = ismall + 1
               endif
               
               if(bigcal_matr_iempty(iempty).eq.i+1024) then
                  fillhist = .false.
                  iempty = iempty + 1
               endif

               c_old = bigcal_rcs_cfac(i)
               g_old = bigcal_rcs_gain_cor(i)
               
               bigcal_rcs_gain_cor(i) = 1.
               bigcal_rcs_cfac(i) = bigcal_vector(i+bigcal_prot_maxhits) * 
     $              c_old * g_old

               g_new = 1.
               c_new = bigcal_vector(i+bigcal_prot_maxhits) * c_old * g_old

               if(bid_bcal_cfac_old.gt.0) call hf1(bid_bcal_cfac_old,
     $              float(i+bigcal_prot_maxhits),c_old*g_old)
               
c$$$               if(bid_bcal_oldxnew.gt.0) call hf1(bid_bcal_oldxnew,
c$$$     $              float(i+bigcal_prot_maxhits),bigcal_rcs_cfac(i))

c$$$               if(bid_bcal_cfac_new.gt.0) call hf1(bid_bcal_cfac_new,
c$$$     $              float(i+bigcal_prot_maxhits),bigcal_vector(i+bigcal_prot_maxhits))
c$$$
c$$$               if(bid_bcal_cfac_dist.gt.0) call hf1(bid_bcal_cfac_dist,
c$$$     $              bigcal_vector(i+bigcal_prot_maxhits),1.)

               newavg = newavg + c_new
               nnewavg = nnewavg + 1
               if(i + bigcal_prot_maxhits.eq.bigcal_ivect(ired)) then
                  
                  ired = ired + 1
                  if(irow.ne.bigcal_calib_iylo.and.irow.ne.bigcal_calib_iyhi.and.
     $                 icol.ne.bigcal_calib_ixlo(2).and.icol.ne.bigcal_calib_ixhi(2)) then
                     if(bid_bcal_cfac_new.gt.0.and.fillhist) call hf1(bid_bcal_cfac_new,
     $                    float(i+bigcal_prot_maxhits),bigcal_vector(i+bigcal_prot_maxhits))
                     
                     if(bid_bcal_cfac_dist.gt.0.and.fillhist) call hf1(bid_bcal_cfac_dist,
     $                    bigcal_vector(i+bigcal_prot_maxhits),1.)
                     redavg = redavg + c_new
                     nredavg = nredavg + 1
                  endif
               endif
            enddo
            
            newavg = newavg / nnewavg 
            redavg = redavg / nredavg

c     replace "empty" and "small diag." channels with the average new calibration constant:
c     also replace any channels that aren't in the reduced calib. matrix with the reduced avg.

            do i=1,nsmalldiag
               icell = bigcal_matr_ismalld(i)

               if(icell.le.bigcal_prot_maxhits) then
                  bigcal_prot_cfac(icell) = newavg
                  if(Nred.gt.0.and.Nred.le.bigcal_all_maxhits) then
                     bigcal_prot_cfac(icell) = redavg
                  endif
               else 
                  bigcal_rcs_cfac(icell-bigcal_prot_maxhits) = newavg
                  if(Nred.gt.0.and.Nred.le.bigcal_all_maxhits) then
                     bigcal_rcs_cfac(icell-bigcal_prot_maxhits) = redavg
                  endif
               endif
            enddo
               
            do i=1,nempty
               icell = bigcal_matr_iempty(i)
               if(icell.le.bigcal_prot_maxhits) then
                  bigcal_prot_cfac(icell) = newavg
                  if(Nred.gt.0.and.Nred.le.bigcal_all_maxhits) then
                     bigcal_prot_cfac(icell) = redavg
                  endif
               else
                  bigcal_rcs_cfac(icell-bigcal_prot_maxhits) = newavg
                  if(Nred.gt.0.and.Nred.le.bigcal_all_maxhits) then
                     bigcal_rcs_cfac(icell-bigcal_prot_maxhits) = redavg
                  endif
               endif
            enddo

c     also replace any channels outside of the specified range with the average of channels calibrated 
c     using the reduced matrix. 

            ired=1

            do i=1,N
               if(i.eq.bigcal_ivect(ired)) then
                  ired = ired + 1
                  if(i.le.1024) then
                     irow= (i-1)/32 + 1
                     icol= mod(i-1,32) + 1
                     if(irow.eq.bigcal_calib_iylo.or.irow.eq.bigcal_calib_iyhi.or.
     $                    icol.eq.bigcal_calib_ixlo(1).or.icol.eq.bigcal_calib_ixhi(1)) then
                        bigcal_prot_cfac(i) = redavg
                     endif
                     
                     if(bigcal_prot_cfac(i).lt..05*redavg.or.bigcal_prot_cfac(i).gt.20.*redavg) then
                        bigcal_prot_cfac(i) = redavg
                     endif
                  else
                     irow=(i-1025)/30 + 33
                     icol=mod(i-1025,30) + 1
                     if(irow.eq.bigcal_calib_iylo.or.irow.eq.bigcal_calib_iyhi.or.
     $                    icol.eq.bigcal_calib_ixlo(2).or.icol.eq.bigcal_calib_ixhi(2)) then
                        bigcal_rcs_cfac(i-1024) = redavg
                     endif
                     
                     if(bigcal_rcs_cfac(i-1024).lt..05*redavg.or.bigcal_rcs_cfac(i-1024).gt.20.*redavg) then
                        bigcal_rcs_cfac(i-1024) = redavg
                     endif

                  endif
               else
                  if(i.le.1024) then
                     bigcal_prot_cfac(i) = redavg
                  else
                     bigcal_rcs_cfac(i-1024) = redavg
                  endif
               endif
            enddo

c     now that all the "empty" and "small diagonal" channels have been replaced with the appropriate averages,
c     fill the histograms with the new calibration constants:
            if(bid_bcal_oldxnew.gt.0) then
               do i=1,bigcal_all_maxhits
                  if(i.le.1024) then
                     call hf1(bid_bcal_oldxnew,float(i),bigcal_prot_cfac(i))
                  else
                     call hf1(bid_bcal_oldxnew,float(i),bigcal_rcs_cfac(i-1024))
                  endif
               enddo
            endif

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
