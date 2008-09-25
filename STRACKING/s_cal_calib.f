*=======================================================================
      subroutine s_cal_calib(mode)
*=======================================================================

c     SOS calorimeter calibration with electrons.
c
c     Input paramater mode = 0 means collect data for calibration,
c     otherwise calibrate.

*
      implicit none
*
      integer mode

      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
      include 'sos_tracking.cmn'
      include 'gen_run_info.cmn'
      include 'gen_event_info.cmn'
*
      integer ihit
      integer nblk
      real adc_pos,adc_neg

      integer nct_hit_blk(66),ipmt !sos
      logical write_out
c
      common/scal_calib/nct_hit_blk,ncall,spare_id
      integer ncall
      data ncall/0/

      real thr_lo,thr_hi        !thresholds on sammed raw calorimeter signal.
c
      integer spare_id
      logical ABORT
      character*80 err
c
      ncall=ncall+1

      if (ncall .eq. 1) then
	call g_IO_control(spare_id,'ANY',ABORT,err)  !get IO channel
         open(spare_id,file='s_cal_calib.raw_data')
         do ipmt=1,66
            nct_hit_blk(ipmt)=0
         enddo
      endif

      if(mode.eq.0) then        !collect data.

c        Choose clean single electron tracks within SOS momentum acceptance.
         if(  (sntracks_fp.eq.1).and.
     &        (snclusters_cal.eq.1).and.
     &        (sntracks_cal.eq.1).and.
     &        (scer_npe_sum.gt.4).and.
     &        (abs(sdelta_tar(1)).lt.20.).and.
     &        (abs(sbeta(1)-1.).lt.0.1).and.
     &        spare_id .ne. 0 ) then
***   &     (sbeta_chisq(1).ge.0.).and.(sbeta_chisq(1).lt.1.)  ) then

c
            write_out = .false.
            do ihit=1,scal_num_hits
               nblk=(scal_cols(ihit)-1)*smax_cal_rows+scal_rows(ihit)
               nct_hit_blk(nblk) = nct_hit_blk(nblk) + 1
               if (nct_hit_blk(nblk) .lt. 4000) write_out = .true.
            enddo
c
            if (write_out) then
c
               write(spare_id,'(i2,1x,f7.4,2(1x,f5.1,1x,f9.6))')
     &              scal_num_hits,sp_tar(1),
     &              strack_xc(1),sxp_fp(1),strack_yc(1),syp_fp(1)

               do ihit=1,scal_num_hits

                  if(scal_cols(ihit).le.scal_num_neg_columns) then
                     adc_neg=scal_adcs_neg(ihit)
                  else
                     adc_neg=0.
                  end if
                  adc_pos=scal_adcs_pos(ihit)
                  nblk=(scal_cols(ihit)-1)*smax_cal_rows+scal_rows(ihit)

                  write(spare_id,'(2(f9.3,1x),i2)'),
     &                 adc_pos,adc_neg,nblk

               end do

            endif               ! if write_out
c
         end if                 !electron in acceptance

      else                      !mode<>0, calibrate.

         close(spare_id)

         print*,'=========================================================='
         print*,'Calibrating SOS Calorimeter at event #',gen_event_id_number

         call scal_raw_thr(spare_id,thr_lo,thr_hi)
         print*,'lo & hi thresholds:', thr_lo,thr_hi
         call scal_clb_det(spare_id,gen_run_number,thr_lo,thr_hi)

         print*,'=========================================================='

      end if                    !mode=0

      end
*=======================================================================
      subroutine scal_raw_thr(lun,thr_lo,thr_hi)

      implicit none
      integer lun
      real thr_lo,thr_hi

c     Get thresholds around electron peak in summed raw calorimeter signal.

      integer*4 num_negs
      parameter (num_negs=22)   !sos
      integer*4 nhit
      real*4 adc_pos,adc_neg
      integer*4 nh
      integer*4 nb
      real*8 eb
c
      integer*4 nrow
      parameter (nrow=11)       !sos
      real*4 zbl
      parameter (zbl=10.)
      real*4 x,xp,y,yp
      real*4 xh,yh
      integer*4 nc
      real*4 sig,avr,t
      real*4 qdc
      integer nev

      real s_correct_cal_neg, s_correct_cal_pos, s_correct_cal

*
*     Get thresholds on total_signal/p_tar.
*
      open(lun,file='s_cal_calib.raw_data',err=989)
      avr=0.
      sig=0.
      nev=0
      do while(.true.)
         read(lun,*,end=3) nhit,eb,x,xp,y,yp
         qdc=0.
         do nh=1,nhit
            read(lun,*,end=3) adc_pos,adc_neg,nb
            nc=(nb-1)/nrow+1
            xh=x+xp*(nc-0.5)*zbl
            yh=y+yp*(nc-0.5)*zbl
            if(nb.le.num_negs) then
               qdc=qdc+adc_pos*s_correct_cal_pos(xh,yh)*0.5
               qdc=qdc+adc_neg*s_correct_cal_neg(xh,yh)*0.5
            else
               qdc=qdc+adc_pos*s_correct_cal(xh,yh)
            end if
         enddo
         eb=eb*1000.
         t=qdc/eb
c          write(lun,*) t
c         write(lun,*) t,nhit,eb,x,xp,y,yp,nev
         avr=avr+t
         sig=sig+t*t
         nev=nev+1
c         print*,eb,qdc,nev
      end do

 3    close(lun)
c      print*,avr,sig,nev
      avr=avr/nev
      sig=sqrt(sig/nev-avr*avr)
      thr_lo=avr-3.*sig
      thr_hi=avr+3.*sig
c      write(*,*) 'thr_lo=',thr_lo,'   thr_hi=',thr_hi

      return

 989  write(*,*) ' error opening file s_cal_calib.raw_data, channel',lun,
     *           ' in scal_raw_thr.f'
c
      end
*=======================================================================
	subroutine scal_clb_det(lun,nrun,thr_lo,thr_hi)
	implicit none
c
	include 'sos_data_structures.cmn'
	include 'sos_calorimeter.cmn'
c
	integer lun,nrun
	real thr_lo,thr_hi

	integer npmts
	parameter (npmts=66)	!sos
	integer npmts2
	parameter (npmts2=npmts*npmts)
	integer nrow
	parameter (nrow=11)     !sos
	real*8 q0(npmts)
	real*8 qm(npmts,npmts)
	real*8 qe(npmts)
	real*8 q(npmts)
	real*8 eb
	real*8 e0
	real*8 ac(npmts)
	real*8 au(npmts)
c      real*8 t
	real*8 s
	integer nev
	logical*1 eod
	integer i,j
	integer nf(npmts)
	integer minf
	parameter (minf=200) ! minimum number to hit pmt before including pmt in  calib
	integer nums(npmts)
	integer numsel
	real*8 q0s(npmts)
	real*8 qes(npmts)
	integer nsi,nsj
	real*8 acs(npmts)
	real*8 aus(npmts)
	real*8 aux(npmts2)
	integer jp
	integer spare_id
        logical ABORT
        character*80 err
	character*40 fn

	real xh,yh

	open(lun,file='s_cal_calib.raw_data')

	do i=1,npmts
	   q0(i)=0.
	   qe(i)=0.
	   do j=1,npmts
	      qm(i,j)=0.
	   end do
	   au(i)=0.
	   ac(i)=0.
	   nf(i)=0
	end do
	e0=0.
c
	nev=0
	eod=.false.
	do while(.not.eod)
	   call s_get_data(lun,eb,q,xh,yh,eod,thr_lo,thr_hi)
	   if(.not.eod) then
	      do i=1,npmts
		 if(q(i).gt.0.) then
		    q0(i)=q0(i)+q(i)
		    qe(i)=qe(i)+eb*q(i)
		    do j=1,npmts
		       qm(i,j)=qm(i,j)+q(i)*q(j)
		    end do
		    nf(i)=nf(i)+1
		 end if
	      end do
	      e0=e0+eb
	      nev=nev+1
c	      if(nev/1000*1000.eq.nev) write(*,'(e10.3,i7)') e0,nev
	   end if
	end do
	close(lun)

	do i=1,npmts
	   q0(i)=q0(i)/nev
	   qe(i)=qe(i)/nev
	   do j=1,npmts
	      qm(i,j)=qm(i,j)/nev
	   end do
	end do
	e0=e0/nev

	numsel=0
	do i=1,npmts
	   if(nf(i).ge.minf) then
	      numsel=numsel+1
	      nums(numsel)=i
c	      print*,nums(numsel),numsel,nf(i)
           else
	      write(*,*) ' PMT ',i,' only ',nf(i),' events. Will not to be calibrated. Gain is set to 0.'
	   end if
	end do
c	print*,'numsel =',numsel
	write(*,'(''Number of events for each PMT for calib for run '',i7,'', '',
     1    i6,'' events processed'')') nrun,nev
	write(*,*) ' PMT with less than', minf,' events  are not included in calibration.'
	write(*,*)
	write(*,11) 'scal_pos_gain_cor=',(nf(i),i=       1,  nrow)
	write(*,11) '                  ',(nf(i),i=  nrow+1,2*nrow)
	write(*,11) '                  ',(nf(i),i=2*nrow+1,3*nrow)
	write(*,11) '                  ',(nf(i),i=3*nrow+1,4*nrow)
	write(*,11) 'scal_neg_gain_cor=',(nf(i),i=4*nrow+1,5*nrow)
	write(*,11) '                  ',(nf(i),i=5*nrow+1,6*nrow)
	write(*,11) '                  ',(0.,   i=6*nrow+1,7*nrow)
	write(*,11) '                  ',(0.,   i=7*nrow+1,8*nrow)
c
	do i=1,numsel
	   nsi=nums(i)
	   q0s(i)=q0(nsi)
	   qes(i)=qe(nsi)
	   do j=1,numsel
	      nsj=nums(j)
	      jp=j+(i-1)*numsel
	      aux(jp)=qm(nsj,nsi)
c	      write(65,'(e12.5)') aux(jp)
	   end do
	end do

	call calib(e0,q0s,qes,aux,numsel,numsel*numsel,aus,acs)

	do i=1,numsel
	   nsi=nums(i)
	   au(nsi)=aus(i)
	   ac(nsi)=acs(i)
	end do

c	write(*,'(2e10.3,i5)') (ac(i),au(i),i,i=1,npmts)

	write(fn,'(a17,i5.5)') 'PARAM/scal.param.',nrun
	call g_IO_control(spare_id,'ANY',ABORT,err)  !get IO channel
	open(spare_id,file=fn)

	write(spare_id,'(''; Calibration constants for run '',i7,'', '',
     1    i6,'' events processed'')') nrun,nev
	write(spare_id,*)

        write(spare_id,10) 'scal_pos_gain_cor=',(ac(i)*1.D+3,i=       1,  nrow)
        write(spare_id,10) '                  ',(ac(i)*1.D+3,i=  nrow+1,2*nrow)
        write(spare_id,10) '                  ',(ac(i)*1.D+3,i=2*nrow+1,3*nrow)
        write(spare_id,10) '                  ',(ac(i)*1.D+3,i=3*nrow+1,4*nrow)
        write(spare_id,10) 'scal_neg_gain_cor=',(ac(i)*1.D+3,i=4*nrow+1,5*nrow)
        write(spare_id,10) '                  ',(ac(i)*1.D+3,i=5*nrow+1,6*nrow)
        write(spare_id,10) '                  ',(0.,         i=6*nrow+1,7*nrow)
        write(spare_id,10) '                  ',(0.,         i=7*nrow+1,8*nrow)
*	write(spare_id,10) 'scal_pos_gain_cor=',(ac(i)*2.D+3,i=       1,  nrow)
*	write(spare_id,10) '                  ',(ac(i)*2.D+3,i=  nrow+1,2*nrow)
*	write(spare_id,10) '                  ',(ac(i)*1.D+3,i=2*nrow+1,3*nrow)
*	write(spare_id,10) '                  ',(ac(i)*1.D+3,i=3*nrow+1,4*nrow)
*	write(spare_id,10) 'scal_neg_gain_cor=',(ac(i)*2.D+3,i=4*nrow+1,5*nrow)
*	write(spare_id,10) '                  ',(ac(i)*2.D+3,i=5*nrow+1,6*nrow)
*	write(spare_id,10) '                  ',(0.,         i=6*nrow+1,7*nrow)
*	write(spare_id,10) '                  ',(0.,         i=7*nrow+1,8*nrow)

	close(spare_id)
        call G_IO_control(spare_ID,'FREE',ABORT,err) !free up IO channel

	write(*,*)
	write(*,'(''Calibration constants for run '',i7,'', '',
     1    i6,'' events processed'')') nrun,nev
	write(*,*)
	write(*,*) ' constants written to ',fn
	write(*,*)
        write(*,10) 'scal_pos_gain_cor=',(ac(i)*1.D+3,i=       1,  nrow)
        write(*,10) '                  ',(ac(i)*1.D+3,i=  nrow+1,2*nrow)
        write(*,10) '                  ',(ac(i)*1.D+3,i=2*nrow+1,3*nrow)
        write(*,10) '                  ',(ac(i)*1.D+3,i=3*nrow+1,4*nrow)
        write(*,10) 'scal_neg_gain_cor=',(ac(i)*1.D+3,i=4*nrow+1,5*nrow)
        write(*,10) '                  ',(ac(i)*1.D+3,i=5*nrow+1,6*nrow)
        write(*,10) '                  ',(0.,         i=6*nrow+1,7*nrow)
        write(*,10) '                  ',(0.,         i=7*nrow+1,8*nrow)
*	write(*,10) 'scal_pos_gain_cor=',(ac(i)*2.D+3,i=       1,  nrow)
*	write(*,10) '                  ',(ac(i)*2.D+3,i=  nrow+1,2*nrow)
*	write(*,10) '                  ',(ac(i)*1.D+3,i=2*nrow+1,3*nrow)
*	write(*,10) '                  ',(ac(i)*1.D+3,i=3*nrow+1,4*nrow)
*	write(*,10) 'scal_neg_gain_cor=',(ac(i)*2.D+3,i=4*nrow+1,5*nrow)
*	write(*,10) '                  ',(ac(i)*2.D+3,i=5*nrow+1,6*nrow)
*	write(*,10) '                  ',(0.,         i=6*nrow+1,7*nrow)
*	write(*,10) '                  ',(0.,         i=7*nrow+1,8*nrow)

 10	format(a18,13(f6.3,','))
 11	format(a18,13(i5,','))

	open(lun,file='s_cal_calib.raw_data')
	call g_IO_control(spare_id,'ANY',ABORT,err)  !get IO channel
        open(spare_id,file='s_cal_calib.cal_data')
	write(*,*) 'In sos shower cal  creating file s_cal_calib.cal_data, ',
     *       'channel ',spare_id

	nev=0
	eod=.false.
	do while(.not.eod)
	   call s_get_data(lun,eb,q,xh,yh,eod,0.,1.E+8)
	   if(.not.eod) then
	      s=0.
*	      t=0.
	      do i=1,npmts
	         s=s+q(i)*ac(i)
*	         t=t+q(i)*au(i)
	      end do
              write(spare_id,*) s,eb,xh,yh
	   end if
	end do

	close(lun)
	close(spare_id)
        call G_IO_control(spare_ID,'FREE',ABORT,err)  !free up IO channel

	end
*=======================================================================
c
c...  Done already in h_cal_calib.f.
c
c	subroutine calib(e0,q0,qe,aux,npmts,npmts2,au,ac)
c	implicit none
c	integer npmts,npmts2
c	real*8 e0
c	real*8 q0(npmts)
c	real*8 qe(npmts)
c	real*8 aux(npmts2)
c	real*8 ac(npmts)
c	real*8 au(npmts)
c	real*8 qm(npmts,npmts)
c	real*8 t
c	real*8 s
c	integer ifail
c	integer i,j
c	integer jp
c
c	do i=1,npmts
c	   do j=1,npmts
c	      jp=j+(i-1)*npmts
c	      qm(j,i)=aux(jp)
cD	      write(66,'(e12.5)') qm(j,i)
c	   end do
c	end do
c
c	print*,'Calib: npmts =',npmts
c	print*,' '
c
c	print*,'Inversing the Matrix...'
c	call smxinv(qm,npmts,ifail)
c	if(ifail.ne.0) then
c	   stop '*** Singular Matrix ***'
c	else
c	   print*,'                    ...done.'
c	end if
c
c	do i=1,npmts
c	   au(i)=0.
c	   do j=1,npmts
c	      au(i)=au(i)+qm(i,j)*qe(j)
c	   end do
c	end do
c
c	s=0.
c	do i=1,npmts
c	   t=0.
c	   do j=1,npmts
c	      t=t+qm(i,j)*q0(j)
c	   end do
c	   s=s+q0(i)*t
c	end do
c
c	t=0.
c	do i=1,npmts
c	   t=t+au(i)*q0(i)
c	end do
c	s=(e0-t)/s
c
c	do i=1,npmts
c	   t=0.
c	   do j=1,npmts
c	      t=t+qm(i,j)*q0(j)
c	   end do
c	   ac(i)=s*t+au(i)
c	end do
c
c	end
*-----------------------------------------------------------------------
      subroutine s_get_data(lun,eb,q,xh,yh,eod,thr_lo,thr_hi)
      implicit none
c
      integer lun
      real*8 eb
      integer*4 num_blocks,num_negs,num_pmts
      parameter (num_blocks=44,num_negs=22,num_pmts=66) !sos.
      real*8 q(num_pmts)
      logical*1 eod

      integer*4 nhit 
      real*4 adc_pos,adc_neg 
      integer*4 nh 
      integer*4 nb 
c
      integer*4 nrow
      parameter (nrow=11) !sos
      real*4 zbl
      parameter (zbl=10.)
      real*4 x,xp,y,yp
      real*4 xh,yh
      integer*4 nc
      real*4 s_correct_cal
      real*4 s_correct_cal_pos,s_correct_cal_neg
      real*4 thr_lo,thr_hi
      logical*1 good_ev
      real*4 qnet

      good_ev=.false.
      do while(.not.good_ev)

	 eb=0.d0
	 do nb=1,num_pmts
	    q(nb)=0.d0
	 end do
	 qnet=0.
	 eod=.true.

	 read(lun,*,end=5) nhit,eb,x,xp,y,yp
	 do nh=1,nhit
	    read(lun,*,end=5) adc_pos,adc_neg,nb
	    nc=(nb-1)/nrow+1
	    xh=x+xp*(nc-0.5)*zbl
	    yh=y+yp*(nc-0.5)*zbl
	    if(nb.le.num_negs) then
	       q(nb)=adc_pos*s_correct_cal_pos(xh,yh)
	       q(num_blocks+nb)=adc_neg*s_correct_cal_neg(xh,yh)
	       qnet=qnet+0.5*(q(nb)+q(num_blocks+nb))
	    else
	       q(nb)=adc_pos*s_correct_cal(xh,yh)
	       qnet=qnet+q(nb)
	    end if
	 enddo
	 eod=.false.

	 qnet=qnet/(eb*1000.)
	 good_ev=(qnet.gt.thr_lo).and.(qnet.lt.thr_hi)

c	 write(99,*) qnet

      end do   !.not.good_ev

 5    continue

      end
*-----------------------------------------------------------------------
c
c...  Done already in h_cal_calib.f.
c
c     SUBROUTINE SMXINV (A,NDIM,IFAIL)
C
C CERN PROGLIB# F107    SMXINV          .VERSION KERNFOR  1.0   720503
C ORIG. 03/05/72 CL
C
c     REAL*8 A(*),RI(100)
c     INTEGER*4 INDEX(100)
C
c     DATA  TOL / 1.D-14/
C
c     IFAIL=0
c     N=NDIM
c     NP1=N+1
c        DO 10 I=1,N
c   10 INDEX(I)=1
C
c        DO 80 I=1,N
C
C--                FIND PIVOT
c     PIVOT=0.0D0
c     JJ=1
c        DO 20 J=1,N
c     IF (INDEX(J).EQ.0) GO TO 19
c     ELM=DABS (A(JJ))
c     IF (ELM.LE.PIVOT) GO TO 19
c     PIVOT=ELM
c     K=J
c     KK=JJ
c   19 JJ=JJ+NP1
c   20 CONTINUE
c     IF (PIVOT/DABS(A(1)).LT.TOL) GO TO 100
c     INDEX(K)=0
c     PIVOT=-A(KK)
C
C--                ELIMINATION
c     KJ=K
c     NP=N
C
c        DO 70 J=1,N
c     IF (J-K) 34,30,34
C
c   30 A(KJ)=1.0D0/PIVOT
c     RI(J)=0.0D0
c     NP=1
c     GO TO 70
C
c   34 ELM=-A(KJ)
c   40 RI(J)=ELM/PIVOT
c     IF (ELM.EQ.0.0D0) GO TO 50
C
c     JL=J
c        DO 45 L=1,J
c     A(JL)=A(JL)+ELM*RI(L)
c   45 JL=JL+N
C
c   50 A(KJ)=RI(J)
C
c   70 KJ=KJ+NP
C
c   80 CONTINUE
C
C--                CHANGE THE SIGN AND PROVISIONAL FILL-UP
c     IJ0=1
c     JI0=1
c        DO 95 I=1,N
c     IJ=IJ0
c     JI=JI0
C
c        DO 90 J=1,I
c     A(IJ)=-A(IJ)
c     A(JI)=A(IJ)
c     IJ=IJ+N
c     JI=JI+1
c   90 CONTINUE
C
c     IJ0=IJ0+1
c     JI0=JI0+N
c   95 CONTINUE
c     RETURN
C
C--                FAILURE RETURN
c  100 IFAIL=1
c     RETURN
c     END
*=======================================================================
