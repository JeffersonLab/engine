	subroutine hcal_clb_det(lun,nrun,thr_lo,thr_hi)
	implicit none
c
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
c
	integer lun,nrun
	real thr_lo,thr_hi

	integer maxev
	parameter (maxev=75 000)
	integer npmts
	parameter (npmts=78)	!hms
	integer npmts2
	parameter (npmts2=npmts*npmts)
	integer nrow
	parameter (nrow=13)     !hms
	real*8 q0(npmts)
	real*8 qm(npmts,npmts)
	real*8 qe(npmts)
	real*8 q(npmts)
	real*8 eb
	real*8 e0
	real*8 ac(npmts)
	real*8 au(npmts)
	real*8 t
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

	real xh,yh		!******

	open(lun)
D	open(99)

	do i=1,npmts
	   q0(i)=0.
	   qe(i)=0.
	   do j=1,npmts
	      qm(i,j)=0.
	   end do
	   au(i)=0.
	   if (i .le. 26) then 
              ac(i)=hcal_pos_gain_cor(i)/2.d+3
	   elseif (i .gt. 26) then
              ac(i)=hcal_pos_gain_cor(i)/1.d+3
	   elseif (i .gt. 52) then
             ac(i)=hcal_neg_gain_cor(i-52)/2.d+3
           else
             ac(i) =0
           endif
	   nf(i)=0
	end do
	e0=0.
c
	nev=0
	eod=.false.
	do while((.not.eod).and.(nev.lt.maxev))
******	   call get_data(lun,eb,q,eod,thr_lo,thr_hi)
	   call get_data(lun,eb,q,xh,yh,eod,thr_lo,thr_hi)
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
D	close(99)

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
	      write(*,*) ' PMT ',i,' only ',nf(i),' events. Will not to be calibrated. Keep old gain value'
	   end if
	end do
D	print*,'numsel =',numsel
	write(*,'(''Number of events for each PMT for calib for run '',i7,'', '',
	1    i6,'' events processed'')') nrun,nev
	write(*,*) ' PMT with less than', minf,'events  are not included in calibration.'
	write(*,*)
      write(*,11) 'hcal_pos_gain_cor=',(nf(i),i=       1,  nrow)
      write(*,11) '                  ',(nf(i),i=  nrow+1,2*nrow)
      write(*,11) '                  ',(nf(i),i=2*nrow+1,3*nrow)
      write(*,11) '                  ',(nf(i),i=3*nrow+1,4*nrow)
      write(*,11) 'hcal_neg_gain_cor=',(nf(i),i=4*nrow+1,5*nrow)
      write(*,11) '                  ',(nf(i),i=5*nrow+1,6*nrow)
      write(*,11) '                  ',(0.,         i=6*nrow+1,7*nrow)
      write(*,11) '                  ',(0.,         i=7*nrow+1,8*nrow)
c
	do i=1,numsel
	   nsi=nums(i)
	   q0s(i)=q0(nsi)
	   qes(i)=qe(nsi)
	   do j=1,numsel
	      nsj=nums(j)
	      jp=j+(i-1)*numsel
	      aux(jp)=qm(nsj,nsi)
D	      write(65,'(e12.5)') aux(jp)
	   end do
	end do

	call calib(e0,q0s,qes,aux,numsel,numsel*numsel,aus,acs)

	do i=1,numsel
	   nsi=nums(i)
	   au(nsi)=aus(i)
	   ac(nsi)=acs(i)
	end do

D	write(*,'(2e10.3,i5)') (ac(i),au(i),i,i=1,npmts)

	write(fn,'(a11,i5.5)') 'hcal.param.',nrun
	call g_IO_control(spare_id,'ANY',ABORT,err)  !get IO channel
	open(spare_id,file=fn)

	write(spare_id,'(''Calibration constants for run '',i7,'', '',
	1    i6,'' events processed'')') nrun,nev
	write(spare_id,*)

*      write(spare_id,10) 'hcal_pos_gain_cor=',(ac(i)*1.D+3,i=       1,  nrow)
*      write(spare_id,10) '                  ',(ac(i)*1.D+3,i=  nrow+1,2*nrow)
*      write(spare_id,10) '                  ',(ac(i)*1.D+3,i=2*nrow+1,3*nrow)
*      write(spare_id,10) '                  ',(ac(i)*1.D+3,i=3*nrow+1,4*nrow)
*      write(spare_id,10) 'hcal_neg_gain_cor=',(ac(i)*1.D+3,i=4*nrow+1,5*nrow)
*      write(spare_id,10) '                  ',(ac(i)*1.D+3,i=5*nrow+1,6*nrow)
*      write(spare_id,10) '                  ',(0.,         i=6*nrow+1,7*nrow)
*      write(spare_id,10) '                  ',(0.,         i=7*nrow+1,8*nrow)
      write(spare_id,10) 'hcal_pos_gain_cor=',(ac(i)*2.D+3,i=       1,  nrow)
      write(spare_id,10) '                  ',(ac(i)*2.D+3,i=  nrow+1,2*nrow)
      write(spare_id,10) '                  ',(ac(i)*1.D+3,i=2*nrow+1,3*nrow)
      write(spare_id,10) '                  ',(ac(i)*1.D+3,i=3*nrow+1,4*nrow)
      write(spare_id,10) 'hcal_neg_gain_cor=',(ac(i)*2.D+3,i=4*nrow+1,5*nrow)
      write(spare_id,10) '                  ',(ac(i)*2.D+3,i=5*nrow+1,6*nrow)
      write(spare_id,10) '                  ',(0.,         i=6*nrow+1,7*nrow)
      write(spare_id,10) '                  ',(0.,         i=7*nrow+1,8*nrow)

	close(spare_id)
        call G_IO_control(spare_ID,'FREE',ABORT,err)  !free up IO channel

	write(*,'(''Calibration constants for run '',i7,'', '',
	1    i6,'' events processed'')') nrun,nev
	write(*,*)
	write(*,*) ' constants wrriten to ',fn
	write(*,*)
      write(*,10) 'hcal_pos_gain_cor=',(ac(i)*2.D+3,i=       1,  nrow)
      write(*,10) '                  ',(ac(i)*2.D+3,i=  nrow+1,2*nrow)
      write(*,10) '                  ',(ac(i)*1.D+3,i=2*nrow+1,3*nrow)
      write(*,10) '                  ',(ac(i)*1.D+3,i=3*nrow+1,4*nrow)
      write(*,10) 'hcal_neg_gain_cor=',(ac(i)*2.D+3,i=4*nrow+1,5*nrow)
      write(*,10) '                  ',(ac(i)*2.D+3,i=5*nrow+1,6*nrow)
      write(*,10) '                  ',(0.,         i=6*nrow+1,7*nrow)
      write(*,10) '                  ',(0.,         i=7*nrow+1,8*nrow)

10    format(a18,13(f6.3,','))
 11	format(a18,13(i5,','))

	open(lun)
	call g_IO_control(spare_id,'ANY',ABORT,err)  !get IO channel
        open(spare_id)
	write(*,*) 'In hms shower cal  creating file fort.',spare_id

	nev=0
	eod=.false.
	do while(.not.eod)
*****	   call get_data(lun,eb,q,eod,0.,1.E+8)
	   call get_data(lun,eb,q,xh,yh,eod,0.,1.E+8)
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
	subroutine calib(e0,q0,qe,aux,npmts,npmts2,au,ac)
	implicit none
	integer npmts,npmts2
	real*8 e0
	real*8 q0(npmts)
	real*8 qe(npmts)
	real*8 aux(npmts2)
	real*8 ac(npmts)
	real*8 au(npmts)
	real*8 qm(npmts,npmts)
	real*8 t
	real*8 s
	integer ifail
	integer i,j
	integer jp

	do i=1,npmts
	   do j=1,npmts
	      jp=j+(i-1)*npmts
	      qm(j,i)=aux(jp)
D	      write(66,'(e12.5)') qm(j,i)
	   end do
	end do

	print*,'Calib: npmts =',npmts
	print*,' '

	print*,'Inversing the Matrix...'
	call smxinv(qm,npmts,ifail)
	if(ifail.ne.0) then
	   stop '*** Singular Matrix ***'
	else
	   print*,'                    ...done.'
	end if

	do i=1,npmts
	   au(i)=0.
	   do j=1,npmts
	      au(i)=au(i)+qm(i,j)*qe(j)
	   end do
	end do

	s=0.
	do i=1,npmts
	   t=0.
	   do j=1,npmts
	      t=t+qm(i,j)*q0(j)
	   end do
	   s=s+q0(i)*t
	end do

	t=0.
	do i=1,npmts
	   t=t+au(i)*q0(i)
	end do
	s=(e0-t)/s

	do i=1,npmts
	   t=0.
	   do j=1,npmts
	      t=t+qm(i,j)*q0(j)
	   end do
	   ac(i)=s*t+au(i)
	end do

	end
*=======================================================================
*****      subroutine get_data(lun,eb,q,eod,thr_lo,thr_hi)
      subroutine get_data(lun,eb,q,xh,yh,eod,thr_lo,thr_hi)
      implicit none
c
      integer lun
      real*8 eb
      integer*4 num_blocks,num_negs,num_pmts
      parameter (num_blocks=52,num_negs=26,num_pmts=78) !hms.
      real*8 q(num_pmts)
      logical*1 eod

      integer*4 nhit 
      real*4 adc_pos,adc_neg 
      integer*4 nh 
      integer*4 nb 
c
      integer*4 nrow
      parameter (nrow=13) !hms
      real*4 zbl
      parameter (zbl=10.)
      real*4 x,xp,y,yp
      real*4 xh,yh
      integer*4 nc
      real*4 h_correct_cal
      real*4 h_correct_cal_pos,h_correct_cal_neg
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
	       q(nb)=adc_pos*h_correct_cal_pos(xh,yh)
	       q(num_blocks+nb)=adc_neg*h_correct_cal_neg(xh,yh)
	       qnet=qnet+0.5*(q(nb)+q(num_blocks+nb))
	    else
	       q(nb)=adc_pos*h_correct_cal(xh,yh)
	       qnet=qnet+q(nb)
	    end if
	 enddo
	 eod=.false.

	 qnet=qnet/(eb*1000.)
	 good_ev=(qnet.gt.thr_lo).and.(qnet.lt.thr_hi)

D	 write(99,*) qnet

      end do   !.not.good_ev



 5    continue

      end
*=======================================================================
*      function correct(x,y) !hms
**
*      correct=exp(y/200.) !200 cm atten length.
*      correct=correct/(1. + y*y/8000.)
**
*      end
*=======================================================================
*      function correct_neg(x,y)
**
*      implicit none
**
*      real*4 a,b,c	! Fit parameters.
*      parameter (a=1.8903885,b=-0.22988863,c=-0.2724031) ! Run # 23121 data.
*
*      real*4 x,y         !Impact point coordinates
*      real*4 correct_neg
*      real*4 d,al	! Auxiliary variables.
**
*      d=35.-y
*      al=alog(d)
*      correct_neg=1./(a+b*al+c/al)
**   
*      end
*=======================================================================
*      function correct_pos(x,y)
**
*      implicit none
**
*      real a,b,c	! Fit parameters.
*      parameter (a=1.8903885,b=-0.22988863,c=-0.2724031) ! Run # 23121 data.
**
*      real*4 x,y         !Impact point coordinates
*      real*4 correct_pos
*      real*4 d,al	! Auxiliary variables.
**
*      d=35.+y
*      al=alog(d)
*      correct_pos=1./(a+b*al+c/al)
**
*      end
*=======================================================================
      SUBROUTINE SMXINV (A,NDIM,IFAIL)
C
C CERN PROGLIB# F107    SMXINV          .VERSION KERNFOR  1.0   720503
C ORIG. 03/05/72 CL
C
      REAL*8 A(*),RI(100)
      INTEGER*4 INDEX(100)
C
      DATA  TOL / 1.D-14/
C
      IFAIL=0
      N=NDIM
      NP1=N+1
         DO 10 I=1,N
   10 INDEX(I)=1
C
         DO 80 I=1,N
C
C--                FIND PIVOT
      PIVOT=0.0D0
      JJ=1
         DO 20 J=1,N
      IF (INDEX(J).EQ.0) GO TO 19
      ELM=DABS (A(JJ))
      IF (ELM.LE.PIVOT) GO TO 19
      PIVOT=ELM
      K=J
      KK=JJ
   19 JJ=JJ+NP1
   20 CONTINUE
      IF (PIVOT/DABS(A(1)).LT.TOL) GO TO 100
      INDEX(K)=0
      PIVOT=-A(KK)
C
C--                ELIMINATION
      KJ=K
      NP=N
C
         DO 70 J=1,N
      IF (J-K) 34,30,34
C
   30 A(KJ)=1.0D0/PIVOT
      RI(J)=0.0D0
      NP=1
      GO TO 70
C
   34 ELM=-A(KJ)
   40 RI(J)=ELM/PIVOT
      IF (ELM.EQ.0.0D0) GO TO 50
C
      JL=J
         DO 45 L=1,J
      A(JL)=A(JL)+ELM*RI(L)
   45 JL=JL+N
C
   50 A(KJ)=RI(J)
C
   70 KJ=KJ+NP
C
   80 CONTINUE
C
C--                CHANGE THE SIGN AND PROVISIONAL FILL-UP
      IJ0=1
      JI0=1
         DO 95 I=1,N
      IJ=IJ0
      JI=JI0
C
         DO 90 J=1,I
      A(IJ)=-A(IJ)
      A(JI)=A(IJ)
      IJ=IJ+N
      JI=JI+1
   90 CONTINUE
C
      IJ0=IJ0+1
      JI0=JI0+N
   95 CONTINUE
      RETURN
C
C--                FAILURE RETURN
  100 IFAIL=1
      RETURN
      END
*=======================================================================
