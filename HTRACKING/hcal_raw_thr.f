      subroutine hcal_raw_thr(lun,thr_lo,thr_hi)
      implicit none
      integer lun
      real thr_lo,thr_hi

c     Get thresholds around electron peak in summed raw calorimeter signal.

      integer*4 num_negs
      parameter (num_negs=26)   !hms
      integer*4 nhit
      real*4 adc_pos,adc_neg
      integer*4 nh
      integer*4 nb
      real*8 eb
c
      integer*4 nrow
      parameter (nrow=13) !hms
      real*4 zbl
      parameter (zbl=10.)
      real*4 x,xp,y,yp
      real*4 xh,yh
      integer*4 nc
      real*4 sig,avr,t
      real*4 qdc
      integer nev

      real h_correct_cal_neg, h_correct_cal_pos, h_correct_cal

*
*     Get thresholds on total_signal/p_tar.
*
      open(lun,err=989)
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
               qdc=qdc+adc_pos*h_correct_cal_pos(xh,yh)*0.5
               qdc=qdc+adc_neg*h_correct_cal_neg(xh,yh)*0.5
            else
               qdc=qdc+adc_pos*h_correct_cal(xh,yh)
            end if
         enddo
         eb=eb*1000.
         t=qdc/eb
D          write(lun,*) t
D         write(lun,*) t,nhit,eb,x,xp,y,yp,nev
         avr=avr+t
         sig=sig+t*t
         nev=nev+1
c         print*,eb,qdc,nev
      end do

 3    close(lun)
D      print*,avr,sig,nev
      avr=avr/nev
      sig=sqrt(sig/nev-avr*avr)
      thr_lo=avr-3.*sig
      thr_hi=avr+3.*sig
D      write(*,*) 'thr_lo=',thr_lo,'   thr_hi=',thr_hi
 1    return
*
 989  write(*,*) ' error opening file',lun,' in hcal_raw_thr.f'
c
      end
