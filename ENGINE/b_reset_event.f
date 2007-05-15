      subroutine b_reset_event(ABORT,err)

      implicit none
      save
      
      character*13 here
      parameter(here='b_reset_event')

      logical ABORT
      character*(*) err

      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_geometry.cmn'
      include 'bigcal_shower_parms.cmn'
      include 'bigcal_tof_parms.cmn'
c      include 'gen_units.par'
      include 'gen_constants.par'

      integer i,j
      integer irow,icol,icell,jrow,jcol,jcell
      integer irow8,icol8,igroup8
      integer igroup64,ihalf64,ilogic

c$$$      BIGCAL_THETA_DEG=90.
c$$$      BIGCAL_THETA_RAD=tt/2.
c$$$      BIGCAL_SINTHETA=1.
c$$$      BIGCAL_COSTHETA=0.
c$$$      BIGCAL_R_TGT = 1000.
c$$$      BIGCAL_HEIGHT = 0.

c     group of 8 hit arrays, raw, decoded, and "good"


      bigcal_max_adc = 0.
      bigcal_iymax_adc = 0
      bigcal_ixmax_adc = 0
      bigcal_max_adc_final = 0.
      bigcal_iymax_final = 0
      bigcal_ixmax_final = 0

      BIGCAL_TDC_NHIT = 0 
      BIGCAL_TDC_NDECODED = 0
      BIGCAL_TIME_NGOOD = 0
      do i=1,BIGCAL_TDC_MAXHITS
         BIGCAL_TDC_RAW_IGROUP(i) = 0
         BIGCAL_TDC_RAW_IROW(i) = 0
         BIGCAL_TDC_RAW(i) = 0
         BIGCAL_TDC_IROW(i) = 0
         BIGCAL_TDC_IGROUP(i) = 0
         BIGCAL_TDC(i) = 0
         BIGCAL_TIME_IROW(i) = 0
         BIGCAL_TIME_IGROUP(i) = 0
         BIGCAL_HIT_TIME(i) = 0.
      enddo

c     trigger hit arrays, raw, "decoded" and "good"

      BIGCAL_TRIG_NHIT=0
      BIGCAL_TRIG_NDECODED = 0
      BIGCAL_TRIG_NGOOD=0
      do i=1,BIGCAL_TRIG_MAXHITS
         BIGCAL_TRIG_IGROUP(i) = 0
         BIGCAL_TRIG_IHALF(i) = 0
         BIGCAL_TRIG_TDC_RAW(i) = 0
         BIGCAL_TRIG_ADC_RAW(i) = 0
         BIGCAL_TRIG_DEC_IGROUP(i) = 0
         BIGCAL_TRIG_DEC_IHALF(i) = 0
         BIGCAL_TRIG_TDC_DECODED(i) = 0
         BIGCAL_TRIG_ADC_DECODED(i) = 0.
         BIGCAL_TRIG_GOOD_IGROUP(i) = 0
         BIGCAL_TRIG_GOOD_IHALF(i) = 0
         BIGCAL_TRIG_TIME_GOOD(i) = 0.
         BIGCAL_TRIG_ADC_GOOD(i) = 0.
      enddo

c     calorimeter hit arrays, raw, decoded, and good
c     also detector arrays
      BIGCAL_PROT_NHIT = 0
      BIGCAL_PROT_NGOOD= 0
      BIGCAL_RCS_NHIT = 0
      BIGCAL_RCS_NGOOD = 0
      do i=1,BIGCAL_PROT_MAXHITS
         BIGCAL_PROT_IX(i) = 0
         BIGCAL_PROT_IY(i) = 0
         BIGCAL_PROT_ADC_RAW(i) = 0
         BIGCAL_PROT_IXGOOD(i) = 0
         BIGCAL_PROT_IYGOOD(i) = 0
         BIGCAL_PROT_ADC_DECODED(i) = 0.
         BIGCAL_PROT_ADC_GOOD(i) = 0.
         BIGCAL_PROT_ECELL(i) = 0.
         BIGCAL_PROT_XGOOD(i) = 0.
         BIGCAL_PROT_YGOOD(i) = 0.
         BIGCAL_PROT_RAW_DET(i) = 0
         BIGCAL_PROT_GOOD_DET(i) = 0.
         BIGCAL_PROT_GOOD_HIT(i) = .false.
      enddo
      
      do i=1,BIGCAL_RCS_MAXHITS
         BIGCAL_RCS_IX(i) = 0
         BIGCAL_RCS_IY(i) = 0
         BIGCAL_RCS_ADC_RAW(i) = 0
         BIGCAL_RCS_IXGOOD(i) = 0
         BIGCAL_RCS_IYGOOD(i) = 0
         BIGCAL_RCS_ADC_DECODED(i) = 0.
         BIGCAL_RCS_ADC_GOOD(i) = 0.
         BIGCAL_RCS_ECELL(i) = 0.
         BIGCAL_RCS_XGOOD(i) = 0.
         BIGCAL_RCS_YGOOD(i) = 0.
         BIGCAL_RCS_RAW_DET(i) = 0
         BIGCAL_RCS_GOOD_DET(i) = 0.
         BIGCAL_RCS_GOOD_HIT(i) = .false.
      enddo

      do i=1,BIGCAL_MAX_TDC
         BIGCAL_TDC_DET(i) = 0
         BIGCAL_TIME_DET(i) = 0.
         BIGCAL_TIME_ADC_SUM(i) = 0.
         BIGCAL_TIME_GOOD_HIT(i) = .false.
         BIGCAL_TIME_DET_NHIT(i) = 0
      enddo

      do i=1,BIGCAL_LOGIC_GROUPS
         BIGCAL_TRIG_TDC_DET(i) = 0
         BIGCAL_TRIG_ADC_DET(i) = 0
         BIGCAL_TRIG_TIME_DET(i) = 0.
         BIGCAL_TRIG_TIME_NHIT(i) = 0
         BIGCAL_TRIG_GOOD_DET(i) = 0.
         BIGCAL_TRIG_GOOD_HIT(i) = .false.
         BIGCAL_TRIG_ADC_SUM(i) = 0.       
      enddo
c     finally, zero the cluster arrays:
      BIGCAL_PROT_NCLSTR = 0
      do i=1,BIGCAL_PROT_NCLSTR_MAX
         BIGCAL_PROT_CLSTR_NCELL(i) = 0
         BIGCAL_PROT_CLSTR_IYMAX(i) = 0
         BIGCAL_PROT_CLSTR_IXMAX(i) = 0
         do j=1,BIGCAL_CLSTR_NCELL_MAX
            BIGCAL_PROT_CLSTR_IYCELL(i,j) = 0
            BIGCAL_PROT_CLSTR_IXCELL(i,j) = 0
            BIGCAL_PROT_CLSTR_ECELL(i,j) = 0.
            BIGCAL_PROT_CLSTR_XCELL(i,j) = 0.
            BIGCAL_PROT_CLSTR_YCELL(i,j) = 0.
            BIGCAL_PROT_CLSTR_TC8(i,j) = 0.
            BIGCAL_PROT_CLSTR_TC64(i,j) = 0.
            BIGCAL_PROT_CLSTR_L8(i,j) = 0
            BIGCAL_PROT_CLSTR_L64(i,j) = 0
         enddo
         BIGCAL_PROT_CLSTR_L8SUM(i) = 0
         BIGCAL_PROT_CLSTR_L64SUM(i) = 0
         BIGCAL_PROT_CLSTR_X(i) = 0.
         BIGCAL_PROT_CLSTR_Y(i) = 0.
         BIGCAL_PROT_CLSTR_ETOT(i) = 0.
         BIGCAL_PROT_CLSTR_XMOM(i) = 0.
         BIGCAL_PROT_CLSTR_YMOM(i) = 0.
         BIGCAL_PROT_CLSTR_TIME8(i) = 0.
         BIGCAL_PROT_CLSTR_TIME64(i) = 0.
      enddo

       BIGCAL_RCS_NCLSTR = 0
       do i=1,BIGCAL_RCS_NCLSTR_MAX
         BIGCAL_RCS_CLSTR_NCELL(i) = 0
         BIGCAL_RCS_CLSTR_IYMAX(i) = 0
         BIGCAL_RCS_CLSTR_IXMAX(i) = 0
         do j=1,BIGCAL_CLSTR_NCELL_MAX
            BIGCAL_RCS_CLSTR_IYCELL(i,j) = 0
            BIGCAL_RCS_CLSTR_IXCELL(i,j) = 0
            BIGCAL_RCS_CLSTR_ECELL(i,j) = 0.
            BIGCAL_RCS_CLSTR_XCELL(i,j) = 0.
            BIGCAL_RCS_CLSTR_YCELL(i,j) = 0.
            BIGCAL_RCS_CLSTR_TC8(i,j) = 0.
            BIGCAL_RCS_CLSTR_TC64(i,j) = 0.
            BIGCAL_RCS_CLSTR_L8(i,j) = 0
            BIGCAL_RCS_CLSTR_L64(i,j) = 0
         enddo
         BIGCAL_RCS_CLSTR_L8SUM(i) = 0
         BIGCAL_RCS_CLSTR_L64SUM(i) = 0
         BIGCAL_RCS_CLSTR_X(i) = 0.
         BIGCAL_RCS_CLSTR_Y(i) = 0.
         BIGCAL_RCS_CLSTR_ETOT(i) = 0.
         BIGCAL_RCS_CLSTR_XMOM(i) = 0.
         BIGCAL_RCS_CLSTR_YMOM(i) = 0.
         BIGCAL_RCS_CLSTR_TIME8(i) = 0.
         BIGCAL_RCS_CLSTR_TIME64(i) = 0.
      enddo

       BIGCAL_MID_NCLSTR = 0
       do i=1,BIGCAL_MID_NCLSTR_MAX
         BIGCAL_MID_CLSTR_NCELL(i) = 0
         BIGCAL_MID_CLSTR_IYMAX(i) = 0
         BIGCAL_MID_CLSTR_IXMAX(i) = 0
         do j=1,BIGCAL_CLSTR_NCELL_MAX
            BIGCAL_MID_CLSTR_IYCELL(i,j) = 0
            BIGCAL_MID_CLSTR_IXCELL(i,j) = 0
            BIGCAL_MID_CLSTR_ECELL(i,j) = 0.
            BIGCAL_MID_CLSTR_XCELL(i,j) = 0.
            BIGCAL_MID_CLSTR_YCELL(i,j) = 0.
            BIGCAL_MID_CLSTR_TC8(i,j) = 0.
            BIGCAL_MID_CLSTR_TC64(i,j) = 0.
            BIGCAL_MID_CLSTR_L8(i,j) = 0
            BIGCAL_MID_CLSTR_L64(i,j) = 0
         enddo
         BIGCAL_MID_CLSTR_L8SUM(i) = 0
         BIGCAL_MID_CLSTR_L64SUM(i) = 0
         BIGCAL_MID_CLSTR_X(i) = 0.
         BIGCAL_MID_CLSTR_Y(i) = 0.
         BIGCAL_MID_CLSTR_ETOT(i) = 0.
         BIGCAL_MID_CLSTR_XMOM(i) = 0.
         BIGCAL_MID_CLSTR_YMOM(i) = 0.
         BIGCAL_MID_CLSTR_TIME8(i) = 0.
         BIGCAL_MID_CLSTR_TIME64(i) = 0.
      enddo

c     finally finally, zero all the bigcal_physics variables

      BIGCAL_PHYS_NTRACK = 0
      BIGCAL_BEST_TRACK = 0
      do i=1,BIGCAL_MAX_NTRACK
         BIGCAL_TRACK_THETARAD(i) = 0.
         BIGCAL_TRACK_THETADEG(i) = 0.
         BIGCAL_TRACK_PHIRAD(i) = 0.
         BIGCAL_TRACK_PHIDEG(i) = 0.
         BIGCAL_TRACK_ENERGY(i) = 0.
         BIGCAL_TRACK_TIME(i) = 0.
         BIGCAL_TRACK_XFACE(i) = 0.
         BIGCAL_TRACK_YFACE(i) = 0.
         BIGCAL_TRACK_ZFACE(i) = 0.
         BIGCAL_TRACK_PX(i) = 0.
         BIGCAL_TRACK_PY(i) = 0.
         BIGCAL_TRACK_PZ(i) = 0.
         BIGCAL_TRACK_BETA(i) = 0.
         BIGCAL_TRACK_TOF(i) = 0.
         BIGCAL_TRACK_COIN_TIME(i) = 0.
      enddo
      
      BIGCAL_BEST_THETARAD = 0.
      BIGCAL_BEST_THETADEG = 0.
      BIGCAL_BEST_PHIRAD = 0.
      BIGCAL_BEST_PHIDEG = 0.
      BIGCAL_BEST_ENERGY = 0.
      BIGCAL_BEST_TIME = 0.
      BIGCAL_BEST_XFACE = 0.
      BIGCAL_BEST_YFACE = 0.
      BIGCAL_BEST_ZFACE = 0.
      BIGCAL_BEST_PX = 0.
      BIGCAL_BEST_PY = 0.
      BIGCAL_BEST_PZ = 0.
      BIGCAL_BEST_BETA = 0.
      BIGCAL_BEST_TOF = 0.
      BIGCAL_BEST_COIN_TIME = 0.

      return
      end
