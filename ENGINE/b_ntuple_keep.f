      subroutine b_ntuple_keep(ABORT,err)

      implicit none
      save

      character*13 here
      parameter(here='b_ntuple_keep')

      logical abort
      character*(*) err

      include 'b_ntuple.cmn'
      include 'bigcal_data_structures.cmn'
      include 'gen_data_structures.cmn'
      include 'gen_event_info.cmn'
c      include 'gen_scalers.cmn'
      

      logical HEXIST ! CERNLIB function

      logical middlebest

      integer m,np,nr,nm,irow,icol,ihit,jhit,itdc
      real ep,er,em
      integer iybest,ixbest,xclst,yclst,Eclst,xmom,ymom,t8avg,t64avg
      integer L8sum,L64sum,iydiff,ixdiff,icell,jcell,iarray
      real ecell(BIGCAL_CLSTR_NCELL_MAX)

c      integer itype
      integer jclust,iclust
      integer nhit_tdc(224)
      integer nhit_ttdc(42)
      integer nhit_tadc(38)

      do ihit=1,224
        nhit_tdc(ihit) = 0
        if(ihit.le.42) nhit_ttdc(ihit) = 0
        if(ihit.le.38) nhit_tadc(ihit) = 0
      enddo

      err=' '
      ABORT=.false.

      if(.not.b_ntuple_exists) return

      if(b_ntuple_max_segmentevents.gt.0) then
         if(b_ntuple_segmentevents.gt.b_ntuple_max_segmentevents) then
            call b_ntuple_change(ABORT,err)
            b_ntuple_segmentevents=0
         else 
            b_ntuple_segmentevents = b_ntuple_segmentevents + 1
         endif
      endif
      
      if(bigcal_ntuple_type.eq.1) then

      m=0

      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_COIN_TIME
      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_THETARAD
      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_PHIRAD
      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_ENERGY
      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_XFACE
      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_YFACE
      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_ZFACE
      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_PX
      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_PY
      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_PZ
      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_BETA
      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_TOF

      np = BIGCAL_PROT_NCLSTR
      nr = BIGCAL_RCS_NCLSTR
      nm = BIGCAL_MID_NCLSTR

      if(np.gt.0) then
         ep = BIGCAL_PROT_CLSTR_ETOT(1)
      else
         ep=0.
      endif
      
      if(nr.gt.0) then
         er = BIGCAL_RCS_CLSTR_ETOT(1)
      else
         er=0.
      endif
      
      if(nm.gt.0) then
         em = BIGCAL_MID_CLSTR_ETOT(1)
      else
         em=0.
      endif
      
      middlebest=.false.

      do icell=1,BIGCAL_CLSTR_NCELL_MAX
         ecell(icell)=0.
      enddo

      if(ep.gt.er) then
         if(ep.gt.em) then ! ep is biggest, use protvino as best cluster
            !Protvino:
            iybest=BIGCAL_PROT_CLSTR_IYMAX(1)
            ixbest=BIGCAL_PROT_CLSTR_IXMAX(1)
            xclst=BIGCAL_PROT_CLSTR_X(1)
            yclst=BIGCAL_PROT_CLSTR_Y(1)
            Eclst=BIGCAL_PROT_CLSTR_ETOT(1)
            xmom=BIGCAL_PROT_CLSTR_XMOM(1)
            ymom=BIGCAL_PROT_CLSTR_YMOM(1)
            t8avg=BIGCAL_PROT_CLSTR_T8BEST(1)
            t64avg=BIGCAL_PROT_CLSTR_T64BEST(1)
            
            do icell=1,BIGCAL_PROT_CLSTR_NCELL(1)
               iydiff = BIGCAL_PROT_CLSTR_IYCELL(1,icell)-iybest
               ixdiff = BIGCAL_PROT_CLSTR_IXCELL(1,icell)-ixbest
               ! calculate appropriate index of ecell array:
               iarray = ixdiff + 3 + 5*(iydiff+2) 
               ecell(iarray) = BIGCAL_PROT_CLSTR_ECELL(1,icell)
            enddo
         else ! em is biggest, use middle as best cluster
            middlebest=.true.
         endif
      else
         if(er.gt.em) then ! er is biggest, use rcs as best cluster
            !RCS:
            iybest=BIGCAL_RCS_CLSTR_IYMAX(1)
            ixbest=BIGCAL_RCS_CLSTR_IXMAX(1)
            xclst=BIGCAL_RCS_CLSTR_X(1)
            yclst=BIGCAL_RCS_CLSTR_Y(1)
            Eclst=BIGCAL_RCS_CLSTR_ETOT(1)
            xmom=BIGCAL_RCS_CLSTR_XMOM(1)
            ymom=BIGCAL_RCS_CLSTR_YMOM(1)
            t8avg=BIGCAL_RCS_CLSTR_T8BEST(1)
            t64avg=BIGCAL_RCS_CLSTR_T64BEST(1)
            do icell=1,BIGCAL_RCS_CLSTR_NCELL(1)
               iydiff = BIGCAL_RCS_CLSTR_IYCELL(1,icell)-iybest
               ixdiff = BIGCAL_RCS_CLSTR_IXCELL(1,icell)-ixbest
               ! calculate appropriate index of ecell array:
               iarray = ixdiff + 3 + 5*(iydiff+2) 
               ecell(iarray) = BIGCAL_RCS_CLSTR_ECELL(1,icell)
            enddo
         else ! em is biggest, use middle as best cluster
            !Middle: 
            middlebest=.true.
         endif
      endif

      if(middlebest) then
         iybest=BIGCAL_MID_CLSTR_IYMAX(1)
         ixbest=BIGCAL_MID_CLSTR_IXMAX(1)
         xclst=BIGCAL_MID_CLSTR_X(1)
         yclst=BIGCAL_MID_CLSTR_Y(1)
         Eclst=BIGCAL_MID_CLSTR_ETOT(1)
         xmom=BIGCAL_MID_CLSTR_XMOM(1)
         ymom=BIGCAL_MID_CLSTR_YMOM(1)
         t8avg=BIGCAL_MID_CLSTR_T8BEST(1)
         t64avg=BIGCAL_MID_CLSTR_T64BEST(1)
         do icell=1,BIGCAL_MID_CLSTR_NCELL(1)
            iydiff = BIGCAL_MID_CLSTR_IYCELL(1,icell)-iybest
            ixdiff = BIGCAL_MID_CLSTR_IXCELL(1,icell)-ixbest
                                ! calculate appropriate index of ecell array:
            iarray = ixdiff + 3 + 5*(iydiff+2) 
            ecell(iarray) = BIGCAL_MID_CLSTR_ECELL(1,icell)
         enddo
      endif

      m=m+1
      b_ntuple_contents(m) = iybest
      m=m+1
      b_ntuple_contents(m) = ixbest
      m=m+1
      b_ntuple_contents(m) = xclst
      m=m+1
      b_ntuple_contents(m) = yclst
      m=m+1
      b_ntuple_contents(m) = Eclst
      m=m+1
      b_ntuple_contents(m) = xmom
      m=m+1
      b_ntuple_contents(m) = ymom
      m=m+1
      b_ntuple_contents(m) = t8avg
      m=m+1
      b_ntuple_contents(m) = t64avg

      do icell=1,25
         m=m+1
         b_ntuple_contents(m) = ecell(icell)
      enddo

      else if(bigcal_ntuple_type.eq.2) then

        iclust = 0

        nclust = bigcal_prot_nclstr + bigcal_rcs_nclstr + 
     $       bigcal_mid_nclstr

        do jclust = 1,bigcal_prot_nclstr
          iclust = iclust + 1
          ncellclust(iclust) = bigcal_prot_clstr_ncell(jclust)

          do icell=1,ncellclust(iclust)
            iycell(icell,iclust)=bigcal_prot_clstr_iycell(jclust,icell)
            ixcell(icell,iclust)=bigcal_prot_clstr_ixcell(jclust,icell)
            xcell(icell,iclust)=bigcal_prot_clstr_xcell(jclust,icell)
            ycell(icell,iclust)=bigcal_prot_clstr_ycell(jclust,icell)
            eblock(icell,iclust)=bigcal_prot_clstr_ecell(jclust,icell)
          enddo
          
          do icell=1,bigcal_prot_clstr_ncell8(jclust)
            do ihit=1,bigcal_prot_clstr_nhit8(jclust,icell)
              tcell8(icell,ihit,iclust) = 
     $             bigcal_prot_clstr_tcell8(jclust,icell,ihit)
            enddo
          enddo
          
          do icell=1,bigcal_prot_clstr_ncell64(jclust)
            do ihit=1,bigcal_prot_clstr_nhit64(jclust,icell)
              tcell64(icell,ihit,iclust) = 
     $             bigcal_prot_clstr_tcell64(jclust,icell,ihit)
            enddo
          enddo
          
          xmoment(iclust) = bigcal_prot_clstr_xmom(jclust)
          ymoment(iclust) = bigcal_prot_clstr_ymom(jclust)
          tclust8(iclust) = bigcal_prot_clstr_t8best(jclust)
          tclust64(iclust) = bigcal_prot_clstr_t64best(jclust)
          xclust(iclust) = bigcal_prot_clstr_x(jclust)
          yclust(iclust) = bigcal_prot_clstr_y(jclust)
          eclust(iclust) = bigcal_prot_clstr_etot(jclust)
          
          thetarad(iclust) = bigcal_track_thetarad(iclust)
          phirad(iclust) = bigcal_track_phirad(iclust)
          xface(iclust) = bigcal_track_xface(iclust)
          yface(iclust) = bigcal_track_yface(iclust)
          zface(iclust) = bigcal_track_zface(iclust)
          px(iclust) = bigcal_track_px(iclust)
          py(iclust) = bigcal_track_py(iclust)
          pz(iclust) = bigcal_track_pz(iclust)
          ctime_clust(iclust) = bigcal_track_coin_time(iclust)

       enddo
       
       do jclust = 1,bigcal_rcs_nclstr
          iclust = iclust + 1
          ncellclust(iclust) = bigcal_rcs_clstr_ncell(jclust)
          do icell=1,ncellclust(iclust)
            iycell(icell,iclust)=bigcal_rcs_clstr_iycell(jclust,icell)
            ixcell(icell,iclust)=bigcal_rcs_clstr_ixcell(jclust,icell)
            xcell(icell,iclust)=bigcal_rcs_clstr_xcell(jclust,icell)
            ycell(icell,iclust)=bigcal_rcs_clstr_ycell(jclust,icell)
            eblock(icell,iclust)=bigcal_rcs_clstr_ecell(jclust,icell)
          enddo

          do icell=1,bigcal_rcs_clstr_ncell8(jclust)
            do ihit=1,bigcal_rcs_clstr_nhit8(jclust,icell)
              tcell8(icell,ihit,iclust) = 
     $             bigcal_rcs_clstr_tcell8(jclust,icell,ihit)
            enddo
          enddo
          
          do icell=1,bigcal_rcs_clstr_ncell64(jclust)
            do ihit=1,bigcal_rcs_clstr_nhit64(jclust,icell)
              tcell64(icell,ihit,iclust) = 
     $             bigcal_rcs_clstr_tcell64(jclust,icell,ihit)
            enddo
          enddo

          xmoment(iclust) = bigcal_rcs_clstr_xmom(jclust)
          ymoment(iclust) = bigcal_rcs_clstr_ymom(jclust)
          tclust8(iclust) = bigcal_rcs_clstr_t8best(jclust)
          tclust64(iclust) = bigcal_rcs_clstr_t64best(jclust)
          xclust(iclust) = bigcal_rcs_clstr_x(jclust)
          yclust(iclust) = bigcal_rcs_clstr_y(jclust)
          eclust(iclust) = bigcal_rcs_clstr_etot(jclust)
          
          thetarad(iclust) = bigcal_track_thetarad(iclust)
          phirad(iclust) = bigcal_track_phirad(iclust)
          xface(iclust) = bigcal_track_xface(iclust)
          yface(iclust) = bigcal_track_yface(iclust)
          zface(iclust) = bigcal_track_zface(iclust)
          px(iclust) = bigcal_track_px(iclust)
          py(iclust) = bigcal_track_py(iclust)
          pz(iclust) = bigcal_track_pz(iclust)
          ctime_clust(iclust) = bigcal_track_coin_time(iclust)
        enddo
        
        do jclust = 1,bigcal_mid_nclstr
          iclust = iclust + 1
          ncellclust(iclust) = bigcal_mid_clstr_ncell(jclust)
          do icell=1,ncellclust(iclust)
            iycell(icell,iclust)=bigcal_mid_clstr_iycell(jclust,icell)
            ixcell(icell,iclust)=bigcal_mid_clstr_ixcell(jclust,icell)
            xcell(icell,iclust)=bigcal_mid_clstr_xcell(jclust,icell)
            ycell(icell,iclust)=bigcal_mid_clstr_ycell(jclust,icell)
            eblock(icell,iclust)=bigcal_mid_clstr_ecell(jclust,icell)
          enddo

          do icell=1,bigcal_mid_clstr_ncell8(jclust)
            do ihit=1,bigcal_mid_clstr_nhit8(jclust,icell)
              tcell8(icell,ihit,iclust) = 
     $             bigcal_mid_clstr_tcell8(jclust,icell,ihit)
            enddo
          enddo
          
          do icell=1,bigcal_mid_clstr_ncell64(jclust)
            do ihit=1,bigcal_mid_clstr_nhit64(jclust,icell)
              tcell64(icell,ihit,iclust) = 
     $             bigcal_mid_clstr_tcell64(jclust,icell,ihit)
            enddo
          enddo

          xmoment(iclust) = bigcal_mid_clstr_xmom(jclust)
          ymoment(iclust) = bigcal_mid_clstr_ymom(jclust)
          tclust8(iclust) = bigcal_mid_clstr_t8best(jclust)
          tclust64(iclust) = bigcal_mid_clstr_t64best(jclust)
          xclust(iclust) = bigcal_mid_clstr_x(jclust)
          yclust(iclust) = bigcal_mid_clstr_y(jclust)
          eclust(iclust) = bigcal_mid_clstr_etot(jclust)
          
          thetarad(iclust) = bigcal_track_thetarad(iclust)
          phirad(iclust) = bigcal_track_phirad(iclust)
          xface(iclust) = bigcal_track_xface(iclust)
          yface(iclust) = bigcal_track_yface(iclust)
          zface(iclust) = bigcal_track_zface(iclust)
          px(iclust) = bigcal_track_px(iclust)
          py(iclust) = bigcal_track_py(iclust)
          pz(iclust) = bigcal_track_pz(iclust)
          ctime_clust(iclust) = bigcal_track_coin_time(iclust)
        enddo

      else if(bigcal_ntuple_type.eq.3) then
        nahit = 0
        if(bigcal_prot_ngood.gt.0) then
          do ihit=1,bigcal_prot_ngood
            icol = bigcal_prot_ixgood(ihit)
            irow = bigcal_prot_iygood(ihit)
            icell = icol + 32*(irow-1)
            if(icell.ge.1.and.icell.le.1024.and.
     $           bigcal_prot_adc_good(ihit).ge.10..and.
     $           bigcal_prot_adc_good(ihit).le.8192.) then
              nahit = nahit + 1
              xa(nahit) = bigcal_prot_ixgood(ihit)
              ya(nahit) = bigcal_prot_iygood(ihit)
              aa(nahit) = bigcal_prot_adc_good(ihit)
            endif
          enddo
        endif
        
        if(bigcal_rcs_ngood.gt.0) then
          do ihit=1,bigcal_rcs_ngood
            icol = bigcal_rcs_ixgood(ihit)
            irow = bigcal_rcs_iygood(ihit)
            icell = icol + 30*(irow - 1)
            if(icell.ge.1.and.icell.le.720.and.
     $           bigcal_rcs_adc_good(ihit).ge.10..and.
     $           bigcal_rcs_adc_good(ihit).le.8192.) then
              nahit = nahit + 1
              xa(nahit) = bigcal_rcs_ixgood(ihit) + icol/16
              ya(nahit) = bigcal_rcs_iygood(ihit) + 32
              aa(nahit) = bigcal_rcs_adc_good(ihit)
            endif
          enddo
        endif
        
        nthit=0
        
        if(bigcal_tdc_nhit.gt.0) then
          do ihit=1,bigcal_tdc_nhit
            if(bigcal_tdc(ihit).gt.10)then
              nthit = nthit + 1
              xt(nthit) = bigcal_tdc_igroup(ihit)
              yt(nthit) = bigcal_tdc_irow(ihit)
              itdc = xt(nthit) + 4*(yt(nthit)-1)
              nhit_tdc(itdc) = nhit_tdc(itdc) + 1
              hn(nthit) = nhit_tdc(itdc)
              tt(nthit) = bigcal_tdc_raw(ihit)
c$$$              write(*,*) 'nthit,xt,yt,hn,tt = ',nthit,xt(nthit),
c$$$     $             yt(nthit),hn(nthit),tt(nthit)
            endif
          enddo
        endif

        ntahit=0
        ntthit=0

        if(bigcal_atrig_ngood.gt.0) then
          do ihit=1,bigcal_atrig_ngood
            icol = bigcal_atrig_good_ihalf(ihit)
            irow = bigcal_atrig_good_igroup(ihit)
            icell = icol + 2*(irow-1)
            if(icell.ge.1.and.icell.le.38.and.
     $           bigcal_atrig_adc_good(ihit).ge.10.and.
     $           bigcal_atrig_adc_good(ihit).le.8192.)then
              ntahit =  ntahit + 1
              xta(ntahit) = icol
              yta(ntahit) = irow
              taa(ntahit) = bigcal_atrig_adc_good(ihit)
            endif
          enddo
        endif

        if(bigcal_ttrig_nhit.gt.0) then
          do ihit=1,bigcal_ttrig_nhit
            icol = bigcal_ttrig_ihalf(ihit)
            irow = bigcal_ttrig_igroup(ihit)
            icell = icol + 2*(irow-1)
            if(bigcal_ttrig_tdc_raw(ihit).gt.10) then
              ntthit = ntthit + 1
              nhit_ttdc(icell) = nhit_ttdc(icell) + 1
              xtt(ntthit) = icol
              ytt(ntthit) = irow
              hnt(ntthit) = nhit_ttdc(icell)
              ttt(ntthit) = bigcal_ttrig_tdc_raw(ihit)
            endif
          enddo
        endif 
      endif
    

      abort=.not.HEXIST(b_ntuple_ID)
      if(abort) then
         call G_build_note(':Ntuple ID#$ does not exist',
     $        '$',b_ntuple_ID,' ',0.,' ',err)
         call G_add_path(here,err)
      else 
        if(bigcal_ntuple_type.eq.1) then
          call HFN(b_ntuple_ID,b_ntuple_contents)
        else if(bigcal_ntuple_type.eq.2) then
          call HFNT(b_ntuple_ID)
        else if(bigcal_ntuple_type.eq.3) then
          call HFNT(b_ntuple_ID)
        endif
      endif

      return 
      end
