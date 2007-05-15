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

      integer m,np,nr,nm
      real ep,er,em
      integer iybest,ixbest,xclst,yclst,Eclst,xmom,ymom,t8avg,t64avg
      integer L8sum,L64sum,iydiff,ixdiff,icell,iarray
      real ecell(BIGCAL_CLSTR_NCELL_MAX)

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
      
      m=0

      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_COIN_TIME
      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_THETADEG
      m=m+1
      b_ntuple_contents(m) = BIGCAL_BEST_PHIDEG
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
            t8avg=BIGCAL_PROT_CLSTR_TIME8(1)
            t64avg=BIGCAL_PROT_CLSTR_TIME64(1)
            L8sum=BIGCAL_PROT_CLSTR_L8SUM(1)
            L64sum=BIGCAL_PROT_CLSTR_L64SUM(1) 
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
            t8avg=BIGCAL_RCS_CLSTR_TIME8(1)
            t64avg=BIGCAL_RCS_CLSTR_TIME64(1)
            L8sum=BIGCAL_RCS_CLSTR_L8SUM(1)
            L64sum=BIGCAL_RCS_CLSTR_L64SUM(1) 
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
         t8avg=BIGCAL_MID_CLSTR_TIME8(1)
         t64avg=BIGCAL_MID_CLSTR_TIME64(1)
         L8sum=BIGCAL_MID_CLSTR_L8SUM(1)
         L64sum=BIGCAL_MID_CLSTR_L64SUM(1) 
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
      m=m+1
      b_ntuple_contents(m) = L8sum
      m=m+1
      b_ntuple_contents(m) = L64sum

      do icell=1,25
         m=m+1
         b_ntuple_contents(m) = ecell(icell)
      enddo

      abort=.not.HEXIST(b_ntuple_ID)
      if(abort) then
         call G_build_note(':Ntuple ID#$ does not exist',
     $        '$',b_ntuple_ID,' ',0.,' ',err)
         call G_add_path(here,err)
      else 
         call HFN(b_ntuple_ID,b_ntuple_contents)
      endif

      return 
      end
