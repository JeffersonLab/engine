      subroutine gep_ntuple_init(ABORT,err)

      implicit none
      save
      
      character*15 here
      parameter(here='gep_ntuple_init')

      character*80 default_name
      parameter(default_name='GEPntuple')

      include 'gep_ntuple.cmn'
      include 'gen_routines.dec'
      include 'bigcal_data_structures.cmn'
      include 'gen_run_info.cmn'
      include 'gep_ntuple.dte'

      logical ABORT
      character*(*) err

      

      character*80 file
      character*80 name
      character*1000 pat,msg
      integer ilo,fn_len,m,i,j,k
      character*1 ifile
      
      err=' '
      abort=.false.

      if(gep_ntuple_exists) then
         call gep_ntuple_shutdown(ABORT,err)
         if(abort) then 
            call G_add_path(here,err)
            return
         endif
      endif

      call no_nulls(gep_ntuple_file) ! replace null characters with blanks

      if(gep_ntuple_file.eq.' ') return
      gep_ntuple_id = default_gep_ntuple_ID
      gep_ntuple_name = default_name
      if(gep_ntuple_title.eq.' ') then
         msg = name//' '//gep_ntuple_file
         call only_one_blank(msg)
         gep_ntuple_title = msg
      endif

      file = gep_ntuple_file
      call g_sub_run_number(file,gen_run_number)

      if(gep_ntuple_max_segmentevents.gt.0) then
         gep_ntuple_filesegments = 1
         ifile = char(ichar('0') + gep_ntuple_filesegments)
         fn_len = g_important_length(file)
         ilo=index(file,'.hbook')
         if((ilo.le.1).or.(ilo.gt.fn_len-5))then
            ilo=index(file,'.rzdat')
         endif
         
         if((ilo.gt.1).and.(ilo.lt.fn_len))then
            file = file(1:ilo-1)//'.'//ifile//file(ilo:fn_len)
         else
            abort=.true.
            return
         endif

         write(*,*) ' using segmented gep rzdat files
     $        first filename: ',file
      else
         write(*,*) ' not using segmented gep rzdat files
     $        first filename: ',file
      endif

      m=0
      m=m+1
      gep_ntuple_tag(m) = 'evid' ! gen_event_id_number
      m=m+1
      gep_ntuple_tag(m) = 'trigtype' ! trigger type
      m=m+1
      gep_ntuple_tag(m) = 'ctimeh' ! hms coin. time
      m=m+1
      gep_ntuple_tag(m) = 'ctimeb' ! bigcal coin. time
      m=m+1
      gep_ntuple_tag(m) = 'nh1' ! number of TDC hits h1
      m=m+1
      gep_ntuple_tag(m) = 'nh2' ! number of TDC hits h2
      m=m+1
      gep_ntuple_tag(m) = 'nb' ! number of TDC hits BigCal
      m=m+1
      gep_ntuple_tag(m) = 'h1time' ! hms1 trigger time
      m=m+1
      gep_ntuple_tag(m) = 'h2time' ! hms2 trigger time
      m=m+1
      gep_ntuple_tag(m) = 'btime' ! bigcal trigger time
      m=m+1
      gep_ntuple_tag(m) = 'Q2' ! q-squared in GeV^2
      m=m+1
      gep_ntuple_tag(m) = 'Q2_H' ! q-squared in GeV^2, HMS
      m=m+1
      gep_ntuple_tag(m) = 'Q2_B' ! q-squared in GeV^2, Calo
      m=m+1
      gep_ntuple_tag(m) = 'E_e' ! electron energy in GeV
      m=m+1
      gep_ntuple_tag(m) = 'P_p' ! proton momentum in GeV/c
      m=m+1
      gep_ntuple_tag(m) = 'Pel_htheta' ! elastic proton momentum for hstheta
      m=m+1
      gep_ntuple_tag(m) = 'Pel_btheta' ! elastic proton momentum for btheta
      m=m+1
      gep_ntuple_tag(m) = 'delta' ! (p-p0)/p0 in %
      m=m+1
      gep_ntuple_tag(m) = 'xfp'! x at the focal plane for proton.
      m=m+1
      gep_ntuple_tag(m) = 'yfp' ! y at the focal plane for the proton.
      m=m+1
      gep_ntuple_tag(m) = 'xpfp'! dx/dz at the focal plane for proton.
      m=m+1
      gep_ntuple_tag(m) = 'ypfp' ! dy/dz at the focal plane for the proton.
      m=m+1
      gep_ntuple_tag(m) = 'xptar'! dx/dz at the target for proton.
      m=m+1
      gep_ntuple_tag(m) = 'yptar' ! dy/dz at the target for the proton.
      m=m+1
      gep_ntuple_tag(m) = 'ytar' ! y at the target for the proton.
      m=m+1
      gep_ntuple_tag(m) = 'epsilon' ! virtual photon long. polarization
      m=m+1
      gep_ntuple_tag(m) = 'etheta' ! electron polar scattering angle in degrees
      m=m+1
      gep_ntuple_tag(m) = 'ephi' ! electron azimuthal scattering angle in degrees
      m=m+1
      gep_ntuple_tag(m) = 'ptheta' ! proton polar scattering angle in degrees
      m=m+1
      gep_ntuple_tag(m) = 'pphi' ! proton azimuthal scattering angle in degrees
      m=m+1
      gep_ntuple_tag(m) = 'Emiss' ! missing energy in GeV
      m=m+1
      gep_ntuple_tag(m) = 'Pmiss' ! magnitude of missing momentum in GeV/c
      m=m+1
      gep_ntuple_tag(m) = 'Pmissx' ! x cpt. of missing mom.
      m=m+1
      gep_ntuple_tag(m) = 'Pmissy' ! y cpt. of missing mom.
      m=m+1
      gep_ntuple_tag(m) = 'Pmissz' ! z cpt. of missing mom. 
      m=m+1
      gep_ntuple_tag(m) = 'W2' ! invariant mass of detected particles W^2 = (p+q)^2 = M_p^2 + 2M*nu - Q^2 = M_p^2 for ep elastic
      m=m+1
      gep_ntuple_tag(m) = 'Mmiss' ! missing mass of detected particles, should be zero for ep elastic
      m=m+1
      gep_ntuple_tag(m) = 'helicite' ! electron beam helicity
      m=m+1
      gep_ntuple_tag(m) = 'ntrack1' ! number of tracks in FPP1
      m=m+1
      gep_ntuple_tag(m) = 'ntrack2' ! number of tracks in FPP2
      m=m+1
      gep_ntuple_tag(m) = 'trk1' ! track number of the chosen track in FPP1
      m=m+1
      gep_ntuple_tag(m) = 'zclos1' ! reconstructed zclose in FPP1
      m=m+1
      gep_ntuple_tag(m) = 'sclos1' ! reconstructed sclose in FPP1
      m=m+1
      gep_ntuple_tag(m) = 'conet1' ! conetest FPP1
      m=m+1
      gep_ntuple_tag(m) = 'theta1' ! polar theta FPP1
      m=m+1
      gep_ntuple_tag(m) = 'phi1' ! azimuthal phi FPP1
      m=m+1
      gep_ntuple_tag(m) = 'trk2' ! track number of the chosen track in FPP2
      m=m+1
      gep_ntuple_tag(m) = 'zclos2' ! reconstructed zclose in FPP2
      m=m+1
      gep_ntuple_tag(m) = 'sclos2' ! reconstructed sclose in FPP2
      m=m+1
      gep_ntuple_tag(m) = 'conet2' ! conetest FPP2
      m=m+1
      gep_ntuple_tag(m) = 'theta2' ! polar theta FPP2
      m=m+1
      gep_ntuple_tag(m) = 'phi2' ! azimuthal phi FPP2

c     now all tags are set, initialize the ntuple:

      gep_ntuple_size = m

      call gep_ntuple_open(file,ABORT,err)

      if(abort) then
         err=':unable to create GEp ntuple'
         call G_add_path(here,err)
      else
         pat= ':created GEp ntuple'
         call G_add_path(here,pat)
         call G_log_message('INFO: '//pat)
      endif

      return 
      end
