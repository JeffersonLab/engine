#
# $Log$
# Revision 1.5  1995/07/28 14:25:20  cdaq
# (SAW) Update list of include files.  Add NFSDIRECTORY stuff
#
# Revision 1.4  1995/05/24  13:50:06  cdaq
# (SAW) Update and make cosmetic changes
#
# Revision 1.3  1995/04/06  20:12:33  cdaq
# (SAW) Remove tmp_pedestals.dte
#
# Revision 1.2  1995/01/27  21:08:31  cdaq
# *** empty log message ***
#
# Revision 1.1  1994/04/15  20:29:37  cdaq
# Initial revision
#
##include $(Csoft)/etc/Makefile

ntuples = c_ntuple.cmn h_ntuple.cmn s_ntuple.cmn

cmn = gen_data_structures.cmn gen_craw.cmn gen_decode_common.cmn \
	gen_event_info.cmn gen_filenames.cmn gen_output_info.cmn \
	gen_pawspace.cmn gen_run_info.cmn gen_run_pref.cmn gen_scalers.cmn \
	hms_data_structures.cmn hms_bypass_switches.cmn hms_calorimeter.cmn \
	hms_filenames.cmn hms_geometry.cmn hms_id_histid.cmn \
	hms_physics_sing.cmn hms_recon_elements.cmn hms_scin_parms.cmn \
	hms_scin_tof.cmn hms_statistics.cmn hms_track_histid.cmn \
	hms_tracking.cmn hms_pedestals.cmn \
	h_ntuple.cmn h_sieve_ntuple.cmn \
	sos_data_structures.cmn sos_bypass_switches.cmn sos_calorimeter.cmn \
	sos_filenames.cmn sos_geometry.cmn sos_id_histid.cmn \
	sos_physics_sing.cmn sos_recon_elements.cmn sos_scin_parms.cmn \
	sos_scin_tof.cmn sos_statistics.cmn sos_track_histid.cmn \
	sos_tracking.cmn sos_pedestals.cmn \
	s_ntuple.cmn \
	coin_data_structures.cmn coin_bypass_switches.cmn c_ntuple.cmn \
	coin_filenames.cmn \
	hack_.cmn \
	mc_structures.cmn

dte = c_ntuple.dte gen_run_pref.dte h_sieve_ntuple.dte \
	gen_run_info.dte h_ntuple.dte s_ntuple.dte

par = gen_constants.par gen_detectorids.par gen_units.par

dec = gen_routines.dec

include_files = $(cmn) $(dte) $(par) $(dec)

install-dirs :=


all : $(include_files)

ifdef NFSDIRECTORY
%.cmn : $(NFSDIRECTORY)/INCLUDE/%.cmn
	ln -s $(NFSDIRECTORY)/INCLUDE/$@ $@

%.dte : $(NFSDIRECTORY)/INCLUDE/%.dte
	ln -s $(NFSDIRECTORY)/INCLUDE/$@ $@

%.par : $(NFSDIRECTORY)/INCLUDE/%.par
	ln -s $(NFSDIRECTORY)/INCLUDE/$@ $@

%.dec : $(NFSDIRECTORY)/INCLUDE/%.dec
	ln -s $(NFSDIRECTORY)/INCLUDE/$@ $@

.PRECIOUS: %.cmn %.dte %.par %.dec

endif
