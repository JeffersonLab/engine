#
# $Log$
# Revision 1.2  1995/01/27 21:08:31  cdaq
# *** empty log message ***
#
# Revision 1.1  1994/04/15  20:29:37  cdaq
# Initial revision
#
##include $(Csoft)/etc/Makefile

ntuples = c_ntuple.cmn h_ntuple.cmn s_ntuple.cmn

cmn = c_ntuple.cmn hack_.cmn s_ntuple.cmn coin_filenames.cmn \
	hms_bypass_switches.cmn sos_bypass_switches.cmn gen_craw.cmn \
	hms_calorimeter.cmn sos_calorimeter.cmn gen_data_structures.cmn \
	hms_filenames.cmn sos_filenames.cmn gen_decode_common.cmn \
	hms_geometry.cmn sos_geometry.cmn gen_event_info.cmn \
	hms_id_histid.cmn sos_id_histid.cmn gen_filenames.cmn \
	hms_physics_sing.cmn sos_physics_sing.cmn gen_output_info.cmn \
	hms_recon_elements.cmn sos_recon_elements.cmn gen_pawspace.cmn \
	hms_scin_parms.cmn sos_scin_parms.cmn gen_run_info.cmn \
	hms_scin_tof.cmn sos_scin_tof.cmn gen_run_pref.cmn hms_statistics.cmn \
	sos_statistics.cmn gen_scalers.cmn hms_track_histid.cmn \
	sos_track_histid.cmn h_ntuple.cmn hms_tracking.cmn sos_tracking.cmn \
	h_sieve_ntuple.cmn mc_structures.cmn

dte = c_ntuple.dte gen_run_pref.dte h_sieve_ntuple.dte tmp_pedestals.dte \
	gen_run_info.dte h_ntuple.dte s_ntuple.dte

par = gen_constants.par gen_detectorids.par gen_units.par

dec = gen_routines.dec

include_files = $(cmn) $(dte) $(par) $(dec)

install-dirs :=


all : $(include_files)






