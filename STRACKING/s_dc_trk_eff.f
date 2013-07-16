      SUBROUTINE S_DC_TRK_EFF(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze DC information for each track 
*-
*-      Required Input BANKS     SOS_STATISTICS
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 9/5/95
*
* s_dc_trk_eff calculates efficiencies for the drift chambers,
*   using the tracking information.
*
* $Log: s_dc_trk_eff.f,v $
* Revision 1.2  1996/01/17 17:09:36  cdaq
* (JRA) Change array sizes from sdc_num_planes to SMAX_NUM_DC_PLANES
*
* Revision 1.1  1995/10/09 20:02:37  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*12 here
      parameter (here= 'S_DC_TRK_EFF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_tracking.cmn'
      include 'sos_geometry.cmn'

      integer*4 pln,hit,ihit
      integer*4 iwire(SMAX_NUM_DC_PLANES)
      integer*4 ihitwire
      real*4 hitwire
      real*4 hitdist(SMAX_NUM_DC_PLANES)

      save

* find nearest wire, and increment 'should have fired' counter.
      do pln=1,sdc_num_planes
        hitwire = sdc_central_wire(pln) +
     &         (ssdc_track_coord(pln)+sdc_center(pln))/sdc_pitch(pln) 
        hitdist(pln) = (hitwire - nint(hitwire))*sdc_pitch(pln)

        if (sdc_wire_counting(pln).eq.0) then         !normal wire numbering.
          ihitwire = nint(hitwire)
        else                                          !backwards numbering.
          ihitwire = (sdc_nrwire(pln) + 1 ) - nint(hitwire)
        endif
        iwire(pln) = max(1,min(sdc_nrwire(pln),ihitwire))
        if (ihitwire.ne.iwire(pln)) hitdist(pln)=99. !if had to reset wire,
                                                    !make it a 'miss'
 
        if (abs(hitdist(pln)).le.0.3) then           !hit close to wire.
          sdc_shouldhit(pln,iwire(pln)) = sdc_shouldhit(pln,iwire(pln)) + 1
        endif
      enddo

* note, this does not look for hits on the track which were NOT in the space
* point used to fit the track!  (though this is probably OK).

      do ihit=2,sntrack_hits(ssnum_fptrack,1)+1
        hit=sntrack_hits(ssnum_fptrack,ihit)
        pln=sdc_plane_num(hit)
        if (iwire(pln).eq.sdc_wire_num(hit) .and. 
     &            abs(hitdist(pln)).le.0.3)then
          sdc_didhit(pln,iwire(pln)) = sdc_didhit(pln,iwire(pln)) + 1
        endif
      enddo

      return
      end
