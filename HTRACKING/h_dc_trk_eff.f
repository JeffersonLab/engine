      SUBROUTINE H_DC_TRK_EFF(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze DC information for each track 
*-
*-      Required Input BANKS     HMS_STATISTICS
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 9/5/95
*
* h_dc_trk_eff calculates efficiencies for the drift chambers,
*   using the tracking information.
*
* $Log: h_dc_trk_eff.f,v $
* Revision 1.2  1996/01/17 18:19:40  cdaq
* (JRA) Change array sizes from hdc_num_planes to HMAX_NUM_DC_PLANES
*
* Revision 1.1  1995/10/09 20:01:28  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*12 here
      parameter (here= 'H_DC_TRK_EFF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_tracking.cmn'
      include 'hms_geometry.cmn'

      integer*4 pln,hit,ihit
      integer*4 iwire(HMAX_NUM_DC_PLANES)
      integer*4 ihitwire
      real*4 hitwire
      real*4 hitdist(HMAX_NUM_DC_PLANES)

      save

* find nearest wire, and increment 'should have fired' counter.
      do pln=1,hdc_num_planes
        hitwire = hdc_central_wire(pln) +
     &         (hsdc_track_coord(pln)+hdc_center(pln))/hdc_pitch(pln) 
        hitdist(pln) = (hitwire - nint(hitwire))*hdc_pitch(pln)

        if (hdc_wire_counting(pln).eq.0) then         !normal wire numbering.
          ihitwire = nint(hitwire)
        else                                          !backwards numbering.
          ihitwire = (hdc_nrwire(pln) + 1 ) - nint(hitwire)
        endif
        iwire(pln) = max(1,min(hdc_nrwire(pln),ihitwire))
        if (ihitwire.ne.iwire(pln)) hitdist(pln)=99. !if had to reset wire,
                                                    !make it a 'miss'

        if (abs(hitdist(pln)).le.0.3) then ! hit close to wire.
          hdc_shouldhit(pln,iwire(pln)) = hdc_shouldhit(pln,iwire(pln)) + 1
        endif
      enddo

* note, this does not look for hits on the track which were NOT in the space
* point used to fit the track!

      do ihit=2,hntrack_hits(hsnum_fptrack,1)+1
        hit=hntrack_hits(hsnum_fptrack,ihit)
        pln=hdc_plane_num(hit)
        if (iwire(pln).eq.hdc_wire_num(hit) .and. abs(hitdist(pln)).le.0.3)then
          hdc_didhit(pln,iwire(pln)) = hdc_didhit(pln,iwire(pln)) + 1
        endif
      enddo

      return
      end
