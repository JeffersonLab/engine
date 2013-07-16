      SUBROUTINE s_track_tests
* 
*  Derek made this in Mar 1996
*
*  This routine delivers some handy tracking information.  It's divided
*  into three parts.  The first part looks at the chambers and their
*  efficiency.  The second part defines some scintillator tests to determine
*  whether the chambers should have fired.  The last part puts this info
*  into different files.  Also, if you want to look at the stub tests you
*  you can uncomment some lines in s_link_stubs.f to get that output.
*  A final note.  Many of these tests have similar counterparts in 
*  trackeff.test;  if you change something here, make sure it agrees with the
*  the tests there!!
*
* $Log: s_track_tests.f,v $
* Revision 1.3  2002/09/26 14:54:03  jones
*    Add variables sweet1xscin,sweet1yscin,sweet2xscin,sweet2yscin
*    which record which scint got hit inside the defined scint region
*    Then hgoodscinhits is set to zero if front and back hodoscopes
*    are abs(sweet1xscin-sweet2xscin).gt.3 or bs(sweet1yscin-sweet2yscin).gt.2
*
* Revision 1.2  1996/09/04 20:19:12  saw
* (JRA) Treat logicals as logicals
*
* Revision 1.1  1996/05/02 14:38:55  saw
* Initial revision
*
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'S_TRACK_TESTS')

       INCLUDE 'sos_data_structures.cmn'
       INCLUDE 'coin_data_structures.cmn'
       INCLUDE 'gen_constants.par'
       INCLUDE 'sos_tracking.cmn'
       INCLUDE 'gen_units.par'
       INCLUDE 'gen_event_info.cmn'
       INCLUDE 'sos_scin_tof.cmn'
       INCLUDE 'sos_scin_parms.cmn'
       INCLUDE 'sos_calorimeter.cmn'
       include 'sos_bypass_switches.cmn'

       integer planetemp
       real*4 stestbeta
       integer txth,txthp,txthps,txthpss,txtft,txtct

       integer i,j,count
       integer testsum
       integer shitsweet1x,shitsweet1y,shitsweet2x,shitsweet2y
       integer sweet1xscin,sweet1yscin,sweet2xscin,sweet2yscin
      
       real*4 lastcointime
       real*4 thiscointime

*In c_keep_results, the cointime is updated depending on tracking information
*Because we're independent of tracking here, we have to do some tricks to
*update the cointime.  We set cointime=100.0 if the code hasn't updated it (ie,
*it's the same as the previous event...).  First, if it's not a coincidence
*event, we just set the cointime to zero.
      
       thiscointime=0.0
       if (gen_event_type.eq.3) then
          thiscointime=ccointime_sos
          if (thiscointime.eq.lastcointime) then
             thiscointime=100.0
          endif
          lastcointime=ccointime_sos
       endif

*this next file prints out events the fail to track and why.  You can then
*look at them with the event display to see if they're worrisome.
       if (sbypass_track_eff_files.eq.0) then
          open(unit=15,file='scalers/strackeff.txt',status='unknown',
     $         access='append')
       endif

*this next file outputs a huge ascii file with many tracking parameters.  It
*is intended for use with physica.  The order of the ouput is given in the write
*statement at the end of this file.
       if (sbypass_track_eff_files.eq.0) then
          open(unit=17,file='scalers/strack.out',status='unknown',
     $         access='append')
       endif

*we start by looking at the chambers.  First, we look to see if each plane fired

       s1hit1 = (SDC_HITS_PER_PLANE(1).GE.1)
       s1hit2 = (SDC_HITS_PER_PLANE(2).GE.1)
       s1hit3 = (SDC_HITS_PER_PLANE(3).GE.1)
       s1hit4 = (SDC_HITS_PER_PLANE(4).GE.1)
       s1hit5 = (SDC_HITS_PER_PLANE(5).GE.1)
       s1hit6 = (SDC_HITS_PER_PLANE(6).GE.1)
       s1hit7 = (SDC_HITS_PER_PLANE(7).GE.1)
       s1hit8 = (SDC_HITS_PER_PLANE(8).GE.1)
       s1hit9 = (SDC_HITS_PER_PLANE(9).GE.1)
       s1hit10 = (SDC_HITS_PER_PLANE(10).GE.1)
       s1hit11 = (SDC_HITS_PER_PLANE(11).GE.1)
       s1hit12 = (SDC_HITS_PER_PLANE(12).GE.1)

*next, we see how many hits per plane there were ...

       snumhit1 = SDC_HITS_PER_PLANE(1)
       snumhit2 = SDC_HITS_PER_PLANE(2)
       snumhit3 = SDC_HITS_PER_PLANE(3)
       snumhit4 = SDC_HITS_PER_PLANE(4)
       snumhit5 = SDC_HITS_PER_PLANE(5)
       snumhit6 = SDC_HITS_PER_PLANE(6)
       snumhit7 = SDC_HITS_PER_PLANE(7)
       snumhit8 = SDC_HITS_PER_PLANE(8)
       snumhit9 = SDC_HITS_PER_PLANE(9)
       snumhit10 = SDC_HITS_PER_PLANE(10)
       snumhit11 = SDC_HITS_PER_PLANE(11)
       snumhit12 = SDC_HITS_PER_PLANE(12)

       snumhits1 = SDC_HITS_PER_PLANE(1) + SDC_HITS_PER_PLANE(2) +
     $      SDC_HITS_PER_PLANE(3) + SDC_HITS_PER_PLANE(4) +
     $      SDC_HITS_PER_PLANE(5) + SDC_HITS_PER_PLANE(6) 
       snumhits2 = SDC_HITS_PER_PLANE(7) + SDC_HITS_PER_PLANE(8) +
     $      SDC_HITS_PER_PLANE(9) + SDC_HITS_PER_PLANE(10) +
     $      SDC_HITS_PER_PLANE(11) + SDC_HITS_PER_PLANE(12) 

*next we check to see if we have fewer than the max allowed hits per chamber
*this number should agree with the value in trackeff.test.

       s1hitslt = snumhits1.LE.smax_pr_hits(1)
       s2hitslt = snumhits2.LE.smax_pr_hits(2)

*next we check to see if we have the minimum number of planes per chamber
*this number should agree with the value in trackeff.test.

       planetemp = 0
       if(SDC_HITS_PER_PLANE(1).GE.1) planetemp = planetemp+1
       if(SDC_HITS_PER_PLANE(2).GE.1) planetemp = planetemp+1
       if(SDC_HITS_PER_PLANE(3).GE.1) planetemp = planetemp+1
       if(SDC_HITS_PER_PLANE(4).GE.1) planetemp = planetemp+1
       if(SDC_HITS_PER_PLANE(5).GE.1) planetemp = planetemp+1
       if(SDC_HITS_PER_PLANE(6).GE.1) planetemp = planetemp+1
       snumplanes1 = planetemp
       planetemp = 0
       if(SDC_HITS_PER_PLANE(7).GE.1) planetemp = planetemp+1
       if(SDC_HITS_PER_PLANE(8).GE.1) planetemp = planetemp+1
       if(SDC_HITS_PER_PLANE(9).GE.1) planetemp = planetemp+1
       if(SDC_HITS_PER_PLANE(10).GE.1) planetemp = planetemp+1
       if(SDC_HITS_PER_PLANE(11).GE.1) planetemp = planetemp+1
       if(SDC_HITS_PER_PLANE(12).GE.1) planetemp = planetemp+1
       snumplanes2 = planetemp

       s1planesgt = snumplanes1.GE.smin_hit(1)
       s2planesgt = snumplanes2.GE.smin_hit(2)

*we now fill in the chamber part of the track tests.

       sfoundtrack = (sntracks_fp.NE.0)
       if (sfoundtrack) then
          stestbeta=ssbeta
       else
          stestbeta=0.0
       endif
       scleantrack = (ssnum_fptrack.NE.0).AND.(ssbeta.GT.(0.1))
*shitslt is less than max allowed hits in both chambers
       shitslt = s1hitslt.AND.s2hitslt
*splanesgt is at least the minimum number of planes fired per chamber
       splanesgt = s1planesgt.AND.s2planesgt
*sspacepoints is finding at least one space point in both chambers
       sspacepoints = ((snspace_points(1).GE.1).AND.(snspace_points(2).GE.1))
*sstublt is passing the stub criteria for at least one spacepoint in both chambers
       sstublt = (sstubtest.ne.0)
*shitsplanes is passing not too many hits and not too few planes
       shitsplanes = shitslt.AND.splanesgt
*shitsplanessps is that and finding a spacepoint
       shitsplanessps = shitsplanes.AND.sspacepoints
*shitsplanesspsstubs is that and passing the stub tests
       shitsplanesspsstubs = shitsplanessps.AND.sstublt
*fXsspacepoints is pasisng htis and planes but failing to find a space point
       f1sspacepoints = s1hitslt.AND.s1planesgt.AND.(snspace_points(1).EQ.0)
       f2sspacepoints = s2hitslt.AND.s2planesgt.AND.(snspace_points(2).EQ.0)
       fsspacepoints = f1sspacepoints.OR.f2sspacepoints

************************now look at some hodoscope tests
*second, we move the scintillators.  here we use scintillator cuts to see
*if a track should have been found.

       snumscins1 = sscin_hits_per_plane(1)
       snumscins2 = sscin_hits_per_plane(2)
       snumscins3 = sscin_hits_per_plane(3)
       snumscins4 = sscin_hits_per_plane(4)

*first, fill the arrays of which scins were sit
       do i=1,4
          do j=1,sscin_2x_nr
             sscinhit(i,j)=0
          enddo
       enddo
       do i=1,sscin_tot_hits
          sscinhit(sscin_plane_num(i),sscin_counter_num(i))=1
       enddo

*next, look for clusters of hits in a scin plane.  a cluster is a group of 
*adjacent scintillator hits separated by a non-firing scintillator.
*Wwe count the number of three adjacent scintillators too.  (A signle track 
*shouldn't fire three adjacent scintillators.

       do i=1,4
          snclust(i)=0
          sthreescin(i)=0
       enddo

*look for clusters in first x plane... (9 scins)
       count=0
       if (sscinhit(1,1).EQ.1) count=count+1
       do i=1,(sscin_1x_nr-1)            !look for number of clusters of 1 or more hits
          if ((sscinhit(1,i).EQ.0).AND.(sscinhit(1,i+1).EQ.1)) count=count+1
       enddo
       snclust(1)=count
       count=0
       do i=1,(sscin_1x_nr-2)            !look for three or more adjacent hits
          if ((sscinhit(1,i).EQ.1).AND.(sscinhit(1,i+1).EQ.1).AND.
     $         (sscinhit(1,i+2).EQ.1)) count=count+1
       enddo
       if (count.GT.0) sthreescin(1)=1
*look for clusters in second x plane... (16 scins)
       count=0
       if (sscinhit(3,1).EQ.1) count=count+1
       do i=1,(sscin_2x_nr-1)            !look for number of clusters of 1 or more hits
          if ((sscinhit(3,i).EQ.0).AND.(sscinhit(3,i+1).EQ.1)) count=count+1
       enddo
       snclust(3)=count
       count=0
       do i=1,(sscin_2x_nr-2)            !look for three or more adjacent hits
          if ((sscinhit(3,i).EQ.1).AND.(sscinhit(3,i+1).EQ.1).AND.
     $         (sscinhit(3,i+2).EQ.1)) count=count+1
       enddo
       if (count.GT.0) sthreescin(3)=1
*look for clusters in y planes... (9 scins)
       do j=2,4,2
          count=0
          if (sscinhit(j,1).EQ.1) count=count+1
          do i=1,(sscin_1y_nr-1)         !look for number of clusters of 1 or more hits
             if ((sscinhit(j,i).EQ.0).AND.(sscinhit(j,i+1).EQ.1)) count=count+1
          enddo
          snclust(j)=count
          count=0
          do i=1,(sscin_1y_nr-2)         !look for three or more adjacent sits
             if ((sscinhit(j,i).EQ.1).AND.(sscinhit(j,i+1).EQ.1).AND.
     $            (sscinhit(j,i+2).EQ.1)) count=count+1
          enddo
          if (count.GT.0) sthreescin(j)=1
       enddo
       if ((gen_event_ID_number.GT.1000).AND.((snclust(2).GT.1).OR.
     $      (sthreescin(2).EQ.1))) then
        if (sbypass_track_eff_files.eq.0) then
          write(15,*) 'three or cluster in 1st yplane event',gen_event_ID_number
        endif
       endif

*now put some "tracking" like cuts on the sslopes, based only on scins...
*by "slope" here, I mean the difference in the position of scin hits in two
*like-planes.  For example, a track that those great straight through will 
*have a slope of zero.  If it moves one scin over from s1x to s2x it has an
*x-slope of 1...  I pick the minimum slope if there are multiple scin hits.
       sbestxpscin=100
       sbestypscin=100
       do i=1,sscin_2x_nr
          do j=1,sscin_2x_nr
             if ((sscinhit(1,i).EQ.1).AND.(sscinhit(3,j).EQ.1)) then
                sslope=abs(i-j)
                if (sslope.LT.sbestxpscin) sbestxpscin=sslope
             endif
          enddo
       enddo
       do i=1,10
          do j=1,10
             if ((sscinhit(2,i).EQ.1).AND.(sscinhit(4,j).EQ.1)) then
                sslope=abs(i-j)
                if (sslope.LT.sbestypscin) sbestypscin=sslope
             endif
          enddo
       enddo

*next we mask out the edge scintillators, and look at triggers that happened
*at the center of the acceptance.  To change which scins are in the mask
*change the values of s*loscin and s*hiscin in stracking.param
       shitsweet1x=0
       shitsweet1y=0
       shitsweet2x=0
       shitsweet2y=0
       sgoodscinhits=0
*first x plane.  first see if there are hits inside the scin region
       do i=sxloscin(1),sxhiscin(1)
          if (sscinhit(1,i).EQ.1) then
             shitsweet1x=1
             sweet1xscin=i
          endif
       enddo
*  next make sure nothing fired outside the good region
       do i=1,sxloscin(1)-1
          if (sscinhit(1,i).EQ.1) shitsweet1x=-1
       enddo
       do i=sxhiscin(1)+1,sscin_1x_nr
          if (sscinhit(1,i).EQ.1) shitsweet1x=-1
       enddo
*second x plane.  first see if there are hits inside the scin region
       do i=sxloscin(2),sxhiscin(2)
          if (sscinhit(3,i).EQ.1) then
             shitsweet2x=1
             sweet2xscin=i
          endif
       enddo
*  next make sure nothing fired outside the good region
       do i=1,sxloscin(2)-1
          if (sscinhit(3,i).EQ.1) shitsweet2x=-1
       enddo
       do i=sxhiscin(2)+1,sscin_2x_nr
          if (sscinhit(3,i).EQ.1) shitsweet2x=-1
       enddo

*first y plane.  first see if there are hits inside the scin region
       do i=syloscin(1),syhiscin(1)
          if (sscinhit(2,i).EQ.1) then
             shitsweet1y=1
             sweet1yscin=i
          endif
       enddo
*  next make sure nothing fired outside the good region
       do i=1,syloscin(1)-1
          if (sscinhit(2,i).EQ.1) shitsweet1y=-1
       enddo
       do i=syhiscin(1)+1,sscin_1y_nr
          if (sscinhit(2,i).EQ.1) shitsweet1y=-1
       enddo
*second y plane.  first see if there are hits inside the scin region
       do i=syloscin(2),syhiscin(2)
          if (sscinhit(4,i).EQ.1) then
             shitsweet2y=1
             sweet2yscin=i
          endif
       enddo
*  next make sure nothing fired outside the good region
       do i=1,syloscin(2)-1
          if (sscinhit(4,i).EQ.1) shitsweet2y=-1
       enddo
       do i=syhiscin(2)+1,sscin_2y_nr
          if (sscinhit(4,i).EQ.1) shitsweet2y=-1
       enddo

       testsum=shitsweet1x+shitsweet1y+shitsweet2x+shitsweet2y
* now define a 3/4 or 4/4 trigger of only good scintillators the value
* is specified in stracking.param...
       if (testsum.GE.strack_eff_test_num_scin_planes) sgoodscinhits=1

* require front/back hodoscopes be within close to each other.,
       if (sgoodscinhits.eq.1 .and. strack_eff_test_num_scin_planes.eq.4) then
          if (abs(sweet1xscin-sweet2xscin).gt.3) sgoodscinhits=0
          if (abs(sweet1yscin-sweet2yscin).gt.2) sgoodscinhits=0
       endif

*******************************************************************************
*     here's where we start writing to the files.  Uncomment these lines and
*     the corresponding file open and close lines at the beginning and end
*     of this file if you want this output.  the scaler report should take
*     care of most people though...
*
       if (sbypass_track_eff_files.eq.0) then
          if (sgoodscinhits.EQ.1) then
             write(15,*) 'sweet spot hit, event number           ',gen_event_ID_number
          endif
          if (.not.shitslt) then
             write(15,*) 'too many hits, event number           ',gen_event_ID_number
          endif
          if (.not.splanesgt) then
             write(15,*)  'too few planes event number                    ',
     $            gen_event_ID_number
          endif
          if (shitsplanes.AND.(.not.sspacepoints)) then
             write(15,*) 'p sits/planes, f sp # = ',gen_event_ID_number
          endif
          if ((.not.sfoundtrack).AND.shitsplanessps) then
             write(15,*) 'p sits/planes/sps, f track # = ',gen_event_ID_number
          endif
          if (sspacepoints.AND.(.not.sstublt)) then
             write(15,*) 'p sp, f stubs # = ',gen_event_ID_number
          endif
       endif

*the rest of this file prepares the output of htrack.out.  If you're not
*writing to that file, do't' worry about this.

           if (sbypass_track_eff_files.eq.0) then
              txth=0
              if (shitslt) txth=1
              txthp=0
              if (shitsplanes) txthp=1
              txthps=0
              if (shitsplanessps) txthps=1
              txthpss=0
              if (shitsplanesspsstubs) txthpss=1        
              txtft=0
              if (sfoundtrack) txtft=1
              txtct=0
              if (scleantrack) txtct=1

              write(17,902) gen_event_ID_number,snumhits1,snumhits2,
     $             snumhit1,snumhit2,snumhit3,snumhit4,
     $             snumhit5,snumhit6,snumhit7,snumhit8,
     $             snumhit9,snumhit10,snumhit11,snumhit12,
     $             snumplanes1,snumplanes2,snumscins1,snumscins2,
     $             snumscins3,snumscins4,snclust(1),snclust(2),
     $             snclust(3),snclust(4),sthreescin(1),sthreescin(2),
     $             sthreescin(3),sthreescin(4),sbestxpscin,sbestypscin,
     $             sgoodscinhits,
     $             txtft,txtct,sntracks_fp,sbeta_notrk,stestbeta,scal_et,ssshtrk,
     $             scer_npe_sum,sschi2perdeg,ssdelta,thiscointime
 902          format(1x,i6,i4,i4,12(i4),2(i2),4(i3),8(i2),2(i4),i2,i2,i2,i2,f10.3,f9.3,
     $             f9.3,f9.3,f9.3,f10.3,f10.3,f10.3)

              close(15)         !closes "strackeff.txt"
              close(17)         !closes "strack.out"
           endif
       end
