      subroutine g_decode_clear(ABORT, error)
*
*     Purpose and Methods:
*
*     Clear the decoding maps.
*
*     Inputs:
*
*     None
*
*     Outputs:
*
*     ABORT
*     error
*
*-
*-     by Steve Wood
*-      modified by Kevin Beard Dec.3,1993
*-    $Log$
*-    Revision 1.1  1994/02/04 21:49:17  cdaq
*-    Initial revision
*-
*-
      implicit none
      SAVE
      logical ABORT
      character*(*) error
      integer roc,slot
*
      include 'gen_decode_common.cmn'
*
*     Clear out slotpointer and subaddcount and mask arrays.
*
      do roc=0, G_DECODE_MAXROCS-1
         do slot=1, G_DECODE_MAXSLOTS
            g_decode_slotpointer(roc+1,slot) = 0
            g_decode_subaddcnt(roc+1,slot) = 0
            g_decode_slotmask(roc+1,slot) = 'FFF'x         ! Default mask
         enddo
      enddo
      g_decode_nextpointer = 1
      ABORT= .FALSE.
      error= ' '
      return
      end
