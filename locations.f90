module locations
   implicit none

   public
   integer, parameter :: LOCSIZ = 150
   integer, dimension(LOCSIZ) :: ATLOC = 0, ABB = 0, LTEXT = 0, STEXT = 0,  &
         KEY = 0, COND = 0

contains

!  LIQLOC(LOC)  = OBJECT NUMBER OF LIQUID (IF ANY) AT LOC
   integer function LIQLOC(LOC)
      use words
      integer, intent(in) :: LOC
      integer :: PBOTL
      PBOTL = (mod(COND(LOC) / 2 * 2, 8) - 5) * mod(COND(LOC) / 4, 2) + 1
      LIQLOC = (1 - PBOTL) * WATER + (PBOTL / 2) * (WATER + OIL)
      return
   end function LIQLOC


!  BITSET(L,N)  = TRUE IF COND(L) HAS BIT N SET (BIT 0 IS UNITS BIT)
   logical function BITSET(L, N)
      integer, intent(in) :: L, N
      BITSET = btest(COND(L), N)
      return
   end function BITSET


!  FORCED(LOC)  = TRUE IF LOC MOVES WITHOUT ASKING FOR INPUT (COND=2)
   logical function FORCED(LOC)
      integer, intent(in) :: LOC
      FORCED = COND(LOC) == 2
      return
   end function FORCED


   subroutine set_ltext(LOC, LINDEX)
      integer, intent(in) :: LOC, LINDEX

      LTEXT(LOC) = LINDEX
      return
   end subroutine set_ltext


   subroutine set_stext(LOC, LINDEX)
      integer, intent(in) :: LOC, LINDEX

      STEXT(LOC) = LINDEX
      return
   end subroutine set_stext

end module locations
