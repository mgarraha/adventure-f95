module objects
   implicit none

   integer, parameter :: OBJSIZ = 100
   integer :: PTEXT(OBJSIZ) = 0
   integer :: LINK(2 * OBJSIZ) = 0
   integer :: PLACE(OBJSIZ) = 0, FIXED(OBJSIZ) = 0
   integer :: PLAC(OBJSIZ) = 0, FIXD(OBJSIZ) = 0
   integer :: PROP(OBJSIZ) = 0
   integer :: HOLDNG

contains

   subroutine DSTROY(OBJECT)

!  PERMANENTLY ELIMINATE "OBJECT" BY MOVING TO A NON-EXISTENT LOCATION.

      integer, intent(in) :: OBJECT

      call MOVE(OBJECT, 0)
      return
   end subroutine DSTROY


   subroutine JUGGLE(OBJECT)

!  JUGGLE AN OBJECT BY PICKING IT UP AND PUTTING IT DOWN AGAIN, THE PURPOSE
!  BEING TO GET THE OBJECT TO THE FRONT OF THE CHAIN OF THINGS AT ITS LOC.

      integer, intent(in) :: OBJECT
      integer :: I,J

      I = PLACE(OBJECT)
      J = FIXED(OBJECT)
      call MOVE(OBJECT, I)
      call MOVE(OBJECT + 100, J)
      return
   end subroutine JUGGLE


   subroutine MOVE(OBJECT, DEST)

!  PLACE ANY OBJECT ANYWHERE BY PICKING IT UP AND DROPPING IT.  MAY ALREADY BE
!  TOTING, IN WHICH CASE THE CARRY IS A NO-OP.  MUSTN'T PICK UP OBJECTS WHICH
!  ARE NOT AT ANY LOC, SINCE CARRY WANTS TO REMOVE OBJECTS FROM ATLOC CHAINS.

      integer, intent(in) :: OBJECT, DEST
      integer :: FROM

      if (OBJECT <= 100) then
         FROM=PLACE(OBJECT)
      else
         FROM=FIXED(OBJECT-100)
      end if
      if (FROM > 0 .and. FROM <= 300) call CARRY(OBJECT, FROM)
      call DROP(OBJECT, DEST)
      return
   end subroutine MOVE


   integer function PUT(OBJECT, DEST, PVAL)

!  PUT IS THE SAME AS MOVE, EXCEPT IT RETURNS A VALUE USED TO SET UP THE
!  NEGATED PROP VALUES FOR THE REPOSITORY OBJECTS.

      integer, intent(in) :: OBJECT, DEST, PVAL

      call MOVE(OBJECT, DEST)
      PUT = (-1) - PVAL
      return
   end function PUT


   subroutine CARRY(OBJECT, DEST)
      use locations, only: ATLOC

!  START TOTING AN OBJECT, REMOVING IT FROM THE LIST OF THINGS AT ITS FORMER
!  LOCATION.  INCR HOLDNG UNLESS IT WAS ALREADY BEING TOTED.  IF OBJECT>100
!  (MOVING "FIXED" SECOND LOC), DON'T CHANGE PLACE OR HOLDNG.

      integer, intent(in) :: OBJECT, DEST
      integer :: TEMP

      if (OBJECT <= 100) then
         if (PLACE(OBJECT) == -1) return
         PLACE(OBJECT) = -1
         HOLDNG = HOLDNG + 1
      end if
      if (ATLOC(DEST) == OBJECT) then
         ATLOC(DEST) = LINK(OBJECT)
         return
      end if
      TEMP = ATLOC(DEST)
      do while (LINK(TEMP) /= OBJECT)
         TEMP = LINK(TEMP)
      end do
      LINK(TEMP) = LINK(OBJECT)
      return
   end subroutine CARRY


   subroutine DROP(OBJECT, DEST)
      use locations, only: ATLOC

!  PLACE AN OBJECT AT A GIVEN LOC, PREFIXING IT ONTO THE ATLOC LIST.  DECR
!  HOLDNG IF THE OBJECT WAS BEING TOTED.

      integer, intent(in) :: OBJECT, DEST
      
      if (OBJECT <= 100) then
         if (PLACE(OBJECT) == -1) HOLDNG = HOLDNG-1
         PLACE(OBJECT) = DEST
      else
         FIXED(OBJECT - 100) = DEST
      end if
      if (DEST <= 0) return
      LINK(OBJECT) = ATLOC(DEST)
      ATLOC(DEST) = OBJECT
      return
   end subroutine DROP


!  TOTING(OBJ)  = TRUE IF THE OBJ IS BEING CARRIED
   logical function TOTING(OBJ)
      integer, intent(in) :: OBJ
      TOTING = PLACE(OBJ) == -1
      return
   end function TOTING


!  LIQ()   = OBJECT NUMBER OF LIQUID IN BOTTLE
   integer function LIQ()
      use words
      integer :: PBOTL
      PBOTL = max(PROP(BOTTLE), -1-PROP(BOTTLE))
      LIQ = (1 - PBOTL) * WATER + (PBOTL / 2) * (WATER + OIL)
      return
   end function LIQ


   subroutine PSPEAK(MSG, SKIP)
      use text

!  FIND THE SKIP+1ST MESSAGE FROM MSG AND PRINT IT.  MSG SHOULD BE THE INDEX OF
!  THE INVENTORY MESSAGE FOR OBJECT.  (INVEN+N+1 MESSAGE IS PROP=N MESSAGE).

      integer, intent(in) :: MSG, SKIP
      integer :: I, M

      M = PTEXT(MSG)
      do I = 0, SKIP
         do
            M = abs(LINES(M))
            if (LINES(M) < 0) exit
         end do
      end do
      call SPEAK(M)
      return
   end subroutine PSPEAK


   subroutine set_ptext(OBJ, LINDEX)
      integer, intent(in) :: OBJ, LINDEX

      if (OBJ > 0 .and. OBJ <= OBJSIZ) then
         PTEXT(OBJ) = LINDEX
      end if
      return
   end subroutine set_ptext

end module objects
