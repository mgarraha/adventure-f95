module places
   implicit none

   public
   integer, parameter :: LOCSIZ = 150
   integer, dimension(LOCSIZ) :: ATLOC, ABB, LTEXT = 0, STEXT = 0,  &
         KEY = 0, COND = 0
   integer :: LINK(200), PLACE(100), FIXED(100)
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

      I=PLACE(OBJECT)
      J=FIXED(OBJECT)
      call MOVE(OBJECT, I)
      call MOVE(OBJECT+100, J)
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
      PUT=(-1)-PVAL
      return
   end function PUT


   subroutine CARRY(OBJECT, DEST)

!  START TOTING AN OBJECT, REMOVING IT FROM THE LIST OF THINGS AT ITS FORMER
!  LOCATION.  INCR HOLDNG UNLESS IT WAS ALREADY BEING TOTED.  IF OBJECT>100
!  (MOVING "FIXED" SECOND LOC), DON'T CHANGE PLACE OR HOLDNG.

      integer, intent(in) :: OBJECT, DEST
      integer :: TEMP

      if (OBJECT <= 100) then
         if (PLACE(OBJECT) == -1) return
         PLACE(OBJECT)=-1
         HOLDNG=HOLDNG+1
      end if
      if (ATLOC(DEST) == OBJECT) then
         ATLOC(DEST) = LINK(OBJECT)
         return
      end if
      TEMP = ATLOC(DEST)
      do while (LINK(TEMP) /= OBJECT)
         TEMP=LINK(TEMP)
      end do
      LINK(TEMP)=LINK(OBJECT)
      return
   end subroutine CARRY


   subroutine DROP(OBJECT, DEST)

!  PLACE AN OBJECT AT A GIVEN LOC, PREFIXING IT ONTO THE ATLOC LIST.  DECR
!  HOLDNG IF THE OBJECT WAS BEING TOTED.

      integer, intent(in) :: OBJECT, DEST
      
      if (OBJECT <= 100) then
         if (PLACE(OBJECT) == -1) HOLDNG=HOLDNG-1
         PLACE(OBJECT) = DEST
      else
         FIXED(OBJECT-100) = DEST
      end if
      if (DEST <= 0) return
      LINK(OBJECT) = ATLOC(DEST)
      ATLOC(DEST) = OBJECT
      return
   end subroutine DROP


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

end module places
