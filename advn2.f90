module advn2
   implicit none

contains
!  I/O ROUTINES (A5TOA1)


      SUBROUTINE A5TOA1(A,B,C,CHARS,LENG)

!  A AND B CONTAIN A 1- TO 9-CHARACTER WORD IN A5 FORMAT, C CONTAINS ANOTHER
!  WORD AND/OR PUNCTUATION.  THEY ARE UNPACKED TO ONE CHARACTER PER WORD IN THE
!  ARRAY "CHARS", WITH EXACTLY ONE BLANK BETWEEN B AND C (OR NONE, IF C >= 0).
!  THE INDEX OF THE LAST NON-BLANK CHAR IN CHARS IS RETURNED IN LENG.

      INTEGER A,B,C,LENG
      CHARACTER*1 CHARS
      INTEGER WORDS
      DIMENSION CHARS(20),WORDS(3)
      INTEGER POSN,WORD,CH
      INTEGER MASK,BLANK
      DATA MASK,BLANK/O"774000000000",O"200000000000"/

      WORDS(1)=A
      WORDS(2)=B
      WORDS(3)=C
      POSN=1
      do WORD=1,3
         if (WORD == 2 .and. POSN /= 6) cycle
         if (WORD == 3 .and. C < 0)POSN=POSN+1
         do CH=1,5
            CHARS(POSN)=ACHAR(ISHFT(IAND(WORDS(WORD),MASK),-29))
            if (CHARS(POSN) == ' ') exit
            LENG=POSN
            WORDS(WORD)=ISHFT(WORDS(WORD),7)
            POSN=POSN+1
         end do
      end do
      RETURN
      END



!  DATA STRUCTURE ROUTINES (VOCAB) 


      INTEGER FUNCTION VOCAB(ID,INIT)

!  LOOK UP ID IN THE VOCABULARY (ATAB) AND RETURN ITS "DEFINITION" (KTAB), OR
!  -1 IF NOT FOUND.  IF INIT IS POSITIVE, THIS IS AN INITIALISATION CALL SETTING
!  UP A KEYWORD VARIABLE, AND NOT FINDING IT CONSTITUTES A BUG.  IT ALSO MEANS
!  THAT ONLY KTAB VALUES WHICH TAKEN OVER 1000 EQUAL INIT MAY BE CONSIDERED.
!  (THUS "STEPS", WHICH IS A MOTION VERB AS WELL AS AN OBJECT, MAY BE LOCATED
!  AS AN OBJECT.)  AND IT ALSO MEANS THE KTAB VALUE IS TAKEN MOD 1000.

      use pdp10
      INTEGER ID,INIT
      INTEGER HASH,I
      COMMON /VOCCOM/ KTAB,ATAB,TABSIZ
      INTEGER KTAB,ATAB,TABSIZ
      DIMENSION KTAB(300),ATAB(300)

      HASH=IEOR(ID,IA5('PHROG'))
      do I=1,TABSIZ
         if (KTAB(I) == -1) GOTO 2
         if (INIT >= 0 .and. KTAB(I)/1000 /= INIT) cycle
         if (ATAB(I) == HASH) GOTO 3
      end do
      CALL BUG(21)

2     VOCAB=-1
      if (INIT < 0)RETURN
      CALL BUG(5)

3     VOCAB=KTAB(I)
      if (INIT >= 0)VOCAB=MOD(VOCAB,1000)
      RETURN
      END



!  UTILITY ROUTINES (RANI, DATIME, CIAO, BUG)


      integer function RANI(rrange)

!  Returns a value uniformly distributed between 0 and rrange-1.
!  Crowther and Woods implemented an LCG (m=2**20, a=1021, c=0)
!  as an alternative to RAN (the infamous RANDU?) in DEC LIB40.
!  Most F95 random_number implementations are better.

      implicit none
      integer, intent(in) :: rrange
      integer, allocatable :: seed(:)
      integer :: D, T, ticks, nseed
      real :: R
      logical, save :: seeded = .false.

      D = 1
      if (.not. seeded) then
         ! F18 random_init was not widespread in 2019.
         ! Some F95 random_seed implementations have predictable results
         ! when called with no argument.
         call random_seed(size=nseed)
         allocate(seed(nseed))
         ! not ideal but should be OK for this program
         call system_clock(count=ticks)
         if (ticks == 0) ticks = 1
         seed(:) = ticks
         call random_seed(put=seed)
         deallocate(seed)
         seeded = .true.
         call DATIME(D, T)
         D = 10 + mod(D, 10)
      end if
      do T = 1, D
         call random_number(R)
      end do
      RANI = int(rrange * R)
      return
      end function RANI



      SUBROUTINE DATIME(D,T)

!  RETURN THE DATE AND TIME IN D AND T.  D IS NUMBER OF DAYS SINCE 01-JAN-77,
!  T IS MINUTES PAST MIDNIGHT.  THIS IS HARDER THAN IT SOUNDS, BECAUSE THE
!  FINAGLED DEC FUNCTIONS RETURN THE VALUES ONLY AS ASCII STRINGS!

      INTEGER D,T
      INTEGER X,MON,YEAR
      INTEGER DTTM,HATH
      DIMENSION DTTM(8),HATH(12)
      DATA HATH/31,28,31,30,31,30,31,31,30,31,30,31/

      CALL DATE_AND_TIME(VALUES=DTTM)

      YEAR=DTTM(1)-1977
      D=DTTM(3)-1
      X=DTTM(2)
      do MON=1,12
         if (X == MON) GOTO 2
         D=D+HATH(MON)
      end do
      CALL BUG(28)

2     D=D+YEAR*365+YEAR/4
      if (MOD(YEAR,4) == 3 .and. MON > 2) D=D+1
      T=DTTM(5)*60+DTTM(6)
      RETURN
      END



      SUBROUTINE CIAO

!  EXITS, AFTER ISSUING REMINDER TO SAVE NEW CORE IMAGE.  USED WHEN SUSPENDING
!  AND WHEN CREATING NEW VERSION VIA MAGIC MODE.  ON SOME SYSTEMS, THE CORE
!  IMAGE IS LOST ONCE THE PROGRAM EXITS.  IF SO, SET K=31 INSTEAD OF 32.

      use text
      INTEGER K,A,B,C,D
      DATA K/32/

      CALL MSPEAK(K)
      if (K == 31) CALL GETIN(A,B,C,D)
      STOP
      END



      SUBROUTINE BUG(NUM)
      INTEGER NUM

!  THE FOLLOWING CONDITIONS ARE CURRENTLY CONSIDERED FATAL BUGS.  NUMBERS < 20
!  ARE DETECTED WHILE READING THE DATABASE; THE OTHERS OCCUR AT "RUN TIME".
!     0     MESSAGE LINE > 70 CHARACTERS
!     1     NULL LINE IN MESSAGE
!     2     TOO MANY WORDS OF MESSAGES
!     3     TOO MANY TRAVEL OPTIONS
!     4     TOO MANY VOCABULARY WORDS
!     5     REQUIRED VOCABULARY WORD NOT FOUND
!     6     TOO MANY RTEXT OR MTEXT MESSAGES
!     7     TOO MANY HINTS
!     8     LOCATION HAS COND BIT BEING SET TWICE
!     9     INVALID SECTION NUMBER IN DATABASE
!     20    SPECIAL TRAVEL (500>L>300) EXCEEDS GOTO LIST
!     21    RAN OFF END OF VOCABULARY TABLE
!     22    VOCABULARY TYPE (N/1000) NOT BETWEEN 0 AND 3
!     23    INTRANSITIVE ACTION VERB EXCEEDS GOTO LIST
!     24    TRANSITIVE ACTION VERB EXCEEDS GOTO LIST
!     25    CONDITIONAL TRAVEL ENTRY WITH NO ALTERNATIVE
!     26    LOCATION HAS NO TRAVEL ENTRIES
!     27    HINT NUMBER EXCEEDS GOTO LIST
!     28    INVALID MONTH RETURNED BY DATE FUNCTION

      PRINT 1, NUM
1     FORMAT (' FATAL ERROR, SEE SOURCE CODE FOR INTERPRETATION.'/  &
              ' PROBABLE CAUSE: ERRONEOUS INFO IN DATABASE.'/  &
              ' ERROR CODE =',I2/)
      STOP
      END

end module advn2
