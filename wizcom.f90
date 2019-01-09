module wizcom
   use pdp10
   use text
   use advn2
   implicit none

   integer :: WKDAY, WKEND, HOLID, HBEGIN, HEND,  &
              SHORT, MAGNM, LATNCY, SAVED, SAVET
   integer(kind=A5) :: MAGIC
   integer :: HNAME(4)
   integer, save :: SETUP = 0

   public :: SHORT, LATNCY, SAVED, SAVET, SETUP
   private :: WKDAY, WKEND, HOLID, HBEGIN, HEND, HNAME, MAGIC, MAGNM

   public :: START, MAINT, HOURS, MOTD, POOF
   private :: WIZARD, HOURSX, NEWHRS, NEWHRX

contains

!  WIZARDRY ROUTINES (START, MAINT, WIZARD, HOURS(X), NEWHRS(X), MOTD, POOF)

   logical function START()

!  CHECK TO SEE IF THIS IS "PRIME TIME".  IF SO, ONLY WIZARDS MAY PLAY, THOUGH
!  OTHERS MAY BE ALLOWED A SHORT GAME FOR DEMONSTRATION PURPOSES.  IF SETUP<0,
!  WE'RE CONTINUING FROM A SAVED GAME, SO CHECK FOR SUITABLE LATENCY.  RETURN
!  TRUE IF THIS IS A DEMO GAME (VALUE IS IGNORED FOR RESTARTS).

      INTEGER D,T,PRIMTM,DELAY
      LOGICAL PTIME,SOON

!  FIRST FIND OUT WHETHER IT IS PRIME TIME (SAVE IN PTIME) AND, IF RESTARTING,
!  WHETHER IT'S TOO SOON (SAVE IN SOON).  PRIME-TIME SPECS ARE IN WKDAY, WKEND,
!  AND HOLID; SEE MAINT ROUTINE FOR DETAILS.  LATNCY IS REQUIRED DELAY BEFORE
!  RESTARTING.  WIZARDS MAY CUT THIS TO A THIRD.

      CALL DATIME(D,T)
      PRIMTM=WKDAY
      if (MOD(D,7) <= 1) PRIMTM=WKEND
      if (D >= HBEGIN .and. D <= HEND) PRIMTM=HOLID
      PTIME=IAND(PRIMTM,ISHFT(1,T/60)) /= 0
      SOON=.false.
      if (SETUP >= 0) GOTO 20
      DELAY=(D-SAVED)*1440+(T-SAVET)
      if (DELAY >= LATNCY) GOTO 20
      PRINT 10,DELAY
10    FORMAT(' THIS ADVENTURE WAS SUSPENDED A MERE',I3,' MINUTES AGO.')
      SOON=.true.
      if (DELAY >= LATNCY/3) GOTO 20
      CALL MSPEAK(2)
      STOP

!  IF NEITHER TOO SOON NOR PRIME TIME, NO PROBLEM.  ELSE SPECIFY WHAT'S WRONG.

20    START=.false.
      if (SOON) GOTO 30
      if (PTIME) GOTO 25
22    SAVED=-1
      RETURN

!  COME HERE IF NOT RESTARTING TOO SOON (MAYBE NOT RESTARTING AT ALL), BUT IT'S
!  PRIME TIME.  GIVE OUR HOURS AND SEE IF HE'S A WIZARD.  IF NOT, THEN CAN'T
!  RESTART, BUT IF JUST BEGINNING THEN WE CAN OFFER A SHORT GAME.

25    CALL MSPEAK(3)
      CALL HOURS
      CALL MSPEAK(4)
      if (WIZARD()) GOTO 22
      if (SETUP < 0) GOTO 33
      START=YESM(5,7,7)
      if (START) GOTO 22
      STOP

!  COME HERE IF RESTARTING TOO SOON.  IF HE'S A WIZARD, LET HIM GO (AND NOTE
!  THAT IT THEN DOESN'T MATTER WHETHER IT'S PRIME TIME).  ELSE, TOUGH BEANS.

30    CALL MSPEAK(8)
      if (WIZARD()) GOTO 22
33    CALL MSPEAK(9)
      STOP
   end function START


   subroutine MAINT

!  SOMEONE SAID THE MAGIC WORD TO INVOKE MAINTENANCE MODE.  MAKE SURE HE'S A
!  WIZARD.  IF SO, LET HIM TWEAK ALL SORTS OF RANDOM THINGS, THEN EXIT SO CAN
!  SAVE TWEAKED VERSION.
      integer :: D, T
      integer(kind=A5) :: X, Y
      COMMON /BLKCOM/ BLKLIN
      LOGICAL BLKLIN

      if (.not.WIZARD()) RETURN
      BLKLIN=.false.
      if (YESM(10,0,0)) CALL HOURS
      if (YESM(11,0,0)) CALL NEWHRS
      if (YESM(26,0,0)) then
         CALL MSPEAK(27)
         READ 1,HBEGIN
1        FORMAT(I4)
         CALL MSPEAK(28)
         READ 1,HEND
         CALL DATIME(D,T)
         HBEGIN=HBEGIN+D
         HEND=HBEGIN+HEND-1
         CALL MSPEAK(29)
         READ 2,HNAME
2        FORMAT(4A5)
      end if
      PRINT 12,SHORT
12    FORMAT(' LENGTH OF SHORT GAME (NULL TO LEAVE AT',I3,'):')
      READ 1,X
      if (X > 0) SHORT=X
      CALL MSPEAK(12)
      CALL GETIN(X,Y,Y,Y)
      if (X /= IA5('     ')) MAGIC=X
      CALL MSPEAK(13)
      READ 1,X
      if (X > 0) MAGNM=X
      PRINT 16,LATNCY
16    FORMAT(' LATENCY FOR RESTART (NULL TO LEAVE AT',I3,'):')
      READ 1,X
      if (X > 0 .and. X < 45) CALL MSPEAK(30)
      if (X > 0) LATNCY=MAX(45,X)
      if (YESM(14,0,0)) CALL MOTD(.true.)
      SAVED=0
      SETUP=2
      CALL MSPEAK(15)
      BLKLIN=.true.
      CALL CIAO
   end subroutine MAINT


   logical function WIZARD()

!  ASK IF HE'S A WIZARD.  IF HE SAYS YES, MAKE HIM PROVE IT.  RETURN TRUE IF HE
!  REALLY IS A WIZARD.

      integer(kind=A5) :: WORD, X, Y, Z
      integer :: D, T
      integer, dimension(5) :: VAL

      WIZARD=YESM(16,0,7)
      if (.not.WIZARD) RETURN

!  HE SAYS HE IS.  FIRST STEP: DOES HE KNOW ANYTHING MAGICAL?

      CALL MSPEAK(17)
      CALL GETIN(WORD,X,Y,Z)
      if (WORD /= MAGIC) GOTO 99

!  HE DOES.  GIVE HIM A RANDOM CHALLENGE AND CHECK HIS REPLY.

      CALL DATIME(D,T)
      T=T*2+1
      WORD=IA5('@@@@@')
      do Y=1,5
         X=79+MOD(D,5)
         D=D/5
         do Z=1,X
            T=MOD(T*1027,1048576)
         end do
         VAL(Y)=(T*26)/1048576+1
         WORD=WORD+ISHFT(VAL(Y),36-7*Y)
      end do
      if (YESM(18,0,0)) GOTO 99
      PRINT 18,A5I(WORD)
18    FORMAT(/1X,A)
      CALL GETIN(WORD,X,Y,Z)
      CALL DATIME(D,T)
      T=(T/60)*40+(T/10)*10
      D=MAGNM
      do Y=1,5
         Z=MOD(Y,5)+1
!  mkg made puzzle time independent
         X=MOD(ABS(VAL(Y)-VAL(Z))*MOD(D,10),26)+1
         T=T/10
         D=D/10
         WORD=WORD-ISHFT(X,36-7*Y)
      end do
      if (WORD /= IA5('@@@@@')) GOTO 99

!  BY GEORGE, HE REALLY *IS* A WIZARD!

      CALL MSPEAK(19)
      RETURN

!  AHA!  AN IMPOSTOR!

99    CALL MSPEAK(20)
      WIZARD=.false.
      RETURN
   end function WIZARD


   subroutine HOURS

!  ANNOUNCE THE CURRENT HOURS WHEN THE CAVE IS OPEN FOR ADVENTURING.  THIS INFO
!  IS STORED IN WKDAY, WKEND, AND HOLID, WHERE BIT ISHFT(1,N) IS ON IFF THE
!  HOUR FROM N:00 TO N:59 IS "PRIME TIME" (CAVE CLOSED).  WKDAY IS FOR
!  WEEKDAYS, WKEND FOR WEEKENDS, HOLID FOR HOLIDAYS.  NEXT HOLIDAY IS FROM
!  HBEGIN TO HEND.

      INTEGER D,T
      integer, dimension(5) :: VAL

      PRINT 1
1     FORMAT()
      CALL HOURSX(WKDAY,'MON -',' FRI:')
      CALL HOURSX(WKEND,'SAT -',' SUN:')
      CALL HOURSX(HOLID,'HOLID','AYS: ')
      CALL DATIME(D,T)
      if (HEND < D .or. HEND < HBEGIN) RETURN
      if (HBEGIN <= D) then
         PRINT 5,HNAME
5        FORMAT(/' TODAY IS A HOLIDAY, NAMELY ',4A5)
         RETURN
      end if
      D=HBEGIN-D
      T=IA5('DAYS,')
      if (D == 1) T=IA5('DAY, ')
      PRINT 15,D,T,HNAME
15    FORMAT(/' THE NEXT HOLIDAY WILL BE IN',I3,' ',A5,' NAMELY ',4A5)
      RETURN
   end subroutine HOURS


   subroutine HOURSX(H,DAY1,DAY2)

!  USED BY HOURS (ABOVE) TO PRINT HOURS FOR EITHER WEEKDAYS OR WEEKENDS.

      integer, intent(in) :: H
      character*5, intent(in) :: DAY1, DAY2
      LOGICAL FIRST
      INTEGER FROM,TILL

      FIRST=.true.
      FROM=-1
      if (H == 0) then
         PRINT 2,DAY1,DAY2
2        FORMAT(10X,2A5,'  OPEN ALL DAY')
         RETURN
      end if
10    FROM=FROM+1
      if (IAND(H,ISHFT(1,FROM)) /= 0) GOTO 10
      if (FROM < 24) then
         TILL=FROM
14       TILL=TILL+1
         if (IAND(H,ISHFT(1,TILL)) == 0 .and. TILL /= 24) GOTO 14
         if (FIRST) then
            PRINT 16,DAY1,DAY2,FROM,TILL
16          FORMAT(10X,2A5,I4,':00 TO',I3,':00')
         else
            PRINT 18,FROM,TILL
18          FORMAT(20X,I4,':00 TO',I3,':00')
         end if
         FIRST=.false.
         FROM=TILL
         GOTO 10
      end if
20    if (FIRST) PRINT 22,DAY1,DAY2
22    FORMAT(10X,2A5,'  CLOSED ALL DAY')
      RETURN
   end subroutine HOURSX


   subroutine NEWHRS

!  SET UP NEW HOURS FOR THE CAVE.  SPECIFIED AS INVERSE--I.E., WHEN IS IT
!  CLOSED DUE TO PRIME TIME?  SEE HOURS (ABOVE) FOR DESC OF VARIABLES.

      CALL MSPEAK(21)
      WKDAY=NEWHRX('WEEKD','AYS: ')
      WKEND=NEWHRX('WEEKE','NDS: ')
      HOLID=NEWHRX('HOLID','AYS: ')
      CALL MSPEAK(22)
      CALL HOURS
      RETURN
   end subroutine NEWHRS


   integer function NEWHRX(DAY1,DAY2)

!  INPUT PRIME TIME SPECS AND SET UP A WORD OF INTERNAL FORMAT.

      character*5, intent(in) :: DAY1, DAY2
      INTEGER FROM,TILL,I

      NEWHRX=0
      PRINT 1,DAY1,DAY2
1     FORMAT(' PRIME TIME ON ',2A5)
10    PRINT 2
2     FORMAT(' FROM:')
      READ 3,FROM
3     FORMAT(I4)
      if (FROM < 0 .or. FROM >= 24) RETURN
      PRINT 4
4     FORMAT(' TILL:')
      READ 3,TILL
      TILL=TILL-1
      if (TILL < FROM .or. TILL >= 24) RETURN
      do I=FROM,TILL
         NEWHRX=IOR(NEWHRX,ISHFT(1,I))
      end do
      GOTO 10
   end function NEWHRX


   subroutine MOTD(ALTER)

!  HANDLES MESSAGE OF THE DAY.  IF ALTER IS TRUE, READ A NEW MESSAGE FROM THE
!  WIZARD.  ELSE PRINT THE CURRENT ONE.  MESSAGE IS INITIALLY NULL.

      logical, intent(in) :: ALTER
      integer :: I, K, M
      integer, dimension(100) :: MSG
      DATA MSG/100*-1/

      if (ALTER) GOTO 50

      K=1
10    if (MSG(K) < 0) RETURN
      PRINT 20,(MSG(I),I=K+1,MSG(K)-1)
20    FORMAT(' ',14A5)
      K=MSG(K)
      GOTO 10

50    M=1
      CALL MSPEAK(23)
55    READ 56,(MSG(I),I=M+1,M+14),K
56    FORMAT(15A5)
      if (K == IA5('     ')) GOTO 60
      CALL MSPEAK(24)
      GOTO 55
60    do I=1,14
         K=M+15-I
         if (MSG(K) /= IA5('     ')) GOTO 65
      end do
      GOTO 90
65    MSG(M)=K+1
      M=K+1
      if (M+14 < 100) GOTO 55
      CALL MSPEAK(25)
90    MSG(M)=-1
      RETURN
   end subroutine MOTD


   subroutine POOF

!  AS PART OF DATABASE INITIALISATION, WE CALL POOF TO SET UP SOME DUMMY
!  PRIME-TIME SPECS, MAGIC WORDS, ETC.

      WKDAY=O"00777400"
      WKEND=0
      HOLID=0
      HBEGIN=0
      HEND=-1
      SHORT=30
      MAGIC=IA5('DWARF')
      MAGNM=11111
      LATNCY=90
      RETURN
   end subroutine POOF

end module wizcom
