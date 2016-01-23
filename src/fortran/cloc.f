       PROGRAM CLOC
C CLOC PACKAGE FOR SUN (SUNOS Unix)
C MAIN PROGRAM STARTS HERE
      INCLUDE 'global.f'
C         CLOC PACKAGE IN FORTRAN 77
C IN 1982 THIS PACKAGE WAS CONVERTED FROM FROM ALGOL68C TO FORTRAN VIA C
C AT THE ACADEMIC COMPUTER CENTRE, YORK UNIVERSITY, TORONTO. THE
C CONVERSION WAS SUPPORTED BY PROFESSORS GREAVES(YORK), BENSON(YORK),
C AND BRAINERD(TORONTO) AND STUDY LEAVE WAS MADE AVAILABLE BY PROFESSOR
C P JARRATT, DIRECTOR OF THE COMPUTER CENTRE UNIVERSITY OF BIRMINGHAM
C ENGLAND. THE DIRECTOR OF THE YORK ACADEMIC COMPUTING CENTRE, TIM
C WARNER, PROVIDED EXCELLENT ACCESS FACILITIES TO A VAX-11/780 AND
C SUBSTANTIAL TECHNICAL SUPPORT WAS GIVEN BY OZAN YIGIT.
C
C AUTHORS ADDRESS:
C          ALAN REED
C          CENTRE FOR COMPUTING AND COMPUTER SCIENCE
C          ELMS ROAD
C          UNIVERSITY OF BIRMINGHAM
C          BIRMINGHAM B15 2TT
C          ENGLAND
C  PHONE:  NATIONAL        (021) 414 3992 (DIRECT DIAL)
C          INTERNATIONAL  +44 21 414 3992
C  ELECTRONIC MAIL:-
C         (REFERENCE CACM OCT. 1986 VOL. 29 NO. 10)
C         INTERNET:  A.REED@BHAM.AC.UK
C         BITNET:    A.REED%BHAM.AC.UK@UKACRL
C         JANET:     A.REED@UK.AC.BHAM
C         UUCP:      ...mcvax!ukc!bham!a.reed
C         JUNET:     A.REED@BHAM.AC.UK.JANET
C
C NOTE THAT THE FORTRAN77 IS NON-STANDARD IN THAT IT USES 'INCLUDE FILES'.
C
      CHARACTER *100 S
      LOGICAL STREQ,STRNE,CHECK
      LOGICAL FAIL,OK
      MARK='2S'
      LPWDTH=120
      CENTRE=LPWDTH/2
      SW1=.FALSE.
      SW2=.FALSE.
      SW3=.TRUE.
      SW4=.TRUE.
      SW5=.FALSE.
      EOTEXT=.FALSE.
      XNLINE=CHAR(10)
      XNPAGE=CHAR(12)
      GAP=24
      TAPE1=1
      TAPE2=2
      TAPE3=3
      TAPE4=4
      ITEXT=5
      ZNORML=0
      ZPADD=1
      ZDEFER=2
      ZLETER=1
      ZSEPAR=2
      ZIGNOR=3
      ZRDASP=4
      ZSKIPS=5
      ZSKEND=6
      ZOPENS=7
      ZOPEND=8
      ZREFSS=9
      ZRFEND=10
      ZCONTI=11
      ZNL=12
      CONTI=0
      ASPACE=ICHAR(' ')
      UPPERA=65
      UPPERZ=90
      LOWERA=97
      LOWERZ=122
      MIXINP=0
      RAIIMP=1
      LOWINP=2
      COFLAG=.FALSE.
      STRAT=1
      ATBSZ1=TABSIZ+1
      BTBSZ4=4*(TABSIZ+1)
      CTAB34=((TABSIZ+1)*3)/4
      WANT=1
      NOWANT=2
      NOTREJ=3
      REJECT=4
      SALPHA=1
      SDALPH=2
      SAFREQ=3
      SDFREQ=4
      SALENG=5
      SDLENG=6
      SAXLEN=7
      SDXLEN=8
      SRVALF=9
      STRVDA=10
      SFIRST=11
      SLAST=12
      CSTYLE=1
      LFTSTY=2
      NONSTY=3
      CTLSTP=0
C OPEN 5 FTEMP FILES
      F1=30
      F2=31
      F3=32
      F4=33
      F5=34
      FTEMP=35
      OPEN(UNIT=F1,STATUS='SCRATCH',FORM='UNFORMATTED',
     +     ACCESS='SEQUENTIAL')
      OPEN(UNIT=F2,STATUS='SCRATCH',FORM='UNFORMATTED',
     +     ACCESS='SEQUENTIAL')
      OPEN(UNIT=F3,STATUS='SCRATCH',FORM='UNFORMATTED',
     +     ACCESS='SEQUENTIAL')
      OPEN(UNIT=F4,STATUS='SCRATCH',FORM='UNFORMATTED',
     +     ACCESS='SEQUENTIAL')
      OPEN(UNIT=F5,STATUS='SCRATCH',FORM='UNFORMATTED',
     +     ACCESS='DIRECT',RECL=4)
      NIN=5
      NOUT=6
      STDOUT=NOUT
C     OPEN(UNIT=NIN,DEVICE='TTY:')
C     OPEN(UNIT=NOUT,DEVICE='TTY:')
C ASK USER FOR A  LOGFILE
      MON=20
      CALL ASK('(logfile) ',MON,'UNKNOWN',NOUT)
      MONFIL=1
C ASK USER FOR A  CONTROLFILE
      CON=21
      CALL ASK('(controlfile) ',CON,'OLD',NIN)
      CONFIL=2
C ASK USER FOR A  RESULTSFILE
      RES=22
      CALL ASK('(resultsfile) ',RES,'UNKNOWN',NOUT)
      RESULT=3
C ASK USER FOR A  TEXTFILE
      DAT=23
      CALL ASK('(textfile) ',DAT,'OLD',NIN)
      DATFIL=4
      SF=24
      IF( MON.EQ.STDOUT )THEN
          GAP=1
      END IF
      IF( RES.EQ.STDOUT )THEN
          LPWDTH=79
      END IF
      IF(MON.EQ.NOUT .AND. CON.EQ.NIN)THEN
          SW3=.FALSE.
          SW4=.FALSE.
      END IF
      KMON=1
      KRES=1
      SAVSW3=SW3
      SAVSW4=SW4
      ERRFIL=MONFIL
      CALL RESET(ITEXT)
C     CALL PUTBN1(ITEXT,0,0)
      PTR=MXWDTH-1
      WIDTH=MXWDTH
      WHSPCE=.TRUE.
      LNUMB=  -1
      ERRNUM=0
      CALL CLRSTR(ERRVEC,MXWDTH+1)
      CALL CLRSTR(CNTLIN,MXWDTH+1)
C TELL THE USER THAT I WROTE THIS PACKAGE
      CALL ADVERT
      ISTATE=MIXINP
      CALL PREIN(INWARD,ISTATE,ZIGNOR)
C OUTWAR=INWARD
      CALL MARGIN
      CALL PUTSTR(MONFIL,'listing of control statements')
      CALL PUTNL(MONFIL)
      CALL PUTNL(MONFIL)
      CALL RDCON
C ENSURE IT IS NON BLANK
    1 IF(  STREQ(CTLFLD,QBLANK,15)  )THEN
          CALL RDCON
          GOTO 1
      END IF
      IF( STREQ(CTLFLD,QGTTXT,15) )THEN
          IF( SW3 )THEN
              CALL PRCON
          END IF
          CALL NSNXTC
          S=' '
          I=0
    2     IF( .NOT. YEND  .AND.  YCHAR.NE.' ' )THEN
              I=I+1
              S(I:I)=YCHAR
              CALL NEXTCH
              GOTO 2
          END IF
          CALL SKPRST
          IF( I.EQ.0 )THEN
              CALL GTTEXT('savtxt.bin')
          ELSE
              CALL GTTEXT(S)
          END IF
      ELSE
C         CALL INPDET(INWDTH,CONT,COPEN,CCLOSE,RUNOVR,BOREFS,
C    +    IGOPEN,IGCLOS,ENDLN,COOPEN,COCLOS,HYPHEN,FAIL)
          CALL INPDET(INWDTH,CONT,COPEN,CCLOSE,RUNOVR,BOREFS,
     +    IGOPEN,IGCLOS,ENDLN,COOPEN,COCLOS,HYPHEN,FAIL)
          EOLIN=' '
          IF(ENDLN.GE.0)EOLIN=CHAR( ENDLN )
          IF( STREQ(CTLFLD,QFROM,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL NSNXTC
              IF( CHECK('item') )THEN
                  CALL NSNXTC
                  CALL GETNUM(WRDFRM,FAIL)
                  IF( FAIL )THEN
                      WRDFRM=1
                  END IF
              END IF
              CALL SKPRST
          END IF
          IF( STREQ(CTLFLD,QTO,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL NSNXTC
              IF( CHECK('item') )THEN
                  CALL NSNXTC
                  CALL GETNUM(WORDTO,FAIL)
                  IF( FAIL )THEN
                      WORDTO= -1
                  END IF
              END IF
              CALL SKPRST
          END IF
          CALL ITMUSE(OK)
C ENSURE USER SUPPLIES ONE
    3     IF(  .NOT. OK  )THEN
              CALL PRERRS
C PRINT ERRORS FROM PREVIOUS CARD (IF ANY)
              CALL PUTSTR(MONFIL,'*** try again')
              CALL PUTNL(MONFIL)
              CALL RDCON
              CALL ITMUSE(OK)
              GOTO 3
          END IF
          MXLENG=0
          RUNWC=0
          OCOUNT=0
          ITMCNT=0
          DISTWC=0
          IF(  RUNOVR  )THEN
              LNUMB= -1
          ELSE IF(  EOLIN.EQ.' '  )THEN
              LNUMB= -1
          ELSE
              LNUMB=0
          END IF
          DCHAR= ICHAR( EOLIN)
          DTYPE=TYPE(DCHAR)
          CALL RDTXT3
          CONTINUE
      END IF
      CALL PUTSTR(MONFIL,'the text contains :')
      CALL PUTNL(MONFIL)
      CALL PUTINT(MONFIL,RUNWC,-8)
      CALL PUTSTR(MONFIL,'  running words')
      CALL PUTNL(MONFIL)
      CALL PUTINT(MONFIL,DISTWC,-8)
      CALL PUTSTR(MONFIL,'  distinct words')
      CALL PUTNL(MONFIL)
      CALL PUTSTR(MONFIL,'and the maximum word length is ')
      CALL PUTINT(MONFIL,MXLENG,0)
      CALL PUTSTR(MONFIL,' characters')
      CALL PUTNL(MONFIL)
      CALL PUTNL(MONFIL)
      MXRECL=MXLENG+5
      IF(  STREQ(CTLFLD,QSVTXT ,15) )THEN
          IF( SW3 )THEN
              CALL PRCON
          END IF
          CALL NSNXTC
          S=' '
          I=0
    4     IF(  .NOT. YEND  .AND.  YCHAR.NE.' ' )THEN
              I=I+1
              S(I:I)=YCHAR
              CALL NEXTCH
              GOTO 4
          END IF
          CALL SKPRST
          IF( I.EQ.0 )THEN
              CALL SAVTXT('savtxt.bin')
          ELSE
              CALL SAVTXT(S)
          END IF
      END IF
      CALL AOUTDE(LPWDTH,FAIL)
      CALL STRTUP
    5 IF(  STREQ(CTLFLD,QCOOC,15) .OR.
     +STREQ(CTLFLD,QWRITE,15) )THEN
          IF( SW3 )THEN
              CALL PRCON
          END IF
          IF( STREQ(CTLFLD,QCOOC,15) )THEN
              CALL COCCUR
          ELSE
C STREQ(CTLFLD,QWRITE,15)
              CALL TXTPRT
          END IF
          GOTO 5
      END IF
      CALL WRDSEL
C     CALL PRCONT(SYMTAB)
    6 IF(  STRNE(CTLFLD,QFINIS,15) )THEN
          IF(  STREQ(CTLFLD,QWLIST ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL WRDLST
          ELSE IF(  STREQ(CTLFLD,QCONC ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL ACONC3
          ELSE IF(  STREQ(CTLFLD,QCOOC ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL COCCUR
          ELSE IF(  STREQ(CTLFLD,QCOLL ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL COLLOC
          ELSE IF(  STREQ(CTLFLD,QEVWRD,15)  .OR.
     +        STREQ(CTLFLD,QSEWRD ,15) )THEN
                  CALL WRDSEL
              ELSE IF(  STREQ(CTLFLD,QRUN ,15) )THEN
                  IF( SW3 )THEN
                      CALL PRCON
                  END IF
    7             IF(  .NOT. YEND  )THEN
                      CALL NEXTCH
                      GOTO 7
                  END IF
                  IF(CONTI.NE.0)CALL RDCON
                  YEND= .FALSE.
              ELSE IF(  STREQ(CTLFLD,QNOTE,15) )THEN
                  IF( SW3 )THEN
                      CALL PRCON
                  END IF
    8             IF(  .NOT. YEND  )THEN
                      CALL NEXTCH
                      GOTO 8
                  END IF
                  IF(CONTI.NE.0)CALL RDCON
                  YEND= .FALSE.
              ELSE IF(  STREQ(CTLFLD,QINDEX,15) )THEN
                  IF( SW3 )THEN
                      CALL PRCON
                  END IF
                  CALL INDEX
              ELSE IF(  STREQ(CTLFLD,QWRITE,15) )THEN
                  IF( SW3 )THEN
                      CALL PRCON
                  END IF
                  CALL TXTPRT
              ELSE IF(  STREQ(CTLFLD,QTESRT,15) )THEN
                  IF( SW3 )THEN
                      CALL PRCON
                  END IF
                  CALL TSTSRT
              ELSE IF(  STREQ(CTLFLD,QNEWL,15) )THEN
                  IF( SW3 )THEN
                      CALL PRCON
                  END IF
                  CALL NSNXTC
                  CALL GETNUM(N,FAIL)
                  IF( FAIL )THEN
                      N=1
                  END IF
                  CALL SKPRST
                  DO 9 DUMMY =1, N
                      CALL PUTNL(RESULT)
    9                 CONTINUE
                  ELSE IF(  STREQ(CTLFLD,QNEWP,15) )THEN
                      IF( SW3 )THEN
                          CALL PRCON
                      END IF
                      CALL SKPRST
                      CALL PUTNP(RESULT)
                  ELSE IF(  STREQ(CTLFLD,QMESSA,15) )THEN
                      IF( SW3 )THEN
                          CALL PRCON
                      END IF
   10                 IF( .NOT. YEND  )THEN
                          CALL PUTCH1(RESULT,YCHAR)
                          CALL NEXTCH
                          GOTO 10
                      END IF
                      IF(CONTI.NE.0)CALL RDCON
                      YEND=.FALSE.
                      CALL PUTNL(RESULT)
                  ELSE IF(  STREQ(CTLFLD,QSTATS,15) )THEN
                      IF( SW3 )THEN
                          CALL PRCON
                      END IF
                      CALL SKPRST
                      CALL STATS
                  ELSE
                      CALL PRCON
                      CALL MARGIN
                      CALL PUTSTR(MONFIL,
     +                     '*** not allowed in this context')
                      CALL PUTNL(MONFIL)
                      CALL RDCON
                  END IF
                  GOTO 6
              END IF
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL CLOSED
              CALL ENDRUN
              END
      SUBROUTINE PTFLOT(F,A,I,J,K)
      REAL A
      INTEGER I,J,K,F
C THIS ROUTINE WILL PRINT A FLOATING POINT NUMBER  A  ON CHANNEL  F
C ON ENTRY
C    INPUT  A  -  THE FLOATING POINT NUMBER TO BE PRINTED
C    INPUT  F  -  THE CHANNEL THE NUMBER WILL BE PRINTED ON
C ON EXIT
C    ALL ARGUMENTS ARE UNCHANGED
      CHARACTER *15 FL
      WRITE(FL,100) A
  100 FORMAT(1PE15.8)
      CALL PUTSTR(F,FL)
      END
      REAL FUNCTION XCLOCK(N)
      INTEGER N
      XCLOCK=0.0
      END
      SUBROUTINE GTLINE(F,ALIST,WIDTH,EOF1)
      INTEGER F,WIDTH
      CHARACTER ALIST(0:119)
      LOGICAL EOF1
C READS A LINE FROM GIVEN CHANNEL  F
C ON ENTRY
C    INPUT  F  THE CHANNEL THE LINE IS TO BE READ FROM
C ON EXIT
C    OUTPUT  ALIST(0:119)  WILL CONTAIN THE CHARACTERS READ IN
C    OUTPUT  WIDTH  WILL CONTAIN A COUNT OF THE NUMBER OF CHARACTERS
C                   READ IN AFTER TRAILING BLANKS HAVE BEEN REMOVED
C    OUTPUT  EOF1  IS .TRUE. IF THE END-OF-FILE CONDITION WAS FOUND WHEN
C                            THE LINE WAS READ
C                  IS .FALSE. IF A VALID LINE WAS READ
      EOF1=.FALSE.
      READ(F,100,END=99) ALIST
  100 FORMAT(120A1)
      WIDTH=120
      DO 10 I=119,0,-1
         IF(ALIST(I).NE.' ')GOTO 910
         WIDTH=WIDTH-1
   10 CONTINUE
  910 CONTINUE
C    0<=WIDTH<=120   WIDTH= NUMBER OF CHARACTERS LESS TRAILING SPACES
      RETURN
   99 EOF1=.TRUE.
      END
      SUBROUTINE ASK(MESAGE,F,MODE,DEFALT)
      INCLUDE 'global.f'
      CHARACTER MESAGE *(*)
      CHARACTER MODE *(*)
      INTEGER F
      CHARACTER *100 NAME
      INTEGER DEFALT
C ASKS THE USER FOR A FILENAME AND OPENS A FILE OF THAT NAME
C ON ENTRY
C    INPUT  MESAGE  AN INFORMATIVE MESAGE TELLING THE USER
C                    WHICH KIND OF FILENAME IS REQUIRED
C    INPUT  F  A CHANNEL NUMBER ON WHICH TO OPEN THE FILE
C    INPUT  MODE  THE FORTRAN77 STATUS OF FILE OPENING E.G. NEW
C    INPUT  DEFALT  THE CHANNEL NUMBER TO BE USED IF THE USER
C                    DOES NOT SUPPLY A FILENAME. THE DEFALT
C                    CHANNEL IS ASSUMED TO BE OPEN ALREADY.
      WRITE(NOUT,100) MESAGE
  100 FORMAT(1X,A,$)
      READ(NIN,200)NAME
  200 FORMAT(A)
      IF(NAME.EQ.' ')GOTO 1
      OPEN(UNIT=F,FILE=NAME,STATUS=MODE)
      RETURN
    1 F=DEFALT
      END
      SUBROUTINE PUTNL(JFN)
      INCLUDE 'global.f'
C SENDS A NEWLINE CODE THE THE CHANNEL  JFN  THIS WILL CAUSE A BUFFER TO
C BE FLUSHED AND CHARACTERS SEND TO THE CHANNEL  JFN
C INPUT  JFN  THE FORTRAN CHANNEL NUMBER TO BE USED
      CALL PUTCH1(JFN,XNLINE)
      END
      SUBROUTINE PUTNP(JFN)
      INCLUDE 'global.f'
      CALL PUTCH1(JFN,XNPAGE)
      END
      SUBROUTINE STOP
C TERMINATES THE CLOC PACKAGE. FORTRAN SYSTEM SOFTWARE MAY CLOSE VARIOUS
C FILES
      STOP
      END
      SUBROUTINE SETTO(A,N,VALUE)
      INTEGER N,VALUE,A(0:N)
      INTEGER I
C SETS THE  N+1  ELEMENTS OF ARRAY  A  TO  VALUE
C INPUT  N  ONE LESS THAN THE SIZE OF THE ARRAY A
C INPUT  VALUE  THE NUMBER TO WHICH THE N+1 ELEMENTS OF  A  WILL BE SET
C OUTPUT  A(0:N)  AN ARRAY TO BE CHANGED.
      DO 10 I=0,N
          A(I)=VALUE
   10 CONTINUE
      END
      SUBROUTINE CLRSTR(S,N)
      CHARACTER S(*)
      INTEGER N
      INTEGER I
C SETS THE  N+1  ELEMENTS OF THE CHARACTER ARRAY  S  TO THE SPACE
C CHARACTER INPUT  N  ONE LESS THAN THE NUMBER OF CHARACTERS IN THE
C ARRAY  S OUTPUT  S(0:N)  AN ARRAY OF CHARACTERS WHICH WILL BE SET
C TO THE SPACE
      DO 10 I=0,N
          S(I)=' '
   10 CONTINUE
      END
      SUBROUTINE RESET(JFN)
      INCLUDE 'global.f'
      INTEGER JFN
      GOTO (101,102,103,104,105),JFN
  101 CONTINUE
      REWIND F1
      GOTO 199
  102 CONTINUE
      REWIND F2
      GOTO 199
  103 CONTINUE
      REWIND F3
      GOTO 199
  104 CONTINUE
      REWIND F4
      GOTO 199
  105 CONTINUE
C     REWIND F5
      GOTO 199
  199 CONTINUE
      END
      SUBROUTINE DELF(JFN)
      INCLUDE 'global.f'
      INTEGER JFN
      GOTO (101,102,103,104,105) ,JFN
  101 CONTINUE
      CLOSE(UNIT=F1,STATUS='DELETE')
      GOTO 199
  102 CONTINUE
      CLOSE(UNIT=F2,STATUS='DELETE')
      GOTO 199
  103 CONTINUE
      CLOSE(UNIT=F3,STATUS='DELETE')
      GOTO 199
  104 CONTINUE
      CLOSE(UNIT=F4,STATUS='DELETE')
      GOTO 199
  105 CONTINUE
      CLOSE(UNIT=F5,STATUS='DELETE')
      GOTO 199
  199 CONTINUE
      END
      SUBROUTINE CLOSF(JFN)
      INCLUDE 'global.f'
      INTEGER JFN
      GOTO (101,102,103,104,105) ,JFN
  101 CONTINUE
      CLOSE(UNIT=F1,STATUS='KEEP')
      GOTO 199
  102 CONTINUE
      CLOSE(UNIT=F2,STATUS='KEEP')
      GOTO 199
  103 CONTINUE
      CLOSE(UNIT=F3,STATUS='KEEP')
      GOTO 199
  104 CONTINUE
      CLOSE(UNIT=F4,STATUS='KEEP')
      GOTO 199
  105 CONTINUE
      CLOSE(UNIT=F5,STATUS='KEEP')
      GOTO 199
  199 CONTINUE
      END
      SUBROUTINE DAYTIM(S)
      INCLUDE 'global.f'
      CHARACTER S*(*)
      CHARACTER *29 DTIME
      CALL FDATE(DTIME)
      S=DTIME
      DTSIZE=29
      END
      SUBROUTINE PUTBN1(F,POSITN,VALUE)
      INCLUDE 'global.f'
      INTEGER F,POSITN,VALUE
      IF(F.LT.1 .OR. F.GT.5)GOTO 177
      GOTO (101,102,103,104,105),F
  101 CONTINUE
      GOTO 199
  102 CONTINUE
      GOTO 199
  103 CONTINUE
      GOTO 199
  104 CONTINUE
      GOTO 199
  105 CONTINUE
      WRITE(F5,REC=POSITN) VALUE
      GOTO 199
  177 CONTINUE
  199 CONTINUE
      END
      SUBROUTINE GETBN1(F,POSITN,VALUE)
      INCLUDE 'global.f'
      INTEGER F,POSITN,VALUE
      IF(F.LT.1 .OR. F.GT.5)GOTO 177
      GOTO (101,102,103,104,105),F
  101 CONTINUE
      GOTO 199
  102 CONTINUE
      GOTO 199
  103 CONTINUE
      GOTO 199
  104 CONTINUE
      GOTO 199
  105 CONTINUE
      IF(POSITN.GT.MXITXT)THEN
          CALL PUTSTR(MONFIL,'**text is too big')
          CALL PUTNL(MONFIL)
          CALL ENDRUN
      END IF
      READ(F5,REC=POSITN) VALUE
      GOTO 199
  177 CONTINUE
  199 CONTINUE
      END
      SUBROUTINE PUTINT(F,N,WIDTH)
      INTEGER F,N,WIDTH
      CHARACTER S(20)
      INTEGER P,T,DUMMY,X
      T=IABS(N)
      P=0
    1 CONTINUE
      P=P+1
      S(P)=CHAR( T-(T/10)*10 + ICHAR('0') )
      T=T/10
      IF(T.GT.0)GOTO 1
      IF(WIDTH.EQ.0)THEN
          IF(N.LT.0) THEN
              P=P+1
              S(P)='-'
          END IF
      ELSE
          T=IABS(WIDTH)
          IF(N.LT.0) THEN
              P=P+1
              S(P)='-'
          ELSE IF(WIDTH.GT.0) THEN
              P=P+1
              S(P)='+'
          ELSE
              P=P+1
              S(P)=' '
          END IF
          X=T-P
          DO 10 DUMMY=1,X
              P=P+1
              S(P)=' '
   10     CONTINUE
          IF(P.GT.T .AND. N.GE.0)P=P-1
          IF(P.GT.T)THEN
              DO 20 P=1,T
                  S(P)='*'
   20         CONTINUE
          END IF
      END IF
      DO 30 DUMMY=1,P
          CALL PUTCH1(F,S(P-DUMMY+1))
   30 CONTINUE
      END
      SUBROUTINE PUTSTR(F,S)
      INTEGER F
      CHARACTER S*(*)
      INTEGER I
      DO 10 I=1,LEN(S)
          CALL PUTCH1(F,S(I:I))
   10 CONTINUE
      END
      SUBROUTINE PUTCH1(F,C)
      INCLUDE 'global.f'
      INTEGER F
      CHARACTER C
      GOTO (101,102,103,104),F
  101 CONTINUE
      IF(C.EQ.XNLINE)THEN
          WRITE(MON,1) (MONBUF(I),I=1,KMON-1)
          KMON=1
      ELSE IF (C.EQ.XNPAGE)THEN
          WRITE(MON,1) (MONBUF(I),I=1,KMON-1)
          KMON=1
          WRITE(MON,2)
    2     FORMAT(1H1)
      ELSE
          IF(KMON.GT.LPWDTH)THEN
              WRITE(MON,1) (MONBUF(I),I=1,LPWDTH)
    1         FORMAT(1X,160A1)
              KMON=1
          END IF
          MONBUF(KMON)=C
          KMON=KMON+1
      END IF
      GOTO 199
  102 CONTINUE
      GOTO 199
  103 CONTINUE
      IF(C.EQ.XNLINE)THEN
          WRITE(RES,1) (RESBUF(I),I=1,KRES-1)
          KRES=1
      ELSE IF (C.EQ.XNPAGE)THEN
          WRITE(RES,1) (RESBUF(I),I=1,KRES-1)
          KRES=1
          WRITE(RES,2)
      ELSE
          IF(KRES.GT.LPWDTH)THEN
              WRITE(RES,1) (RESBUF(I),I=1,LPWDTH)
              KRES=1
          END IF
          RESBUF(KRES)=C
          KRES=KRES+1
      END IF
      GOTO 199
  104 CONTINUE
      GOTO 199
  199 CONTINUE
      END
      INTEGER FUNCTION CHRNUM(V)
      INCLUDE 'global.f'
      INTEGER V
      GOTO (101,102,103,104) ,V
  101 CONTINUE
      CHRNUM=KMON
      RETURN
  102 CONTINUE
      CHRNUM=1
      RETURN
  103 CONTINUE
      CHRNUM=KRES
      RETURN
  104 CONTINUE
      CHRNUM=1
      RETURN
      END
      SUBROUTINE STCNUM(V,N)
      INCLUDE 'global.f'
      INTEGER V,N
      GOTO (101,102,103,104),V
  101 CONTINUE
    1 IF(KMON.LT.N)THEN
          CALL PUTCH1(V,' ')
          GOTO 1
      END IF
      KMON=N
      GOTO 199
  102 CONTINUE
      GOTO 199
  103 CONTINUE
    2 IF(KRES.LT.N)THEN
          CALL PUTCH1(V,' ')
          GOTO 2
      END IF
      KRES=N
      GOTO 199
  104 CONTINUE
      GOTO 199
  199 CONTINUE
      END
      SUBROUTINE SLICE(N,A,IA,B,IB,K)
      INTEGER N,A(0:K),IA,B(0:K),IB,K
      DO 10 I=0,K-1
          A(IA+I)=B(IB+I)
   10 CONTINUE
      END
      LOGICAL FUNCTION STREQ(A,B,N)
      CHARACTER A(80), B*(*)
      INTEGER N
      INTEGER I
      STREQ=.FALSE.
      DO 10 I=1,N
         IF(A(I).NE.B(I:I)) RETURN
   10 CONTINUE
      STREQ=.TRUE.
      END
      LOGICAL FUNCTION STRNE(A,B,N)
      CHARACTER A(80), B*(*)
      INTEGER N
      LOGICAL STREQ
      STRNE=.NOT.STREQ(A,B,N)
      END

      SUBROUTINE UNDERL(V,C)
      INCLUDE 'global.f'
      INTEGER V
      CHARACTER C
      INTEGER I
      INTEGER DUMMY
      I=CHRNUM(V)-1
      CALL PUTNL(V)
      DO 1 DUMMY =1, I
          CALL PUTCH1(V,C)
    1 CONTINUE
      CALL PUTNL(V)
      END
      SUBROUTINE PRDAYT(F)
      INCLUDE 'global.f'
      INTEGER F
      CHARACTER *100 DT
      DT=' '
      CALL DAYTIM(DT)
      CALL PUTSTR(F,DT(1:DTSIZE))
      END
      SUBROUTINE ADVERT
      INCLUDE 'global.f'
      CALL PUTSTR(MONFIL,'CLOC mark ')
      CALL PUTSTR(MONFIL,MARK)
      CALL PUTSTR(MONFIL,' run on ')
      CALL PRDAYT(MONFIL)
      CALL PUTSTR(MONFIL,' (Copyright(C) Alan Reed,1984)')
      CALL PUTNL(MONFIL)
      END
      SUBROUTINE STRTUP
      INCLUDE 'global.f'
      CALL PUTNL(RESULT)
      CALL PUTSTR(RESULT,'output from CLOC mark ')
      CALL PUTSTR(RESULT,MARK)
      CALL PUTSTR(RESULT,' run on ')
      CALL PRDAYT(RESULT)
      CALL UNDERL(RESULT,'-')
      END
      SUBROUTINE CLOSED
      INCLUDE 'global.f'
      CALL PUTSTR(RESULT,'end of output from CLOC mark ')
      CALL PUTSTR(RESULT,MARK)
      CALL PUTSTR(RESULT,' run on ')
      CALL PRDAYT(RESULT)
      CALL UNDERL(RESULT,'-')
      END
      SUBROUTINE MARGIN
      INCLUDE 'global.f'
C PUTS A MARGIN OF LENGTH  GAP  ONTO MONFIL CHANNEL
      INTEGER DUMMY
      DO 1 DUMMY =1,  GAP-1
          CALL PUTCH1(MONFIL,' ')
    1 CONTINUE
      END
      LOGICAL FUNCTION EQUAL(A,A1,A2,B,B1,B2)
      INCLUDE 'global.f'
      INTEGER A(0:UPBSYM)
      INTEGER A1,A2
      INTEGER B(0:MXWORD+6)
      INTEGER B1,B2
      INTEGER I
      INTEGER J
      LOGICAL EQ
      INTEGER DUMMY
      I=A1
      J=B1
      EQ=(A2-A1 .EQ. B2-B1)
      DO 1 DUMMY=1, A2-A1+1
          IF(.NOT.EQ )GOTO 91
          EQ=(A(I).EQ.B(J))
          IF( EQ )THEN
              I=I+1
              J=J+1
          END IF
    1 CONTINUE
   91 CONTINUE
      EQUAL=EQ
      END
C ERROR SEGMENT
      SUBROUTINE ERRMES(E,I)
      INCLUDE 'global.f'
      INTEGER E,I
      IF(  I.LT.1  .OR.  I.GT.26  )THEN
          CALL PUTSTR(E,'system error ')
          CALL PUTINT(E,-I,0)
      ELSE
          GOTO (101,102,103,104,105,106,107,108,109,110,111,112,113,114,
     +          115,116,117,118,119,120,121,122,123,124,125,126),I
  101 CONTINUE
      CALL PUTSTR(E,'incorrect control statement')
      GOTO 199
  102 CONTINUE
      CALL PUTSTR(E,'control statement ends prematurely')
      GOTO 199
  103 CONTINUE
      CALL PUTSTR(E,'missing mandatory control statement')
      GOTO 199
  104 CONTINUE
      CALL PUTSTR(E,'unknown symbol')
      GOTO 199
  105 CONTINUE
      CALL PUTSTR(E,'symbol not allowed in this context')
      GOTO 199
  106 CONTINUE
      CALL PUTSTR(E,'number is too large')
      GOTO 199
  107 CONTINUE
      CALL PUTSTR(E,'upper value does not exceed lower')
      GOTO 199
  108 CONTINUE
      CALL PUTSTR(E,'no words found')
      GOTO 199
  109 CONTINUE
      CALL PUTSTR(E,'character already defined')
      GOTO 199
  110 CONTINUE
      CALL PUTSTR(E,'no letters provided')
      GOTO 199
  111 CONTINUE
      CALL PUTSTR(E,'file not produced by CLOC mark ')
      CALL PUTSTR(E,MARK)
      GOTO 199
  112 CONTINUE
      CALL PUTSTR(E,'capacity exceeded')
      GOTO 199
  113 CONTINUE
      CALL PUTSTR(E,'number of references exceeds ')
      CALL PUTINT(E,MXREFS,0)
      GOTO 199
  114 CONTINUE
      CALL PUTSTR(E,'no word selection statements provided')
      GOTO 199
  115 CONTINUE
      CALL PUTSTR(E,'head word not found')
      GOTO 199
  116 CONTINUE
      CALL PUTSTR(E,'number expected at this position')
      GOTO 199
  117 CONTINUE
      CALL PUTSTR(E,'a number cannot be placed here')
      GOTO 199
  118 CONTINUE
      CALL PUTSTR(E,'zero number not permitted')
      GOTO 199
  119 CONTINUE
      CALL PUTSTR(E,'space not allowed here')
      GOTO 199
  120 CONTINUE
      CALL PUTSTR(E,'equates not correctly paired')
      GOTO 199
  121 CONTINUE
      CALL PUTSTR(E,'letter not allowed here')
      GOTO 199
  122 CONTINUE
      CALL PUTSTR(E,'character not on *letters statement')
      GOTO 199
  123 CONTINUE
      CALL PUTSTR(E,'character ignored')
      GOTO 199
  124 CONTINUE
      CALL PUTSTR(E,'word(s) not found')
      GOTO 199
  125 CONTINUE
      CALL PUTSTR(E,'no words match this pattern')
      GOTO 199
  126 CONTINUE
      CALL PUTSTR(E,'there are no references in the text')
      GOTO 199
  199 CONTINUE
      CALL PUTNL(E)
      END IF
      END
      SUBROUTINE ERROR(I)
      INCLUDE 'global.f'
      INTEGER  I
      INTEGER  J
      CHARACTER *9 D1TO9
      DATA D1TO9/'123456789'/
      ERRNUM=ERRNUM+1
      ERRVAL(ERRNUM)=I
      J=1
    1 IF(  ERRVAL(J).NE.I  )THEN
          J =J+ 1
          GOTO 1
      END IF
      IF( J.NE.ERRNUM )THEN
          ERRNUM =ERRNUM- 1
      END IF
      ERRVEC(CNTRLP-1)= D1TO9(J:J)
      IF(  ERRNUM.GT.9  )THEN
          ERRNUM=9
          ERRVAL(10)=1
          ERRVEC(CNTRLP-1)= '?'
      END IF
      END
      SUBROUTINE PRERRS
      INCLUDE 'global.f'
      INTEGER I,J,K
      IF(  ERRNUM.GT.0  )THEN
          CALL MARGIN
          I=MXWDTH+1
          DO 99 K=MXWDTH+1,0,-1
              IF(ERRVEC(K).NE.' ')GOTO 999
              I=I-1
   99     CONTINUE
  999     CONTINUE
          DO 1 J=0,I
              CALL PUTCH1(ERRFIL,ERRVEC(J))
    1     CONTINUE
          CALL PUTNL(ERRFIL)
          CALL PUTSTR(ERRFIL,'error(s)')
          DO 2   I  =1,  ERRNUM
              CALL STCNUM(ERRFIL,10)
              CALL PUTINT(ERRFIL,I,-2)
              CALL PUTSTR(ERRFIL,': ')
              CALL ERRMES(ERRFIL,ERRVAL(I))
    2     CONTINUE
          ERRNUM=0
          CALL CLRSTR(ERRVEC,MXWDTH+1)
      END IF
      END
      SUBROUTINE NXLINE
      INCLUDE 'global.f'
      INTEGER I
      LOGICAL COND
      CALL GTLINE(DAT,LIST,WIDTH,EOTEXT)
C VALID CHARACTERS ARE IN LIST(0) LIST(1) ... LIST(WIDTH-1)
      IF(WIDTH.GT.INWDTH)THEN
          DO 10 I=INWDTH-1,0,-1
              IF(LIST(I).NE.' ')GOTO 11
   10     CONTINUE
          I=(-1)
   11     WIDTH=I+1
      END IF
C 0<=WIDTH<=INWDTH
C AT THIS POINT PROVIDED THAT WIDTH GT 0
C WE KNOW THAT  LIST(WIDTH-1) IS A NON SPACE CHARACTER
      IF(WIDTH.LT.INWDTH .AND. RUNOVR)THEN
          LIST(WIDTH)=' '
          WIDTH=WIDTH+1
      END IF
      IF(WIDTH.GT.0 .AND. HYPHEN.NE.' ')THEN
          COND= LIST(WIDTH-1).EQ.HYPHEN
      ELSE
          COND= .FALSE.
      ENDIF
      IF(  RUNOVR  )THEN
          IF(.NOT.BOREFS .OR. .NOT. WHSPCE)THEN
              LNUMB=LNUMB+1
          ENDIF
      ELSE IF(COND)THEN
          WIDTH=WIDTH-1
          IF(.NOT.BOREFS .OR. .NOT. WHSPCE)THEN
              LNUMB=LNUMB+1
          ENDIF
      ELSE
          LIST(WIDTH)=EOLIN
          WIDTH=WIDTH+1
          IF( EOLIN.EQ.' ' )THEN
              IF(.NOT.BOREFS .OR. .NOT. WHSPCE)THEN
                  LNUMB=LNUMB+1
              ENDIF
          END IF
      END IF
      IF(  SW1 .AND. .NOT. EOTEXT  )THEN
          CALL STCNUM(ERRFIL,14)
          CALL PUTINT(ERRFIL,LNUMB,-5)
          CALL PUTCH1(ERRFIL,' ')
          DO 20 I = 0 , WIDTH-1
              CALL PUTCH1(ERRFIL,LIST(I))
   20     CONTINUE
          CALL PUTNL(ERRFIL)
      END IF
      WHSPCE=.TRUE.
      END
C END OF ERROR SEGMENT
      SUBROUTINE PUTOUT(TAPE,ALIST,N)
      INCLUDE 'global.f'
      INTEGER TAPE
      INTEGER ALIST(0:N)
C COPIES THE RECORD  ALIST  TO TAPE
      IF(TAPE.LT.1 .OR. TAPE.GT.4)GOTO 177
      GOTO (101,102,103,104),TAPE
  101 CONTINUE
      WRITE(F1)(ALIST(I),I=1,N)
      GOTO 199
  102 CONTINUE
      WRITE(F2)(ALIST(I),I=1,N)
      GOTO 199
  103 CONTINUE
      WRITE(F3)(ALIST(I),I=1,N)
      GOTO 199
  104 CONTINUE
      WRITE(F4)(ALIST(I),I=1,N)
      GOTO 199
  177 CONTINUE
  199 CONTINUE
      END
      SUBROUTINE GETIN(TAPE,ALIST,N,NULREC)
      INCLUDE 'global.f'
      INTEGER TAPE
      INTEGER ALIST(0:N)
      LOGICAL NULREC
C READS A RECORD FROM TAPE INTO  LIST
      IF(TAPE.LT.1 .OR. TAPE.GT.4)GOTO 177
      GOTO (101,102,103,104),TAPE
  101 CONTINUE
      READ(F1)(ALIST(I),I=1,N)
      GOTO 199
  102 CONTINUE
      READ(F2)(ALIST(I),I=1,N)
      GOTO 199
  103 CONTINUE
      READ(F3)(ALIST(I),I=1,N)
      GOTO 199
  104 CONTINUE
      READ(F4)(ALIST(I),I=1,N)
      GOTO 199
  177 CONTINUE
  199 CONTINUE
      NULREC=(ALIST(1).LT.0)
      END
      SUBROUTINE REREAD(TAPCOD)
      INCLUDE 'global.f'
      INTEGER TAPCOD
      IF(TAPCOD.LT.1 .OR. TAPCOD.GT.4)GOTO 177
      GOTO (101,102,103,104),TAPCOD
  101 CONTINUE
      REWIND F1
      GOTO 199
  102 CONTINUE
      REWIND F2
      GOTO 199
  103 CONTINUE
      REWIND F3
      GOTO 199
  104 CONTINUE
      REWIND F4
      GOTO 199
  177 CONTINUE
  199 CONTINUE
      END
      SUBROUTINE REWRIT(TAPCOD)
      INCLUDE 'global.f'
      INTEGER TAPCOD
C      REWINDS TAPE NUMBER  TAPCOD.
      IF(TAPCOD.LT.1 .OR. TAPCOD.GT.4)GOTO 177
      GOTO (101,102,103,104),TAPCOD
  101 CONTINUE
      REWIND F1
      GOTO 199
  102 CONTINUE
      REWIND F2
      GOTO 199
  103 CONTINUE
      REWIND F3
      GOTO 199
  104 CONTINUE
      REWIND F4
      GOTO 199
  177 CONTINUE
  199 CONTINUE
      END
      LOGICAL FUNCTION  UPPLET(C)
      INCLUDE 'global.f'
      INTEGER C
       UPPLET=((UPPERA.LE.C .AND. C.LE.UPPERZ))
      END
      LOGICAL FUNCTION  LOWLET(C)
      INCLUDE 'global.f'
      INTEGER C
       LOWLET=((LOWERA.LE.C  .AND.  C.LE.LOWERZ))
      END
      SUBROUTINE FUPPER(C)
      INCLUDE 'global.f'
      CHARACTER C
      LOGICAL LOWLET
       IF(  LOWLET(ICHAR(C))  )THEN
          C=CHAR(ICHAR(C) -LOWERA+UPPERA)
      END IF
      END
      SUBROUTINE FLOWER(C)
      INCLUDE 'global.f'
      CHARACTER C
      LOGICAL UPPLET
       IF(  UPPLET( ICHAR(C)) )THEN
          C=CHAR(ICHAR(C) -UPPERA+LOWERA)
      END IF
      END
      SUBROUTINE ADJLET(C)
      INCLUDE 'global.f'
      CHARACTER C
       IF(  ISTATE.EQ.RAIIMP  )THEN
          CALL FUPPER(C)
      ELSE IF(  ISTATE.EQ.LOWINP  )THEN
          CALL FLOWER(C)
      END IF
      END
      SUBROUTINE DMPREF(REFS)
      INCLUDE 'global.f'
      INTEGER REFS(0:RMAX+1,0:27)
      INTEGER I,J
      CALL PUTSTR(MONFIL,'references')
      CALL PUTNL(MONFIL)
      DO 1   I  =  1  ,  26
          IF(  REFS(0,I).NE.0  )THEN
              CALL PUTCH1(MONFIL,CHAR(UPPERA-1+I))
              CALL PUTCH1(MONFIL,' ')
              DO 2   J  =1,  REFS(0,I)
                  CALL PUTCH1(MONFIL, CHAR( REFS(J,I)) )
    2         CONTINUE
              CALL PUTNL(MONFIL)
          END IF
    1 CONTINUE
      CALL PUTNL(MONFIL)
      END
      SUBROUTINE PREIN(AINWAR,STATE,CODE)
      INCLUDE 'global.f'
      INTEGER AINWAR(0:256)
      INTEGER STATE,CODE
      INTEGER I
      CALL SETTO(TYPE,256,0)
      TYPE(ICHAR(' '))=ZSEPAR
      TYPE(256)=0
      CALL SETTO(PRRANK,256,0)
      CALL SETTO(SECRNK,256,0)
      CALL SETTO(SPECAL,256,0)
      DO 1   I  =  0  ,  31
          AINWAR(I)= 256
    1 CONTINUE
      DO 2   I  =  32  ,  64
          AINWAR(I)=I
    2 CONTINUE
      IF(  STATE.EQ.LOWINP  )THEN
          DO 3   I  =  65  ,  90
              AINWAR(I)=I+32
C MAKE UPPERCASE LETTER LOWERCASE
              TYPE(I)=CODE
    3     CONTINUE
      ELSE
          DO 4   I  =  65  ,  90
              AINWAR(I)=I
    4     CONTINUE
      END IF
      DO 5   I  =  91  ,  96
          AINWAR(I)=I
    5 CONTINUE
      IF(  STATE.EQ.RAIIMP  )THEN
          DO 6   I  =  97  ,  122
              AINWAR(I)=I-32
C MAKE LOWERCASE LETTER UPPERCASE
            TYPE(I)=CODE
C PCALL RESET TYPE OF IMPOSSIBLE SYMBOL
    6     CONTINUE
      ELSE
          DO 7   I  =  97  ,  126
              AINWAR(I)=I
    7     CONTINUE
      END IF
      DO 8   I  =  127  ,  255
          AINWAR(I)= 256
    8 CONTINUE
      END
      SUBROUTINE ENDRUN
      INCLUDE 'global.f'
C     C GET RID OF THE FTEMP FILES, BEFORE ENDING THE RUN
      CALL DELF(TAPE1)
      CALL DELF(TAPE2)
      CALL DELF(TAPE3)
      CALL DELF(TAPE4)
      CALL DELF(ITEXT)
      CALL STOP
      END
      SUBROUTINE FAULT(X)
      INCLUDE 'global.f'
      CHARACTER X*(*)
C PUTS THE TEXT STRING  X  INTO THE CLOC MONFIL FILE
C      AND TERMINATES THE RUN
      CALL PUTNL(MONFIL)
      CALL PUTSTR(MONFIL,'fault   ')
      CALL PUTSTR(MONFIL,X)
      CALL PUTNL(MONFIL)
      CALL ENDRUN
      END
      SUBROUTINE GTCHBA
      INCLUDE 'global.f'
C ON EXIT
C      DCHAR  CONTAINS THE NEXT CHARACTER IN THE TEXT DATA
C      DTYPE  CONTAINS ITS CATAGORY TYPE
   71 CONTINUE
   72 CONTINUE
      IF( PTR.EQ.WIDTH-1 )THEN
          CALL NXLINE
          PTR=(-1)
          IF( EOTEXT )THEN
              GOTO 3
          END IF
      END IF
      PTR=PTR+1
C WE KNOW THAT   0<=PTR<=WIDTH-1
      DCHAR= ICHAR(LIST(PTR))
      IF( DCHAR.GT.255 ) GOTO 72
      DCHAR=INWARD(DCHAR)
      IF( DCHAR.EQ.256)  GOTO 71
      DTYPE=TYPE(DCHAR)
      GOTO 4
   3  DCHAR=ASPACE
      DTYPE=0
   4  CONTINUE
      RETURN
      END
      SUBROUTINE MYGTCH
      INCLUDE 'global.f'
C TAKES THE NEXT CHARACTER IN THE TEXT DATA  DCHAR  LOOKS AT ITS
C CATAGORY TYPE  DTYPE  AND PERFORMS SOME ACTIONS BASED ON THE TYPE.
C IT IS HERE THAT IGNORABLE CHARACTERS ARE REMOVED, COMMENTS DETECTED,
C SECTIONS OF TEXT ARE SKIPPED, AND TEXT REFERENCES ARE EXTRACTED.
C ON EXIT,  DCHAR  CONTAINS THE NEXT USEFULL CHARACTER IN THE TEXT DATA
C AND  DTYPE  CONTAINS ITS CATAGORY TYPE
      LOGICAL UPPLET,LOWLET
   99 CALL GTCHBA
      IF(DTYPE.LT.1 .OR. DTYPE.GT.12)GOTO 177
      GOTO (101,102,103,104,105,106,107,108,109,110,111,112),DTYPE
  101 CONTINUE
C LETTER
      WHSPCE=.FALSE.
      GOTO 199
  102 CONTINUE
C SEPARATOR
      CONTINUE
      GOTO 199
  103 CONTINUE
C IGNOREABLE
      GOTO  99
C     GOTO 199
  104 CONTINUE
C READ AS SPACE
      DCHAR=ASPACE
      DTYPE=ZSEPAR
      GOTO 199
  105 CONTINUE
C START OF SKIP SEQUENCE
      CALL GTCHBA
    1 IF(  .NOT. EOTEXT .AND. DTYPE.NE.ZSKEND )THEN
          CALL GTCHBA
          GOTO 1
      END IF
      IF(  .NOT. EOTEXT  )THEN
          DCHAR=ASPACE
          DTYPE=ZSEPAR
      END IF
      GOTO 199
  106 CONTINUE
CSKIPEND FOUND TOO EARLY
      DTYPE=ZSEPAR
      GOTO 199
  107 CONTINUE
C OPEN COMMENT SYMBOL  ((
      IF(TYPE(INWARD(ICHAR(LIST(PTR+1)))).EQ.ZOPENS)THEN
          COFLAG= .TRUE.
      END IF
      DTYPE=ZSEPAR
      GOTO 199
  108 CONTINUE
C CLOSE COMMENT SYMBOL ))
      IF(TYPE(INWARD(ICHAR(LIST(PTR+1)))).EQ.ZOPEND  )THEN
          COFLAG= .FALSE.
      END IF
      DTYPE=ZSEPAR
      GOTO 199
  109 CONTINUE
C  REFS START SYMBOL <
      CALL GTCHBA
      C=DCHAR
      IF( LOWLET(C) )THEN
          C=C-LOWERA+UPPERA
      END IF
      IF(  C.EQ.ICHAR( 'L' ) )THEN
          VALUE=0
          CALL GTCHBA
    2     IF( DCHAR.EQ.ICHAR(' ') .AND. .NOT. EOTEXT )THEN
              CALL GTCHBA
              GOTO 2
          END IF
          D=DCHAR-ICHAR('0')
    3     IF( .NOT. EOTEXT .AND. D.GE.0 .AND. D.LT.10   )THEN
              VALUE=10  *     VALUE+D
              CALL GTCHBA
              D=DCHAR-ICHAR('0')
              GOTO 3
          END IF
          C=C-ICHAR('A')+1
    4     IF(.NOT. EOTEXT .AND. DTYPE.NE.ZRFEND)THEN
              CALL GTCHBA
              GOTO 4
          END IF
          REFERS(0,C)=1
          REFERS(1,C)=VALUE-LNUMB
          NREFER=NREFER+1
          LREFER(NREFER)=NXTITX
          PREFER(NREFER)=C
C         CALL SLICE(I,TREFER(1,NREFER),0,REFERS(1,C),0,RMAX)
          DO 661 I=0,RMAX-1
              TREFER(I,NREFER)=REFERS(I,C)
  661     CONTINUE
      ELSE IF(  UPPLET(C) )THEN
          I=0
          C=C-ICHAR('A')+1
          CALL GTCHBA
    5     IF(  DCHAR.EQ.ASPACE  )THEN
              CALL GTCHBA
              GOTO 5
          END IF
    6     IF(  .NOT. EOTEXT  .AND.  DTYPE.NE.ZRFEND  )THEN
              IF(  I.LT.RMAX  )THEN
                  I=I+1
                  REFERS(I,C)=DCHAR
              END IF
              CALL GTCHBA
              GOTO 6
          END IF
          REFERS(0,C)=I
          DO 7  J = I+1 , RMAX
              REFERS(J,C)=ICHAR(' ')
    7     CONTINUE
          NREFER=NREFER+1
          LREFER(NREFER)=NXTITX
          PREFER(NREFER)=C
C         CALL SLICE(I,TREFER(1,NREFER),0,REFERS(1,C),0,RMAX)
          DO 662 I=0,RMAX-1
              TREFER(I,NREFER)=REFERS(I,C)
  662     CONTINUE
      ELSE
    8     IF(  .NOT. EOTEXT  .AND.  DTYPE.NE.ZRFEND  )THEN
              CALL GTCHBA
              GOTO 8
          END IF
      END IF
      IF(   .NOT. EOTEXT  )THEN
          DCHAR=ASPACE
          DTYPE=ZSEPAR
      END IF
      GOTO 199
  110 CONTINUE
C END OF REFS FOUND TOO EARLY
      DTYPE=ZSEPAR
      GOTO 199
  111 CONTINUE
C  CONTINUATION SYMBOL + FOUND
      IF(   .NOT. COFLAG  )THEN
          CALL NXLINE
          PTR=(-1)
          GOTO 99
      END IF
      GOTO 199
  112 CONTINUE
C  NEWLINE SYMBOL / FOUND
      IF(   .NOT. COFLAG  )THEN
          LNUMB = LNUMB+1
      END IF
      DTYPE=ZSEPAR
      GOTO 199
  177 CONTINUE
      IF(   .NOT. EOTEXT  )THEN
          CALL FAULT('unexpected type')
      END IF
C FORCE END-OF-FILE TYPE
      DTYPE=0
  199 CONTINUE
      IF(  COFLAG  )THEN
          DTYPE=ZSEPAR
      END IF
      RETURN
C SOME COMPILERS SEEM TO NEED A  RETURN  HERE
      END
      SUBROUTINE ANXTIT(ITMBUF,ITMPTR,ITYPE)
      INCLUDE 'global.f'
      INTEGER ITMBUF(0:MXWORD)
      INTEGER ITMPTR
      INTEGER ITYPE
C ON EXIT
C      ITMBUF  CONTAINS A SERIES OF SYMBOLS REPRESENTING
C      EITHER A TEXT WORD OR A SEPARATOR.
C      ITMPTR CONTAINS THE NUMBER OF SYMBOLS IN THE TEXT WORD
C
C            OR SEPARATOR
C      ITYPE  IS  ZLETER  WHEN A TEXT WORD IS PRESENT
C      ZSEPAR  WHEN A SEPARATOR IS PRESENT
C      WHEN A TEXT WORD IS READ THE RUNNING WORD COUNT IS INCREASED BY 1
      ITYPE=DTYPE
      ITMPTR=1
      ITMBUF(1)=DCHAR
      CALL MYGTCH
    1 IF(  DTYPE.EQ.       ITYPE  )THEN
          IF(       ITMPTR.LT. MXWORD  )THEN
              ITMPTR=ITMPTR+1
              ITMBUF(       ITMPTR)=DCHAR
          END IF
          CALL MYGTCH
          GOTO 1
      END IF
      IF(       ITYPE.EQ.ZLETER  )THEN
          RUNWC=RUNWC+1
          IF(       ITMPTR.GT.MXLENG )THEN
              MXLENG=       ITMPTR
          END IF
      END IF
      END
      SUBROUTINE PRCONT(ASYMTB)
      INCLUDE 'global.f'
      INTEGER ASYMTB(0:UPBSYM+1)
      INTEGER I
      INTEGER DUMMY,J
C THIS ROUTINE WILL DUMP THE CONTENTS OF THE  ASYMTB
C ONTO THE CLOC MONFIL FILE. THE CONTENT OF THIS ROUTINE DEPENDS ON HOW
C THE SYMBOL TABLE IS DEFINED. AT PRESENT EACH ROW CONTAINS INFORMATION
C ASSOCIATED WITH A TEXTWORD (OR SEPARATOR) AND ITS SYMBOLIC VALUE.
      I=0
      CALL PUTSTR(MONFIL,'dump of symbol table''s ')
      CALL PUTINT(MONFIL,ITMCNT,0)
      CALL PUTSTR(MONFIL,' entries')
      CALL PUTNL(MONFIL)
      DO 1 DUMMY =1, ITMCNT
    2     IF( ASYMTB(I).EQ.0 )THEN
              I=I+ITSLOT
              GOTO 2
          END IF
          CALL PUTINT(MONFIL,I,-6)
          CALL PUTSTR(MONFIL,' :')
          DO 3   J  =  I  ,  I+ITSLOT-1
              CALL PUTINT(MONFIL,ASYMTB(J),-6)
              CALL PUTSTR(MONFIL,'  ')
    3     CONTINUE
          DO 4   J  =  ASYMTB(I)  ,  ASYMTB(I+1)
              CALL PUTCH1(MONFIL, CHAR( ASYMTB(J)) )
    4     CONTINUE
          CALL PUTNL(MONFIL)
          I=I+ITSLOT
    1 CONTINUE
      CALL PUTNL(MONFIL)
      END
      SUBROUTINE DUMPIT(AITEXT,N)
      INCLUDE 'global.f'
      INTEGER N
      INTEGER TEMP
      INTEGER I
C THIS ROUTINE WILL DUMP ONTO THE  MONFIL  FILE THE INTEGER
C      REPRESENTATION OF THE TEXT DATA THAT IS STORED ON THE FILE AITEXT
      CALL PUTSTR(MONFIL,'contents of itext')
      CALL PUTNL(MONFIL)
      DO 1   I  =1,  N
          CALL GETBN1(AITEXT,I,TEMP)
          CALL PUTINT(MONFIL,I,-5)
          CALL PUTCH1(MONFIL,':')
          CALL PUTINT(MONFIL,TEMP,-5)
          CALL PUTSTR(MONFIL,'  ')
          IF( MOD(I,2).EQ.1 )THEN
              CALL PENTRY(MONFIL,TEMP)
          END IF
          CALL PUTNL(MONFIL)
    1 CONTINUE
      CALL PUTNL(MONFIL)
      END
      SUBROUTINE PENTRY(TV,LOC)
      INCLUDE 'global.f'
      INTEGER TV,LOC
      INTEGER I
C THIS ROUTINE WILL PRINT (ON FILE  TV  ) THE FREQUENCY
C      OF OCCURRENCE OF THE TEXT WORD AND ITS SYMBOLIC FORM.
C     C PRINT THE FREQUENCY OF OCCURRENCE
      CALL PUTINT(TV,ABS(SYMTAB(LOC+2)),-5)
      CALL PUTCH1(TV,' ')
C     C PRINT THE ITEM SYMBOLICALLY
      DO 1   I  =  SYMTAB(LOC)  ,  SYMTAB(LOC+1)
          CALL PUTCH1(TV,  CHAR(SYMTAB(I)) )
    1 CONTINUE
      END
      SUBROUTINE SETSYM
      INCLUDE 'global.f'
C INITIALISES THE SYMTAB HOWEVER YOU CARE TO DEFINE IT
      CALL SETTO(SYMTAB,UPBSYM,0)
      PTRTOP=ITSLOT   *    ATBSZ1
      TABBOT=UPBSYM
      END
      SUBROUTINE SETTST
      INCLUDE 'global.f'
C INITIALISES THE TEXT DATA STRUCTURE HOWEVER IT IS DEFINED
      NXTITX=1
      END
      INTEGER FUNCTION HASH(ITMBUF,ITMPTR)
      INCLUDE 'global.f'
      INTEGER ITMBUF(0:MXWORD)
      INTEGER ITMPTR
      INTEGER N,TEMP
      INTEGER I
C PRIMARY HASHING FUNCTION. THIS WILL PRODUCES A NUMBER IN
C      THE RANGE   0 .LE. HASH .LE. TABSIZ WHICH WILL (HOPEFULLY)
C      BE UNIQUE
      N=ITMBUF(1)
      IF( ITMPTR.GT.4 )THEN
          TEMP=4
       ELSE
          TEMP=ITMPTR
      END IF
      DO 1  I = 2 , TEMP
          N=N+ITMBUF(I)
    1 CONTINUE
      HASH=MOD(N , ATBSZ1)
      END
      SUBROUTINE APTMAP(ASTRAT,VAR,DUM,FROM,NF,TO,NT,REPLY)
      INCLUDE 'global.f'
      INTEGER ASTRAT,VAR,DUM
      INTEGER FROM(0:MXWORD)
      INTEGER NF
      INTEGER TO(0:MXWORD)
      INTEGER NT
      INTEGER REPLY
      INTEGER I
      LOGICAL UPPLET,LOWLET
      LOGICAL LOWERD,RAISED
      IF(ASTRAT.LT.1 .OR. ASTRAT.GT.6)GOTO 177
      GOTO (101,102,103,104,105,106),ASTRAT
  101 CONTINUE
C UNCHANGED
      NT=NF
C     CALL SLICE(I,TO,1,FROM,1,       NT)
      DO 661 I=0,NT-1
          TO(1+I)=FROM(1+I)
  661 CONTINUE
      REPLY=0
      GOTO 199
  102 CONTINUE
C REMOVE PADDING LETTERS
      NT=0
      DO 1  I =1, NF
          C=FROM(I)
          IF( C.EQ.VAR .OR. C.EQ.DUM .OR. SPECAL(C).NE.ZPADD )THEN
              NT=NT+1
              TO(NT)=C
          END IF
    1 CONTINUE
      IF(       NT.EQ.0 )THEN
C THE PATTERN CONTAINS PADDING LETTERS ONLY
      REPLY=2
      NT=NF
C     CALL SLICE(I,TO,1,FROM,1,       NT)
      DO 662 I=1,NT-1
          TO(1+I)=FROM(1+I)
  662 CONTINUE
      ELSE IF(       NT.LT.NF )THEN
C SOME PADDING LETTERS HAVE GONE
          REPLY=1
      ELSE
          REPLY=0
C THERE HAVE BEEN NO CHANGES
      END IF
      GOTO 199
  103 CONTINUE
C REMOVE PADDING LETTERS AND FORCE TO LOWERCASE
      LOWERD=.FALSE.
      NT=0
      DO 2  I =1, NF
          C=FROM(I)
          IF( C.EQ.VAR .OR. C.EQ.DUM .OR. SPECAL(C).NE.ZPADD )THEN
              IF( C.NE.VAR .AND. C.NE.DUM .AND. UPPLET(C) )THEN
                  LOWERD=.TRUE.
                  C=C-UPPERA+LOWERA
              END IF
              NT=NT+1
              TO(NT)=C
          END IF
    2 CONTINUE
      IF(       NT.EQ.0 )THEN
C THE PATTERN CONTAINS PADDING CHARACTERS ONLY
          REPLY=2
          NT=NF
C         CALL SLICE(I,TO,1,FROM,1,       NT)
          DO 663 I=0,NT-1
              TO(1+I)=FROM(1+I)
  663     CONTINUE
      ELSE IF(       NT.LT.NF )THEN
C SOME PADDING LETTERS HAVE GONE
      REPLY=1
      ELSE IF( LOWERD )THEN
C LOWERING HAS TAKEN PLACE
          REPLY=1
      ELSE
          REPLY=0
C THERE HAVE BEEN NO CHANGES
      END IF
      GOTO 199
  104 CONTINUE
C REMOVE PADDING LETTERS AND FORCE TO UPPERCASE
      RAISED=.FALSE.
      NT=0
      DO 3  I =1, NF
          C=FROM(I)
          IF( C.EQ.VAR .OR. C.EQ.DUM .OR. SPECAL(C).NE.ZPADD )THEN
              IF( C.NE.VAR .AND. C.NE.DUM .AND. LOWLET(C) )THEN
                  RAISED=.TRUE.
                  C=C-LOWERA+UPPERA
              END IF
              NT=NT+1
              TO(NT)=C
          END IF
    3 CONTINUE
      IF(       NT.EQ.0 )THEN
C THE PATTERN CONTAINS PADDING CHARACTERS ONLY
          REPLY=2
          NT=NF
C         CALL SLICE(I,TO,1,FROM,1,       NT)
          DO 664 I=0,NT-1
              TO(1+I)=FROM(1+I)
  664     CONTINUE
      ELSE IF(       NT.LT.NF )THEN
C SOME PADDING LETTERS HAVE GONE
          REPLY=1
      ELSE IF( RAISED )THEN
C RAISING HAS TAKEN PLACE
          REPLY=1
      ELSE
          REPLY=0
C THERE HAVE BEEN NO CHANGES
      END IF
      GOTO 199
  105 CONTINUE
C  FORCE TO LOWER CASE, AND DO NOT REMOVE PADDING LETTERS
      LOWERD=.FALSE.
      NT=0
      DO 4  I =1, NF
          C=FROM(I)
          IF( C.NE.VAR .AND. C.NE.DUM .AND. UPPLET(C) )THEN
              LOWERD=.TRUE.
              C=C-UPPERA+LOWERA
          END IF
          NT=NT+1
          TO(NT)=C
    4 CONTINUE
      IF( LOWERD )THEN
C LOWERING HAS TAKEN PLACE
          REPLY=1
      ELSE
          REPLY=0
C THERE HAVE BEEN NO CHANGES
      END IF
      GOTO 199
  106 CONTINUE
C FORCE TO UPPER CASE, AND DO NOT REMOVE PADDING LETTERS
      RAISED=.FALSE.
      NT=0
      DO 5  I =1, NF
          C=FROM(I)
          IF( C.NE.VAR .AND. C.NE.DUM .AND. LOWLET(C) )THEN
              RAISED=.TRUE.
              C=C-LOWERA+UPPERA
          END IF
          NT=NT+1
          TO(NT)=C
    5 CONTINUE
      IF( RAISED )THEN
C RAISEING HAS TAKEN PLACE
          REPLY=1
      ELSE
          REPLY=0
C THERE HAVE BEEN NO CHANGES
      END IF
      GOTO 199
  177 CONTINUE
  199 CONTINUE
      END
      SUBROUTINE MAPPER(ASTRAT,FROM,NF,TO,NT,REPLY)
      INCLUDE 'global.f'
      INTEGER ASTRAT
      INTEGER FROM(0:MXWORD)
      INTEGER NF
      INTEGER TO(0:MXWORD)
      INTEGER NT
      INTEGER REPLY
      INTEGER I
      LOGICAL UPPLET,LOWLET
      LOGICAL LOWERD,RAISED
      IF(ASTRAT.LT.1 .AND. ASTRAT.GT.6)GOTO 177
      GOTO (101,102,103,104,105,106),ASTRAT
  101 CONTINUE
C UNCHANGED
      NT=NF
      CALL SLICE(I,TO,1,FROM,1,       NT)
      DO 661 I=0,NT-1
          TO(1+I)=FROM(1+I)
  661 CONTINUE
      REPLY=0
      GOTO 199
  102 CONTINUE
C REMOVE PADDING LETTERS
      NT=0
      DO 1  I =1, NF
          C=FROM(I)
          IF( SPECAL(C).NE.ZPADD )THEN
              NT=NT+1
              TO(NT)=C
          END IF
    1 CONTINUE
      IF(       NT.EQ.0 )THEN
C THE STRING CONTAINS PADDING LETTERS ONLY
          REPLY=2
          NT=NF
C         CALL SLICE(I,TO,1,FROM,1,       NT)
          DO 662 I=0,NT
              TO(1+I)=FROM(1+I)
  662     CONTINUE
      ELSE IF(       NT.LT.NF )THEN
C SOME PADDING LETTERS HAVE GONE
          REPLY=1
      ELSE
          REPLY=0
C THERE HAVE BEEN NO CHANGES
      END IF
      GOTO 199
  103 CONTINUE
C REMOVE PADDING LETTERS AND FORCE TO LOWERCASE
      LOWERD=.FALSE.
      NT=0
      DO 2  I =1, NF
          C=FROM(I)
          IF( SPECAL(C).NE.ZPADD )THEN
              IF( UPPLET(C) )THEN
                  LOWERD=.TRUE.
                  C=C-UPPERA+LOWERA
              END IF
              NT=NT+1
              TO(NT)=C
          END IF
    2 CONTINUE
      IF(       NT.EQ.0 )THEN
C THE STRING CONTAINS PADDING CHARACTERS ONLY
          REPLY=2
          NT=NF
C         CALL SLICE(I,TO,1,FROM,1,       NT)
          DO 663 I=0,NT-1
              TO(1+I)=FROM(1+I)
  663     CONTINUE
      ELSE IF(       NT.LT.NF )THEN
C SOME PADDING LETTERS HAVE GONE
          REPLY=1
      ELSE IF( LOWERD )THEN
C LOWERING HAS TAKEN PLACE
          REPLY=1
      ELSE
          REPLY=0
C THERE HAVE BEEN NO CHANGES
      END IF
      GOTO 199
  104 CONTINUE
C REMOVE PADDING LETTERS AND FORCE TO UPPERCASE
      RAISED=.FALSE.
      NT=0
      DO 3  I =1, NF
          C=FROM(I)
          IF( SPECAL(C).NE.ZPADD )THEN
              IF( LOWLET(C) )THEN
                  RAISED=.TRUE.
                  C=C-LOWERA+UPPERA
              END IF
       NT=NT+1
              TO(NT)=C
          END IF
    3 CONTINUE
      IF(       NT.EQ.0 )THEN
C THE STRING CONTAINS PADDING CHARACTERS ONLY
          REPLY=2
          NT=NF
C         CALL SLICE(I,TO,1,FROM,1,       NT)
          DO 664 I=0,NT-1
              TO(1+I)=FROM(1+I)
  664     CONTINUE
      ELSE IF(       NT.LT.NF )THEN
C SOME PADDING LETTERS HAVE GONE
         REPLY=1
      ELSE IF( RAISED )THEN
C RAISING HAS TAKEN PLACE
          REPLY=1
      ELSE
          REPLY=0
C THERE HAVE BEEN NO CHANGES
      END IF
      GOTO 199
  105 CONTINUE
C FORCE TO LOWER CASE, AND DO NOT REMOVE PADDING LETTERS
      LOWERD=.FALSE.
      NT=0
      DO 4  I =1, NF
          C=FROM(I)
          IF( UPPLET(C) )THEN
              LOWERD=.TRUE.
              C=C-UPPERA+LOWERA
          END IF
          NT=NT+1
          TO(NT)=C
    4 CONTINUE
      IF( LOWERD )THEN
C LOWERING HAS TAKEN PLACE
          REPLY=1
      ELSE
          REPLY=0
C THERE HAVE BEEN NO CHANGES
      END IF
      GOTO 199
  106 CONTINUE
C FORCE TO UPPER CASE, AND DO NOT REMOVE PADDING LETTERS
      RAISED=.FALSE.
      NT=0
      DO 5  I =1, NF
          C=FROM(I)
          IF( LOWLET(C) )THEN
              RAISED=.TRUE.
              C=C-LOWERA+UPPERA
          END IF
          NT=NT+1
          TO(NT)=C
    5 CONTINUE
      IF( RAISED )THEN
C RAISEING HAS TAKEN PLACE
          REPLY=1
      ELSE
          REPLY=0
C THERE HAVE BEEN NO CHANGES
      END IF
      GOTO 199
  177 CONTINUE
  199 CONTINUE
      END
      SUBROUTINE WRDLOC(WORD,ACOUNT,PRESNT,CODENU)
      INCLUDE 'global.f'
      INTEGER WORD(0:MXWORD)
      INTEGER ACOUNT
      LOGICAL PRESNT
      INTEGER CODENU
      LOGICAL EQUAL
C      LOCATES THE  WORD  IN THE SYMTAB (HOWEVER DEFINED) AND RETURNS
C      A UNIQUE INTEGER  CODENU  WHICH WE CAN USE TO ACCESS THE WORD.
C      ON EXIT  IN IS TRUE  IF WORD IS IN THE TABLE, FALSE OTHERWISE.
C      WHEN PRESNT IS TRUE, CODENU IS ITS LOCAT
C      IS FALSE, CODENU IS THE NEXT AVAILABLE LOCAT
      LOGICAL IN
      INTEGER START,END,R,H0,POSITN
      INTEGER ITMBUF(0: MXWORD+1)
      INTEGER ITMPTR
      INTEGER REPLY
      CALL MAPPER(STRAT,WORD,ACOUNT,ITMBUF,ITMPTR,REPLY)
C THE METHOD OF INSERTION IS HASHING WITH A RANDOM OFFSET
C      PLEASE REFER TO PAGES 797,798 AND 807,808
C      A.V. AHO,J.D.ULLMAN,'the theory of parsing,translation and
C      compiling'
C      VOLUME II:COMPILING
C      PUBLISHED BY PRENTICE HALL
      H0=HASH(ITMBUF,ITMPTR)
      POSITN=H0
C THE PRIMARY HASHED VALUE
      R=1
      IN=(SYMTAB(ITSLOT  *     POSITN).EQ.0)
    1 IF( .NOT. IN )THEN
          START=SYMTAB(ITSLOT *      POSITN)
          END=SYMTAB(ITSLOT   *    POSITN+1)
          IN=EQUAL(SYMTAB,START,END,ITMBUF,1,ITMPTR)
          IF( .NOT. IN )THEN
              R       =R*5
              R=MOD(R , BTBSZ4)
              POSITN=MOD((H0+(R/4)),ATBSZ1)
              IN=(SYMTAB(ITSLOT   *   POSITN).EQ.0)
          END IF
          GOTO 1
      END IF
      CODENU=ITSLOT  *     POSITN
      PRESNT=(SYMTAB(       CODENU+4).EQ.-1)
      END
      SUBROUTINE PUTIN(ITMBUF,ITMPTR,ITYPE,CODENU)
      INCLUDE 'global.f'
      INTEGER ITMBUF(0:MXWORD)
      INTEGER ITMPTR,ITYPE
      INTEGER CODENU
      LOGICAL IN
      LOGICAL EQUAL
C      PUTS THE ITEM INTO THE SYMTAB (HOWEVER DEFINED) AND RETURNS
C      A UNIQUE INTEGER  CODENU  WHICH WE CAN USE TO ACCESS THE ITEM
C THE METHOD OF INSERTION IS HASHING WITH A RANDOM OFFSET
C      PLEASE REFER TO PAGES 797,798 AND 807,808
C      A.V. AHO,J.D.ULLMAN,'the theory of parsing,translation and
C      compiling'
C      VOLUME II:COMPILING
C      PUBLISHED BY PRENTICE HALL
      H0=HASH(ITMBUF,ITMPTR)
      POSITN=H0
C THE PRIMARY HASHED VALUE
      R=1
      IN=(SYMTAB(ITSLOT  *     POSITN).EQ.0)
    2 IF( .NOT. IN )THEN
          START=SYMTAB(ITSLOT *      POSITN)
          END=SYMTAB(ITSLOT   *    POSITN+1)
          IN=EQUAL(SYMTAB,START,END,ITMBUF,1,ITMPTR)
          IF( .NOT. IN )THEN
              R       =R*5
              R=MOD(R , BTBSZ4)
              POSITN=MOD( (H0+(R/4)) , ATBSZ1)
              IN=(SYMTAB(ITSLOT   *    POSITN).EQ.0)
          END IF
          GOTO 2
      END IF
      CODENU=ITSLOT  *     POSITN
      IF( SYMTAB(       CODENU).EQ.0 )THEN
C THE ITEM IS NEW AND CAN BE STORED
C      IN THE TABLE
C FIRST WE DETERMINE IF THERE IS ROOM TO ADD IT
          IF( ITMCNT.GT.CTAB34 .OR.
     +    (TABBOT-ITMPTR+1).LE.PTRTOP )THEN
              CALL PUTSTR(MONFIL,
     +                  '** capacity of symtab exceeded')
              CALL PUTNL(MONFIL)
              CALL PUTSTR(MONFIL,'after ')
              CALL PUTINT(MONFIL,ITMCNT,0)
              CALL PUTSTR(MONFIL,' items(')
              CALL PUTINT(MONFIL,LNUMB,0)
              CALL PUTSTR(MONFIL,' lines)')
              CALL PUTNL(MONFIL)
              CALL ENDRUN
          END IF
          SYMTAB(       CODENU)=TABBOT-ITMPTR+1
          SYMTAB(       CODENU+1)=TABBOT
C   SYMTAB( CODENU+2)  WANT/NOWANT + FREQUENCY OF OCCURRENCE
C   SYMTAB( CODENU+3)  NOTREJ/REJECT + OCCURRENCE NUMBER
          IF( ITYPE.EQ.ZLETER )THEN
              SYMTAB(       CODENU+4)= -1
          ELSE
              SYMTAB(       CODENU+4)=0
          END IF
CMAPPED ITEM POINTER
C         CALL SLICE(I,SYMTAB,TABBOT-ITMPTR+1,
C    +    ITMBUF,1,ITMPTR)
          DO 665 I=0,ITMPTR-1
              SYMTAB(TABBOT-ITMPTR+1+I)=
     +                  ITMBUF(1+I)
  665     CONTINUE
          TABBOT=TABBOT-ITMPTR
          ITMCNT=ITMCNT+1
      END IF
      IF( ITYPE.EQ.ZLETER )THEN
          SYMTAB(CODENU+2)=SYMTAB(CODENU+2)+1
      END IF
      END
      SUBROUTINE ADDTXT(CODENU,VALUE)
      INCLUDE 'global.f'
      INTEGER CODENU,VALUE
C PUTS THE  VALUE  AND THE  CODENU  INTO THE FILE  ITEXT
C      AT THE NEXT SERIAL POSITION
      IF( NXTITX .GT. MXITXT )THEN
          CALL PUTSTR(MONFIL,'** capacity of itext is exceeded')
          CALL PUTNL(MONFIL)
          CALL ENDRUN
      END IF
      CALL PUTBN1(ITEXT,NXTITX,CODENU)
      CALL PUTBN1(ITEXT,NXTITX+1,VALUE)
      NXTITX=NXTITX+2
      END
      SUBROUTINE CHAINT
      INCLUDE 'global.f'
      INTEGER LOC,PREPOS
      REAL T,XCLOCK
      INTEGER I
C LINKS EVERY TEXT WORD WITH ITS NEXT OCCURRENCE.
C   ON EXIT, EACH (REPRESENTATION OF A) TEXT WORD IN THE  ITEXT  FILEA
C   IS FOLLOWED BY THE ADDRESS IN  ITEXT  OF THE NEXT OCCURRENCE
C   OF THE SAME TEXT WORD
      T=XCLOCK(1)
      DO 1  I = NXTITX-4 ,3, (-4)
          CALL GETBN1(ITEXT,I,LOC)
          IF( SYMTAB(LOC+4) .GE. 0 )THEN
              LOC=SYMTAB(LOC+4)
          END IF
          LOC=LOC+3
C POSITION OF WORDS PREVIOUS OCCURRENCE
          PREPOS=SYMTAB(LOC)
C CURRENT POSITION IS  I  IN ITEXT
          CALL PUTBN1(ITEXT,I+1,PREPOS)
C CHAIN CURRENT POSITION
C             TO PREVIOUS POSITION
          SYMTAB(LOC)=I
C MAKE PREVIOUS POSITION CURRENT
    1 CONTINUE
      T=XCLOCK(1)-T
      IF( SW5 )THEN
          CALL PUTSTR(MONFIL,'chaining time= ')
          CALL PTFLOT(MONFIL,T,15,8,2)
          CALL PUTSTR(MONFIL,' secs')
          CALL PUTNL(MONFIL)
      END IF
      END
C LAST UPDATED 26 MAY 1982
      SUBROUTINE RDTXT3
      INCLUDE 'global.f'
      INTEGER ITMBUF(0:MXWORD+1),ITMPTR,ITYPE,CODENU
      INTEGER HEADBF(0:MXWORD+1)
      REAL T,XCLOCK
      INTEGER I
C THIS ROUTINE READS TEXT DATA SERIALLY AS 'separator' 'word' PAIRS.
C   AN INTEGER REPRESENTATION OF THE TEXT DATA WILL BE STORED ON THE
C   FILE  ITEXT  . THIS ROUTINE ENSURES THAT THE TEXT DATA WILL ALLWAYS
C   START AND END WITH A 'separator'.
      CALL SETSYM
      CALL SETTST
      T=XCLOCK(1)
   71 CONTINUE
      CALL ANXTIT(ITMBUF,ITMPTR,ITYPE)
      CALL PUTIN(ITMBUF,ITMPTR,ITYPE,CODENU)
      IF( ITYPE.EQ.ZLETER )THEN
C DO THE MAPPING
          CALL MAPPER(STRAT,ITMBUF,ITMPTR,
     +                HEADBF,J,REPLY)
          IF(REPLY.LT.1 .OR. REPLY.GT.2)GOTO 177
          GOTO (101,102),REPLY
  101 CONTINUE
C  A MAPPING HAS TAKEN PLACE
      CALL PUTIN(HEADBF,J,ITYPE,N)
      SYMTAB(CODENU+4)=N
      GOTO 199
  102 CONTINUE
C  HEADBF CONTAINS PADDING LETTERS ONLY
      SYMTAB(CODENU+4)= -2
      GOTO 199
  177 CONTINUE
C THERE HAVE BEEN NO CHANGES
  199 CONTINUE
      END IF
      IF( ITYPE.EQ.ZLETER )THEN
          CALL ADDTXT(CODENU,0)
      ELSE
          CALL ADDTXT(CODENU,LNUMB)
      END IF
        IF( .NOT. EOTEXT)GOTO 71
      IF( ITYPE.EQ.ZLETER )THEN
C FORCE A SPACE AT FILE END
          ITYPE=ZSEPAR
          ITMBUF(1)=ASPACE
          ITMPTR=1
          CALL PUTIN(ITMBUF,ITMPTR,ITYPE,CODENU)
          CALL ADDTXT(CODENU,LNUMB)
      END IF
      DISTWC=0
      DO 1  I = 0 , PTRTOP-1,ITSLOT
          IF( SYMTAB(I+4).EQ.-1 )THEN
              DISTWC=DISTWC+1
          END IF
    1 CONTINUE
      T=XCLOCK(1)-T
      IF( SW5 )THEN
          CALL PUTSTR(MONFIL,'text reading time=')
          CALL PTFLOT(MONFIL,T,15,8,2)
          CALL PUTSTR(MONFIL,' secs')
          CALL PUTNL(MONFIL)
      END IF
      CALL CHAINT
      END
      SUBROUTINE SETFLD(LOC,I)
      INCLUDE 'global.f'
      INTEGER LOC,I
C WHEN I.EQ.1 , SET WANT/NOT WANT FIELD
C      I.EQ.2 , UNSET DITTO
C      I.EQ.3 , SET REJECT/NOT REJECT FIELD
C      I.EQ.4 , UNSET DITTO
      IF(I.LT.1 .OR. I.GT.4)GOTO 177
      GOTO (101,102,103,104),I
  101 CONTINUE
      SYMTAB(LOC+2)= -ABS(SYMTAB(LOC+2))
      GOTO 199
  102 CONTINUE
      SYMTAB(LOC+2)= ABS(SYMTAB(LOC+2))
      GOTO 199
  103 CONTINUE
      SYMTAB(LOC+3)= -ABS(SYMTAB(LOC+3))
      GOTO 199
  104 CONTINUE
      SYMTAB(LOC+3)= ABS(SYMTAB(LOC+3))
      GOTO 199
  177 CONTINUE
  199 CONTINUE
      END
      SUBROUTINE GTWORD(WORD,OK,METHOD)
      INCLUDE 'global.f'
      INTEGER WORD(0:MXWORD+6)
      LOGICAL OK
      INTEGER METHOD
      INTEGER  A,B,LENGTH
      INTEGER I,DUMMY
C THIS ROUTINE GETS A WORD FROM THE PREVIOUSLY
C      CONSTRUCTED SYMBOL TABLE.
C      WORD(1)   WILL CONTAIN THE ADDRESS OF WORD IN SYMTAB
C      WORD(2)   WILL CONTAIN THE FREQUENCY OF OCCURRENCE
C      WORD(3)   WILL CONTAIN THE LENGTH IN CHARACTERS OF
C      THE SPECAL LETTERS
C      WORD(4)   WILL CONTAIN THE START ADDRESS IN  WORD  WHERE
C      THE SPECAL LETTERS BEGIN
C      WORD(5)   WILL CONTAIN THE LENGTH IN CHARACTERS OF
C      THE TEXT WORD WHICH FOLLOWS.
C      WORD(6:5+WORD(5)) WILL CONTAIN THE LETTERS IN THE TEXT WORD
C      EXCLUDING THE SPECAL LETTERS.
C      WORD(6+WORD(5):ONWARDS)  WILL CONTAIN THE SPECAL LETTERS.
C      ON EXIT OK WILL BE SET FALSE AND
C      THE LAST OBJECT TO BE RETURNED IS SUCH THAT:-
C      WORD(1:ONWARDS) IS ZERO
C      LOCAT   -   WHERE THE NEXT FREE ENTRY STARTS
C      COUNT    -   NUMBER OF ENTRIES LEFT
    1 IF(COUNT.GT.0 .AND. SYMTAB(LOCAT+2).GE.0)THEN
          COUNT=COUNT-1
          LOCAT=LOCAT+ITSLOT
          GOTO 1
      END IF
      IF(  COUNT.EQ.0  )THEN
          CALL SETTO(WORD,MXWORD+5,0)
      ELSE
          A=SYMTAB(LOCAT)
          B=SYMTAB(LOCAT+1)
          WORD(1)=LOCAT
          WORD(2)=IABS(SYMTAB(LOCAT+2))
          LENGTH=0
          DO 2  I = A , B
              IF( SPECAL(SYMTAB(I)).EQ.ZNORML )THEN
                  LENGTH=LENGTH+1
                  WORD(5+LENGTH)=SYMTAB(I)
              END IF
    2     CONTINUE
          WORD(5)=LENGTH
          LENGTH=0
          DO 3  I = A , B
              IF( SPECAL(SYMTAB(I)).NE.ZNORML )THEN
                  LENGTH=LENGTH+1
                  WORD(5+LENGTH+WORD(5))=SYMTAB(I)
              END IF
    3     CONTINUE
          WORD(3)=LENGTH
          IF( METHOD.EQ.SLAST )THEN
              I= ABS( SYMTAB(LOCAT+3))
              TEMP=ABS( SYMTAB(LOCAT+2) ) -1
              DO 4 DUMMY =1, TEMP
                  CALL GETBN1(ITEXT,I+1,I)
    4         CONTINUE
C NOW WE ZAP THE WORD(4) FIELD OF WORD, BECAUSE WE KNOW
C      WE ARE NOT GOING TO USE IT......
              WORD(4)=I
          END IF
      END IF
      OK=(COUNT.NE.0)
      COUNT=COUNT-1
      LOCAT=LOCAT+ITSLOT
      END
      SUBROUTINE PRITEM(TV,LOC)
      INCLUDE 'global.f'
      INTEGER TV,LOC
      INTEGER I
C THIS ROUTINE WILL PRINT (ON FILE  TV  ) THE TEXT WORD (OR SEPARATOR)
C      IN SYMBOLIC FORM, WHICH OCCURRS AT POSITION  LOC  IN THE SYMTAB
      DO 1   I  =  SYMTAB(LOC) , SYMTAB(LOC+1)
          CALL PUTCH1(TV,CHAR( SYMTAB(I)))
    1 CONTINUE
      END
      SUBROUTINE STATS
      INCLUDE 'global.f'
      INTEGER WRDDAT(0:MXWORD+6)
      INTEGER BINSIZ
      LOGICAL NULL,FAIL
      INTEGER FREQ, TFREQ, UPPER, CUMM
      INTEGER TAPOUT,TOTAL
      LOGICAL STREQ,CHECK
      BINSIZ=100
      SORTSZ=MXSORT
    1 CONTINUE
      IF(STREQ(CTLFLD,QPROFI,15))THEN
          IF( SW3 )THEN
              CALL PRCON
          ENDIF
          CALL NSNXTC
          IF(.NOT.YEND)THEN
              IF(CHECK('binsize'))THEN
                  CALL NSNXTC
                  CALL GETNUM(BINSIZ,FAIL)
                  IF(BINSIZ.EQ.0)BINSIZ=100
                  IF(FAIL)BINSIZ=100
              ELSE
                  CALL ERROR(1)
              ENDIF
              CALL SKPRST
          ENDIF
          CALL PUTSTR(RESULT,'profile of the number of words falling')
          CALL PUTSTR(RESULT,' into frequency bands ')
          CALL PUTSTR(RESULT,'1 to ')
          CALL PUTINT(RESULT,BINSIZ,0)
          CALL PUTSTR(RESULT,' etc.')
          CALL UNDERL(RESULT,'=')
          CALL PUTSTR(RESULT,' from   to count  total')
          CALL PUTNL(RESULT)
          CALL SRTMER(SAFREQ,TAPOUT,TOTAL)
          CUMM=0
          TFREQ=0
          UPPER=BINSIZ
          CALL REREAD(TAPOUT)
          CALL GETIN(TAPOUT,WRDDAT,MXLENG+5,NULL)
    2     CONTINUE
          IF(NULL)GOTO 5
    3         IF(NULL)GOTO 4
                  FREQ=WRDDAT(2)
                  IF(FREQ.LE.UPPER)THEN
                      TFREQ=TFREQ+FREQ
                      CALL GETIN(TAPOUT,WRDDAT,MXLENG+5,NULL)
                      GOTO 3
                  ENDIF
    4         CONTINUE
              CUMM=CUMM+TFREQ
              CALL PUTINT(RESULT,UPPER-BINSIZ+1,-5)
              CALL PUTINT(RESULT,UPPER,-5)
              CALL PUTINT(RESULT,TFREQ,-6)
              CALL PUTINT(RESULT,CUMM,-7)
              CALL PUTNL(RESULT)
              TFREQ=0
              UPPER=UPPER+BINSIZ
              GOTO 2
    5     CONTINUE
          CALL PUTNL(RESULT)
          YEND=.FALSE.
          GOTO 1
      ENDIF
      RETURN
      END
      SUBROUTINE PRWORD(TV,LOC,STYLE)
      INCLUDE 'global.f'
      INTEGER TV,LOC,STYLE
C THIS ROUTINE WILL PRINT (ON FILE TV) THE TEXT WORD IN SYMBOLIC FORM
C      WHICH OCCURS AT POSITION  LOC  IN THE SYMBOL TABLE. THE FINAL
C      PRINTED FORM WILL BE DECIDED BY THE VALUE OF  STYLE.
      IF(STYLE.LT.1 .OR. STYLE.GT.2)GOTO 177
      GOTO (101,102),STYLE
  101 CONTINUE
C  NATURAL STYLE OF SYMBOL TABLE ENTRY
      CALL PRITEM(TV,LOC)
      GOTO 199
  102 CONTINUE
C  IN THE STYLE OF THE WORDS FIRST OCCURRENCE IN THE TEXT
      CALL GETBN1(ITEXT,IABS(SYMTAB(LOC+3)),TEMP)
      CALL PRITEM(TV,TEMP)
      GOTO 199
  177 CONTINUE
  199 CONTINUE
      END
      SUBROUTINE PRFRWD(TV,LOC,STYLE)
      INCLUDE 'global.f'
      INTEGER TV,LOC,STYLE
C THIS ROUTINE WILL PRINT ON FILE  TV  THE FREQUENCY OF OCCURRENCED
C      OF THE WORD AT POSITION LOC IN THE SYMBOL TABLE, TOGETHER WITH
C      ITS SYMBOLIC VALUE. THE WORD WILL BE PRINTED IN THE GIVEN  STYLE.
      CALL PUTINT(TV,ABS(SYMTAB(LOC+2)),-5)
      CALL PUTCH1(TV,' ')
      CALL PRWORD(TV,LOC,STYLE)
      END
C  BALANCED 2-WAY MERGE SORT, AS DESCRIBED IN
C      KNUTH: THE ART OF COMPUTER PROGRAMMING:
C     VOL.3 SORTING AND SEARCHING (EXTERNAL SORTING, PP 247-250)
C  EACH RECORD IS UP TO  MXRECL  WORDS LONG,
C    AND REPRESENTS A TEXT WORD.
C      (THE LENGTH IN THE 3RD POSITION).
C      IN THIS CASE THE RECORD IS ITS OWN KEY
C      THIS SORT-MERGE (2C) ASSUMES THAT EACH RUN IS TERMINATED
C      BY A UNIQUE NULL RECORD
      SUBROUTINE PRTAPE(TAPE,N,S)
      INCLUDE 'global.f'
      INTEGER  TAPE, N
      CHARACTER S*(*)
      INTEGER A(0:MXWORD+6)
      LOGICAL NULL
      INTEGER I
C PRINTS ALL THE  N  TEXT WORDS ON  TAPE
C      (INCLUDING THE NULLWORDS USED AS SEPARATORS)
      CALL PUTSTR(MONFIL,S)
      CALL PUTNL(MONFIL)
      CALL PUTSTR(MONFIL,'tape contains')
      CALL PUTINT(MONFIL,N,0)
      CALL PUTSTR(MONFIL,' text words')
      CALL PUTNL(MONFIL)
      CALL REREAD(TAPE)
      DO 1   I  =1,  N
   71     CONTINUE
          CALL GETIN(TAPE,A,MXLENG+5,NULL)
          IF(NULL)THEN
              CALL PUTSTR(MONFIL,'NULL')
              CALL PUTNL(MONFIL)
              GOTO 71
          END IF
          CALL PRITEM(MONFIL,A(1))
          CALL PUTNL(MONFIL)
    1 CONTINUE
      CALL PUTNL(MONFIL)
      END
      LOGICAL FUNCTION  LEALPH(WORD1,WORD2)
      INCLUDE 'global.f'
      INTEGER WORD1(0:MXWORD+6)
      INTEGER WORD2(0:MXWORD+6)
      INTEGER  I,ALENG1,BLENG2,N,DUMMY
      LOGICAL LE
C THIS ROUTINE TESTS TWO WORDS FOR .LE. ORDERING.
C      THE FOLLOWING INFORMATION IS CONTAINED IN THE  WORD  RECORDS:
C      WORD(1)   WILL CONTAIN THE ADDRESS OF WORD IN SYMTAB
C      WORD(2)   WILL CONTAIN THE FREQUENCY OF OCCURRENCE
C      WORD(3)   WILL CONTAIN THE LENGTH IN CHARACTERS OF
C      THE SPECAL LETTERS
C      WORD(4)   WILL CONTAIN THE START ADDRESS IN  WORD  WHERE
C      THE SPECAL LETTERS BEGIN
C      WORD(5)   WILL CONTAIN THE LENGTH IN CHARACTERS OF
C      THE TEXT WORD WHICH FOLLOWS.
C      WORD(6:5+WORD(5)) WILL CONTAIN THE LETTERS IN THE TEXT WORD
C      EXCLUDING THE SPECAL LETTERS.
C      WORD(6+WORD(5):ONWARDS)  WILL CONTAIN THE SPECAL LETTERS.
      ALENG1=WORD1(5)
      BLENG2=WORD2(5)
      IF( ALENG1.LT.BLENG2 )THEN
          N=ALENG1
      ELSE
          N=BLENG2
      END IF
      I=6
C THIS LOOP MAKES  I  POINT TO THE 1ST PAIR OF DISTINCT LETTERS
      DO 1 DUMMY=1 , N
          IF( PRRANK(WORD1(I)).NE.PRRANK(WORD2(I)) )GOTO 91
          I=I+1
    1 CONTINUE
   91 CONTINUE
C
C      THERE ARE NOW TWO POSSIBILITIES:-
C      1) THE LOOP GOES TO COMPLETION - SHOWING THAT THE SMALLER WORD
C      IS A PREFIX OF THE LARGER. E.G.  FRED  FREDDY
C      2) THE LOOP TERMINATES EARLY - WITH  I  POINTING TO THE 1ST
C      PAIR OF DISTINCT LETTERS
C      THE VALUE OF  I  ALLOWS ONE TO DISTINGUISH BETWEEN THE ABOVE
C      CASES:-
C      CASE 1) OCCURS WHEN   I.EQ.(N+6)
C      CASE 2) OCCURS WHEN   I.LT.(N+6) WHICH IS THE SAME AS
C              I.NE.(N+6)
       IF(  I.EQ.(N+6)  )THEN
C SMALLER WORD IS A PREFIX OF THE LARGER
             IF( ALENG1.EQ.BLENG2 )THEN
C USE SECONDARY RANK TO SEPARATE THEM
              I=6
              DO 2 DUMMY =1, ALENG1
                  IF(SECRNK(WORD1(I)).NE.
     +               SECRNK(WORD2(I)))GOTO 92
                  I=I+1
    2         CONTINUE
   92         CONTINUE
              IF( I.EQ.N+6 )THEN
C USE THE SPECAL LETTERS TO RESOLVE THE
C                            ORDERING
                  ALENG1=WORD1(3)
                  BLENG2=WORD2(3)
                  IF( ALENG1.LT.BLENG2 )THEN
                      N=ALENG1
                  ELSE
                      N=BLENG2
                  END IF
                  I=6+WORD1(5)
C OR WORD2(5)  BECAUSE WORDLENGTH1.EQ.WORDLENGTH2
                  DO 3 DUMMY =1, N
                      IF( PRRANK(WORD1(I)).NE.
     +                    PRRANK(WORD2(I)) )GOTO 93
                      I=I+1
    3             CONTINUE
   93             CONTINUE
                  IF( I.EQ.(N+6+WORD1(5)) )THEN
C THE SMALLER SPECAL SEQUENCE IS USED
                                IF( ALENG1.EQ.BLENG2 )THEN
C USE SECONDARY RANK
                          I=6+WORD1(5)
                          DO 4 DUMMY=1 , ALENG1
                              IF(SECRNK(WORD1(I)).NE.
     +                           SECRNK(WORD2(I)) )GOTO 94
                              I=I+1
    4                     CONTINUE
   94                     CONTINUE
                          IF( I.EQ.N+6+WORD1(5) )THEN
                              LE=.TRUE.
C              WE CANNOT RESOLVE ANY MORE
                          ELSE
                              LE=SECRNK(WORD1(I)).LT.
     +                           SECRNK(WORD2(I))
                          END IF
                      ELSE
C SHORTER SEQUENCE COMES FIRST
                          LE=ALENG1.LT.BLENG2
                      END IF
                  ELSE
C SHORTEST IS ONE WITH LEAST PRIMARY RANK
                      LE=PRRANK(WORD1(I)).LT.PRRANK(WORD2(I))
                  END IF
              ELSE
                  LE=SECRNK(WORD1(I)).LT.SECRNK(WORD2(I))
              END IF
          ELSE
C THE SHORTER WORD COMES FIRST
              LE=ALENG1.LT.BLENG2
          END IF
      ELSE
C SHORTEST WORD IS THE LETTER WITH THE SMALLEST PRIMARY RANK
          LE=PRRANK(WORD1(I)) .LT. PRRANK(WORD2(I))
      END IF
      LEALPH=LE
      END
      LOGICAL FUNCTION  LEAUX(METHOD,A,B)
      INCLUDE 'global.f'
      INTEGER METHOD
      INTEGER A(0:MXWORD+6)
      INTEGER B(0:MXWORD+6)
      LOGICAL LE
      INTEGER I
      INTEGER LA,LB,A1(0:MXWORD+6),B1(0:MXWORD+6)
      LOGICAL LEALPH
C ON EXIT THIS ROUTINE YIELDS  TRUE  WHEN TEXT WORD RECORD
C      PRECEEDS TEXT WORD RECORD  B , USING A GIVEN  METHOD  OF
C      COMPARISON.
C      THE ORDERING IS SIMILAR TO  LESSTHAN-OR-EQUAL-TO .LE.  .
      IF(METHOD.LT.1 .OR. METHOD.GT.12)GOTO 177
      GOTO (101,102,103,104,105,106,107,108,109,110,111,112),METHOD
  101 CONTINUE
C  ASCENDING ALPHABETIC ORDER
      LE=LEALPH(A,B)
      GOTO 199
  102 CONTINUE
C  DESCENDING ALPHABETIC ORDER
      LE=LEALPH(B,A)
      GOTO 199
  103 CONTINUE
C  ASCENDING FREQUENCY ORDER
      IF(  A(2).LT.B(2)  )THEN
          LE=.TRUE.
      ELSE IF(  A(2).GT.B(2)  )THEN
          LE= .FALSE.
      ELSE
          LE=LEALPH(A,B)
      END IF
      GOTO 199
  104 CONTINUE
C  DESCENDING FREQUENCY ORDER
      IF(  A(2).GT.B(2)  )THEN
          LE=.TRUE.
      ELSE IF(  A(2).LT.B(2)  )THEN
          LE=.FALSE.
      ELSE
          LE=LEALPH(A,B)
      END IF
      GOTO 199
  105 CONTINUE
C  ASCENDING LENGTH ORDER
      IF( A(5).LT.B(5) )THEN
          LE=.TRUE.
      ELSE IF( A(5).GT.B(5) )THEN
          LE=.FALSE.
      ELSE
          LE=LEALPH(A,B)
      END IF
      GOTO 199
  106 CONTINUE
C  DESCENDING LENGTH ORDER
      IF(  A(5).GT.B(5) )THEN
          LE=.TRUE.
      ELSE IF(  A(5).LT.B(5) )THEN
          LE=.FALSE.
      ELSE
          LE=LEALPH(A,B)
      END IF
      GOTO 199
  107 CONTINUE
C  ASCENDING EXTENDED LENGTH ORDER
      IF(  A(3)+A(5).LT.B(3)+B(5) )THEN
          LE=.TRUE.
      ELSE IF(  A(3)+A(5).GT.B(3)+B(5) )THEN
          LE=.FALSE.
      ELSE
          LE=LEALPH(A,B)
      END IF
      GOTO 199
  108 CONTINUE
C  DESCENDING EXTENDED LENGTH ORDER
      IF(  A(3)+A(5).GT.B(3)+B(5) )THEN
          LE=.TRUE.
      ELSE IF( A(3)+A(5).LT.B(3)+B(5) )THEN
          LE=.FALSE.
      ELSE
          LE=LEALPH(A,B)
      END IF
      GOTO 199
  109 CONTINUE
C  REVERSE ASCENDING ALPHABETIC ORDER
      LA=A(3)+A(5)+5
C TOTAL LENGTH OF WORD A DATA
      LB=B(3)+B(5)+5
C TOTAL LENGTH OF WORD B DATA
C     CALL SLICE(I,A1,1,A,1,LA)
      DO 661 I=0,LA-1
          A1(1+I)=A(1+I)
  661 CONTINUE
      DO 1   I  =  6  ,  5+A(5)
          A1(A(5)+5-I+6)=A(I)
    1 CONTINUE
C     CALL SLICE(I,B1,1,B,1,LB)
      DO 662 I=0,LB-1
          B1(1+I)=B(1+I)
  662 CONTINUE
      DO 2   I  =  6  ,  5+B(5)
          B1(B(5)+5-I+6)=B(I)
    2 CONTINUE
      LE=LEALPH(A1,B1)
      GOTO 199
  110 CONTINUE
C  REVERSE DESCENDING ALPHABETIC ORDER
      LA=A(3)+A(5)+5
C TOTAL LENGTH OF WORD A DATA
      LB=B(3)+B(5)+5
C TOTAL LENGTH OF WORD B DATA
C     CALL SLICE(I,A1,1,A,1,LA)
      DO 663 I=0,LA-1
          A1(1+I)=A(1+I)
  663 CONTINUE
      DO 3   I  =  6  ,  5+A(5)
          A1(A(5)+5-I+6)=A(I)
    3 CONTINUE
C     CALL SLICE(I,B1,1,B,1,LB)
      DO 664 I=0,LB-1
          B1(1+I)=B(1+I)
  664 CONTINUE
      DO 4   I  =  6  ,  5+B(5)
          B1(B(5)+5-I+6)=B(I)
    4 CONTINUE
      LE=LEALPH(B1,A1)
      GOTO 199
  111 CONTINUE
C  FIRST OCCURRENCE ORDER
      LE=ABS(SYMTAB(A(1)+3)) .LE. ABS(SYMTAB(B(1)+3))
      GOTO 199
  112 CONTINUE
C  LAST OCCURRENCE  ORDER
      LE= B(4).LE.A(4)
C RECALL THAT IN CALL GTWORD WE ZAPPED THIS POSITION...
  177 CONTINUE
  199 CONTINUE
      LEAUX=LE
      END
      SUBROUTINE ORDER(BUFF,NREC,METHOD)
      INCLUDE 'global.f'
      INTEGER  BUFF(0:MXWORD+6,0:NREC)
      INTEGER  METHOD,NREC
      INTEGER I,J,K
      INTEGER  L,M
      INTEGER  MXENTR
      INTEGER TEMP(0:MXWORD+6)
      LOGICAL  B
      LOGICAL LEAUX
C THIS ROUTINE WILL SHELLSORT A SET OF TEXT WORD RECORDS
C      HELD IN  BUFF  INTO THE ORDER DETERMINED BY  METHOD
      MXENTR=NREC
      L=1
    1 IF(  L.LE.MXENTR  )THEN
          M=2  *     L-1
          L = L+L
          GOTO 1
      END IF
    2 CONTINUE
      M=M/2
      IF(  M.NE.0  )THEN
          DO 3   J  =1,  MXENTR-M
              B= .TRUE.
              DO 4   I  =  J  , 1, -M
                  IF(.NOT.B)GOTO 94
                  B=.NOT.LEAUX(METHOD,BUFF(0,I),BUFF(0,I+M))
                  IF(  B  )THEN
C TOO MUCH MOVING, TRY USING POINTERS
                      CALL SLICE(K,TEMP,1,BUFF(0,I),1,MXLENG+5)
                      CALL SLICE(K,BUFF(0,I),1,BUFF(0,I+M),
     +                           1,MXLENG+5)
                      CALL SLICE(K,BUFF(0,I+M),1,TEMP,1,MXLENG+5)
                  END IF
    4         CONTINUE
   94         CONTINUE
    3     CONTINUE
          GOTO 2
      END IF
      END
      SUBROUTINE INISRT(ATAPE1,BTAPE2,NREC,METHOD,N1,N2)
      INCLUDE 'global.f'
      INTEGER  ATAPE1,BTAPE2,NREC,METHOD
      INTEGER N1
      INTEGER N2
      INTEGER AREA(0:MXWORD+6,0:MXSORT+1)
      INTEGER POINTR
      INTEGER I
      INTEGER A(0:MXWORD+6), NULREC(0:MXWORD+6)
      LOGICAL OK
      INTEGER  TAPEA,TAPEB,TAPTMP
C  THIS ROUTINE READS WORDS FROM THE SYMBOL TABLE
C      AND SORTS EACH GROUP OF  NREC  INTO ORDER. EACH SORTED
C      GROUP IS WRITTEN TO TAPE1 AND TAPE2 ALTERNATLEY. THE
C      PROCESS CALL STOPS WHEN A NULL WORD IS READ. THE PARTIALLY
C      FILLED GROUP IS SORTED INTO ORDER AND WRITTEN TO THE
C      RELEVANT TAPE
      NULREC(1)=(-1)
      NULREC(2)=(-1)
      NULREC(3)=(-1)
      CALL REWRIT(ATAPE1)
      CALL REWRIT(BTAPE2)
      CALL PUTOUT(ATAPE1,NULREC,MXLENG+5)
      CALL PUTOUT(BTAPE2,NULREC,MXLENG+5)
      CALL REWRIT(ATAPE1)
      CALL REWRIT(BTAPE2)
      TAPEA=ATAPE1
      TAPEB=BTAPE2
      N2=0
      N1=0
      POINTR=0
      LOCAT=0
      COUNT=ATBSZ1
      CALL GTWORD(A,OK,METHOD)
    1 IF(  OK  )THEN
          POINTR=POINTR+1
          CALL SLICE(I,AREA(0,POINTR),1,A,1,MXLENG+5)
          IF(  POINTR.EQ.NREC  )THEN
              CALL ORDER(AREA,POINTR,METHOD)
              DO 2   I  =1,  NREC
                  CALL PUTOUT(TAPEA,AREA(0,I),MXLENG+5)
    2         CONTINUE
              CALL PUTOUT(TAPEA,NULREC,MXLENG+5)
              POINTR=0
              IF( TAPEA.EQ.ATAPE1 )THEN
                  N1=N1+NREC
               ELSE
                  N2=N2+NREC
              END IF
              TAPTMP=TAPEA
              TAPEA=TAPEB
              TAPEB=TAPTMP
          END IF
          CALL GTWORD(A,OK,METHOD)
          GOTO 1
      END IF
      IF(  POINTR.NE.0  )THEN
          CALL ORDER(AREA,POINTR,METHOD)
          DO 3   I  =1,  POINTR
              CALL PUTOUT(TAPEA,AREA(0,I),MXLENG+5)
    3     CONTINUE
          CALL PUTOUT(TAPEA,NULREC,MXLENG+5)
          IF( TAPEA.EQ.ATAPE1 )THEN
              N1=N1+POINTR
          ELSE
              N2=N2+POINTR
          END IF
      END IF
      CALL PUTOUT(TAPEB,NULREC,MXLENG+5)
      END
      SUBROUTINE MERGE2(ATAPE1,BTAPE2,CTAPE3,DTAPE4,K,N1,N2,
     +                  METHOD,TAPOUT)
      INCLUDE 'global.f'
      INTEGER  ATAPE1,BTAPE2,CTAPE3,DTAPE4,K,N1,N2,METHOD
      INTEGER TAPOUT
      INTEGER  TAPEA,TAPEB,TAPEC,TAPED,TAPTMP
      INTEGER  NK,TOTAL
      LOGICAL  ENDRAB,NULLA,NULLB
      INTEGER TA(0:MXWORD+6),TB(0:MXWORD+6),
     +        TC(0:MXWORD+6),
     +NULREC(0:MXWORD+6)
      INTEGER SA,SB
      INTEGER I
      LOGICAL LEAUX
C THIS ROUTINE WILL MERGE THE PARTIALLY SORTED RUNS
C      HELD ON  TAPE1  AND  TAPE2  ONTO A SINGLE TAPE
C      WHOSE IDENTITY WILL BE STORED IN  TAPOUT  .
C      K  IS THE NUMBER OF RECORDS PER RUN
C      N1 IS THE NUMBER OF RECORDS ON  TAPE1
C      N2 IS THE NUMBER OF RECORDS ON  TAPE2
      NULREC(1)=(-1)
      NULREC(2)=(-1)
      NULREC(3)=(-1)
      TAPEA=ATAPE1
      TAPEB=BTAPE2
      TAPEC=CTAPE3
      TAPED=DTAPE4
      CALL REWRIT(CTAPE3)
      CALL PUTOUT(CTAPE3,NULREC,MXLENG+5)
      CALL REWRIT(DTAPE4)
      CALL PUTOUT(DTAPE4,NULREC,MXLENG+5)
      IF(  N1.GT.N2  )THEN
          TAPEA=BTAPE2
          TAPEB=ATAPE1
      END IF
      NK=K
    1 IF(  NK.LT.(N1+N2)  )THEN
          CALL REREAD(TAPEA)
          CALL REREAD(TAPEB)
          CALL REWRIT(TAPEC)
          CALL REWRIT(TAPED)
          TOTAL=N1+N2
    2     IF(  TOTAL.GT.0  )THEN
              CALL GETIN(TAPEA,TA,MXLENG+5,NULLA)
              CALL GETIN(TAPEB,TB,MXLENG+5,NULLB)
              ENDRAB=(NULLA .OR. NULLB)
    3         IF(   .NOT. ENDRAB  )THEN
                  IF(  LEAUX(METHOD,TA,TB)  )THEN
                      SA=TA(3)+TA(5)+5
                      CALL SLICE(I,TC,1,TA,1,SA)
                      CALL GETIN(TAPEA,TA,MXLENG+5,NULLA)
                      ENDRAB=NULLA
                  ELSE
                      SB=TB(3)+TB(5)+5
                      CALL SLICE(I,TC,1,TB,1,SB)
                      CALL GETIN(TAPEB,TB,MXLENG+5,NULLB)
                      ENDRAB=NULLB
                  END IF
                  CALL PUTOUT(TAPEC,TC,MXLENG+5)
                  GOTO 3
              END IF
    4         IF( .NOT. NULLB )THEN
                  CALL PUTOUT(TAPEC,TB,MXLENG+5)
                  CALL GETIN(TAPEB,TB,MXLENG+5,NULLB)
                  GOTO 4
              END IF
    5         IF( .NOT. NULLA )THEN
                  CALL PUTOUT(TAPEC,TA,MXLENG+5)
                  CALL GETIN(TAPEA,TA,MXLENG+5,NULLA)
                  GOTO 5
              END IF
              CALL PUTOUT(TAPEC,NULREC,MXLENG+5)
              TAPTMP=TAPEC
              TAPEC=TAPED
              TAPED=TAPTMP
              TOTAL=TOTAL-(2 *     NK)
              GOTO 2
          END IF
          CALL PUTOUT(TAPEC,NULREC,MXLENG+5)
          TAPTMP=TAPEA
          TAPEA=TAPEC
          TAPEC=TAPTMP
          TAPTMP=TAPEB
          TAPEB=TAPED
          TAPED=TAPTMP
          NK=NK+NK
          GOTO 1
      END IF
      TAPOUT=TAPEB
      END
      SUBROUTINE SRTMER(SORTME,TAPOUT,TOTWRD)
      INCLUDE 'global.f'
      INTEGER SORTME
      INTEGER TAPOUT
      INTEGER TOTWRD
      INTEGER N1,N2
      REAL TOTTIM,LASTTM,TIMSAV,XCLOCK
C THIS ROUTINE WILL SORT A FILE OF TEXT WORDS BY FIRST INITIALISING
C      A PAIR OF TAPES WITH PARTIALLY SORTED RUNS THEN MERGING THESE
C      RUNS TOGETHER. THE FINAL SORTED WORD RECORDS WILL BE HELD ON
C      THE TAPE CALLED  TAPOUT  , WHICH WILL CONTAIN THE NUMBER OF
C      RECORDS HELD IN  TOTWRD
      LASTTM=XCLOCK(1)
C FIRST DO THE INITIALISATION RUN
      CALL INISRT(1,2,SORTSZ,SORTME,N1,N2)
      TIMSAV=XCLOCK(1)-LASTTM
      TOTTIM=TIMSAV
      IF( SW5 )THEN
          CALL PUTSTR(MONFIL,'sortsize= ')
          CALL PUTINT(MONFIL,SORTSZ,0)
          CALL PUTNL(MONFIL)
          CALL PUTSTR(MONFIL,'initialisation time= ')
          CALL PTFLOT(MONFIL,TIMSAV,15,8,2)
          CALL PUTSTR(MONFIL,' secs')
          CALL PUTNL(MONFIL)
      END IF
      LASTTM=XCLOCK(1)
C NOW DO THE MERGING
      CALL MERGE2(1,2,3,4,SORTSZ,N1,N2,SORTME,TAPOUT)
      TIMSAV=XCLOCK(1)-LASTTM
      TOTTIM=TOTTIM+TIMSAV
      IF( SW5 )THEN
          CALL PUTSTR(MONFIL,'mergeing time= ')
          CALL PTFLOT(MONFIL,TIMSAV,15,8,2)
          CALL PUTSTR(MONFIL,' secs')
          CALL PUTNL(MONFIL)
          CALL PUTSTR(MONFIL,'total time= ')
          CALL PTFLOT(MONFIL,TOTTIM,15,8,2)
          CALL PUTSTR(MONFIL,' secs')
          CALL PUTNL(MONFIL)
          CALL PUTNL(MONFIL)
      END IF
      TOTWRD=N1+N2
      END
C HERE WE PRINT OUT THE PREVIOUSLY SORTED WORDS
      SUBROUTINE PTMETH(F,SORTME)
      INCLUDE 'global.f'
      INTEGER F,SORTME
C PRINTS THE NAME OF THE METHOD OF SORTING
      IF(SORTME.LT.1 .OR. SORTME.GT.12)GOTO 177
      GOTO (101,102,103,104,105,106,107,108,109,110,111,
     +     112), SORTME
  101 CONTINUE
      CALL PUTSTR(F,'ascending alphabetic')
      GOTO 199
  102 CONTINUE
      CALL PUTSTR(F,'descending alphabetic')
      GOTO 199
  103 CONTINUE
      CALL PUTSTR(F,'ascending frequency')
      GOTO 199
  104 CONTINUE
      CALL PUTSTR(F,'descending frequency')
      GOTO 199
  105 CONTINUE
      CALL PUTSTR(F,'ascending length')
      GOTO 199
  106 CONTINUE
      CALL PUTSTR(F,'descending length')
      GOTO 199
  107 CONTINUE
      CALL PUTSTR(F,'ascending extended length')
      GOTO 199
  108 CONTINUE
      CALL PUTSTR(F,'descending extended length')
      GOTO 199
  109 CONTINUE
      CALL PUTSTR(F,'reverse ascending alphabetic')
      GOTO 199
  110 CONTINUE
      CALL PUTSTR(F,'reverse descending alphabetic')
      GOTO 199
  111 CONTINUE
      CALL PUTSTR(F,'first occurrence')
      GOTO 199
  112 CONTINUE
      CALL PUTSTR(F,'last occurrence')
      GOTO 199
  177 CONTINUE
  199 CONTINUE
      CALL PUTSTR(F,' order')
      END

C LAST UPDATED 26 MAY 1982
      SUBROUTINE PRWRDS(TAPE,N,FLDWTH,METHOD)
      INCLUDE 'global.f'
      INTEGER TAPE,N,FLDWTH,METHOD
      INTEGER  WRDDAT(0:MXWORD+6)
      INTEGER POSITN
      LOGICAL NULL
      INTEGER DUMMY
      INTEGER CHRNUM
C THIS ROUTINE WILL PRINT A PREVIOUSLY SORTED VOCABULARY.
C      THE LAYOUT WILL BE ACROSS THE PAGE
      CALL PUTSTR(RESULT,'table of ')
      CALL PUTINT(RESULT,N,0)
      CALL PUTSTR(RESULT,' words in ')
      CALL PTMETH(RESULT,METHOD)
      CALL UNDERL(RESULT,'=')
      POSITN=1
      CALL REREAD(TAPE)
      DO 1 DUMMY =1,  N
          CALL GETIN(TAPE,WRDDAT,MXLENG+5,NULL)
          IF(  FLDWTH+CHRNUM(RESULT)-1.GT.LPWDTH  )THEN
              CALL PUTNL(RESULT)
              POSITN=1
          END IF
          CALL PRFRWD(RESULT,WRDDAT(1),1)
          POSITN=POSITN+ FLDWTH
          CALL STCNUM(RESULT,POSITN)
    1 CONTINUE
      IF(  POSITN.NE.1  )THEN
          CALL PUTNL(RESULT)
      END IF
      CALL PUTNL(RESULT)
      END
      LOGICAL FUNCTION  LETTER(C)
      INCLUDE 'global.f'
      CHARACTER *1 C
C TRUE  ONLY WHEN  C  IS AN ALPHABETIC LETTER
      LETTER=   ( ('a'.LE.C)  .AND.  (C.LE.'z') ) .OR.
     +( ('A'.LE.C)  .AND.  (C.LE.'Z') )
      END
      LOGICAL FUNCTION  DIGIT(C)
      INCLUDE 'global.f'
      CHARACTER *1 C
C TRUE  ONLY WHEN  C  IS A 0 TO 9 DIGIT
      DIGIT=('0'.LE.C  .AND.  C.LE.'9')
      END
C BEGIN CONTROL STATEMENT SECTION
      SUBROUTINE PRCON
      INCLUDE 'global.f'
      INTEGER P
      INTEGER DUMMY,I
C PRINTS THE CURRENT CONTROL STATEMENT ON THE CLOC MONFIL FILE
      CALL MARGIN
      P=CARDSZ
      DO 1 DUMMY =1, CARDSZ
          IF( CNTLIN(P).NE.' ' )GOTO 91
          P=P-1
    1 CONTINUE
   91 CONTINUE
      DO 2  I = 0 , P
          CALL PUTCH1(MONFIL,CNTLIN(I))
    2 CONTINUE
      CALL PUTNL(MONFIL)
      END
      SUBROUTINE CLASSY(F,CLASS)
      INCLUDE 'global.f'
      CHARACTER  F(*)
      INTEGER CLASS
      LOGICAL STREQ
C SEPARATES CONTROL CARDS INTO CATEGORIES
C      ON EXIT  CLASS.EQ.0 MEANS AN ILLEGAL CONTROL CARD
C      CLASS.EQ.1 MEANS A STANDARD CLOC CONTROL CARD
C      CLASS.EQ.2 MEANS A SPECAL CONTROL CARD
      IF(
     +STREQ( F,QBLANK,15) .OR.
     +STREQ(F,QINDET, 15) .OR.
     +STREQ(F,QGTTXT,15) .OR.
     +STREQ(F,QITUSE,15) .OR.
     +STREQ(F,QITUZE,15) .OR.
     +STREQ(F,QLETT,15) .OR.
     +STREQ(F,QSEPAR,15) .OR.
     +STREQ(F,QIGNOR,15) .OR.
     +STREQ(F,QRDASP,15) .OR.
     +STREQ(F,QPADD,15) .OR.
     +STREQ(F,QDEFER,15) )THEN
          CLASS=1
      ELSE IF(
     +    STREQ(F,QFINIS,15) .OR.
     +    STREQ(F,QEVWRD,15) .OR.
     +    STREQ(F,QSEWRD,15) .OR.
     +    STREQ(F,QLWRDS,15) .OR.
     +    STREQ(F,QPATT,15) .OR.
     +    STREQ(F,QFREQ,15) .OR.
     +    STREQ(F,QEXCLU,15) .OR.
     +    STREQ(F,QCONC,15) .OR.
     +    STREQ(F,QCOLL,15) )THEN
              CLASS=1
      ELSE IF(
     +    STREQ(F,QEVCOL,15) .OR.
     +    STREQ(F,QSECOL,15) .OR.
     +    STREQ(F,QACCEP,15) .OR.
     +    STREQ(F,QREJEC,15) .OR.
     +    STREQ(F,QCOOC,15) .OR.
     +    STREQ(F,QWLIST,15) .OR.
     +    STREQ(F,QOUTDT,15) .OR.
     +    STREQ(F,QSVTXT,15) .OR.
     +    STREQ(F,QPHRAS,15) .OR.
     +    STREQ(F,QSERIS,15) .OR.
     +    STREQ(F,QCHOIC,15) .OR.
     +    STREQ(F,QSPAN,15) .OR.
     +    STREQ(F,QRUN,15) .OR.
     +    STREQ(F,QNOTE,15)  )THEN
               CLASS=1
      ELSE IF(
     +    STREQ(F,QINDEX,15) .OR.
     +    STREQ(F,QWRITE,15) .OR.
     +    STREQ(F,QTESRT,15) .OR.
     +    STREQ(F,QFROM,15) .OR.
     +    STREQ(F,QTO,15) .OR.
     +    STREQ(F,QINCLU,15) .OR.
     +    STREQ(F,QSTATS,15) .OR.
     +    STREQ(F,QNEWL,15) .OR.
     +    STREQ(F,QNEWP,15) .OR.
     +    STREQ(F,QPROFI,15) .OR.
     +    STREQ(F,QMESSA,15) )THEN
               CLASS=1
      ELSE IF(
     +    STREQ(F,QINSRT,15) .OR.
     +    STREQ(F,QSEND,15) .OR.
     +    STREQ(F,QNOSND,15) .OR.
     +    STREQ(F,QCONTI,15) .OR.
     +    STREQ(F,QCSS,15) .OR.
     +    STREQ(F,QCUS,15) .OR.
     +    STREQ(F,QCLSYM,15) .OR.
     +    STREQ(F,QCLITX,15) .OR.
     +    STREQ(F,QCREFS,15) )THEN
               CLASS=2
      ELSE
          CLASS=0
      END IF
      END
      SUBROUTINE RDCON
      INCLUDE 'global.f'
      INTEGER  FLDPTR
      LOGICAL EOF1
      INTEGER CLASS
      INTEGER DUMMY
      LOGICAL LETTER
      CHARACTER C
      CHARACTER *100 FILSPC
      LOGICAL STREQ,CHECK
C      THIS ROUTINE READS A LINE PLACING THE CONTENTS INTO  CNTLIN  .
C      THE 1ST 15 COLUMNS OF THIS LINE ARE EXAMINED AND THE LETTERS ONLY
C      ARE PLACED IN  CONTROLFIELD  . IF A RIGHT PARENTHESIS IS FOUND
C      WITHIN COLUMNS 1 TO 15 THE LETTERS TO THE LEFT OF IT ARE PLACED
C      INTO  CTLFLD  .
C      E.G.   CNTLIN         CTLFLD
C      INPUT DETAILS       INPUTDETAILS
C      *LIST OF WORDS      LISTOFWORDS
C      ITEMIZE USING)CLOC  ITEMIZEUSING
C      ON EXIT
C      CNTRLP.EQ.15
C      OR  CNTRLP.LT.15  AND  CNTLIN(CNTRLP-1).EQ.')'
   91 CONTINUE
      CALL PRERRS
      CALL GTLINE(CON,CNTLIN,CARDSZ,EOF1)
    1 IF(EOF1)THEN
          IF( CTLSTP.EQ.0 )THEN
              CALL PUTSTR(MONFIL,
     +        '**  you have run off the end of your controlfile')
              CALL PUTNL(MONFIL)
              CALL ENDRUN
          ELSE
              CLOSE(UNIT=CON,STATUS='KEEP')
              FTEMP=FTEMP-1
              CTLSTP=CTLSTP-1
              CON=CTLSTK(CTLSTP)
              IF( CTLSTP.EQ.0 )THEN
                  SW3=SAVSW3
                  SW4=SAVSW4
              END IF
          END IF
          CALL GTLINE(CON,CNTLIN,CARDSZ,EOF1)
          GOTO 1
      END IF
C NOW PAD THE LINE WITH SPACE CHARACTERS
C     CARDSZ=CARDSZ-1
    2 IF( CARDSZ.LT.MXWDTH+1 )THEN
           CARDSZ=CARDSZ+1
          CNTLIN(CARDSZ)=' '
          GOTO 2
      END IF
C NOW ISOLATE THE TWO FIELDS
      CNTRLP=0
      FLDPTR=0
      DO 3 DUMMY=1,  15
          IF( CNTLIN(CNTRLP).EQ.')')GOTO 93
          IF(  LETTER(CNTLIN(CNTRLP))  )THEN
              CTLFLD(FLDPTR)=CNTLIN(CNTRLP)
              CALL FLOWER(CTLFLD(FLDPTR))
              FLDPTR=FLDPTR+1
          END IF
          CNTRLP=CNTRLP+1
    3 CONTINUE
   93 CONTINUE
    4 IF( FLDPTR.LT.15 )THEN
          CTLFLD(FLDPTR)=' '
          FLDPTR=FLDPTR+1
          GOTO 4
      END IF
      IF( CNTRLP.NE.15 )THEN
C WE CALL STOPPED BECAUSE OF )
          CNTRLP=CNTRLP+1
      END IF
      CALL CLASSY(CTLFLD,CLASS)
      IF( CLASS.EQ.1 )THEN
          GOTO 94
      END IF
      CALL PRCON
      IF( CLASS.EQ.2 )THEN
C A SYSTEM CONTROL CARD
      IF( STREQ(CTLFLD,QINSRT,15) )THEN
          SW3=.TRUE.
          SW4=.TRUE.
C REMOVE LEADING SPACES
    5     IF( CNTRLP.LT.MXWDTH+1 .AND.
     +    CNTLIN(CNTRLP).EQ.' ')THEN
              CNTRLP=CNTRLP+1
              GOTO 5
          END IF
C GET A FILSPC
          P=1
    6     IF( CNTRLP.LT.MXWDTH+1 .AND.
     +    CNTLIN(CNTRLP).NE.' ' )THEN
              IF( P.LT.100 )THEN
                  FILSPC(P:P)=CNTLIN(CNTRLP)
                  P=P+1
              END IF
              CNTRLP=CNTRLP+1
              GOTO 6
          END IF
          FILSPC(P:P)=' '
          IF( CTLSTP.GT.100 )THEN
              CALL PUTSTR(MONFIL,
     +                      '** insert commands too deeply nested ')
              CALL PUTNL(MONFIL)
              CALL ENDRUN
          END IF
          CTLSTK(CTLSTP)=CON
          CTLSTP=CTLSTP+1
          FTEMP=FTEMP+1
          OPEN(UNIT=FTEMP,FILE=FILSPC(1:P-1),STATUS='OLD',ERR=33)
          GOTO 34
   33     CONTINUE
              CALL PUTSTR(MONFIL,'***try again')
              CALL PUTNL(MONFIL)
              CALL PUTNL(MONFIL)
              FTEMP=FTEMP-1
              CTLSTP=CTLSTP-1
              IF( CTLSTP.EQ.0 )THEN
                  SW3=SAVSW3
                  SW4=SAVSW4
              END IF
              GOTO 35
   34     CON=FTEMP
   35     CONTINUE
      ELSE IF( STREQ(CTLFLD,QSEND,15) )THEN
          SW3=.TRUE.
      ELSE IF( STREQ(CTLFLD,QNOSND,15) )THEN
          SW3=.FALSE.
      ELSE IF( STREQ(CTLFLD,QCONTI,15) )THEN
C REMOVE LEADING SPACES
    7     IF( CNTRLP.LT.MXWDTH+1 .AND.
     +    CNTLIN(CNTRLP).EQ.' ')THEN
              CNTRLP=CNTRLP+1
              GOTO 7
          END IF
          CONTI=0
          IF(CHECK('blank'))THEN
              CONTI=0
          ELSE IF(CHECK('none'))THEN
              CONTI=(-1)
          ELSE IF(CHECK('endline'))THEN
              CONTI=ICHAR(CNTLIN(CNTRLP))
              IF(CONTI.EQ.ASPACE)CONTI=0
          END IF
      ELSE IF( STREQ(CTLFLD,QCSS,15) )THEN
          C=CNTLIN(CNTRLP)
          IF( C.EQ.'1' )THEN
              SW1=.TRUE.
          ELSE IF( C.EQ.'2' )THEN
              SW2=.TRUE.
          ELSE IF( C.EQ.'3' )THEN
              SW3=.TRUE.
          ELSE IF( C.EQ.'4' )THEN
              SW4=.TRUE.
          ELSE IF( C.EQ.'5' )THEN
              SW5=.TRUE.
          END IF
      ELSE IF( STREQ(CTLFLD,QCUS,15) )THEN
          C=CNTLIN(CNTRLP)
          IF( C.EQ.'1' )THEN
              SW1=.FALSE.
          ELSE IF( C.EQ.'2' )THEN
              SW2=.FALSE.
          ELSE IF( C.EQ.'3' )THEN
              SW3=.FALSE.
          ELSE IF( C.EQ.'4' )THEN
              SW4=.FALSE.
          ELSE IF( C.EQ.'5' )THEN
              SW5=.FALSE.
          END IF
      ELSE IF( STREQ(CTLFLD,QCLSYM,15) )THEN
          CALL PRCONT(SYMTAB)
      ELSE IF( STREQ(CTLFLD,QCLITX,15) )THEN
          CALL DUMPIT(ITEXT,NXTITX-1)
      ELSE IF( STREQ(CTLFLD,QCREFS,15) )THEN
          CALL DMPREF(REFERS)
      ELSE
          CALL FAULT('**unknown control field')
      END IF
      ELSE
          CALL MARGIN
          CALL PUTSTR(MONFIL,'**invalid control field')
          CALL PUTNL(MONFIL)
      END IF
      GOTO 91
   94 CONTINUE
      YCHAR=CNTLIN(CNTRLP)
      YEND= .FALSE.
      CALL ADJLET(YCHAR)
      END
      SUBROUTINE NEXTCH
      INCLUDE 'global.f'
      LOGICAL STRNE
C      THIS ROUTINE GETS THE NEXT CHARACTER FROM THE SPECIFICATION
C      PART OF THE CNTLIN.  THIS IS THE PART OF  CONTROLCARD
C      WHICH EXCLUDES THE  CTLFLD  .  A CONTROL CARD IS DEFINED TO
C      CONTINUE WHENEVER THE  CTLFLD  CONTAINS SPACES. THE END
C      OF INFORMATION IS THUS SIGNALLED BY A NON EMPTY  CTLFLD.
C YEND IS FALSE MEANS MORE CHARACTERS CAN BE READ
C YEND IS TRUE MEANS THE END OF THE CONTROL FIELD HAS BEEN REACHED
      CNTRLP=CNTRLP+1
      IF(  CNTRLP.GE.MXWDTH+1  )THEN
          IF(CONTI.EQ.0)THEN
              CALL RDCON
C RESET THE CNTRLP
              IF( STRNE(CTLFLD,QBLANK,15) )THEN
                  YEND=.TRUE.
              ELSE
                  YEND=.FALSE.
              END IF
          END IF
          IF(CONTI.EQ.(-1))THEN
              YEND=.TRUE.
          END IF
          IF( .NOT. YEND )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
          END IF
      END IF
      YCHAR=CNTLIN(CNTRLP)
      CALL ADJLET(YCHAR)
      END
      SUBROUTINE NSNXTC
      INCLUDE 'global.f'
C      THIS ROUTINE GETS THE NEXT NON SPACE CHARACTER FROM THE
C      SPECIFICATION PART OF THE CNTLIN
    1 IF(   .NOT. YEND  .AND.  YCHAR.EQ.' '  )THEN
          CALL NEXTCH
          GOTO 1
      END IF
C     C ON EXIT,  EITHER YEND.EQ.TRUE  OR  YCHAR.NE.' '
      END
      SUBROUTINE SKPRST
      INCLUDE 'global.f'
C GOES TO NEXT CONTROLCARD, FLAGGING SPURIOUS CHARACTERS
    1 IF(   .NOT. YEND  )THEN
          IF( YCHAR.NE.' ' )THEN
              CALL ERROR(23)
          END IF
          CALL NEXTCH
          GOTO 1
      END IF
      IF(CONTI.NE.0)CALL RDCON
      YEND= .FALSE.
      END
      LOGICAL FUNCTION CHECK(KEYWRD)
      INCLUDE 'global.f'
      CHARACTER  KEYWRD*(*)
      LOGICAL  KEYPRE
      INTEGER  SAVPTR
      CHARACTER *1  TEMP
      INTEGER I,L
C      THIS ROUTINE RETURNS  TRUE  WHENEVER  KEYWRD IS PRESNT
C      AT THE CURRENT POSITION IN THE CNTLIN
C      IF KEYWRD IS PRESNT, CNTRLP IS POSITIONED JUST
C      AFTER THE KEYWRD
C      IF KEYWRD IS ABSENT,  THE VALUE OF CNTRLP IS UNCHANGED
      KEYPRE= .TRUE.
      SAVPTR=CNTRLP
      L=LEN(KEYWRD)
      DO 1   I=1  ,  L
          IF(.NOT.( .NOT. YEND .AND. KEYPRE))GOTO 91
          TEMP=YCHAR
          CALL FLOWER(TEMP)
          KEYPRE=(TEMP.EQ.KEYWRD(I:I))
          CALL NEXTCH
    1 CONTINUE
   91 CONTINUE
      IF(   YEND .OR. .NOT. KEYPRE  )THEN
          CNTRLP=SAVPTR
          YCHAR=CNTLIN(CNTRLP)
          CALL ADJLET(YCHAR)
      END IF
      CHECK=( .NOT. YEND .AND. KEYPRE )
      END
      SUBROUTINE GETNUM(VALUE,FAILED)
      INCLUDE 'global.f'
      INTEGER VALUE
      LOGICAL FAILED
      LOGICAL DIGIT
C LOOKS FOR AN INTEGER AT THE CURRENT POSITION
C      FAILED.EQ.FALSE,WHEN ONE IS FOUND(VALUE  CONTAINS IT)
C      FAILED.EQ.TRUE,OTHERWISE(VALUE IS UNCHANGED)
      FAILED= .TRUE.
      IF(  DIGIT(YCHAR) )THEN
          FAILED= .FALSE.
          VALUE=  ICHAR(YCHAR)-  ICHAR('0')
          CALL NEXTCH
    1     IF(  .NOT. YEND  .AND.  DIGIT(YCHAR ))THEN
              VALUE=10 *VALUE + ICHAR(YCHAR)-  ICHAR('0')
              CALL NEXTCH
              GOTO 1
          END IF
      END IF
      END
      SUBROUTINE GTITEM(ITEM,SIZE,FAILED)
      INCLUDE 'global.f'
      INTEGER ITEM(0:MXWORD)
      INTEGER SIZE
      LOGICAL FAILED
C PUTS THE NEXT SEQUENCE OF NON-SPACE CHARACTERS INTO  ITEM.
C      FAILED.EQ.FALSE, WHEN END-OF-INFORMATION REACHED
C      FAILED.EQ.TRUE,  OTHERWISE. SIZE CONTAINS THE NUMBER OF
C      SYMBOLS PLACED IN  ITEM
    1 IF(   .NOT. YEND  .AND.  YCHAR.EQ.' '  )THEN
          CALL NEXTCH
          GOTO 1
      END IF
      SIZE=0
      FAILED=YEND
      IF(   .NOT. YEND  )THEN
    2     IF(  .NOT. YEND  .AND.  YCHAR.NE.' '  )THEN
              SIZE=SIZE+1
              ITEM(SIZE)=  ICHAR(YCHAR)
              CALL NEXTCH
              GOTO 2
          END IF
      END IF
      END
      SUBROUTINE SETITM(ITEM,SIZE,N)
      INCLUDE 'global.f'
      INTEGER ITEM(0:MXWORD)
      INTEGER SIZE,N
      LOGICAL PRESNT
      INTEGER LOC
C FINDS  ITEM  IN SYMTAB AND AMMENDS
C      THE INFORMATION FIELD.
      CALL WRDLOC(ITEM,SIZE,PRESNT,LOC)
      IF( PRESNT )THEN
          CALL SETFLD(LOC,N)
      ELSE
          CALL ERROR(24)
      END IF
      END
      SUBROUTINE GTLIST(N)
      INCLUDE 'global.f'
      INTEGER N
      INTEGER WORD(0: MXWORD+1)
      INTEGER SIZE
      LOGICAL FAILED
C GETS A SERIES OF WORDS FROM THE SPECIFICATION PART OF A
C      CONTROL STATEMENT AND SETS THE INFORMATION FIELD OF THE
C      SYMBOL TABLE ENTRY FOR THAT ITEM TO BE  N
      CALL GTITEM(WORD,SIZE,FAILED)
    1 IF(  .NOT. FAILED  )THEN
          CALL SETITM(WORD,SIZE,N)
          CALL GTITEM(WORD,SIZE,FAILED)
          GOTO 1
      END IF
      END
      LOGICAL FUNCTION MATCH(STR,S1,PAT,P1,VAR,DUM)
      INTEGER S1,P1
      INTEGER STR(0:S1),PAT(0:P1)
      INTEGER VAR,DUM
      INTEGER P,S,VARPTR,NEXTS
      LOGICAL AMATCH,NOMAT,VARFOU,BACKTR
C THIS ROUTINE IS TRUE ONLY WHEN THE  STR  MATCHES THE  PAT  .
C FOR EXAMPLE WHEN  VAR  IS    *    AND  DUM  IS  . (FULL STOP) WE HAVE
C      STRING        PATTERN      MATCHES?
C      =======       ========   =======
C      RUNNING         *ING         YES
C      RUNNING          R*G         YES
C      FRED            FRED         YES
C      FRED            ....         YES
C      FRED             ...          NO
C      *  MATCHES ANY SEQUENCE OF LETTERS (INCLUDING NONE)
C      .  MATCHES ANY GIVEN LETTER
      AMATCH=.FALSE.
      NOMAT=.FALSE.
      VARFOU=.FALSE.
      BACKTR=.FALSE.
      S=S1
      P=P1
    1 IF(.NOT.AMATCH .AND. .NOT.NOMAT)THEN
          IF(S.EQ.0) THEN
              IF(P.EQ.0)THEN
                  AMATCH=.TRUE.
              ELSE
                  IF(PAT(P).EQ.VAR) THEN
                      P=P-1
                  ELSE
                      NOMAT=.TRUE.
                  END IF
              END IF
          ELSE
              IF(P.EQ.0)THEN
                  BACKTR=.TRUE.
              ELSE
                  IF(PAT(P).EQ.VAR)THEN
                      IF(P.EQ.1)THEN
                          AMATCH=.TRUE.
                      ELSE
                          VARFOU=.TRUE.
                          P=P-1
                          VARPTR=P
                          NEXTS=S-1
                      END IF
                  ELSE
                      IF(STR(S).EQ.PAT(P) .OR.
     +                   PAT(P).EQ.DUM)THEN
                          S=S-1
                          P=P-1
                      ELSE
                          BACKTR=.TRUE.
                      END IF
                  END IF
              END IF
          END IF
          IF(BACKTR)THEN
              IF(VARFOU)THEN
                 P=VARPTR
                 S=NEXTS
                 NEXTS=NEXTS-1
                 BACKTR=.FALSE.
              ELSE
                 NOMAT=.TRUE.
              END IF
          END IF
          GOTO 1
      END IF
      MATCH=AMATCH
      END
      SUBROUTINE SETPAT(PATT,S1,VAR,DUM,N)
      INCLUDE 'global.f'
      INTEGER PATT(0:MXWORD)
      INTEGER S1,VAR,DUM,N
      LOGICAL PRESNT
      INTEGER REPLY
      INTEGER PATERN(0:MXWORD+1)
      INTEGER SIZE,I
      LOGICAL MATCH
      CALL APTMAP(STRAT,VAR,DUM,PATT,S1,PATERN,SIZE,REPLY)
C SETS THE INFORMATION FIELD OF EVERY ENTRY IN THE SYMTAB
C      THAT MATCHES THE GIVEN  PATERN
      PRESNT=.FALSE.
      IF( REPLY.NE.2 )THEN
C THE PATERN IS NOT EMPTY AFTER MAPPING, SO WE CAN USE IT
          DO 1   I = 0  , PTRTOP-1, ITSLOT
              IF(  SYMTAB(I+4).EQ. -1 )THEN
                  IF( MATCH(SYMTAB(SYMTAB(I)-1),
     +            SYMTAB(I+1)-SYMTAB(I)+1,
     +            PATERN,SIZE,VAR,DUM) )THEN
                      PRESNT=.TRUE.
                      CALL SETFLD(I,N)
                  END IF
              END IF
    1     CONTINUE
      END IF
      IF( .NOT. PRESNT )THEN
          CALL ERROR(25)
      END IF
      END
      SUBROUTINE GTPATS(VAR,DUM,N)
      INCLUDE 'global.f'
      INTEGER VAR,DUM,N
      INTEGER PATERN(0:MXWORD+1)
      INTEGER SIZE
      LOGICAL FAILED
C READS A SEQUENCE OF  PATTERNS  FROM THE SPECIFICATION
C      FIELD OF A CONTROL STATEMENT AND SETS THE INFORMATION FIELD
C      OF THE SYMBOL TABLE ENTRY ONLY IF IT MATCHES THE GIVEN PATTERN
      CALL GTITEM(PATERN,SIZE,FAILED)
    1 IF(   .NOT. FAILED  )THEN
          CALL SETPAT(PATERN,SIZE,VAR,DUM,N)
          CALL GTITEM(PATERN,SIZE,FAILED)
          GOTO 1
      END IF
      END
      LOGICAL FUNCTION TSTFRQ(X)
      INCLUDE 'global.f'
      INTEGER  X
      LOGICAL  B
      INTEGER  I
C TRUE ONLY WHEN THE FREQUENCY OF OCCURRENCE  X  SATISFIES
C      THE CONDITIONS HELD IN  EXPR
      B= .FALSE.
      I=1
   71 CONTINUE
      IF(FEXPR(I).LT.1 .OR. FEXPR(I).GT.4) GOTO 177
      GOTO (101,102,103,104), FEXPR(I)
  101 CONTINUE
      B= X.LT.VEXPR(I)
      GOTO 199
  102 CONTINUE
      B= X.EQ.VEXPR(I)
      GOTO 199
  103 CONTINUE
      B= X.GT.VEXPR(I)
      GOTO 199
  104 CONTINUE
      I = I+1
      B= X .GE. VEXPR(I-1)  .AND.  X .LE. VEXPR(I)
      GOTO 199
  177 CONTINUE
      CALL ERROR(-273)
      B=  .TRUE.
  199 CONTINUE
      I=I+1
      IF(.NOT.  B  .AND.  (I ) .LE. NEXPR)GOTO 71
      TSTFRQ=B
      END
      SUBROUTINE SETFRQ(N)
      INCLUDE 'global.f'
      INTEGER N
      INTEGER I
      LOGICAL TSTFRQ
C SETS THE INFORMATION FIELD OF EVERY TEXT WORD IN THE
C      SYMTAB WHICH SATISFIES THE  EXPR  CRITERIA
      DO 1   I = 0  , PTRTOP-1, ITSLOT
          IF(  SYMTAB(I+4).EQ. -1 )THEN
              IF( TSTFRQ(ABS(SYMTAB(I+2)))  )THEN
                  CALL SETFLD(I,N)
              END IF
          END IF
    1 CONTINUE
      END
      SUBROUTINE RDEXPR(FEXPR1,VEXPR1,NEXPR1)
      INCLUDE 'global.f'
      INTEGER FEXPR1(0:MXEXPR)
      INTEGER VEXPR1(0:MXEXPR)
      INTEGER NEXPR1
      INTEGER  I,J
      LOGICAL FAILED
      LOGICAL CHECK
C READS A FREQUENCY EXPRESSION FROM THE SPECIFICATION FIELD
C      OF A CONTROL STATEMENT AND PUTS A REPRESENTATION OF IT
C      INTO  EXPR .
C      EXAMPLES OF FREQUENCY EXPRESSION
C      ================================
C      5
C      <50
C      =100
C      >99
C      (5 TO 123)
C      5 OR (10 TO 64)
C      GENERAL FORM
C      ========================
C      EXPRESSION  =  TERMINAL,
C      EXPRESSION  =  TERMINAL 'OR' EXPRESSION
C      TERMINAL  =  INTEGER
C      TERMINAL  =  OPERATOR INTEGER.
C      TERMINAL  =  '(' INTEGER 'TO' INTEGER ')
C      OPERATOR  =  '<'
C      OPERATOR  =  '='
C      OPERATOR  =  '>'
      NEXPR1=0
      CALL NSNXTC
      FAILED=.FALSE.
    1 IF(   .NOT. YEND  .AND.  .NOT. FAILED  )THEN
          IF(  CHECK('<')  )THEN
              J=1
              CALL GETNUM(I,FAILED)
          ELSE IF(  CHECK('=')  )THEN
              J=2
              CALL GETNUM(I,FAILED)
          ELSE IF(  CHECK('>')  )THEN
              J=3
              CALL GETNUM(I,FAILED)
          ELSE IF(  CHECK('(')  )THEN
              J=4
              CALL NSNXTC
              CALL GETNUM(I,FAILED)
              IF( .NOT. FAILED )THEN
                  IF(       NEXPR1.LT.MXEXPR )THEN
                      NEXPR1=NEXPR1+1
                      FEXPR1(      NEXPR1)=J
                      VEXPR1(      NEXPR1)=I
                  END IF
                  CALL NSNXTC
                  IF( CHECK('to') )THEN
                      J=4
                      CALL NSNXTC
                      CALL GETNUM(I,FAILED)
                      CALL NSNXTC
                      FAILED= .NOT. CHECK(')')
                  END IF
              END IF
          ELSE
              CALL GETNUM(I,FAILED)
              IF( .NOT. FAILED )THEN
                  J=2
              ELSE
                  CALL ERROR(1)
                  CALL NEXTCH
              END IF
          END IF
          IF( .NOT. FAILED  )THEN
              IF(       NEXPR1.LT.MXEXPR )THEN
                  NEXPR1=NEXPR1+1
                  FEXPR1(      NEXPR1)=J
                  VEXPR1(      NEXPR1)=I
              END IF
          END IF
          CALL NSNXTC
          IF( YEND )THEN
              CONTINUE
          ELSE IF( CHECK('or') )THEN
              CALL NSNXTC
          ELSE
              CALL ERROR(1)
              FAILED=.TRUE.
          END IF
          GOTO 1
      END IF
      IF( FAILED )THEN
          NEXPR1=0
      END IF
      CALL SKPRST
      END
      SUBROUTINE GTFREQ(N)
      INCLUDE 'global.f'
      INTEGER N
C READS AN EXPRESSION FROM THE SPECIFICATION FIELD OF A CONTROL
C      STATEMENT INTO  EXPR  . THE ROUTINE THEN SETS THE INFORMATION
C      FIELD OF EVERY TEXT WORD IN THE SYMBOL TABLE WHOSE FREQUENCY
C      OF OCCURRENCE SATISFYS THE EXPRESSION
      CALL RDEXPR(FEXPR,VEXPR,NEXPR)
      IF(  NEXPR.GT.0  )THEN
          CALL SETFRQ(N)
      END IF
      END
      SUBROUTINE SETTAB(N)
      INCLUDE 'global.f'
      INTEGER N
      INTEGER I
C THE INFORMATION FIELD OF EVERY TEXT WORD IN THE SYMTAB
C      IS SET TO SOME VALUE  N
      DO 1   I = 0  ,PTRTOP-1,  ITSLOT
          IF( SYMTAB(I+4).EQ. -1 )THEN
              CALL SETFLD(I,N)
          END IF
    1 CONTINUE
      END
      SUBROUTINE FINDSM(SORTME,FAILED)
      INCLUDE 'global.f'
      INTEGER SORTME
      LOGICAL FAILED
      LOGICAL FINI
      LOGICAL CHECK
      FINI=.FALSE.
C ON EXIT  SORTME  CONTAINS THE SORTING CRITERION TO BE USED
      SORTSZ=MXSORT
      SORTME=SALPHA
      CALL NSNXTC
      FAILED= .FALSE.
    1 IF( .NOT. YEND .AND. .NOT.       FAILED .AND. .NOT. FINI )THEN
          IF(  CHECK('alpha')  )THEN
              SORTME=SALPHA
          ELSE IF(  CHECK('dalpha')  )THEN
              SORTME=SDALPH
          ELSE IF(  CHECK('afreq')  )THEN
              SORTME=SAFREQ
          ELSE IF(  CHECK('dfreq')  )THEN
              SORTME=SDFREQ
          ELSE IF(  CHECK('alength')  )THEN
              SORTME=SALENG
          ELSE IF(  CHECK('dlength')  )THEN
              SORTME=SDLENG
          ELSE IF(  CHECK('axlength')  )THEN
              SORTME=SAXLEN
          ELSE IF(  CHECK('dxlength')  )THEN
              SORTME=SDXLEN
          ELSE IF(  CHECK('revalpha')  )THEN
              SORTME=SRVALF
          ELSE IF(  CHECK('revdalpha') )THEN
              SORTME=STRVDA
          ELSE IF(  CHECK('first')  )THEN
              SORTME=SFIRST
          ELSE IF(  CHECK('last')   )THEN
              SORTME=SLAST
          ELSE IF(  CHECK('size') )THEN
              CALL NSNXTC
              CALL GETNUM(SORTSZ,FAILED)
              IF( SORTSZ.GT.MXSORT )THEN
                  SORTSZ=MXSORT
              END IF
          ELSE IF(  CHECK(',') )THEN
              CONTINUE
          ELSE
              FINI=.TRUE.
          END IF
          CALL NSNXTC
          GOTO 1
      END IF
      END
      SUBROUTINE FNDPAR(SORTME,LEFT,RIGHT,CISTYL,FROM,TO,
     +EX,WHERE,OFFSET,CONDEN,FAIL)
      INCLUDE 'global.f'
      INTEGER SORTME
      INTEGER LEFT
      INTEGER RIGHT
      INTEGER CISTYL
      INTEGER FROM
      INTEGER TO
      LOGICAL EX
      INTEGER WHERE
      INTEGER OFFSET
      INTEGER CONDEN
      LOGICAL FAIL
      INTEGER  VALUE
      CHARACTER *1  SAVYCH
      LOGICAL CHECK
      LOGICAL LETTER
C THIS ROUTINE FINDS OUT HOW THE CITATIONS ARE TO BE PRINTED
      SORTSZ=MXSORT
      SORTME=SALPHA
C ALPHABETIC SORTING (DEFAULT)
      LEFT=4
C DEFAULT NUMBER OF WORDS IN LEFT CONTEXT
      RIGHT=4
C DEFAULT NUMBER OF WORDS IN RIGHT CONTEXT
      CISTYL=CSTYLE
C CENTRALISED CITATIONS
      FROM=(-1)
C UNBOUNDED LFTCON
      TO=(-1)
C UNBOUNDED RHTCTX
      WHERE=1
      OFFSET=0
C CITE ABOUT  NODE+0
      NOREFS=.FALSE.
      CONDEN= 0
      FAIL= .FALSE.
      NINES=99
      NBWID=2
      NREFS=0
      REFOFF=1
      CALL NSNXTC
    1 IF(   .NOT. YEND  .AND.   .NOT.       FAIL  )THEN
          IF(  CHECK('alpha')  )THEN
              SORTME=SALPHA
          ELSE IF(  CHECK('dalpha')  )THEN
              SORTME=SDALPH
          ELSE IF(  CHECK('afreq')  )THEN
              SORTME=SAFREQ
          ELSE IF(  CHECK('dfreq')  )THEN
              SORTME=SDFREQ
          ELSE IF(  CHECK('alength') )THEN
              SORTME=SALENG
          ELSE IF(  CHECK('dlength') )THEN
              SORTME=SDLENG
          ELSE IF(  CHECK('axlength')  )THEN
              SORTME=SAXLEN
          ELSE IF(  CHECK('dxlength')  )THEN
              SORTME=SDXLEN
          ELSE IF(  CHECK('revalpha')  )THEN
              SORTME=SRVALF
          ELSE IF(  CHECK('revdalpha') )THEN
              SORTME=STRVDA
          ELSE IF(  CHECK('first')  )THEN
              SORTME=SFIRST
          ELSE IF(  CHECK('last')  )THEN
              SORTME=SLAST
          ELSE IF(  CHECK('size') )THEN
              CALL NSNXTC
              CALL GETNUM(SORTSZ,FAIL)
              IF( SORTSZ.GT.MXSORT )THEN
                  SORTSZ=MXSORT
              END IF
          ELSE IF(  CHECK(',') )THEN
              CONTINUE
          ELSE IF(  CHECK('cent')  )THEN
              CISTYL=CSTYLE
          ELSE IF(  CHECK('kwic')  )THEN
              CISTYL=CSTYLE
          ELSE IF(  CHECK('left')  )THEN
              CISTYL=LFTSTY
          ELSE IF(  CHECK('null')  )THEN
              CISTYL=NONSTY
          ELSE IF(  CHECK('condensed')  )THEN
              CONDEN= 1
          ELSE IF(  CHECK('fullcondensed')  )THEN
              CONDEN= 2
          ELSE IF(CHECK('count'))THEN
              CONDEN=1
          ELSE IF(CHECK('nocount'))THEN
              CONDEN=0
          ELSE IF(  CHECK('cite')  )THEN
C LOOK FOR   CITE SOMETHING
              CALL NSNXTC
              IF(  CHECK('from')  )THEN
C LOOK FOR  CITE FROMXTOY IN(EX)CLUSIVE
                  LEFT=100
                  RIGHT=100
C SOME LARGE EVEN VALUE
                  IF( YCHAR.EQ.' ' )THEN
                      CALL ERROR(19)
                      FAIL= .TRUE.
                  ELSE
                      FROM=ICHAR(YCHAR)
                      CALL NEXTCH
                      IF( CHECK('to')  )THEN
                          IF( YCHAR.EQ.' ' )THEN
                              CALL ERROR(19)
                              FAIL= .TRUE.
                          ELSE
                              TO= ICHAR(YCHAR)
                              CALL NEXTCH
                              IF( CHECK('exclusive') )THEN
                                  EX=.TRUE.
                              ELSE IF( CHECK('inclusive') )THEN
                                  EX=.FALSE.
                              ELSE
                                  CALL ERROR(1)
                                  FAIL= .TRUE.
                              END IF
                          END IF
                      ELSE
                          CALL ERROR(1)
                          FAIL=.TRUE.
                      END IF
                  END IF
              ELSE
C LOOK FOR  CITE N BY M
                  CALL GETNUM(LEFT,FAIL)
                  IF(       FAIL  )THEN
                      CALL ERROR(16)
                      CALL NEXTCH
                  ELSE
                      CALL NSNXTC
                      IF( .NOT. CHECK('by')  )THEN
                          FAIL= .TRUE.
                          CALL ERROR(1)
                      ELSE
                          CALL NSNXTC
                          CALL GETNUM(RIGHT,FAIL)
                          IF(       FAIL )THEN
                              CALL ERROR(16)
                          END IF
                      END IF
                  END IF
              END IF
      ELSE IF(  CHECK('about') )THEN
          CALL NSNXTC
          IF( CHECK('node') )THEN
              WHERE=1
          ELSE IF( CHECK('key') )THEN
              WHERE=1
          ELSE IF( CHECK('collocate') )THEN
              WHERE=2
          ELSE
              CALL ERROR(1)
              FAIL=.TRUE.
          END IF
          CALL NSNXTC
          IF( CHECK('+') )THEN
              CALL NSNXTC
              CALL GETNUM(OFFSET,FAIL)
              OFFSET=4  *     (       OFFSET)
              ELSE IF( CHECK('-') )THEN
                  CALL NSNXTC
                  CALL GETNUM(OFFSET,FAIL)
                  OFFSET= -4   *    (       OFFSET)
              ELSE
                  OFFSET=0
              END IF
          ELSE IF(  CHECK('refs')  )THEN
C LOOK FOR REFS LETTER NUMBER ...
              CALL NSNXTC
              IF(  .NOT.  LETTER(YCHAR))THEN
                  CALL ERROR(1)
                  CALL NEXTCH
                  FAIL= .TRUE.
              END IF
              IF(NREFER.EQ.0)THEN
                  CALL ERROR(26)
                  CALL NEXTCH
                  FAIL=.TRUE.
              END IF
              LMAX=0
    2         IF(.NOT.YEND  .AND. .NOT.FAIL .AND. LETTER(YCHAR))THEN
                  SAVYCH=YCHAR
                  CALL FUPPER(SAVYCH)
                  NREFS=NREFS+1
                  LTVREF(NREFS)=  ICHAR(SAVYCH)
                  CALL NEXTCH
                  CALL NSNXTC
                  IF(  YEND  )THEN
                      CALL ERROR(2)
                      FAIL=  .TRUE.
                  ELSE
                      CALL GETNUM(VALUE,FAIL)
                      IF(  .NOT.       FAIL  .AND.  VALUE.GT.8 )THEN
                          CALL ERROR(16)
                          FAIL= .TRUE.
                      END IF
                      IF( .NOT.       FAIL  .AND.  VALUE.EQ.0 )THEN
                          CALL ERROR(18)
                          FAIL= .TRUE.
                      END IF
                      IF(  SAVYCH.EQ.'L'  )THEN
                          NBWID=VALUE-1
                          T1=VALUE
                          IF(VALUE.GT.6)T1=6
                          NINES=1
                          DO 3 DUMMY=1,T1
                              NINES       =NINES*10
    3                     CONTINUE
                          NINES=NINES-1
                      END IF
                      LMAX=LMAX+VALUE
                      LEVREF(NREFS)=VALUE
                  END IF
                  CALL NSNXTC
                  GOTO 2
              END IF
          ELSE IF(  CHECK('norefs') )THEN
              NOREFS=.TRUE.
          ELSE IF( CHECK('wordrefs0') )THEN
              NREFS= -1
              REFOFF= -1
          ELSE IF( CHECK('wordrefs1') )THEN
              NREFS= -1
              REFOFF=0
          ELSE IF( CHECK('linerefs0') )THEN
              NREFS= -1
              REFOFF= -1
          ELSE IF( CHECK('linerefs1') )THEN
              NREFS= -1
              REFOFF=0
          ELSE
              FAIL=.TRUE.
          END IF
          CALL NSNXTC
          GOTO 1
      END IF
      IF( NREFS.LE.0 )THEN
          LMAX=8
      END IF
      END
      SUBROUTINE SETTYP(TYPE1,VALUE)
      INCLUDE 'global.f'
      INTEGER TYPE1(0:256)
      INTEGER VALUE
C READS THE SPECIFICATION PART OF A CONTROL STATEMENT AND
C      SETS THE CATAGORY TYPE OF ALL NON SPACE CHARACTERS TO BE  VALUE
      CALL NSNXTC
    1 IF(   .NOT. YEND  )THEN
          IF(  TYPE1(  ICHAR(YCHAR)).EQ.0  )THEN
              TYPE1(  ICHAR(YCHAR))=VALUE
          ELSE
              CALL ERROR(9)
          END IF
          CALL NEXTCH
          CALL NSNXTC
          GOTO 1
      END IF
      END
      SUBROUTINE SETLET(TYPE1,OK)
      INCLUDE 'global.f'
      INTEGER TYPE1(0:256)
      LOGICAL OK
      INTEGER  I
      LOGICAL UPPLET,LOWLET
      I=0
      OK= .TRUE.
C READS THE SPECIFICATION PART OF A CONTROL STATEMENT
C      AND SETS THE CATAGORY TYPE OF ALL NON SPACE CHARACTERS
C      TO BE  ZLETER  . IT ALSO SETS THE PRIMARY AND SECONDARY
C      RANKS OF THESE LETTERS SO THAT UPPER CASE LETTERS
C      ALWAYS PRECEED LOWER CASE ONES
      CALL NSNXTC
      IF( YEND )THEN
          CALL ERROR(10)
          OK= .FALSE.
      END IF
    1 IF(   .NOT. YEND  )THEN
          IF( TYPE1( ICHAR(YCHAR)).NE.0 )THEN
              CALL ERROR(9)
              OK= .FALSE.
          END IF
          IF( LOWLET( ICHAR( YCHAR)))THEN
              CALL FUPPER(YCHAR)
          END IF
          I=I+1
          PRRANK( ICHAR(YCHAR))=I
          TYPE1( ICHAR(YCHAR)) =ZLETER
          IF( UPPLET(ICHAR(YCHAR)) )THEN
              SECRNK(ICHAR(YCHAR))=1
              CALL FLOWER(YCHAR)
              PRRANK( ICHAR( YCHAR))=I
              SECRNK( ICHAR(YCHAR))=2
              TYPE1( ICHAR(YCHAR))=ZLETER
          END IF
          CALL NEXTCH
          CALL NSNXTC
          GOTO 1
      END IF
      END
      SUBROUTINE SETSPE(ZSTATE,TYPE1,OK)
      INCLUDE 'global.f'
      INTEGER ZSTATE
      INTEGER TYPE1(0:256)
      LOGICAL OK
      OK= .TRUE.
C READS THE SPECIFICATION PART OF A CONTROL STATEMENT AND
C      TURNS ON THE SPECAL STATUS OF ALL NON SPACE (LETTER) CHARACTERS
C      TO BE ZSTATE
      CALL NSNXTC
    1 IF(   .NOT. YEND  )THEN
          IF(  TYPE1(  ICHAR(YCHAR)).NE.ZLETER  )THEN
CTYPE1( YCHAR)=ZLETER
              CALL ERROR(22)
              OK= .FALSE.
          END IF
          SPECAL( ICHAR(YCHAR))=ZSTATE
          CALL NEXTCH
          CALL NSNXTC
          GOTO 1
      END IF
      END
      SUBROUTINE  SETREM(TYPE1,CODE,MESAGE)
      INCLUDE 'global.f'
      INTEGER TYPE1(0:256)
      INTEGER CODE
      CHARACTER MESAGE*(*)
      INTEGER I
C SETS THE CATAGORY TYPE OF ALL UNSPECIFIED CHARACTERS TO BE  CODE
      IF( SW4 )THEN
          CALL PUTSTR(MONFIL,'default')
          CALL STCNUM(MONFIL,GAP)
          CALL PUTSTR(MONFIL,MESAGE)
      END IF
      DO 1   I  =  0  ,  255
          IF( INWARD(I).EQ.256 )THEN
              TYPE1(I)=ZIGNOR
          END IF
          IF(  TYPE1(I).EQ.0  )THEN
              TYPE1(I)=CODE
              IF( SW4 )THEN
                  CALL PUTCH1(MONFIL, CHAR(I))
              END IF
          END IF
    1 CONTINUE
      IF( SW4 )THEN
          CALL PUTNL(MONFIL)
      END IF
      END
      SUBROUTINE CLOCIT
      INCLUDE 'global.f'
      LOGICAL  OK
      LOGICAL CARYON
      LOGICAL STREQ
C READS A SERIES OF CLOC CONTROL STATEMENTS WHICH TOGETHER DESCRIBE
C      HOW A TEXT WORD IS TO BE EXTRACTED FROM THE TEXT DATA.
C      THESE FORM THE CLOC ITEMIZING STRAT
      IF( SW3 )THEN
          CALL PRCON
      END IF
      IF(  STREQ(CTLFLD,QLETT ,15) )THEN
          CALL SETLET(TYPE,OK)
      ELSE
          CALL ERROR(1)
      END IF
      CALL SKPRST
      CARYON=.TRUE.
    1 IF(CARYON)THEN
          IF(  STREQ(CTLFLD,QSEPAR ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL SETTYP(TYPE,ZSEPAR)
          ELSE IF(  STREQ(CTLFLD,QIGNOR ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL SETTYP(TYPE,ZIGNOR)
          ELSE IF(  STREQ(CTLFLD,QPADD ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL SETSPE(ZPADD,TYPE,OK)
          ELSE IF(  STREQ(CTLFLD,QDEFER ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL SETSPE(ZDEFER,TYPE,OK)
          ELSE IF(  STREQ(CTLFLD,QRDASP ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL SETTYP(TYPE,ZRDASP)
          ELSE
              CARYON=.FALSE.
          END IF
          IF( CARYON )THEN
              CALL SKPRST
          END IF
          GOTO 1
      END IF
      CALL SETREM(TYPE,ZSEPAR,'*separators    ')
      END
      SUBROUTINE DCHTYP(TYPE1,PRANK,SRANK)
      INCLUDE 'global.f'
      INTEGER TYPE1(0:256)
      INTEGER PRANK(0:256)
      INTEGER SRANK(0:256)
      INTEGER I
C PRODUCES A NUMERIC DUMP OF THE CATAGORY TYPE, PRIMARY RANK,
C      AND SECONDARY RANK, OF EVERY CHARACTER IN THE CURRENT ALPHABET
      CALL PUTSTR(MONFIL,'      index        type  primaryrank')
      CALL PUTSTR(MONFIL,'   secondaryrank')
      CALL PUTNL(MONFIL)
      DO 1   I  =   0  ,   256
          CALL PUTINT(MONFIL,I,-16)
          CALL PUTINT(MONFIL,TYPE1(I),-16)
          CALL PUTINT(MONFIL,PRANK(I),-16)
          CALL PUTINT(MONFIL,SRANK(I),-16)
          CALL PUTNL(MONFIL)
    1 CONTINUE
      END
      SUBROUTINE INPDET(N,C,C1,C2,R,B,
     +AIGOPN,AIGCLO,AENDLN,ACOMOP,ACOCLO,AH,FAILED)
      INCLUDE 'global.f'
      INTEGER N
      INTEGER C
      INTEGER C1
      INTEGER C2
      LOGICAL R
      LOGICAL B
      INTEGER AIGOPN
      INTEGER AIGCLO
      INTEGER AENDLN
      INTEGER ACOMOP
      INTEGER ACOCLO
      CHARACTER *1 AH
      LOGICAL FAILED
      LOGICAL STREQ,CHECK
C THIS ROUTINE PARSES THE 'input details' CONTROL STATEMENT
      N=80
      C=(-1)
      C1=(-1)
      C2=(-1)
      R=.FALSE.
      B= .FALSE.
      AIGOPN=(-1)
      AIGCLO=(-1)
      AENDLN=(-1)
      ACOMOP=(-1)
      ACOCLO=(-1)
      HYPHEN=' '
      FAILED= .FALSE.
      IF(  STREQ(CTLFLD,QINDET ,15) )THEN
          IF( SW3 )THEN
              CALL PRCON
          END IF
          CALL NSNXTC
    1     IF(  .NOT. YEND  .AND.  .NOT.       FAILED  )THEN
              IF(  CHECK(',')  )THEN
                  CONTINUE
              ELSE IF(CHECK('endhyphen'))THEN
C PARSE ENDHYPHEN<CHAR>
                  HYPHEN=YCHAR
                  IF(YCHAR.EQ.' ')HYPHEN='-'
                  CALL NEXTCH
              ELSE IF(  CHECK('width')  )THEN
C PARSE WIDTH<NUMBER>
                  CALL NSNXTC
                  CALL GETNUM(N,FAILED)
                  IF(       FAILED )THEN
                      CALL ERROR(16)
                  END IF
                  IF(       N.GT.MXWDTH )THEN
                      CALL ERROR(6)
                      FAILED= .TRUE.
                  END IF
              ELSE IF(  CHECK('continue')  )THEN
C PARSE CONTINUE<CHAR>
                  CALL NSNXTC
                  IF(  TYPE(  ICHAR(YCHAR)).EQ.0  )THEN
                      C=  ICHAR(YCHAR)
                      TYPE(       C)=ZCONTI
                  ELSE
                      CALL ERROR(9)
                      FAILED= .TRUE.
                  END IF
                  CALL NEXTCH
              ELSE IF(  CHECK('newline')  )THEN
C PARSE NEWLINE<CHAR>
                  CALL NSNXTC
                  IF(  TYPE(  ICHAR(  YCHAR)).EQ.0  )THEN
                      AENDLN=  ICHAR(YCHAR)
                      TYPE(      AENDLN)=ZNL
                  ELSE
                      CALL ERROR(9)
                      FAILED= .TRUE.
                  END IF
                  CALL NEXTCH
              ELSE IF(  CHECK('runover')  )THEN
C PARSE RUNOVER
                  R= .TRUE.
              ELSE IF(  CHECK('refs')  )THEN
C PARSE REFS<CHAR1><CHAR2>
                  B= .TRUE.
                  CALL NSNXTC
                  C1=  ICHAR(YCHAR)
                  IF(  TYPE(       C1).NE.0  )THEN
                      CALL ERROR(9)
                      FAILED= .TRUE.
                  ELSE
                      TYPE(       C1)=ZREFSS
                  END IF
                  CALL NEXTCH
                  C2=  ICHAR(YCHAR)
                  IF(  YCHAR.EQ.' '  )THEN
                      CALL ERROR(19)
                  ELSE IF(  TYPE(       C2).NE.0  )THEN
                      CALL ERROR(9)
                  ELSE
                      TYPE(       C2)=ZRFEND
                  END IF
                  CALL NEXTCH
              ELSE IF(  CHECK('skip')  )THEN
C PARSE SKIP<CHAR1><CHAR2>
                  CALL NSNXTC
                  AIGOPN=  ICHAR(YCHAR)
                  IF(  TYPE(      AIGOPN).NE.0  )THEN
                      CALL ERROR(9)
                      FAILED= .TRUE.
                  ELSE
                      TYPE(      AIGOPN)=ZSKIPS
                  END IF
                  CALL NEXTCH
                  AIGCLO=  ICHAR( YCHAR)
                  IF(  YCHAR.EQ.' '  )THEN
                      CALL ERROR(19)
                  ELSE IF(  TYPE(      AIGCLO).NE.0  )THEN
                      CALL ERROR(9)
                  ELSE
                      TYPE(      AIGCLO)=ZSKEND
                  END IF
                  CALL NEXTCH
              ELSE IF(  CHECK('comment')  )THEN
C PARSE COMMENT<CHAR1><CHAR2>
                  CALL NSNXTC
                  ACOMOP=  ICHAR( YCHAR)
                  IF(  TYPE(      ACOMOP).NE.0  )THEN
                      CALL ERROR(9)
                      FAILED= .TRUE.
                  ELSE
                      TYPE(      ACOMOP)=ZOPENS
                  END IF
                  CALL NEXTCH
                  ACOCLO=  ICHAR(YCHAR)
                  IF(  YCHAR.EQ.' '  )THEN
                      CALL ERROR(19)
                  ELSE IF(  TYPE(      ACOCLO).NE.0  )THEN
                      CALL ERROR(9)
                  ELSE
                      TYPE(      ACOCLO)=ZOPEND
                  END IF
                  CALL NEXTCH
              ELSE
                  CALL ERROR(1)
                  CALL NEXTCH
                  FAILED= .TRUE.
              END IF
              CALL NSNXTC
              GOTO 1
          END IF
          CALL SKPRST
      ELSE IF(  SW4  )THEN
          CALL PUTSTR(MONFIL,'default')
          CALL STCNUM(MONFIL,GAP)
          CALL PUTSTR(MONFIL,'input details  width80')
          CALL PUTNL(MONFIL)
      END IF
      END
      SUBROUTINE AOUTDE(N,FAILED)
      INCLUDE 'global.f'
      INTEGER N
      LOGICAL FAILED
      LOGICAL STREQ,CHECK
C THIS ROUTINE PARSES THE 'output details' CONTROL STATEMENT
      N=LPWDTH
      FAILED= .FALSE.
      IF(  STREQ(CTLFLD,QOUTDT,15) )THEN
          IF( SW3 )THEN
              CALL PRCON
          END IF
          CALL NSNXTC
          IF(   .NOT. YEND  )THEN
              IF(  CHECK('width')  )THEN
                  CALL NSNXTC
                  CALL GETNUM(N,FAILED)
                  IF(       FAILED )THEN
                      CALL ERROR(16)
                  END IF
                  IF(       N.GT.LPWDTH )THEN
                      CALL ERROR(6)
                      FAILED= .TRUE.
                  END IF
              ELSE
                  CALL ERROR(1)
                  FAILED= .TRUE.
              END IF
              CALL NEXTCH
          END IF
          CALL SKPRST
      ELSE IF(  SW4  )THEN
          CALL PUTSTR(MONFIL,'default')
          CALL STCNUM(MONFIL,GAP)
          CALL PUTSTR(MONFIL,'output details width')
          CALL PUTINT(MONFIL,       N,0)
          CALL PUTNL(MONFIL)
      END IF
      END
      SUBROUTINE ITMUSE(OK)
      INCLUDE 'global.f'
      LOGICAL  OK
      LOGICAL STREQ,CHECK
      OK= .TRUE.
C THIS ROUTINE PARSES THE 'itemizeusing' CONTROL STATEMENT
      IF(  STREQ(CTLFLD,QITUSE,15) .OR.
     +STREQ(CTLFLD,QITUZE,15) )THEN
          IF( SW3 )THEN
              CALL PRCON
          END IF
          CALL NSNXTC
          IF(  YEND  .OR.  CHECK('cloc')  )THEN
              CALL NSNXTC
              STRAT=3
C MAKE THE DEFAULT  NOPADDING AND LOWERED
              IF( CHECK(',') )THEN
                  CALL NSNXTC
              END IF
              IF( CHECK('nopadding') )THEN
                  CALL NSNXTC
                  STRAT=2
                  IF( CHECK('and') )THEN
                      CALL NSNXTC
                  END IF
                  IF( CHECK('lowered') )THEN
                      STRAT=3
                  ELSE IF( CHECK('raised') )THEN
                      STRAT=4
                  END IF
              ELSE IF( CHECK('lowered') )THEN
                  CALL NSNXTC
                  STRAT=5
                  IF( CHECK('and') )THEN
                      CALL NSNXTC
                  END IF
                  IF( CHECK('nopadding') )THEN
                      STRAT=3
                  END IF
              ELSE IF( CHECK('raised') )THEN
                  CALL NSNXTC
                  STRAT=6
                  IF( CHECK('and') )THEN
                      CALL NSNXTC
                  END IF
                  IF( CHECK('nopadding') )THEN
                      STRAT=4
                  END IF
              ELSE IF( CHECK('unchanged') )THEN
                  STRAT=1
              ELSE
                  STRAT=3
C DEFAULT
              END IF
              CALL SKPRST
              CALL CLOCIT
          ELSE
              CALL ERROR(1)
              OK= .FALSE.
          END IF
      ELSE
          OK= .FALSE.
      END IF
      END
      SUBROUTINE SETDES(N)
      INCLUDE 'global.f'
      INTEGER N
      INTEGER  DUMMY,VARIAB
      LOGICAL OK
      LOGICAL CARYON
      LOGICAL STREQ,CHECK
      CARYON=.TRUE.
C READS A SERIES OF CLOC CONTROL STATEMENTS WHICH TOGETHER SPECIFY
C      A SUBSET OF WORDS OF INTEREST
    1 IF(CARYON)THEN
          IF(  STREQ(CTLFLD,QLWRDS ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL GTLIST(N)
          ELSE IF(  STREQ(CTLFLD,QPATT ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              OK= .TRUE.
              CALL NSNXTC
              DUMMY=  ICHAR('.')
              VARIAB=  ICHAR('*')
              IF(  CHECK('dummy')  )THEN
                  DUMMY=  ICHAR(YCHAR)
                  IF(  DUMMY.EQ.ASPACE  )THEN
                      CALL ERROR(19)
                      CALL SKPRST
                      OK= .FALSE.
                  ELSE
                      CALL NEXTCH
                      IF(  CHECK('variable')  )THEN
                          VARIAB=  ICHAR(YCHAR)
                          IF(  VARIAB.EQ.ASPACE  )THEN
                              CALL ERROR(19)
                              CALL SKPRST
                              OK= .FALSE.
                          ELSE IF(  DUMMY.EQ.VARIAB  )THEN
                              CALL ERROR(9)
                              CALL SKPRST
                              OK= .FALSE.
                          ELSE
                              CALL NEXTCH
                          END IF
                      ELSE
                          CALL ERROR(1)
                          CALL SKPRST
                          OK= .FALSE.
                      END IF
                  END IF
              END IF
              IF(  OK  )THEN
                  CALL GTPATS(VARIAB,DUMMY,N)
              END IF
          ELSE IF(  STREQ(CTLFLD,QFREQ ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL GTFREQ(N)
          ELSE
              CARYON=.FALSE.
          END IF
          IF( CARYON )THEN
              IF(CONTI.NE.0)CALL RDCON
              YEND= .FALSE.
          END IF
          GOTO 1
      END IF
      END
      SUBROUTINE WRDSEL
      INCLUDE 'global.f'
      REAL TIME,XCLOCK
      LOGICAL STREQ
      TIME=XCLOCK(1)
C DETERMINES WHICH PORTION OF THE VOCABULARY CLOC IS TO USE
      CALL SETTAB(NOWANT)
      IF(  STREQ(CTLFLD,QEVWRD ,15) )THEN
          IF( SW3 )THEN
              CALL PRCON
          END IF
          CALL SETTAB(WANT)
          CALL SKPRST
      ELSE IF(  STREQ(CTLFLD,QSEWRD ,15) )THEN
          IF( SW3 )THEN
              CALL PRCON
          END IF
          CALL SKPRST
          CALL SETDES(WANT)
      ELSE
          CALL SETTAB(WANT)
      END IF
    1 IF( STREQ(CTLFLD,QEXCLU,15) .OR.
     +STREQ(CTLFLD,QINCLU,15) )THEN
          IF( STREQ(CTLFLD,QEXCLU ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL SKPRST
              CALL SETDES(NOWANT)
          ELSE
C STREQ(CTLFLD,QINCLU,15)
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL SKPRST
              CALL SETDES(WANT)
          END IF
          GOTO 1
      END IF
      IF( SW5 )THEN
          CALL PUTSTR(MONFIL,'word selection time = ')
          CALL PTFLOT(MONFIL,XCLOCK(1)-TIME,15,8,2)
          CALL PUTSTR(MONFIL,' secs')
          CALL PUTNL(MONFIL)
          CALL PUTNL(MONFIL)
      END IF
      END
C AUXILLARY PROCEDURES
      SUBROUTINE CONC3(I,FREQ,LEFT,RIGHT,CISTYL,FROM,TO,EX,OFFSET)
      INCLUDE 'global.f'
      INTEGER I,FREQ,LEFT,RIGHT,CISTYL,FROM,TO
      LOGICAL EX
      INTEGER OFFSET
      INTEGER J,DUMMY
      J=ABS(SYMTAB(I+3))
C PRODUCES A CONCORDANCE OF THAT WORD
C      WHOSE CODE NUMBER (I.E. SYMBOL TABLE ADDRESS) IS  I
      IF(CISTYL.EQ.NONSTY)RETURN
      DO 1 DUMMY =1,  FREQ
          CALL PRCITE(J+OFFSET,LEFT,RIGHT,CISTYL,FROM,TO,EX)
          CALL GETBN1(ITEXT,J+1,J)
    1 CONTINUE
      CALL PUTNL(RESULT)
      END
      SUBROUTINE  PRCOOC(STRTND,MSPAN2,NSPAN2,LEFT,RIGHT,
     +CISTYL,FROM,TO,EX,WHERE,OFFSET,CODCOL,FRQCOL)
      INCLUDE 'global.f'
      INTEGER  STRTND,MSPAN2,NSPAN2,LEFT,RIGHT,CISTYL,FROM,TO
      LOGICAL EX
      INTEGER WHERE, OFFSET
      INTEGER CODCOL
      INTEGER FRQCOL
      INTEGER  I,J1,J,NPRINT
C PRINTS THE CITATIONS OF THE COLLOCATIONS OF THOSE
C      COLLOCATES THAT ARE IN THE  COLLOCATE  TABLE
      IF(CISTYL.EQ.NONSTY)RETURN
      I=STRTND
      J= ABS(SYMTAB(CODCOL+3))
      NPRINT=FRQCOL
    1 IF(NPRINT.NE.0)THEN
    2     IF(  J.LT.I-MSPAN2  )THEN
              CALL GETBN1(ITEXT,J+1,J)
              GOTO 2
          END IF
          J1=J
    3     IF(  J.LE.I+NSPAN2  .AND.  J.NE.0  )THEN
              IF(  I.NE.J  )THEN
                  IF( WHERE.EQ.1 )THEN
C CITE ABOUT THE  NODE
                      CALL PRCITE(I+OFFSET,LEFT,RIGHT,CISTYL,
     +                                FROM,TO,EX)
                  ELSE
                      CALL PRCITE(J+OFFSET,LEFT,RIGHT,CISTYL,
     +                                FROM,TO,EX)
                  END IF
                  NPRINT =NPRINT- 1
              END IF
              CALL GETBN1(ITEXT,J+1,J)
              GOTO 3
          END IF
          J=J1
          IF( NPRINT.GT.0 )THEN
              CALL GETBN1(ITEXT,I+1,I)
          END IF
          GOTO 1
      END IF
      CALL PUTNL(RESULT)
      END
      SUBROUTINE PRREF(F,START)
      INCLUDE 'global.f'
      INTEGER F,START
      INTEGER  T1,T2,T3
      INTEGER I,J
C PRINTS ONTO THE FILE  F  THE TEXT REFERENCES ASSOCIATED.
C      WITH THE POSITION  START  IN THE FILE  ITEXT
      CALL GETBN1(ITEXT,START-1,T3)
C RECORD NUMBER PLACED IN T3
      IF( NREFS.EQ.0  )THEN
          CALL PUTINT(F,T3+REFOFF,0)
      ELSE IF( NREFS.LT.0 )THEN
          CALL PUTINT(F,(START+1)/4 + REFOFF,0)
      ELSE
          DO 1  I=1, NREFER
              IF( .NOT.(LREFER(I).LT.START)  )GOTO 91
              CALL SLICE(J,TXTRFS(0,PREFER(I)),0,TREFER(0,I),0,RMAX)
    1     CONTINUE
   91     CONTINUE
          DO 2  I  =1,  NREFS
              IF( I.NE.1 )THEN
                  CALL PUTCH1(F,' ')
              END IF
              T1=LTVREF(I)
              T2=LEVREF(I)
              IF( T1.EQ. ICHAR('L') )THEN
                  T1=T1-ICHAR('A')+1
                  T3=TXTRFS(1,T1)+T3
    3             IF( T3.GT.NINES )THEN
                      NBWID=NBWID+1
                      NINES=10*NINES+9
                      GOTO 3
                  END IF
C T3 IS THE NUMBER WE WISH TO PRINT
C      T2 IS THE USER SUPPLIED FLDWTH
C      NBWID IS THE MAXIMUM FLDWTH ACTUALLY FOUND
                  IF( T2.LT.NBWID )THEN
                      T2=NBWID
                  END IF
                  CALL PUTINT(F,T3,-T2)
              ELSE
                  T1=T1-ICHAR('A')+1
                  DO 4  J  =1,  T2
                      CALL PUTCH1(F,CHAR( TXTRFS(J,T1)))
    4             CONTINUE
              END IF
    2     CONTINUE
      END IF
      END
      SUBROUTINE PRREFS(F,START)
      INCLUDE 'global.f'
      INTEGER F,START
C PRINTS THE TEXT REFERENCE FOR LINE NUMBER ITEXT(START+3)
      IF( NOREFS )THEN
          CONTINUE
      ELSE
          CALL PRREF(F,START)
          CALL PUTCH1(F,' ')
          IF( NREFS.LE.0 )THEN
              CALL STCNUM(F,9)
          END IF
      END IF
      END
      SUBROUTINE PRCITE(I,LEFT,RIGHT,CISTYL,FROM,TO,EX)
      INCLUDE 'global.f'
      INTEGER I,LEFT,RIGHT,CISTYL,FROM,TO
      LOGICAL EX
      INTEGER  LOW,HIGH
      INTEGER T1,T2,T3
      INTEGER LFTLIM,RHTLIM
      INTEGER J,K,DUMMY
C PRINTS A CITATION FOR THE CODE NUMBER (I.E. SYMBOL TABLE ADDRESS)
C      CONTAINED IN  I  . EACH CITATION WILL BE PREFIXED BY A
C      (POSSIBLY EMPTY) TEXT REFERENCE
      IF( NOREFS )THEN
          CONTINUE
      ELSE
          CALL PRREF(RESULT,I)
          CALL PUTCH1(RESULT,' ')
          IF( NREFS.LE.0 )THEN
              CALL STCNUM(RESULT,9)
          END IF
      END IF
      LOW=I-LEFT
      IF( LOW.LT.3 )THEN
          LOW=1
      END IF
      HIGH=I+RIGHT
      IF( HIGH.GT.NXTITX-4 )THEN
          HIGH=NXTITX-2
      END IF
      T1=(LPWDTH-CHRNUM(RESULT))/2
C LOCATE CENTRE OF REST OF PAGE
      CENTRE=CHRNUM(RESULT)+T1
      RHTLIM=LPWDTH-CENTRE
      LFTLIM=LPWDTH-CHRNUM(RESULT)-RHTLIM
      RHTPTR=0
      DO 1   J=  I  ,HIGH,  2
          CALL GETBN1(ITEXT,J,T1)
          T2=SYMTAB(T1+1)
          T3=SYMTAB(T1)
          IF(  T2-T3+1+RHTPTR .GT. RHTLIM  )THEN
              GOTO 71
          END IF
          DO 2   K  =  T3  ,  T2
              T1=SYMTAB(K)
              RHTPTR=RHTPTR+1
              RHTCTX(RHTPTR)=T1
              IF( T1.EQ.TO )THEN
                  IF( EX )THEN
                      RHTPTR=RHTPTR-1
                  END IF
                  GOTO 71
              END IF
    2     CONTINUE
    1 CONTINUE
   71 LFTPTR=0
      IF(  CISTYL.EQ.LFTSTY  )THEN
          LFTLIM=LFTLIM+(RHTLIM-RHTPTR)
      END IF
      DO 3   J=  I-2  , LOW, -2
          CALL GETBN1(ITEXT,J,T1)
          T2=SYMTAB(T1+1)
          T3=SYMTAB(T1)
          IF(  T2-T3+1+LFTPTR .GT. LFTLIM  )THEN
              GOTO 72
          END IF
          DO 4   K  =  T2  , T3,-1
              T1=SYMTAB(K)
              LFTPTR=LFTPTR+1
              LFTCON(LFTPTR)=T1
              IF( T1.EQ.FROM )THEN
                  IF( EX )THEN
                      LFTPTR=LFTPTR-1
                  END IF
                  GOTO 72
              END IF
    4     CONTINUE
    3 CONTINUE
   72 IF( CISTYL.EQ.CSTYLE  )THEN
          DO 5  DUMMY =LFTPTR+1,LFTLIM
              CALL PUTCH1(RESULT,' ')
    5     CONTINUE
      END IF
      DO 6   J  =  LFTPTR  , 1, -1
          CALL PUTCH1(RESULT, CHAR( LFTCON(J)) )
    6 CONTINUE
      DO 7   J  =1,  RHTPTR
          CALL PUTCH1(RESULT, CHAR( RHTCTX(J)))
    7 CONTINUE
      CALL PUTNL(RESULT)
      END
C   TASK PROCEDURES PLACED HERE
      SUBROUTINE TSTSRT
      INCLUDE 'global.f'
      INTEGER ASORTM
      LOGICAL FAILED
      INTEGER TOTAL,TAPOUT
C PARSES THE 'testsort' CONTROL STATEMENT. THIS DOES A SORT/MERGE
C      WITHOUT PRINTING THE SORTED WORDS. IT IS USED, IN CONJUNCTION
C      WITH  SW5  , TO TEST THE SPEED OF THE SORTING PROCESS
      CALL FINDSM(ASORTM,FAILED)
      IF( FAILED )THEN
          CALL ERROR(1)
      END IF
      CALL SKPRST
      IF( .NOT. FAILED )THEN
          CALL SRTMER(ASORTM,TAPOUT,TOTAL)
      END IF
      END
      SUBROUTINE TXTPRT
      INCLUDE 'global.f'
      INTEGER CURLIN
      INTEGER I
C THIS ROUTINE WILL PRINT OUT THE ORIGINAL TEXT FILE FROM THE
C      INTERNAL REPRESENTATION
      INTEGER ASORTM,LEFT,RIGHT,CISTYL,FROM,TO
      INTEGER CONDEN
      LOGICAL EX,FAILED
      INTEGER TEMP
      INTEGER WHERE,OFFSET
      CALL FNDPAR(ASORTM,LEFT,RIGHT,CISTYL,FROM,TO,EX,
     +WHERE,OFFSET,CONDEN,FAILED)
      CALL SKPRST
      CALL PUTSTR(RESULT,'text data from the writetext command')
      CALL UNDERL(RESULT,'=')
      CALL GETBN1(ITEXT,2,CURLIN)
      CALL PRREFS(RESULT,3)
      CALL GETBN1(ITEXT,1,TEMP)
      CALL PRITEM(RESULT,TEMP)
C 1ST SEPARATOR
      DO 1  I= 2 ,NXTITX-5, 4
          CALL GETBN1(ITEXT,I+1,TEMP)
          CALL PRITEM(RESULT,TEMP)
C TEXT WORD
          CALL GETBN1(ITEXT,I+3,TEMP)
          CALL PRITEM(RESULT,TEMP)
C SEPARATOR
          CALL GETBN1(ITEXT,I+4,TEMP)
          IF( TEMP.NE.CURLIN )THEN
              CALL PUTNL(RESULT)
              CALL GETBN1(ITEXT,I+4,CURLIN)
              IF( I+5.LT.NXTITX )THEN
                  CALL PRREFS(RESULT,I+5)
              END IF
          END IF
    1 CONTINUE
      CALL PUTNL(RESULT)
      CALL PUTNL(RESULT)
      END
      SUBROUTINE WRDLST
      INCLUDE 'global.f'
      INTEGER  ASORTM
      LOGICAL  FAILED
      INTEGER  TOTAL,TAPOUT
C PARSES THE 'wordlist' CONTROL STATEMENT. THE CHOSEN VOCABULARY SET
C      IS SORT/MERGED INTO CALL ORDER AND PRINTED OUT
      CALL FINDSM(ASORTM,FAILED)
      IF( FAILED )THEN
          CALL ERROR(1)
      END IF
      CALL SKPRST
      IF(   .NOT. FAILED  )THEN
          CALL SRTMER(ASORTM,TAPOUT,TOTAL)
          CALL PRWRDS(TAPOUT,TOTAL,MXLENG+6,ASORTM)
      END IF
      END
      SUBROUTINE WRDIND(TAPE,N,FLDWTH,METHOD)
      INCLUDE 'global.f'
      INTEGER TAPE,N,FLDWTH,METHOD
      INTEGER DUMMY
C PRODUCES A WORD INDEX FOR ALL WORDS ON THE TAPE
      INTEGER WRDDAT(0: MXWORD+6)
      INTEGER MXWID1
      INTEGER POSITN
      LOGICAL NULL
      MXWID1=LPWDTH-LMAX
      CALL PUTSTR(RESULT,'index of ')
      CALL PUTINT(RESULT,N,0)
      CALL PUTSTR(RESULT,' words in ')
      CALL PTMETH(RESULT,METHOD)
      CALL UNDERL(RESULT,'=')
      CALL REREAD(TAPE)
      DO 1 DUMMY =1, N
          CALL GETIN(TAPE,WRDDAT,MXLENG+5,NULL)
          CALL PRFRWD(RESULT,WRDDAT(1),1)
          POSITN= ABS(SYMTAB(WRDDAT(1)+3))
          IF( NOREFS )THEN
              CONTINUE
          ELSE
              CALL STCNUM(RESULT,FLDWTH)
   99         CONTINUE
              IF( CHRNUM(RESULT).GT.MXWID1 )THEN
                  CALL PUTNL(RESULT)
                  CALL STCNUM(RESULT,FLDWTH)
              END IF
              CALL PRREF(RESULT,POSITN)
              CALL GETBN1(ITEXT,POSITN+1,POSITN)
              IF( POSITN.NE.0 )THEN
                  CALL PUTSTR(RESULT,', ')
              END IF
                    IF(POSITN.NE.0)GOTO 99
          END IF
          CALL PUTNL(RESULT)
    1 CONTINUE
      CALL PUTNL(RESULT)
      END
      SUBROUTINE INDEX
      INCLUDE 'global.f'
      INTEGER  ASORTM
      LOGICAL  FAILED
      INTEGER LEFT,RIGHT
      INTEGER CISTYL,FROM,TO
      INTEGER CONDEN
      LOGICAL  EX
      INTEGER  TOTAL,TAPOUT
      INTEGER  WHERE,OFFSET
C PARSES THE 'index' CONTROL STATEMENT. THIS PRODUCES A LIST OF
C      WORDS EACH PREFIXED WITH ITS FREQUENCY OF OCCURRENCE AND
C      FOLLOWED BY A LIST OF REFERENCES TO WHERE IT OCCURS
C      IN THE TEXT
      CALL FNDPAR(ASORTM,LEFT,RIGHT,CISTYL,FROM,TO,EX,
     +WHERE,OFFSET,CONDEN,FAILED)
      IF( FAILED )THEN
          CALL ERROR(1)
      END IF
      CALL SKPRST
      IF(   .NOT. FAILED  )THEN
          CALL SRTMER(ASORTM,TAPOUT,TOTAL)
          CALL WRDIND(TAPOUT,TOTAL,MXLENG+8,ASORTM)
      END IF
      END
      SUBROUTINE PROFFS(OFFSET)
      INCLUDE 'global.f'
      INTEGER OFFSET
      IF( OFFSET.NE.0 )THEN
          CALL PUTSTR(RESULT,' (cited about node')
          IF( OFFSET.GT.0 )THEN
              CALL PUTCH1(RESULT,'+')
          END IF
          CALL PUTINT(RESULT,OFFSET/4,0)
          CALL PUTCH1(RESULT,')')
      END IF
      END
      SUBROUTINE ACONC3
      INCLUDE 'global.f'
      INTEGER  ASORTM
      LOGICAL  FAILED
      INTEGER  LEFT,RIGHT
      INTEGER WRDDAT(0:MXWORD+6)
      LOGICAL NULL
      INTEGER  TOTAL,TAPOUT
      INTEGER CONDEN
      LOGICAL EX
      INTEGER  CISTYL,FROM,TO
      INTEGER  WHERE,OFFSET
      INTEGER DUMMY
C PARSES THE 'concordance' CONTROL STATEMENT. PRODUCES A CONCORDANCE
C      OF EACH WORD IN THE CHOSEN VOCABULARY
      CALL FNDPAR(ASORTM,LEFT,RIGHT,CISTYL,FROM,TO,EX,
     +WHERE,OFFSET,CONDEN,FAILED)
      RIGHT       =RIGHT*4
      LEFT       =LEFT*4
      CALL SKPRST
      IF(   .NOT. FAILED  )THEN
          CALL SRTMER(ASORTM,TAPOUT,TOTAL)
          CALL PUTSTR(RESULT,'concordance of ')
          CALL PUTINT(RESULT,TOTAL,0)
          CALL PUTSTR(RESULT,' nodes')
          CALL PROFFS(OFFSET)
          CALL UNDERL(RESULT,'=')
          CALL REREAD(TAPOUT)
          DO 1 DUMMY =1, TOTAL
              CALL GETIN(TAPOUT,WRDDAT,MXLENG+5,NULL)
              CALL PUTSTR(RESULT,'node  ')
              CALL PRWORD(RESULT,WRDDAT(1),1)
              CALL PUTSTR(RESULT,'  occurs ')
              CALL PUTINT(RESULT,WRDDAT(2),0)
              CALL PUTSTR(RESULT,' times')
              CALL PUTNL(RESULT)
              CALL CONC3(WRDDAT(1),WRDDAT(2),LEFT,RIGHT,
     +        CISTYL,FROM,TO,EX,OFFSET)
    1     CONTINUE
      END IF
      END
      SUBROUTINE ADDLST(CODE1)
      INCLUDE 'global.f'
      INTEGER CODE1
      LOGICAL  IN
      INTEGER I
      IN=.FALSE.
C TESTS IF  CODE1  ALREADY EXISTS IN  LIST2  IF IT DOES
C      THEN INCREASE ITS FREQUENCY COUNT OTHERWISE PUT IT INTO  LIST2
C      AND SET ITS FREQUENCY COUNT TO 1
      DO 1   I =1 ,  NLIST
          IF(IN)GOTO 91
          IF(  CODE1.EQ.CODLS2(I)  )THEN
              FRQLS2(I) =FRQLS2(I)+ 1
              IN= .TRUE.
          END IF
    1 CONTINUE
   91 CONTINUE
      IF(   .NOT. IN  .AND.  NLIST.LT. MXCOLL  )THEN
          NLIST=NLIST+1
          CODLS2(NLIST)=CODE1
          FRQLS2(NLIST)=1
      END IF
      END
      SUBROUTINE ACOLAB(POSITN,SPNCNT)
      INCLUDE 'global.f'
      INTEGER POSITN,SPNCNT
C POSITN IS 1ST OCCURRENCE OF WORD IN ITEXT
      INTEGER  K,LOW,HIGH,TEMP,I
      SPNCNT=0
      K=POSITN
C FINDS ALL THE COLLOCATES OF A WORD WHICH STARTS AT
C      LOCAT  POSITN  IN ITEXT
   99 CONTINUE
      LOW=K-LFTSPN
      IF( LOW.LT.3 )THEN
          LOW=3
      END IF
      HIGH=K+RHTSPN
      IF( HIGH.GT.NXTITX-4 )THEN
          HIGH=NXTITX-4
      END IF
      DO 1   I  =  LOW  ,K-4,  4
          CALL GETBN1(ITEXT,I,TEMP)
          IF( SYMTAB(TEMP+4).GE.0 )THEN
              TEMP=SYMTAB(TEMP+4)
          END IF
          CALL ADDLST(TEMP)
    1 CONTINUE
      DO 2  I  =  K+4  , HIGH, 4
          CALL GETBN1(ITEXT,I,TEMP)
          IF( SYMTAB(TEMP+4).GE.0 )THEN
              TEMP=SYMTAB(TEMP+4)
          END IF
          CALL ADDLST(TEMP)
    2 CONTINUE
      SPNCNT=SPNCNT+(HIGH-LOW+4)/4-1
      CALL GETBN1(ITEXT,K+1,K)
      IF(K.NE.0)GOTO 99
      END
      SUBROUTINE ACOL2(NODE,POSITN,SPNCNT)
      INCLUDE 'global.f'
      INTEGER NODE,POSITN,SPNCNT
C NODE IS THE CODE NUMBER OF THE NODE
C POSITN IS 1ST OCCURRENCE OF WORD IN ITEXT
      INTEGER  K,LOW,HIGH,TEMP,I,PREVK
      SPNCNT=0
      K=POSITN
      PREVK=K-RHTSPN-LFTSPN-4
C FINDS ALL THE COLLOCATES OF A WORD WHICH STARTS AT
C      LOCAT  POSITN  IN ITEXT
   99 CONTINUE
      IF(PREVK+RHTSPN.GE.K-LFTSPN)THEN
          IF(PREVK.GE.K-LFTSPN)THEN
              LOW=K
          ELSE
              LOW=PREVK+RHTSPN+4
          ENDIF
      ELSE
          LOW=K-LFTSPN
      END IF
      IF( LOW.LT.3 )THEN
          LOW=3
      END IF
      HIGH=K+RHTSPN
      IF( HIGH.GT.NXTITX-4 )THEN
          HIGH=NXTITX-4
      END IF
      DO 1   I  =  LOW  ,K-4,  4
          CALL GETBN1(ITEXT,I,TEMP)
          IF( SYMTAB(TEMP+4).GE.0 )THEN
              TEMP=SYMTAB(TEMP+4)
          END IF
          CALL ADDLST(TEMP)
    1 CONTINUE
      DO 2  I  =  K+4  , HIGH, 4
          CALL GETBN1(ITEXT,I,TEMP)
          IF( SYMTAB(TEMP+4).GE.0 )THEN
              TEMP=SYMTAB(TEMP+4)
          END IF
          IF(TEMP.EQ.NODE)GOTO 3
          CALL ADDLST(TEMP)
    2 CONTINUE
      I=HIGH+4
    3 CONTINUE
      SPNCNT=SPNCNT+(I-LOW)/4-1
      PREVK=K
      CALL GETBN1(ITEXT,K+1,K)
           IF(K.NE.0)GOTO 99
      END
      SUBROUTINE IMPLAT
      INCLUDE 'global.f'
C USED WHEN IMPLEMENTING A NEW TASK
      CALL PRCON
      CALL MARGIN
      CALL PUTSTR(MONFIL,'***not yet available')
      CALL PUTNL(MONFIL)
      CALL RDCON
      END
      SUBROUTINE COLLOC
      INCLUDE 'global.f'
      INTEGER ANOFSE
      INTEGER ASORTM
      LOGICAL FAILED
      INTEGER  LEFT,RIGHT
      INTEGER WRDDAT(0: MXWORD+6)
      LOGICAL NULL
      INTEGER  CISTYL,FROM,TO
      INTEGER  FSTPOS
      INTEGER SPNCNT,CONDEN
      LOGICAL  EX
      INTEGER  TOTAL,TAPOUT
      INTEGER  WHERE, OFFSET
      LOGICAL  RESTRI
      INTEGER DUMMY,I,K
      REAL SPANC0
      CHARACTER *10 SPANCF
      LOGICAL STREQ,CHECK
      LOGICAL TSTFRQ
C PARSES THE 'collocations' CONTROL STATEMENT AND ALSO A SERIES
C      OF STATEMENTS WHICH FOLLOW IT. CAUSES A COLLOCATION ANALYSIS
C      TO BE PERFORMED AND THE RESULT TO BE PRINTED OUT
      ANOFSE=4
      IF(MXLENG.GT.ANOFSE)ANOFSE=MXLENG
      RESTRI=.FALSE.
      CALL FNDPAR(ASORTM,LEFT,RIGHT,CISTYL,FROM,TO,EX,
     +WHERE,OFFSET,CONDEN,FAILED)
      LEFT       =LEFT*4
      RIGHT       =RIGHT*4
      CALL SKPRST
      LFTSPN=4
      RHTSPN=4
      IF(  STREQ(CTLFLD,QSPAN ,15) )THEN
          IF( SW3 )THEN
              CALL PRCON
          END IF
          CALL NSNXTC
          IF(   .NOT. YEND  )THEN
              CALL GETNUM(LFTSPN,FAILED)
              IF(   .NOT. FAILED  )THEN
                  CALL NSNXTC
                  IF(  CHECK('by')  )THEN
                      CALL NSNXTC
                      CALL GETNUM(RHTSPN,FAILED)
                  END IF
              END IF
          END IF
          IF(.NOT.FAILED)THEN
              CALL NSNXTC
              IF(CHECK('restricted'))THEN
                  RESTRI=.TRUE.
              ELSE IF(CHECK('unrestricted'))THEN
                  RESTRI=.FALSE.
              ELSE
                  RESTRI=.FALSE.
              END IF
          END IF
          CALL SKPRST
      ELSE IF(  SW4  )THEN
          CALL PUTSTR(MONFIL,'default')
          CALL STCNUM(MONFIL,GAP)
          CALL PUTSTR(MONFIL,'*span          4 by 4')
          CALL PUTNL(MONFIL)
      END IF
      IF(  STREQ(CTLFLD,QFREQ ,15) )THEN
          IF( SW3 )THEN
              CALL PRCON
          END IF
          CALL RDEXPR(FEXPR,VEXPR,NEXPR)
      ELSE
          FEXPR(1)=3
          VEXPR(1)=1
          NEXPR=1
          IF(  SW4  )THEN
              CALL PUTSTR(MONFIL,'default')
              CALL STCNUM(MONFIL,GAP)
              CALL PUTSTR(MONFIL,'*frequency     >1')
              CALL PUTNL(MONFIL)
          END IF
      END IF
      IF(STREQ(CTLFLD,QEVCOL,15))THEN
          IF( SW3 )THEN
              CALL PRCON
          END IF
          CALL SKPRST
          CALL SETTAB(NOTREJ)
      ELSE IF(STREQ(CTLFLD,QSECOL,15))THEN
          IF( SW3 )THEN
              CALL PRCON
          END IF
          CALL SKPRST
          CALL SETTAB(REJECT)
          CALL SETDES(NOTREJ)
      ELSE
          CALL SETTAB(NOTREJ)
      ENDIF
    6 CONTINUE
      IF( STREQ(CTLFLD,QACCEP,15).OR.STREQ(CTLFLD,QREJEC,15))THEN
          IF(  STREQ(CTLFLD,QACCEP ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL SKPRST
              CALL SETDES(NOTREJ)
          END IF
          IF(  STREQ(CTLFLD,QREJEC ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
              CALL SKPRST
              CALL SETDES(REJECT)
          END IF
          GOTO 6
      END IF
      LFTSPN       =LFTSPN*4
      RHTSPN       =RHTSPN*4
      IF(   .NOT. FAILED  )THEN
          CALL SRTMER(ASORTM,TAPOUT,TOTAL)
          IF( CONDEN.EQ.1 )THEN
              CALL PUTSTR(RESULT,'condensed ')
          ELSEIF( CONDEN.EQ.2 )THEN
              CALL PUTSTR(RESULT,'full condensed ')
          END IF
          CALL PUTSTR(RESULT,'collocation analysis of ')
          CALL PUTINT(RESULT,TOTAL,0)
          CALL PUTSTR(RESULT,' nodes')
          IF( CONDEN.EQ.0 )THEN
              CALL PUTSTR(RESULT,' (cited about ')
              IF( WHERE.EQ.1 )THEN
                  CALL PUTSTR(RESULT,'node')
              ELSE
                  CALL PUTSTR(RESULT,'collocate')
              END IF
              IF( OFFSET.NE.0 )THEN
                  IF( OFFSET.GT.0 )THEN
                      CALL PUTCH1(RESULT,'+')
                  END IF
                  CALL PUTINT(RESULT,OFFSET/4,0)
              END IF
              CALL PUTSTR(RESULT,')')
          END IF
          CALL UNDERL(RESULT,'=')
          IF(CONDEN.EQ.2)THEN
              CALL PUTSTR(RESULT,'* text has ')
              CALL PUTINT(RESULT,RUNWC,0)
              CALL PUTSTR(RESULT,' running words, ')
              CALL PUTINT(RESULT,DISTWC,0)
              CALL PUTSTR(RESULT,' distinct words')
              CALL PUTNL(RESULT)
              CALL PUTSTR(RESULT,'* span ')
              CALL PUTINT(RESULT,LFTSPN/4,0)
              CALL PUTSTR(RESULT,' by ')
              CALL PUTINT(RESULT,RHTSPN/4,0)
              IF(RESTRI)THEN
                  CALL PUTSTR(RESULT,' restricted')
              ELSE
                  CALL PUTSTR(RESULT,' unrestricted')
              END IF
              CALL PUTNL(RESULT)
              CALL PUTSTR(RESULT,'* meanspan = totalspan/')
              CALL PUTSTR(RESULT,' (nodefrequency * (leftspan +')
              CALL PUTSTR(RESULT,' rightspan))')
              CALL PUTNL(RESULT)
          ENDIF
          IF(  CONDEN.NE.0 )THEN
              CALL PUTSTR(RESULT,'    node')
              CALL STCNUM(RESULT,ANOFSE+10)
              CALL PUTSTR(RESULT,'collocate')
              CALL STCNUM(RESULT,ANOFSE   *   2+16)
              CALL PUTSTR(RESULT,'pair')
              IF(CONDEN.EQ.2)THEN
                  CALL STCNUM(RESULT,ANOFSE*2+22)
                  CALL PUTSTR(RESULT,'totalspan')
                  CALL STCNUM(RESULT,ANOFSE*2+33)
                  CALL PUTSTR(RESULT,'meanspan')
              END IF
              CALL PUTNL(RESULT)
C NOW WE UNDERL THE HEADING
              CALL PUTSTR(RESULT,'    ----')
              CALL STCNUM(RESULT,ANOFSE+10)
              CALL PUTSTR(RESULT,'---------')
              CALL STCNUM(RESULT,ANOFSE  *    2+16)
              CALL PUTSTR(RESULT,'----')
              IF(CONDEN.EQ.2)THEN
                  CALL STCNUM(RESULT,ANOFSE*2+22)
                  CALL PUTSTR(RESULT,'---------')
                  CALL STCNUM(RESULT,ANOFSE*2+33)
                  CALL PUTSTR(RESULT,'--------')
              END IF
              CALL PUTNL(RESULT)
          END IF
          CALL REREAD(TAPOUT)
          DO 1 DUMMY =1, TOTAL
              CALL GETIN(TAPOUT,WRDDAT,MXLENG+5,NULL)
              NLIST=0
              FSTPOS=IABS(SYMTAB(WRDDAT(1)+3))
              IF(RESTRI)THEN
                  CALL ACOL2(WRDDAT(1),FSTPOS,SPNCNT)
              ELSE
                  CALL ACOLAB(FSTPOS,SPNCNT)
              END IF
C NOW REDUCE COLLOCATES TO THOSE REQUESTED ON
C      THE *FREQUENCY CARD
              L=0
              DO 2   K  =1,  NLIST
                  IF(  TSTFRQ(FRQLS2(K)) )THEN
                      L=L+1
                      CODLS2(L)=CODLS2(K)
                      FRQLS2(L)=FRQLS2(K)
                  END IF
    2         CONTINUE
              NLIST=L
C NOW REJECT THE REST OF THE COLLOCATES
              L=0
              DO 3   K  =1,  NLIST
                  TEMP=SYMTAB(CODLS2(K)+3)
                  IF( TEMP.LE.0 )THEN
C KEEP THE COLLOCATE
                      L =L+ 1
                      CODLS2(L)=CODLS2(K)
                      FRQLS2(L)=FRQLS2(K)
                  END IF
    3         CONTINUE
              NLIST=L
C NOW PRINT THE COLLOCATES
          IF( CONDEN.NE.0 )THEN
C THE CONDENSED OPTION - A TABULAR PRESENTATION
              DO 4   I  =1,  NLIST
                  CALL PUTINT(RESULT,WRDDAT(2),-5)
                  CALL PUTSTR(RESULT,' ')
                  CALL PRWORD(RESULT,WRDDAT(1),1)
                  CALL STCNUM(RESULT,ANOFSE+8)
                  CALL PUTINT(RESULT,ABS(SYMTAB(CODLS2(I)+2)),-5)
                  CALL PUTCH1(RESULT,' ')
                  CALL PRWORD(RESULT,CODLS2(I),1)
                  CALL STCNUM(RESULT,ANOFSE   *   2+14)
                  CALL PUTINT(RESULT,FRQLS2(I),-5)
                  IF(CONDEN.EQ.2)THEN
                      CALL STCNUM(RESULT,ANOFSE   *   2+21)
                      CALL PUTINT(RESULT,SPNCNT,-10)
                      CALL STCNUM(RESULT,ANOFSE   *   2+31)
                      SPANC0=FLOAT(SPNCNT)
     +                       /FLOAT(WRDDAT(2)*(LFTSPN+RHTSPN)/4)
                      WRITE(SPANCF,'(F10.6)')SPANC0
                      CALL PUTSTR(RESULT,SPANCF)
                  END IF
                  CALL PUTNL(RESULT)
    4         CONTINUE
          ELSE
C THE STANDARD COLLOCATION PRESENTATION
              DO 5   I  =1,  NLIST
                  CALL PUTSTR(RESULT,'node  ')
                  CALL PRWORD(RESULT,WRDDAT(1),1)
                  CALL PUTSTR(RESULT,'  occurs ')
                  CALL PUTINT(RESULT,WRDDAT(2),0)
                  CALL PUTSTR(RESULT,' times')
                  CALL PUTNL(RESULT)
                  CALL PUTSTR(RESULT,'collocate  ')
                  CALL PRWORD(RESULT,CODLS2(I),1)
                  CALL PUTSTR(RESULT,'  occurs ')
                  CALL PUTINT(RESULT,
     +                  IABS(SYMTAB(CODLS2(I)+2)),0)
                  CALL PUTSTR(RESULT,' times')
                  CALL PUTNL(RESULT)
                  CALL PUTSTR(RESULT,'node-collocate pair occurs ')
                  CALL PUTINT(RESULT,FRQLS2(I),0)
                  CALL PUTSTR(RESULT,' times')
                  CALL PUTNL(RESULT)
                  CALL PRCOOC(FSTPOS,LFTSPN,
     +                 RHTSPN,LEFT,RIGHT,
     +                 CISTYL,FROM,TO,EX,WHERE,OFFSET,
     +                 CODLS2(I),FRQLS2(I))
    5         CONTINUE
          END IF
    1 CONTINUE
      CALL PUTNL(RESULT)
      END IF
      END
      SUBROUTINE RDPHRA(AG,BG,GRPPTR,OK)
      INCLUDE 'global.f'
      INTEGER AG(0:MXCOOC+1)
      INTEGER BG(0:MXCOOC+1)
      INTEGER GRPPTR
      LOGICAL OK
      INTEGER WORD(0:MXWORD+1)
      INTEGER LENGTH
      LOGICAL FAILED,OK1
      INTEGER POSITN
C GETS A SEQUENCE OF WORDS FROM THE SPECIFICATION PART OF
C      A CONTROL STATEMENT AND PUTS THERE REPRESENTATION (I.E. THEIR
C      SYMBOL TABLE ADDRESES) INTO AG BG
      OK= .TRUE.
      CALL GTITEM(WORD,LENGTH,FAILED)
    1 IF(.NOT. FAILED)THEN
          CALL WRDLOC(WORD,LENGTH,OK1,POSITN)
          IF(  OK1 )THEN
              IF(       GRPPTR.LT.MXCOOC )THEN
                  GRPPTR=GRPPTR+1
                  AG(       GRPPTR)=POSITN
                  BG(       GRPPTR)=0
              END IF
          ELSE
              CALL ERROR(8)
              OK= .FALSE.
          END IF
          CALL GTITEM(WORD,LENGTH,FAILED)
          GOTO 1
      END IF
      CALL SKPRST
      END
      SUBROUTINE RDSERI(AG,BG,GRPPTR,OK)
      INCLUDE 'global.f'
      INTEGER AG(0:MXCOOC+1)
      INTEGER BG(0:MXCOOC+1)
      INTEGER GRPPTR
      LOGICAL OK
      INTEGER WORD(0: MXWORD+1)
      INTEGER LENGTH
      LOGICAL FAILED,OK1
      INTEGER POSITN,N
      LOGICAL B1
      LOGICAL CHECK
      OK= .TRUE.
C GETS A SEQUENCE OF WORD,NUMBER,WORD TRIPLES FROM THE SPECIFICATION
C      PART OF A CONTROL STATEMENT AND PUTS THEIR WORD REPRESENTATION:
C      (I.E. SYMBOL TABLE ADDRESSES) INTO  G  TOGETHER WITH THE
C      ASSOCIATED NUMBER
   99 CONTINUE
      CALL GTITEM(WORD,LENGTH,FAILED)
      CALL NSNXTC
      FAILED=YEND
      IF( .NOT. FAILED )THEN
          IF( CHECK('upto') )THEN
              B1= .TRUE.
          ELSE IF( CHECK('gap') )THEN
              B1= .FALSE.
          ELSE
              CALL ERROR(5)
              OK= .FALSE.
          END IF
          CALL GETNUM(N,FAILED)
          IF( B1 )THEN
              N= -N
          END IF
      END IF
      IF(( .NOT. FAILED ))THEN
          CALL WRDLOC(WORD,LENGTH,OK1,POSITN)
          IF( OK1 )THEN
              IF(       GRPPTR.LT.MXCOOC )THEN
                  GRPPTR=GRPPTR+1
                  AG(       GRPPTR)=POSITN
                  BG(       GRPPTR)=N
              END IF
          ELSE
              CALL ERROR(8)
              OK= .FALSE.
          END IF
      END IF
            IF(.NOT.FAILED)GOTO 99
      IF( LENGTH.GT.0 )THEN
          CALL WRDLOC(WORD,LENGTH,OK1,POSITN)
          IF( OK1 )THEN
              IF(       GRPPTR.LT.MXCOOC )THEN
                  GRPPTR=GRPPTR+1
                  AG(       GRPPTR)=POSITN
                  BG(       GRPPTR)=0
              END IF
          ELSE
              CALL ERROR(8)
              OK= .FALSE.
          END IF
      END IF
      CALL SKPRST
      END
      INTEGER FUNCTION  COUNTG(AG,BG,N,LA)
      INCLUDE 'global.f'
      INTEGER AG(0:MXCOOC+1)
      INTEGER BG(0:MXCOOC+1)
      INTEGER CG(0:MXCOOC+1)
      INTEGER N,LA,ACOUNT,TEMP,BASE,I
      INTEGER END1,END2,END3,END4,END5,END6,END7,END8,END9
      ACOUNT=0
      GOTO (100,101,102,103,104,105,106,107,108,109),N
  109 CONTINUE
      CG(9)=IABS(BG(9))
      END9=BG(9)
      IF(END9.LT.0)END9=0
    9 CONTINUE
      IF(CG(9).LT.END9)GOTO 79
  108 CONTINUE
      CG(8)=IABS(BG(8))
      END8=BG(8)
      IF(END8.LT.0)END8=0
    8 CONTINUE
      IF(CG(8).LT.END8)GOTO 78
  107 CONTINUE
      CG(7)=IABS(BG(7))
      END7=BG(7)
      IF(END7.LT.0)END7=0
    7 CONTINUE
      IF(CG(7).LT.END7)GOTO 77
  106 CONTINUE
      CG(6)=IABS(BG(6))
      END6=BG(6)
      IF(END6.LT.0)END6=0
    6 CONTINUE
      IF(CG(6).LT.END6)GOTO 76
  105 CONTINUE
      CG(5)=IABS(BG(5))
      END5=BG(5)
      IF(END5.LT.0)END5=0
    5 CONTINUE
      IF(CG(5).LT.END5)GOTO 75
  104 CONTINUE
      CG(4)=IABS(BG(4))
      END4=BG(4)
      IF(END4.LT.0)END4=0
    4 CONTINUE
      IF(CG(4).LT.END4)GOTO 74
  103 CONTINUE
      CG(3)=IABS(BG(3))
      END3=BG(3)
      IF(END3.LT.0)END3=0
    3 CONTINUE
      IF(CG(3).LT.END3)GOTO 73
  102 CONTINUE
      CG(2)=IABS(BG(2))
      END2=BG(2)
      IF(END2.LT.0)END2=0
    2 CONTINUE
      IF(CG(2).LT.END2)GOTO 72
  101 CONTINUE
      CG(1)=IABS(BG(1))
      END1=BG(1)
      IF(END1.LT.0)END1=0
    1 CONTINUE
      IF(CG(1).LT.END1)GOTO 71
  100 CONTINUE
      BASE=LA
      DO 10 I=1,N
          IF(BASE.GT.NXTITX-4)GOTO 99
          CALL GETBN1(ITEXT,BASE,TEMP)
          IF(SYMTAB(TEMP+4).GE.0) THEN
              TEMP=SYMTAB(TEMP+4)
          END IF
          IF(TEMP.NE.AG(I))GOTO 99
          BASE=BASE+4*(1+CG(I))
   10 CONTINUE
      ACOUNT=ACOUNT+1
   99 CONTINUE
      IF(N.EQ.1)GOTO 999
      CG(1)=CG(1)-1
      GOTO 1
   71 CONTINUE
      IF(N.EQ.2)GOTO 999
      CG(2)=CG(2)-1
      GOTO 2
   72 CONTINUE
      IF(N.EQ.3)GOTO 999
      CG(3)=CG(3)-1
      GOTO 3
   73 CONTINUE
      IF(N.EQ.4)GOTO 999
      CG(4)=CG(4)-1
      GOTO 4
   74 CONTINUE
      IF(N.EQ.5)GOTO 999
      CG(5)=CG(5)-1
      GOTO 5
   75 CONTINUE
      IF(N.EQ.6)GOTO 999
      CG(6)=CG(6)-1
      GOTO 6
   76 CONTINUE
      IF(N.EQ.7)GOTO 999
      CG(7)=CG(7)-1
      GOTO 7
   77 CONTINUE
      IF(N.EQ.8)GOTO 999
      CG(8)=CG(8)-1
      GOTO 8
   78 CONTINUE
      IF(N.EQ.9)GOTO 999
      CG(9)=CG(9)-1
      GOTO 9
   79 CONTINUE
  999 CONTINUE
      COUNTG=ACOUNT
      END
      SUBROUTINE PRGRUP(AG,BG,N,LEFT,RIGHT,CISTYL,
     +                    FROM,TO,EX,OFFSET)
      INCLUDE 'global.f'
      INTEGER N
      INTEGER AG(0:MXCOOC+1)
      INTEGER BG(0:MXCOOC+1)
      INTEGER LEFT,RIGHT,CISTYL,FROM,TO
      LOGICAL EX
      INTEGER OFFSET
      INTEGER S
      INTEGER DUMMY,TEMP,COUNTG
C LOCATES AND PRINTS ALL THOSE CITATIONS WHICH MATCH THE SERIES
C      OF WORDS IN  AG BG
      S=IABS(SYMTAB(AG(1)+3))
    1 IF( S.NE.0 )THEN
          TEMP=COUNTG(AG,BG,N,S)
          DO 2 DUMMY =1, TEMP
              CALL PRCITE(S+OFFSET,LEFT,RIGHT,CISTYL,FROM,TO,EX)
    2     CONTINUE
          CALL GETBN1(ITEXT,S+1,S)
          GOTO 1
      END IF
      END
      SUBROUTINE CNGRUP(AG,BG,N,SUM)
      INCLUDE 'global.f'
      INTEGER N,SUM
      INTEGER AG(0:MXCOOC+1)
      INTEGER BG(0:MXCOOC+1)
      INTEGER S,COUNTG
C LOCATES AND COUNTS ALL THOSE CITATIONS WHICH MATCH THE SERIES
C      OF WORDS IN  AG BG
      SUM=0
      S=IABS(SYMTAB(AG(1)+3))
    1 IF( S.NE.0 )THEN
          SUM=SUM+COUNTG(AG,BG,N,S)
          CALL GETBN1(ITEXT,S+1,S)
          GOTO 1
      END IF
      END
      SUBROUTINE NXTWRD(WORD,WLNGTH,FAILED)
      INCLUDE 'global.f'
      INTEGER WORD(0:MXWORD+1)
      INTEGER WLNGTH,W
      INTEGER F,T,I
      LOGICAL FAILED
      INTEGER ITHWRD,PST,LENGTH,PAREA
      COMMON/COOC/ITHWRD,PST,LENGTH,PAREA(MXPSIZ)
      FAILED=.FALSE.
      IF(ITHWRD.GE.NXTITX)THEN
          FAILED=.TRUE.
          RETURN
      END IF
      CALL GETBN1(ITEXT,ITHWRD,W)
      IF(SYMTAB(W+4).GE.0)THEN
          W=SYMTAB(W+4)
      END IF
      T=SYMTAB(W+1)
      F=SYMTAB(W)
      WLNGTH=T-F+1
      DO 10 I=F,T
          WORD(I-F+1)=SYMTAB(I)
   10 CONTINUE
      ITHWRD=ITHWRD+4
      END
      SUBROUTINE NXTPAT(PATERN,PLNGTH,FAILED)
      INCLUDE 'global.f'
      INTEGER PATERN(0:MXWORD+1),PLNGTH
      LOGICAL FAILED
      INTEGER I
      INTEGER ITHWRD,PST,LENGTH,PAREA
      COMMON/COOC/ITHWRD,PST,LENGTH,PAREA(MXPSIZ)
      FAILED=.FALSE.
      IF(PST.GT.LENGTH)THEN
          FAILED=.TRUE.
          RETURN
      END IF
      I=1
    1 IF(PAREA(PST).NE.ASPACE)THEN
          PATERN(I)=PAREA(PST)
          PST=PST+1
          I=I+1
          GOTO 1
      END IF
      PST=PST+1
      PLNGTH=I-1
      END
      SUBROUTINE RDALLP(PAREA,PSIZE,LENGTH,VAR,DUM,FAILED)
      INCLUDE 'global.f'
      INTEGER PSIZE,LENGTH,VAR,DUM
      INTEGER PAREA(PSIZE)
      INTEGER PATERN(0:MXWORD+1)
      INTEGER PLNGTH,I
      LOGICAL CHECK,OK
      LOGICAL FAILED,FA
      VAR=ICHAR('*')
      DUM=ICHAR('.')
      OK=.TRUE.
      CALL NSNXTC
      IF(  CHECK('dummy')  )THEN
          DUM =  ICHAR(YCHAR)
          IF(  DUM.EQ.ASPACE  )THEN
              CALL ERROR(19)
              CALL SKPRST
              OK= .FALSE.
          ELSE
              CALL NEXTCH
              IF(  CHECK('variable')  )THEN
                  VAR=  ICHAR(YCHAR)
                  IF(  VAR.EQ.ASPACE  )THEN
                      CALL ERROR(19)
                      CALL SKPRST
                      OK= .FALSE.
                  ELSE IF(  DUM.EQ.VAR  )THEN
                      CALL ERROR(9)
                      CALL SKPRST
                      OK= .FALSE.
                  ELSE
                      CALL NEXTCH
                  END IF
              ELSE
                  CALL ERROR(1)
                  CALL SKPRST
                  OK= .FALSE.
              END IF
          END IF
      END IF
      IF(.NOT.OK)RETURN
      FAILED=.FALSE.
      LENGTH=1
      CALL NSNXTC
    1 CONTINUE
      CALL GTITEM(PATERN,PLNGTH,FA)
      CALL NSNXTC
      DO 10 I=LENGTH,LENGTH+PLNGTH-1
          IF(I.GT.PSIZE)THEN
              FAILED=.TRUE.
              GOTO 9
          END IF
          PAREA(I)=PATERN(I-LENGTH+1)
   10 CONTINUE
      IF(LENGTH+PLNGTH.GT.PSIZE)THEN
          FAILED=.TRUE.
          GOTO 9
      END IF
      PAREA(LENGTH+PLNGTH)=ASPACE
      LENGTH=LENGTH+PLNGTH+1
      IF(.NOT.YEND)GOTO 1
      LENGTH=LENGTH-1
    9 CALL SKPRST
      END
      SUBROUTINE COUNT3(VAR,DUM,N)
      INCLUDE 'global.f'
      INTEGER VAR,DUM,N
      INTEGER PLNGTH,WLNGTH
      INTEGER WORD(0:MXWORD+1),PATERN(0:MXWORD+1)
      LOGICAL MATCH
      LOGICAL WFAILD,PFAILD
      INTEGER WSAVE
      INTEGER ITHWRD,PST,LENGTH,PAREA
      COMMON/COOC/ITHWRD,PST,LENGTH,PAREA(MXPSIZ)
              N=0
              PST=1
              CALL NXTPAT(PATERN,PLNGTH,PFAILD)
              IF(PFAILD)GOTO 1
              ITHWRD=3
   11         CALL NXTWRD(WORD,WLNGTH,WFAILD)
              IF(WFAILD)GOTO 1
              IF(MATCH(WORD,WLNGTH,PATERN,PLNGTH,VAR,DUM))THEN
                  WSAVE=ITHWRD
   12             CALL NXTWRD(WORD,WLNGTH,WFAILD)
                  CALL NXTPAT(PATERN,PLNGTH,PFAILD)
                  IF(WFAILD.AND.PFAILD)THEN
                      GOTO 1
                  ELSE IF(.NOT.WFAILD.AND.PFAILD)THEN
                      N=N+1
                      PST=1
                      CALL NXTPAT(PATERN,PLNGTH,PFAILD)
                      ITHWRD=WSAVE
                      GOTO 11
                  ELSE IF(WFAILD.AND..NOT.PFAILD)THEN
                      GOTO 1
                  ELSE
                      IF(MATCH(WORD,WLNGTH,PATERN,PLNGTH,
     +                         VAR,DUM))THEN
                          GOTO 12
                      ELSE
                          PST=1
                          CALL NXTPAT(PATERN,PLNGTH,PFAILD)
                          ITHWRD=WSAVE
                          GOTO 11
                      END IF
                  END IF
              END IF
              GOTO 11
    1 RETURN
      END
      SUBROUTINE COCCUR
      INCLUDE 'global.f'
      INTEGER ASORTM,LEFT,RIGHT
      LOGICAL  FAILED,OK
      INTEGER CISTYL,FROM,TO
      INTEGER WHERE,OFFSET
      INTEGER AGRP(0:MXCOOC+1),BGRP(0:MXCOOC+1)
      INTEGER GRPPTR
      INTEGER PSIZE,PLNGTH,WLNGTH
      INTEGER CONDEN
      LOGICAL  EX
      INTEGER I,J,VAR,DUM
      INTEGER WORD(0:MXWORD+1),PATERN(0:MXWORD+1)
      LOGICAL STREQ,MATCH
      LOGICAL WFAILD,PFAILD
      INTEGER WSAVE
      INTEGER ITHWRD,PST,LENGTH,PAREA
      COMMON/COOC/ITHWRD,PST,LENGTH,PAREA(MXPSIZ)
C PARSES THE 'co-occurrence' CONTROL STATEMENT, AND CERTAIN
C      CONTROL STATEMENTS WHICH FOLLOW IT
      CALL FNDPAR(ASORTM,LEFT,RIGHT,CISTYL,FROM,TO,EX,
     +WHERE,OFFSET,CONDEN,FAILED)
      LEFT       =LEFT*4
      RIGHT       =RIGHT*4
      PSIZE=MXPSIZ
      CALL SKPRST
      CALL PUTSTR(RESULT,'listing of co-occurrences')
      CALL PROFFS(OFFSET)
      CALL UNDERL(RESULT,'=')
    1 IF(  STREQ(CTLFLD,QPHRAS,15)  .OR.
     +     STREQ(CTLFLD,QSERIS,15)  .OR.
     +     STREQ(CTLFLD,QCHOIC,15)  .OR.
     +     STREQ(CTLFLD,QPATT,15) )THEN
          IF(  STREQ(CTLFLD,QPHRAS ,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
C GET A PHRASE INTO GRP
              GRPPTR=0
              CALL RDPHRA(AGRP,BGRP,GRPPTR,OK)
C NOW PRINTOUT THE PHRASE JUST READ IN
              CALL PUTSTR(RESULT,'phrase ')
              DO 2   I  =1,  GRPPTR
                  CALL PUTCH1(RESULT,' ')
                  CALL PRITEM(RESULT,AGRP(I))
    2         CONTINUE
              CALL PUTNL(RESULT)
              IF(CONDEN.EQ.1)THEN
                  CALL PUTSTR(RESULT,'occurs ')
                  CALL CNGRUP(AGRP,BGRP,GRPPTR,I)
                  CALL PUTINT(RESULT,I,0)
                  CALL PUTSTR(RESULT,' times')
                  CALL PUTNL(RESULT)
              ENDIF
              IF(CISTYL.EQ.NONSTY)GOTO 1
C NOW PRINT ALL OCCURRENCES OF THE PHRASE
              IF(  GRPPTR.GT.0 .AND. OK  )THEN
                  CALL PRGRUP(AGRP,BGRP,GRPPTR,LEFT,RIGHT,
     +            CISTYL,FROM,TO,EX,OFFSET)
              END IF
          ELSE IF(STREQ(CTLFLD,QSERIS,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
C GET A SERIES INTO GRP
              GRPPTR=0
              CALL RDSERI(AGRP,BGRP,GRPPTR,OK)
C NOW PRINTOUT THE SERIES JUST READ IN
              CALL PUTSTR(RESULT,'series ')
              DO 3   I  =1,  GRPPTR-1
                  CALL PUTCH1(RESULT,' ')
                  CALL PRITEM(RESULT,AGRP(I))
                  IF( BGRP(I) .LT. 0 )THEN
                      CALL PUTSTR(RESULT,' upto')
                  ELSE
                      CALL PUTSTR(RESULT,'  gap')
                  END IF
                  CALL PUTINT(RESULT, ABS(BGRP(I)),0)
    3         CONTINUE
              IF( GRPPTR.GT.1 )THEN
                  CALL PUTCH1(RESULT,' ')
                  CALL PRITEM(RESULT,AGRP(GRPPTR))
              END IF
              CALL PUTNL(RESULT)
              IF(CONDEN.EQ.1)THEN
                  CALL PUTSTR(RESULT,'occurs ')
                  CALL CNGRUP(AGRP,BGRP,GRPPTR,I)
                  CALL PUTINT(RESULT,I,0)
                  CALL PUTSTR(RESULT,' times')
                  CALL PUTNL(RESULT)
              ENDIF
              IF(CISTYL.EQ.NONSTY)GOTO 1
C NOW PRINT ALL OCCURRENCES OF SERIES
              IF( GRPPTR.GT.0 .AND. OK  )THEN
                  CALL PRGRUP(AGRP,BGRP,GRPPTR,LEFT,RIGHT,
     +            CISTYL,FROM,TO,EX,OFFSET)
              END IF
          ELSE IF(STREQ(CTLFLD,QCHOIC,15) )THEN
              IF( SW3 )THEN
                  CALL PRCON
              END IF
C GET A PHRASE INTO GRP
              GRPPTR=0
              CALL RDPHRA(AGRP,BGRP,GRPPTR,OK)
C NOW PRINTOUT THE CHOICE JUST READ IN
              CALL PUTSTR(RESULT,'choice ')
              DO 4   I  =1,  GRPPTR
                  CALL PUTCH1(RESULT,' ')
                  CALL PRITEM(RESULT,AGRP(I))
    4         CONTINUE
              CALL PUTNL(RESULT)
              IF(CONDEN.EQ.1)THEN
                  CALL PUTSTR(RESULT,'occurs ')
                  J=0
                  DO 41 I=1,GRPPTR
                      J=J+IABS(SYMTAB(AGRP(I)+2))
   41             CONTINUE
                  CALL PUTINT(RESULT,J,0)
                  CALL PUTSTR(RESULT,' times')
                  CALL PUTNL(RESULT)
              ENDIF
              IF(CISTYL.EQ.NONSTY)GOTO 1
              ITHWRD=3
              FAILED=.FALSE.
    5         IF(ITHWRD.GE.NXTITX)THEN
                  GOTO 1
              END IF
              IF(J.EQ.0)GOTO 1
              CALL GETBN1(ITEXT,ITHWRD,W)
              IF(SYMTAB(W+4).GE.0)THEN
                  W=SYMTAB(W+4)
              END IF
              DO 6 I=1,GRPPTR
                  IF(W.EQ.AGRP(I))THEN
                      CALL PRCITE(ITHWRD+OFFSET,
     +                            LEFT,RIGHT,CISTYL,FROM,TO,EX)
                      J=J-1
                      GOTO 6
                  ENDIF
    6         CONTINUE
              ITHWRD=ITHWRD+4
              GOTO 5
          ELSE
C NOW PARSE THE *PATTERN OPTION
              IF(SW3)CALL PRCON
              CALL RDALLP(PAREA,PSIZE,LENGTH,VAR,DUM,FAILED)
              IF(FAILED)THEN
                  CALL PUTSTR(MONFIL,'too many patterns')
                  CALL PUTNL(MONFIL)
                  GOTO 1
              END IF
              CALL PUTSTR(RESULT,'pattern ')
              DO 10 I=1,LENGTH-1
                  CALL PUTCH1(RESULT,CHAR(PAREA(I)))
   10         CONTINUE
              CALL PUTNL(RESULT)
              IF(CONDEN.EQ.1)THEN
                  CALL PUTSTR(RESULT,'occurs ')
                  CALL COUNT3(VAR,DUM,J)
                  CALL PUTINT(RESULT,J,0)
                  CALL PUTSTR(RESULT,' times')
                  CALL PUTNL(RESULT)
              ENDIF
              IF(CISTYL.EQ.NONSTY)GOTO 1
              PST=1
              CALL NXTPAT(PATERN,PLNGTH,PFAILD)
              IF(PFAILD)GOTO 1
              ITHWRD=3
   11         CALL NXTWRD(WORD,WLNGTH,WFAILD)
              IF(WFAILD)GOTO 1
              IF(MATCH(WORD,WLNGTH,PATERN,PLNGTH,VAR,DUM))THEN
                  WSAVE=ITHWRD
   12             CALL NXTWRD(WORD,WLNGTH,WFAILD)
                  CALL NXTPAT(PATERN,PLNGTH,PFAILD)
                  IF(WFAILD.AND.PFAILD)THEN
                      GOTO 1
                  ELSE IF(.NOT.WFAILD.AND.PFAILD)THEN
                      CALL PRCITE(WSAVE-4+OFFSET,
     +                                LEFT,RIGHT,CISTYL,FROM,TO,EX)
                      PST=1
                      CALL NXTPAT(PATERN,PLNGTH,PFAILD)
                      ITHWRD=WSAVE
                      GOTO 11
                  ELSE IF(WFAILD.AND..NOT.PFAILD)THEN
                      GOTO 1
                  ELSE
                      IF(MATCH(WORD,WLNGTH,PATERN,PLNGTH,
     +                         VAR,DUM))THEN
                          GOTO 12
                      ELSE
                          PST=1
                          CALL NXTPAT(PATERN,PLNGTH,PFAILD)
                          ITHWRD=WSAVE
                          GOTO 11
                      END IF
                  END IF
              END IF
              GOTO 11
          END IF
          GOTO 1
      END IF
      CALL PUTNL(RESULT)
      END
      SUBROUTINE SAVTXT(S)
      INCLUDE 'global.f'
      CHARACTER  S*(*)
      INTEGER TEMP
      CHARACTER *100 DT
      INTEGER I,J
      CHARACTER *4 ZCLOC
      DATA ZCLOC/'cloc'/
C SAVES ONTO THE FILE  S  ENOUGH DATA SO THAT THE CALL GETTEXT CONTROL
C      STATEMENT CAN RESTORE IT
      OPEN(UNIT=SF,FILE=S,FORM='UNFORMATTED',STATUS='UNKNOWN')
      WRITE(SF) MXLENG,RUNWC,OCOUNT,DISTWC,
     +          ITMCNT,BOREFS,STRAT,ZCLOC,GSCODE,
     +          TYPE,SPECAL,PRRANK,SECRNK,NXTITX,
     +          PTRTOP,TABBOT
C SEND   ITEXT
      DO 3  I =1, NXTITX-2
          CALL GETBN1(ITEXT,I,TEMP)
          WRITE(SF) TEMP
    3 CONTINUE
C SEND   SYMTAB
      DO 4  I = 0 ,PTRTOP-1, ITSLOT
          IF( SYMTAB(I).NE.0 )THEN
              WRITE(SF) I,(SYMTAB(J),J=I,I+ITSLOT-1)
          END IF
    4 CONTINUE
      WRITE(SF) (SYMTAB(J),J=PTRTOP,UPBSYM)
C SEND   REFER
      WRITE(SF) NREFER,(LREFER(I),PREFER(I),I=1,NREFER)
      WRITE(SF) ((TREFER(J,I),J=0,RMAX),I=1,NREFER)
      DT=' '
      CALL DAYTIM(DT)
C SEND  DATE AND TIME OF SAVING
      WRITE(SF) DT
      CLOSE(UNIT=SF,STATUS='KEEP')
      CALL PUTSTR(MONFIL,'text file ')
      CALL PUTSTR(MONFIL,S)
      CALL PUTSTR(MONFIL,' saved on ')
      CALL PUTSTR(MONFIL,DT(1:DTSIZE))
C PRINT DATE
      CALL PUTNL(MONFIL)
      END
      SUBROUTINE GTTEXT(S)
      INCLUDE 'global.f'
      CHARACTER S*(*)
      INTEGER TEMP
      CHARACTER *100 DT
      INTEGER I,J,DUMMY,GSTEST
      CHARACTER *4 ZCLOC, ZTEST
      DATA ZCLOC/'cloc'/
C GETS FROM FILE  S  PREVIOUSLY SAVED TABLES
      DT=' '
      OPEN(UNIT=SF,FILE=S,FORM='UNFORMATTED',STATUS='OLD')
      READ(SF) MXLENG,RUNWC,OCOUNT,DISTWC,
     +         ITMCNT,BOREFS,STRAT,ZTEST,GSTEST,
     +         TYPE,SPECAL,PRRANK,SECRNK,NXTITX,
     +         PTRTOP,TABBOT
      IF(ZTEST.NE.ZCLOC .OR. GSTEST.NE.GSCODE)THEN
          CALL ERRMES(MONFIL,11)
          CALL ENDRUN
      END IF
C FETCH  ITEXT
      DO 3  I =1, NXTITX-2
          READ(SF) TEMP
          CALL PUTBN1(ITEXT,I,TEMP)
    3 CONTINUE
C FETCH  SYMTAB
      CALL SETTO(SYMTAB,UPBSYM,0)
      DO 4 DUMMY =1, ITMCNT
          READ(SF) I,(SYMTAB(J),J=I,I+ITSLOT-1)
    4 CONTINUE
      READ(SF) (SYMTAB(J),J=PTRTOP,UPBSYM)
C FETCH  REFER
      READ(SF) NREFER,(LREFER(I),PREFER(I),I=1,NREFER)
      READ(SF) ((TREFER(J,I),J=0,RMAX),I=1,NREFER)
C FETCH  DATE AND TIME OF SAVING
      READ(SF) DT
      CALL PUTSTR(MONFIL,'text file ')
      CALL PUTSTR(MONFIL,S)
      CALL PUTSTR(MONFIL,' accessed. (saved on ')
      CALL PUTSTR(MONFIL,DT(1:DTSIZE))
C PRINT OLD DATE
      CALL PUTCH1(MONFIL,')')
      CALL PUTNL(MONFIL)
      END
