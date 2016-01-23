      IMPLICIT INTEGER(A-Z)
      PARAMETER(
     +GSCODE=1,
     +ITSLOT=5,
     +MXCOOC=10,
     +MXLPWD=160,
     +MXWDTH=121)
C YOU CAN CHANGE THE FOLLOWING NUMBERS
      PARAMETER(
     +MXSORT=500,
     +MXEXPR=10,
     +MXCOLL=4000,
     +MXITXT=4000000,
     +RMAX=8,
     +MXREFS=1000,
     +MXWORD=100,
     +MXPSIZ=300)
C TABSIZ MUST BE A POWER OF 2 MINUS 1
C            E.G. 8191 16383 32767 65535
      PARAMETER(
     +TABSIZ=16383,
     +MLNGTH=5,
     +UPBSYM=
     +   ITSLOT*(TABSIZ+1)-1+MLNGTH*(TABSIZ+1) )

      CHARACTER *(*) QINSRT,QSEND,QNOSND,QCONTI,QCSS,
     +QCUS,QCLSYM,QCLITX,QCREFS,QBLANK,
     +QINDET,QGTTXT,QITUSE,QITUZE,
     +QLETT,QSEPAR,QIGNOR,QRDASP,QPADD,QDEFER,
     +QFINIS,QEVWRD,QSEWRD,QLWRDS,QPATT,
     +QFREQ,QEXCLU,QCONC,QCOLL,QEVCOL,QSECOL,QACCEP,QREJEC,
     +QCOOC,QWLIST,QOUTDT,QSVTXT,QPHRAS,
     +QSERIS,QCHOIC,QSPAN,QRUN,QNOTE,QINDEX,QWRITE,QTESRT,QFROM,
     +QTO,QINCLU,QSTATS,QNEWL,QNEWP,QMESSA,QPROFI
      PARAMETER(
     +QINSRT          ='insert         ',
     +QSEND           ='send           ',
     +QNOSND          ='nosend         ',
     +QCONTI          ='continue       ',
     +QCSS            ='clocsetswitch  ',
     +QCUS            ='clocunsetswitch',
     +QCLSYM          ='clocsymtab     ',
     +QCLITX          ='clocitext      ',
     +QCREFS          ='clocrefs       ',
     +QBLANK          ='               ',
     +QINDET          ='inputdetails   ',
     +QOUTDT          ='outputdetails  ',
     +QGTTXT          ='gettext        ',
     +QSVTXT          ='savetext       ',
     +QITUSE          ='itemiseusing   ',
     +QITUZE          ='itemizeusing   ',
     +QLETT           ='letters        ')
      PARAMETER(
     +QSEPAR          ='separators     ',
     +QIGNOR          ='ignore         ',
     +QRDASP          ='readasspace    ',
     +QPADD           ='padding        ',
     +QDEFER          ='deferred       ',
     +QFINIS          ='finish         ',
     +QEVWRD          ='everyword      ',
     +QSEWRD          ='selectwords    ',
     +QLWRDS          ='listofwords    ',
     +QPATT           ='pattern        ',
     +QFREQ           ='frequency      ',
     +QEXCLU          ='excluding      ',
     +QINCLU          ='including      ')
      PARAMETER(
     +QCONC           ='concordance    ',
     +QCOLL           ='collocations   ',
     +QSPAN           ='span           ',
     +QEVCOL          ='everycollocate ',
     +QSECOL          ='selectcollocate',
     +QACCEP          ='accepting      ',
     +QREJEC          ='rejecting      ',
     +QCOOC           ='cooccurrence   ',
     +QPHRAS          ='phrase         ',
     +QSERIS          ='series         ',
     +QCHOIC          ='choice         ',
     +QWLIST          ='wordlist       ',
     +QRUN            ='run            ',
     +QNOTE           ='note           ',
     +QINDEX          ='index          ',
     +QWRITE          ='writetext      ',
     +QTESRT          ='testsort       ',
     +QFROM           ='from           ')
      PARAMETER(
     +QTO             ='to             ',
     +QNEWL           ='newline        ',
     +QNEWP           ='newpage        ',
     +QMESSA          ='message        ',
     +QSTATS          ='statistics     ',
     +QPROFI          ='profile        ')
      INTEGER
     +KMON,KRES, RES,MON,DAT,CON,NIN,NOUT,LPWDTH, F1,F2,F3,F4,F5,
     +FTEMP,SF,TAPE1,TAPE2,TAPE3,TAPE4,ITEXT,SORTSZ,
     +ERRVAL(0:11),INWARD(0:256),OUTWAR(0:256),
     +REFERS(0:RMAX+1,0:27),LTVREF(0:27),LEVREF(0:27),
     +LREFER(0:MXREFS+1),PREFER(0:MXREFS+1),
     +TREFER(0: RMAX+1,0:MXREFS+1),TXTRFS(0:RMAX+1,0:27),
     +SYMTAB(0: UPBSYM+1),FEXPR(0:MXEXPR+1),
     +VEXPR(0:MXEXPR+1),LFTCON(0:MXLPWD),
     +RHTCTX(0:MXLPWD),CODLS2(0:MXCOLL+1),
     +FRQLS2(0:MXCOLL+1),TYPE(0:257),PRRANK(0:257)
      INTEGER
     +SECRNK(0:257),SPECAL(0:257),GAP,NXTITX,CNTRLP,
     +ERRNUM,WIDTH,CENTRE,LNUMB,PTR,WRDFRM,WORDTO,DCHAR,DTYPE,
     +ZNORML,ZPADD,ZDEFER,ZLETER,ZSEPAR,ZIGNOR,
     +ZRDASP,ZSKIPS,ZSKEND,ZOPENS,ZOPEND,ZREFSS,
     +ZRFEND,ZCONTI,ZNL,CONTI,ASPACE,UPPERA,UPPERZ,LOWERA,LOWERZ,
     +MIXINP,RAIIMP,LOWINP,ISTATE,OUTSTA,NREFS,REFOFF,
     +NINES,NBWID,NREFER,STRAT,ATBSZ1,BTBSZ4,
     +CTAB34,PTRTOP,TABBOT,MXLENG,MXRECL,
     +RUNWC,OCOUNT,DISTWC,ITMCNT,WANT,NOWANT,
     +NOTREJ,REJECT,LOCAT,COUNT,SALPHA,SDALPH,SAFREQ,SDFREQ,
     +SALENG,SDLENG,SAXLEN,SDXLEN,SRVALF,STRVDA,
     +SFIRST,SLAST,CARDSZ,CTLSTP,NEXPR,CSTYLE,LFTSTY,NONSTY,
     +CTLSTK(0:101),LMAX,EOINF,LFTPTR,RHTPTR,NLIST,LFTSPN,
     +RHTSPN,INWDTH,CONT,COPEN,CCLOSE,IGOPEN,IGCLOS,ENDLN,
     +COOPEN,COCLOS,DTSIZE

      LOGICAL
     +SW1,SW2,SW3,SW4,SW5,RUNOVR,SAVSW3,SAVSW4,NOREFS,EOF,
     +EOTEXT,COFLAG,YEND,BOREFS,WHSPCE

      INTEGER RESULT, CONFIL, MONFIL, DATFIL, ERRFIL

      CHARACTER *1
     +ERRVEC(0:MXWDTH+1),CNTLIN(0:MXWDTH+1),LIST(0:MXWDTH+1),
     +CTLFLD(0:16), XNLINE,XNPAGE,
     +EOLIN, YCHAR,MONBUF(0:MXLPWD),RESBUF(0:MXLPWD),HYPHEN

      COMMON/A/
     +KMON,KRES, RES,MON,DAT,CON, NIN,NOUT,LPWDTH, F1,F2,F3,F4,F5,
     +FTEMP,SF,TAPE1,TAPE2,TAPE3,TAPE4,ITEXT,SORTSZ,WHSPCE,BOREFS,
     +ERRVAL,INWARD,OUTWAR,REFERS,LTVREF,LEVREF,LREFER,PREFER,
     +TREFER,TXTRFS,SYMTAB,FEXPR,VEXPR,LFTCON,RHTCTX,CODLS2,
     +FRQLS2,TYPE,PRRANK,SECRNK,SPECAL,GAP,NXTITX, CNTRLP,ERRNUM,
     +WIDTH,CENTRE,LNUMB,PTR,WRDFRM,WORDTO,DCHAR,DTYPE,ZNORML,
     +ZPADD,ZDEFER,ZLETER,ZSEPAR,ZIGNOR,ZRDASP,ZSKIPS,ZSKEND,
     +ZOPENS,ZOPEND,ZREFSS,ZRFEND,ZCONTI,ZNL,CONTI,ASPACE,UPPERA,
     +UPPERZ,LOWERA,LOWERZ,MIXINP,RAIIMP,LOWINP,ISTATE,OUTSTA,
     +NREFS,REFOFF,NINES,NBWID,NREFER,STRAT,ATBSZ1,BTBSZ4,CTAB34,
     +PTRTOP,TABBOT,MXLENG,MXRECL,RUNWC,OCOUNT,DISTWC,ITMCNT,WANT,
     +NOWANT,NOTREJ,REJECT,LOCAT,COUNT,SALPHA,SDALPH,SAFREQ,SDFREQ,
     +SALENG,SDLENG,SAXLEN,SDXLEN,SRVALF,STRVDA,SFIRST,SLAST,
     +CARDSZ,CTLSTP,NEXPR,CSTYLE,LFTSTY,NONSTY,CTLSTK,LMAX,EOINF,
     +LFTPTR,RHTPTR,NLIST,LFTSPN,RHTSPN,INWDTH,CONT,COPEN,
     +CCLOSE,IGOPEN,IGCLOS,ENDLN,COOPEN,COCLOS,DTSIZE,
     +SW1,SW2,SW3,SW4,SW5,RUNOVR,SAVSW3,SAVSW4,NOREFS,EOF,EOTEXT,
     +COFLAG,YEND,RESULT,CONFIL,MONFIL,DATFIL,ERRFIL

      COMMON/B/
     +ERRVEC,CNTLIN,LIST,CTLFLD, XNLINE, XNPAGE,
     +EOLIN, YCHAR ,MONBUF,RESBUF,HYPHEN

      CHARACTER *2 MARK
      COMMON/C/ MARK
