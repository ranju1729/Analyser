000010 IDENTIFICATION DIVISION.                                         08/25/14
000020 PROGRAM-ID. SAMPLE.                                              SAMPLE
000030 AUTHOR.  HARI.                                                   LV01

000440 ENVIRONMENT DIVISION.                                            SAMPLE
000450 CONFIGURATION SECTION.                                           SAMPLE
000460 SOURCE-COMPUTER.  IBM-370.                                       SAMPLE
000470 OBJECT-COMPUTER.  IBM-370.                                       SAMPLE
000480 SPECIAL-NAMES.                                                   SAMPLE
000490     C01 IS  TOP-OF-PAGE.                                         SAMPLE
000500 INPUT-OUTPUT SECTION.                                            SAMPLE
000510 FILE-CONTROL.                                                    SAMPLE
000520     SELECT PROFILE-VALID-DLR  ASSIGN TO UT-S-HIERSERV.           SAMPLE
000530     SELECT COMPANY-PARM       ASSIGN TO UT-S-COMPARMI.           SAMPLE
000540     SELECT DLRSUMM-CARMB      ASSIGN TO UT-S-DLRCAR.             SAMPLE
000550     SELECT DLRSUMM-TRKBVF     ASSIGN TO UT-S-DLRTRK.             SAMPLE
000560     SELECT DLRSUMM-BYPASS     ASSIGN TO UT-S-DLRBYP.             SAMPLE
000570     SELECT ERROR-REPORT       ASSIGN TO UT-S-ERRRPT.             SAMPLE
000580     SELECT RUN-DATE-FILE      ASSIGN TO UT-S-RUNDATE.            SAMPLE
000590                                                                  SAMPLE
000600 DATA DIVISION.                                                   SAMPLE
000610 FILE SECTION.                                                    SAMPLE
000620                                                                  SAMPLE
000630 FD  PROFILE-VALID-DLR                                            SAMPLE
000640     RECORDING MODE IS F                                          SAMPLE
000650     LABEL RECORDS ARE STANDARD                                   SAMPLE
000660     BLOCK CONTAINS 0 RECORDS.                                    SAMPLE
000670 01  VALID-DLR-REC.                                               SAMPLE
000680     COPY CZDERRD                                                 SAMPLE
000690         REPLACING ==:VALDLR:==                                   SAMPLE
000700         BY        ==VALID-DLR==.                                 SAMPLE
000710                                                                  SAMPLE
000720 FD  COMPANY-PARM                                                 SAMPLE
000730     RECORDING MODE IS F                                          SAMPLE
000740     LABEL RECORDS ARE STANDARD                                   SAMPLE
000750     BLOCK CONTAINS 0 RECORDS.                                    SAMPLE
000760 01  COMPANY-PARMS-REC.                                           SAMPLE
000770     COPY CZDERNM   REPLACING ==:CORPNAME:==                      SAMPLE
000780                    BY        ==CORPNAME==.                       SAMPLE
000790                                                                  SAMPLE
000800 FD  DLRSUMM-CARMB                                                SAMPLE
000810     RECORDING MODE IS F                                          SAMPLE
000820     LABEL RECORDS ARE STANDARD                                   SAMPLE
000830     BLOCK CONTAINS 0 RECORDS.                                    SAMPLE
000840 01  DEALER-CAR-REC.                                              SAMPLE
000850     COPY CZDERDL                                                 SAMPLE
000860         REPLACING ==:DEALER:==                                   SAMPLE
000870         BY        ==DEALER-CAR==.                                SAMPLE
000880                                                                  SAMPLE
000890 FD  DLRSUMM-TRKBVF                                               SAMPLE
000900     RECORDING MODE IS F                                          SAMPLE
000910     LABEL RECORDS ARE STANDARD                                   SAMPLE
000920     BLOCK CONTAINS 0 RECORDS.                                    SAMPLE
000930 01  DEALER-TRK-REC.                                              SAMPLE
000940     COPY CZDERDL                                                 SAMPLE
000950         REPLACING ==:DEALER:==                                   SAMPLE
000960         BY        ==DEALER-TRK==.                                SAMPLE
000970                                                                  SAMPLE
000980 FD  DLRSUMM-BYPASS                                               SAMPLE
000990     RECORDING MODE IS F                                          SAMPLE
001000     LABEL RECORDS ARE STANDARD                                   SAMPLE
001010     BLOCK CONTAINS 0 RECORDS.                                    SAMPLE
001020 01  DEALER-BYP-REC.                                              SAMPLE
001030     COPY CZDERDL                                                 SAMPLE
001040         REPLACING ==:DEALER:==                                   SAMPLE
001050         BY        ==DEALER-BYP==.                                SAMPLE
001060                                                                  SAMPLE
001070 FD  ERROR-REPORT                                                 SAMPLE
001080     RECORDING MODE IS F                                          SAMPLE
001090     LABEL RECORDS ARE STANDARD                                   SAMPLE
001100     BLOCK CONTAINS 0 RECORDS.                                    SAMPLE
001110 01  ERROR-OUT                        PIC X(132).                 SAMPLE
001120                                                                  SAMPLE
001130 FD  RUN-DATE-FILE                                                SAMPLE
001140     RECORDING MODE IS F                                          SAMPLE
001150     LABEL RECORDS ARE STANDARD                                   SAMPLE
001160     BLOCK CONTAINS 0 RECORDS.                                    SAMPLE
001170 01  RUNDATE-REC.                                                 SAMPLE
001180     COPY CZDERME                                                 SAMPLE
001190         REPLACING ==:RUNDATE:==                                  SAMPLE
001200         BY        ==RUNDATE==.                                   SAMPLE
001210                                                                  SAMPLE
001220 WORKING-STORAGE SECTION.                                         SAMPLE
001225 77  PAN-VALET PICTURE X(24) VALUE '011ABCD  08/25/14'.           SAMPLE
001230 01  WORKING-STORAGE-AREA.                                        SAMPLE
001240     05  FILLER                 PIC X(33)                         SAMPLE
001250         VALUE '** WORKING STORAGE STARTS HERE **'.               SAMPLE
001260                                                                  SAMPLE
001270 01  ERROR-TEXT-LENGTH                   PIC S9(9) COMP           SAMPLE
001280                                         VALUE +120.              SAMPLE
001290 01  ERROR-MESSAGE.                                               SAMPLE
001300     05  ERROR-LTH                       PIC S9(4) COMP           SAMPLE
001310                               VALUE +960.                        SAMPLE
001320     05  ERROR-TEXT                      PIC X(120)               SAMPLE
001330         OCCURS 8 TIMES INDEXED BY ERROR-INDEX.                   SAMPLE
001340                                                                  SAMPLE
001350 01  WS-SWITCHES.                                                 SAMPLE
001360     05  WS-ZDERDLR-SW               PIC X VALUE 'N'.                CL**3
001370         88  ZDERDLR-EOF             VALUE 'Y'.                      CL**3
001380     05  TABLE-SWITCHES.                                          SAMPLE
001390         10  ZPCMAST-SW                  PIC X VALUE 'N'.         SAMPLE
001400             88  ZPCMAST-FOUND           VALUE 'Y'.               SAMPLE
001410         10  ZPCRETL-SW                  PIC X VALUE 'N'.         SAMPLE
001420             88  ZPCRETL-FOUND           VALUE 'Y'.               SAMPLE
001430**                                                                SAMPLE
001440     05  WS-BVF-DEALER-SW        PIC X(01) VALUE ' '.             SAMPLE
001450         88  BVF-DEALER-NO     VALUE 'N'.                         SAMPLE
001460         88  BVF-DEALER-YES    VALUE 'Y'.                         SAMPLE
001470     05  WS-BVF-DEALER           PIC X(06) VALUE ' '.             SAMPLE
001480     05  FILLER REDEFINES        WS-BVF-DEALER.                   SAMPLE
001490         10  WS-BVF-DLR          PIC X(05).                       SAMPLE
001500         10  WS-BVF-DLR-SUFF     PIC X(01).                       SAMPLE
001510**                                                                SAMPLE
001520     05  WS-DEALER-ONLY          PIC X(01) VALUE ' '.             SAMPLE
001530         88  DEALER-ONLY-NO    VALUE 'N'.                         SAMPLE
001540         88  DEALER-ONLY-YES   VALUE 'Y'.                         SAMPLE
001550     05  DATE-CMP-SW             PIC S9(4) COMP VALUE +0.         SAMPLE
001560     05  WS-BRAND-EOF             PIC X VALUE 'N'.                SAMPLE
001570         88  BRAND-EOF          VALUE 'Y'.                        SAMPLE
001580     05  WS-TABLE-ENTRY-FOUND-SW  PIC X VALUE 'N'.                SAMPLE
001590         88  TABLE-ENTRY-FOUND  VALUE 'Y'.                        SAMPLE
001600     05  WS-SEARCH-EXHAUSTED      PIC X VALUE 'N'.                SAMPLE
001610         88  SEARCH-EXHAUSTED   VALUE 'Y'.                        SAMPLE
001620     05  WS-AT-LEAST-1-FOUND      PIC X VALUE 'N'.                SAMPLE
001630         88  AT-LEAST-1-FOUND   VALUE 'Y'.                        SAMPLE
001640     05  NEW-FZCOMMON-SW          PIC X VALUE 'N'.                SAMPLE
001650         88  NEW-FZCOMMON       VALUE 'Y'.                        SAMPLE
001660     05  VALID-DEALER-SW          PIC X VALUE 'N'.                SAMPLE
001670         88  VALID-DEALER       VALUE 'Y'.                        SAMPLE
001680     05  END-OF-FILE-SW         PIC X VALUE 'N'.                  SAMPLE
001690         88  END-OF-FILE        VALUE 'Y'.                        SAMPLE
001700     05  RUN-DATE-SW            PIC X VALUE 'N'.                  SAMPLE
001710         88  RUN-DATE-EOF       VALUE 'Y'.                        SAMPLE
001720     05  COMPANY-PARM-SW        PIC X VALUE 'N'.                  SAMPLE
001730         88  COMPANY-PARM-EOF   VALUE 'Y'.                        SAMPLE
001740     05  WS-HIER-SERV-SW        PIC X VALUE 'N'.                  SAMPLE
001750         88  HIER-SERV-EOF      VALUE 'Y'.                        SAMPLE
001760     05  WS-ERROR-CODE          PIC X VALUE ' '.                  SAMPLE
001770         88  ERR-NO-ZPCMAST         VALUE '1'.                    SAMPLE
001780         88  ERR-NO-ZPCRETL         VALUE '2'.                    SAMPLE
001790         88  ERR-RDLR-NOBRND        VALUE '3'.                    SAMPLE
001800         88  ERR-NORDLR-NOBRND      VALUE '4'.                    SAMPLE
001810     05  WS-NO-MORE-BRANDS PIC X VALUE ' '.                       SAMPLE
001820         88  MORE-BRANDS            VALUE 'Y'.                    SAMPLE
001830         88  NO-MORE-BRANDS         VALUE 'N'.                    SAMPLE
001840 01  WS-CONSTANTS.                                                SAMPLE
001850     05  C-DEALER-D             PIC X(01) VALUE 'D'.              SAMPLE
001860     05  C-SLASH                PIC X(01) VALUE '/'.              SAMPLE
001870     05  C-0                    PIC X(01) VALUE '0'.              SAMPLE
001880     05  C-1                    PIC X(01) VALUE '1'.              SAMPLE
001890     05  C-2                    PIC X(01) VALUE '2'.              SAMPLE
001900     05  C-3                    PIC X(01) VALUE '3'.              SAMPLE
001910     05  C-4                    PIC X(01) VALUE '4'.              SAMPLE
001920     05  C-5                    PIC X(01) VALUE '5'.              SAMPLE
001930     05  C-6                    PIC X(01) VALUE '6'.              SAMPLE
001940     05  C-60                   PIC S9(2) VALUE +60.              SAMPLE
001950     05  C-N                    PIC X(01) VALUE 'N'.              SAMPLE
001960     05  C-Y                    PIC X(01) VALUE 'Y'.              SAMPLE
001970     05  C-NO-N                 PIC X(01) VALUE 'N'.              SAMPLE
001980     05  C-YES-Y                PIC X(01) VALUE 'Y'.              SAMPLE
001990     05  C-IN                   PIC X(03) VALUE 'IN '.            SAMPLE
002000     05  C-OUT                  PIC X(03) VALUE 'OUT'.            SAMPLE
002010     05  C-BVF                  PIC X(03) VALUE 'BVF'.            SAMPLE
002020**   05  C-CFCBVF               PIC X(06) VALUE 'CFCBVF'.         SAMPLE
002030**   05  C-CFCCAN               PIC X(06) VALUE 'CFCCAN'.         SAMPLE
002040**   05  C-CFCUS                PIC X(06) VALUE 'CFCUS '.         SAMPLE
002050     05  C-MBBVF                PIC X(06) VALUE 'MBBVF '.         SAMPLE
002060     05  C-MBCAN                PIC X(06) VALUE 'MBCAN '.         SAMPLE
002070     05  C-MBUS                 PIC X(06) VALUE 'MBUS  '.         SAMPLE
002080     05  C-TRKCAN               PIC X(06) VALUE 'TRKCAN'.         SAMPLE
002090     05  C-TRKUS                PIC X(06) VALUE 'TRKUS '.         SAMPLE
002100     05  C-3-SPLATS             PIC X(03) VALUE '***'.            SAMPLE
002110     05  C-1-SPLAT              PIC X(03) VALUE '*'.              SAMPLE
002120     05  C-2631020              PIC X(10) VALUE '2631020'.        SAMPLE
002130     05  C-2270800              PIC X(10) VALUE '2270800'.           CL**7
002140                                                                  SAMPLE
002150     05  C-HEIR-SERV-DESC       PIC X(15) VALUE 'HEIR SERV  '.    SAMPLE
002160     05  C-ZDERDLR-DESC         PIC X(15) VALUE 'ZDERDLR    '.    SAMPLE
002170     05  C-ZDERDLR-ERR-DESC     PIC X(15) VALUE 'ZDERDLR ERR'.    SAMPLE
002180     05  C-ZDERDLR-BYPASS-DESC  PIC X(15) VALUE 'ZDERDLR BYPASS'. SAMPLE
002190     05  C-CAR-OUT-DESC         PIC X(15) VALUE 'CAR OUT    '.    SAMPLE
002200     05  C-CAR-OUT-DESC-NEW     PIC X(15) VALUE 'CAR OUT NEW'.    SAMPLE
002210     05  C-TRUCK-OUT-DESC       PIC X(15) VALUE 'TRUCK OUT  '.    SAMPLE
002220     05  C-TRUCK-OUT-DESC-NEW   PIC X(15) VALUE 'TRUCK OUT NEW'.  SAMPLE
002230                                                                  SAMPLE
002240     05  WS-ERROR-CODE-DESC.                                      SAMPLE
002250         15  ERR-NO-ZPCMAST-DESC1         PIC X(30) VALUE         SAMPLE
002260              'NO ZPCMAST- STATUS/DLR TYPE'.                      SAMPLE
002270         15  ERR-NO-ZPCMAST-DESC1A        PIC X(30) VALUE         SAMPLE
002280              'NO ZPCMAST, NO ZDERDLR '.                          SAMPLE
002290         15  ERR-NO-ZPCRETL-DESC2         PIC X(30) VALUE         SAMPLE
002300              'NO ZPCRETL- STATUS/DLR TYPE'.                      SAMPLE
002310         15  ERR-NO-ZPCRETL-DESC2A        PIC X(30) VALUE         SAMPLE
002320              'NO ZPCRETL, NO ZDERDLR '.                          SAMPLE
002330         15  ERR-RDLR-NOBRND-DESC3        PIC X(30) VALUE         SAMPLE
002340              'ZDERDLR RECORD & NO BRAND '.                       SAMPLE
002350         15  ERR-NORDLR-NOBRND-DESC4      PIC X(30) VALUE         SAMPLE
002360              'NO ZDERDLR AND NO BRAND   '.                       SAMPLE
002370                                                                  SAMPLE
002380 01  WS-ACCUMULATORS.                                             SAMPLE
002390     05  WS-HEIR-SERV-COUNT-IN  PIC S9(9) COMP-3 VALUE +0.        SAMPLE
002400     05  WS-ZDERDLR-COUNT-IN    PIC S9(9) COMP-3 VALUE +0.        SAMPLE
002410     05  WS-ZDERDLR-ERR-CNTR    PIC S9(9) COMP-3 VALUE +0.        SAMPLE
002420     05  WS-ZDERDLR-BYPASS-CNTR PIC S9(9) COMP-3 VALUE +0.        SAMPLE
002430     05  WS-DEALER-CAR-OUT      PIC S9(9) COMP-3 VALUE +0.        SAMPLE
002440     05  WS-DEALER-CAR-OUT-NEW  PIC S9(9) COMP-3 VALUE +0.        SAMPLE
002450     05  WS-DEALER-TRK-OUT      PIC S9(9) COMP-3 VALUE +0.        SAMPLE
002460     05  WS-DEALER-TRK-OUT-NEW  PIC S9(9) COMP-3 VALUE +0.        SAMPLE
002470     05  WS-DEALER-BYP-OUT      PIC S9(9) COMP-3 VALUE +0.        SAMPLE
002480     05  ERROR-LINE-CNTR        PIC S9(3) COMP-3 VALUE +60.       SAMPLE
002490     05  ERROR-PAGE-CNTR        PIC S9(3) COMP-3 VALUE +0.        SAMPLE
002500                                                                  SAMPLE
002510 01  DISPLAY-TOTALS.                                              SAMPLE
002520     05  DISPLAY-IN-OUT         PIC X(04) VALUE SPACES.           SAMPLE
002530     05  DISPLAY-DESC           PIC X(20) VALUE SPACES.           SAMPLE
002540     05  DISPLAY-ACCUMULATOR    PIC ZZZ,ZZZ,ZZ9-.                 SAMPLE
002550     05  DISPLAY-FILLER         PIC X(05) VALUE SPACES.           SAMPLE
002560     05  DISPLAY-ACCUMULATOR2   PIC ZZZ,ZZZ,ZZ9-.                 SAMPLE
002570                                                                  SAMPLE
002580 01  WS-KEYS.                                                     SAMPLE
002590     05  WS-VALID-DLR-KEY.                                        SAMPLE
002600         10  WS-VALID-COMPANY        PIC X(01).                   SAMPLE
002610         10  WS-VALID-BRANCH         PIC X(05).                   SAMPLE
002620         10  FILLER REDEFINES WS-VALID-BRANCH.                    SAMPLE
002630             15  WS-VALID-AREA       PIC X(01).                   SAMPLE
002640             15  WS-VALID-REGION     PIC X(01).                   SAMPLE
002650             15  WS-VALID-ZONE       PIC X(03).                   SAMPLE
002660         10  WS-VALID-DEALER         PIC X(06).                   SAMPLE
002670         10  WS-VALID-BRAND-ID       PIC X(06).                   SAMPLE
002680      05 FILLER REDEFINES WS-VALID-DLR-KEY.                       SAMPLE
002690         10  WS-VALID-DLR-CBD        PIC X(12).                   SAMPLE
002700         10  WS-VALID-DLR-BRAND      PIC X(06).                   SAMPLE
002710                                                                  SAMPLE
002720     05  WS-ZDERDLR-KEY.                                          SAMPLE
002730         10  WS-ZDERDLR-CBD.                                      SAMPLE
002740             15  WS-ZDERDLR-CB.                                   SAMPLE
002750                 20  WS-ZDERDLR-COMPANY      PIC X(01).           SAMPLE
002760                 20  WS-ZDERDLR-BRANCH.                           SAMPLE
002770                    25  WS-ZDERDLR-AREA      PIC X(01).           SAMPLE
002780                    25  WS-ZDERDLR-REG       PIC X(01).           SAMPLE
002790                    25  WS-ZDERDLR-ZONE      PIC X(03).           SAMPLE
002800             15  WS-ZDERDLR-DEALER.                               SAMPLE
002810                    20  WS-ZDERDLR-DEALER-NMBR  PIC X(05).        SAMPLE
002820                    20  WS-ZDERDLR-DEALER-SUFF  PIC X(01).        SAMPLE
002830         10  WS-ZDERDLR-BRAND                PIC X(06).           SAMPLE
002840                                                                  SAMPLE
002850     05  WS-KEY.                                                  SAMPLE
002860         10  WS-KEY-CO-BRANCH.                                    SAMPLE
002870             15  WS-COMPANY                      PIC X(01).       SAMPLE
002880             15  WS-AREA                         PIC X(01).       SAMPLE
002890             15  WS-REGION                       PIC X(01).       SAMPLE
002900             15  WS-ZONE                         PIC X(03).       SAMPLE
002910         10  WS-BRAND-ID                         PIC X(06).          CL*11
002920 01  WS-TABLE-VALUES.                                             SAMPLE
002930                                                                  SAMPLE
002940**   05  C-REACHING-MAX-BRANDS    PIC S9(7) COMP-3 VALUE +7500.      CL*11
002950**   05  C-MAXED-BRANDS           PIC S9(7) COMP-3 VALUE +7775.      CL*11
002960     05  C-REACHING-MAX-BRANDS    PIC S9(7) COMP-3 VALUE +15.        CL*11
002970     05  C-MAXED-BRANDS           PIC S9(7) COMP-3 VALUE +18.        CL*11
002980                                                                  SAMPLE
002990     05  CNTR-SUB                 PIC S9(04) COMP   VALUE +0.     SAMPLE
003000     05  MAX-BRAND-COUNTER        PIC S9(07) COMP-3 VALUE +0.     SAMPLE
003010                                                                  SAMPLE
003020**                                                                   CL*11
003030**TABLE WAS SET HIGH FOR CF & MB WITH 8000 ELEMENTS                  CL*11
003040**      CHANGED TABLE TO 20 ELEMENTS WHICH IS STILL HIGH             CL*11
003050**    THERE ARE 4 FOR MB:                                            CL*11
003060**           13***  - MBUS                                           CL*11
003070**           143328 - TRKUS                                          CL*11
003080**           192910 - MBCAN                                          CL*11
003090**           192911 - TRKCAN                                         CL*11
003100**                                                                   CL*11
003110 01  WS-BRAND-TABLE.                                              SAMPLE
003120                                                                  SAMPLE
003130     05  BRAND-TABLE              OCCURS 1 TO 20 TIMES               CL*11
003140                                  DEPENDING ON MAX-BRAND-COUNTER  SAMPLE
003150                                  ASCENDING KEY BRND-KEY          SAMPLE
003160                                  INDEXED BY BRND-INDX.           SAMPLE
003170        10  BRND-KEY.                                             SAMPLE
003180            15  BRND-COMPANY-BRANCH.                                 CL*11
003190                20  BRND-COMPANY            PIC X(01).               CL*11
003200                20  BRND-BRANCH.                                     CL*11
003210                    25  BRND-AREA           PIC X(01).               CL*11
003220                    25  BRND-REGION         PIC X(01).               CL*11
003230                    25  BRND-ZONE           PIC X(03).               CL*11
003240        10  BRND-BRAND-ID.                                           CL*11
003250            15  BRND-BRAND-CFC      PIC X(03).                       CL*11
003260            15  BRND-BRAND-CFCBVF   PIC X(03).                       CL*11
003270            15  FILLER              PIC X(01).                       CL*11
003280        10  FILLER REDEFINES BRND-BRAND-ID.                          CL*11
003290            15  BRND-BRAND-MB       PIC X(02).                       CL*11
003300            15  BRND-BRAND-MBBVF    PIC X(03).                       CL*11
003310        10  BRND-GL-ACCT            PIC X(10).                       CL*11
003320**                                                                SAMPLE
003330 01  WS-COPYBOOKS            PIC X(20) VALUE '** WS-COPYBOOKS **'.SAMPLE
003340**                                                                SAMPLE
003350 01  WS-DEALER-REC.                                               SAMPLE
003360     COPY CZDERDL                                                 SAMPLE
003370         REPLACING ==:DEALER:==                                   SAMPLE
003380         BY        ==WS-DEALER==.                                 SAMPLE
003390**                                                                SAMPLE
003400 01  WS-RUN-DATE.                                                 SAMPLE
003410     COPY CZDERME                                                 SAMPLE
003420         REPLACING ==:RUNDATE:==                                  SAMPLE
003430         BY        ==WS-RUN-DATE==.                               SAMPLE
003440**                                                                SAMPLE
003450 01  WS-COMPANY-PARM.                                             SAMPLE
003460     COPY CZDERNM                                                 SAMPLE
003470         REPLACING ==:CORPNAME:==                                 SAMPLE
003480         BY        ==WS-CORPNAME==.                               SAMPLE
003490**                                                                SAMPLE
003500 01  WS-VALID-DLR-REC.                                            SAMPLE
003510     COPY CZDERRD                                                 SAMPLE
003520         REPLACING ==:VALDLR:==                                   SAMPLE
003530         BY        ==WS-VALID-DLR==.                              SAMPLE
003540**                                                                SAMPLE
003550 01  WS-WORK-AREAS.                                               SAMPLE
003560**                                                                SAMPLE
003570     05  WS-CITY-STATE.                                           SAMPLE
003580         10  WS-CITY              PIC X(11).                      SAMPLE
003590         10  WS-STATE             PIC X(04).                      SAMPLE
003600**                                                                SAMPLE
003610     05  WS-COMP-COMPANY.                                         SAMPLE
003620         10  WS-COMP-POS1              PIC X(01).                 SAMPLE
003630         10  WS-COMP-POS2              PIC X(01).                 SAMPLE
003640**                                                                SAMPLE
003650     05  WORK-PARAGRAPH                  PIC X(32) VALUE SPACES.  SAMPLE
003660     05  WORK-STAT-APRVL                 PIC S9(4) COMP VALUE +0. SAMPLE
003670                                                                  SAMPLE
003680     05   SYSTEM-DATE.                                            SAMPLE
003690         10  SYSTEM-DATE-CCYY          PIC X(04).                 SAMPLE
003700         10  SYSTEM-DATE-MM            PIC X(02).                 SAMPLE
003710         10  SYSTEM-DATE-DD            PIC X(02).                 SAMPLE
003720     05   SYSTEM-TIME.                                            SAMPLE
003730         10  SYSTEM-TIME-HH            PIC X(02).                 SAMPLE
003740         10  SYSTEM-TIME-MM            PIC X(02).                 SAMPLE
003750         10  SYSTEM-TIME-SS            PIC X(02).                 SAMPLE
003760         10  SYSTEM-TIME-TT            PIC X(02).                 SAMPLE
003770     05 WORK-COMPANY.                                             SAMPLE
003780         10  FILLER                      PIC X VALUE '0'.         SAMPLE
003790         10  WORK-CO                     PIC X.                   SAMPLE
003800     05  WORK-DATE              PIC 9(7).                         SAMPLE
003810     05  WORK-DATE-1 REDEFINES WORK-DATE.                         SAMPLE
003820         10  WORK-CC            PIC X.                            SAMPLE
003830         10  WORK-PART-DATE.                                      SAMPLE
003840             15 WORK-YY         PIC XX.                           SAMPLE
003850             15 WORK-MM         PIC XX.                           SAMPLE
003860             15 WORK-DD         PIC XX.                           SAMPLE
003870     05  WORK-DATE-2 REDEFINES WORK-DATE.                         SAMPLE
003880         10  WORK2-PART-DATE.                                     SAMPLE
003890             15 WORK2-MM         PIC XX.                          SAMPLE
003900             15 WORK2-DD         PIC XX.                          SAMPLE
003910             15 WORK2-YY         PIC XX.                          SAMPLE
003920         10  WORK2-CC            PIC X.                           SAMPLE
003930     05  CONVERT-DATE           PIC X(10).                        SAMPLE
003940     05  CONVERT-DATE-1 REDEFINES CONVERT-DATE.                   SAMPLE
003950         10  CONVERT-MM         PIC XX.                           SAMPLE
003960         10  FILLER             PIC X.                            SAMPLE
003970         10  CONVERT-DD         PIC XX.                           SAMPLE
003980         10  FILLER             PIC X.                            SAMPLE
003990         10  CONVERT-CC         PIC XX.                           SAMPLE
004000         10  CONVERT-YY         PIC XX.                           SAMPLE
004010                                                                  SAMPLE
004020     05  WS-ERR1.                                                 SAMPLE
004030         10 FILLER                       PIC X(01).               SAMPLE
004040         10 WS-ERR1-COMPANY              PIC X(01).               SAMPLE
004050         10 FILLER                       PIC X(01).               SAMPLE
004060         10 WS-ERR1-BRANCH               PIC X(05).               SAMPLE
004070         10 FILLER REDEFINES WS-ERR1-BRANCH.                      SAMPLE
004080            15  WS-ERR1-AREA             PIC X(01).               SAMPLE
004090            15  WS-ERR1-REGION           PIC X(01).               SAMPLE
004100            15  WS-ERR1-ZONE             PIC X(03).               SAMPLE
004110         10 FILLER                       PIC X(01).               SAMPLE
004120         10 WS-ERR1-DLR.                                          SAMPLE
004130            15 WS-ERR1-DEALER            PIC X(05).               SAMPLE
004140            15 WS-ERR1-DEALER-SUFF       PIC X(01).               SAMPLE
004150         10 FILLER                       PIC X(01).               SAMPLE
004160         10 WS-ERR1-BRAND-ID             PIC X(06).               SAMPLE
004170         10 FILLER                       PIC X(01).               SAMPLE
004180         10 WS-ERR1-DEAL-NAME            PIC X(23).               SAMPLE
004190         10 FILLER                       PIC X(01).               SAMPLE
004200         10 WS-ERR1-REGULAR              PIC ZZZZZ,ZZZ,ZZZ.99-.   SAMPLE
004210         10 FILLER                       PIC X(01).               SAMPLE
004220         10 WS-ERR1-SPECIAL              PIC ZZZZZ,ZZZ,ZZZ.99-.   SAMPLE
004230         10 FILLER                       PIC X(01).               SAMPLE
004240         10 WS-ERR1-REPURCH              PIC ZZZZZ,ZZZ,ZZZ.99-.   SAMPLE
004250         10 FILLER                       PIC X(01).               SAMPLE
004260         10 WS-ERR1-DESC                 PIC X(30).               SAMPLE
004270                                                                  SAMPLE
004280 01  ABEND-PARM.                                                  SAMPLE
004290     05  DUMP-CODE  PIC X VALUE 'Y'.                              SAMPLE
004300     05  ABEND-CODE PIC 9999 VALUE 99.                            SAMPLE
004310     05  LOG-MSG    PIC X(60) VALUE SPACES.                       SAMPLE
004320                                                                  SAMPLE
004330      EXEC SQL                                                    SAMPLE
004340          INCLUDE SQLCA                                           SAMPLE
004350      END-EXEC                                                    SAMPLE
004360                                                                  SAMPLE
004370      EXEC SQL                                                    SAMPLE
004380          INCLUDE ZDEBRND                                         SAMPLE
004390      END-EXEC                                                    SAMPLE
004400                                                                  SAMPLE
004410      EXEC SQL                                                    SAMPLE
004420          INCLUDE ZDERDLR                                         SAMPLE
004430      END-EXEC                                                    SAMPLE
004440                                                                  SAMPLE
004450      EXEC SQL                                                    SAMPLE
004460          INCLUDE ZPCMAST                                         SAMPLE
004470      END-EXEC                                                    SAMPLE
004480                                                                  SAMPLE
004490      EXEC SQL                                                    SAMPLE
004500          INCLUDE ZPCRETL                                         SAMPLE
004510      END-EXEC                                                    SAMPLE
004520                                                                  SAMPLE
004530                                                                  SAMPLE
004540     EXEC SQL                                                     SAMPLE
004550         DECLARE ZDEBRND_CURSOR CURSOR FOR                        SAMPLE
004560             SELECT                                               SAMPLE
004570             C_COMPANY                                            SAMPLE
004580            ,I_AREA                                               SAMPLE
004590            ,I_REGION                                             SAMPLE
004600            ,I_ZONE                                               SAMPLE
004610            ,C_GL_ACCOUNT                                         SAMPLE
004620            ,I_BRAND_ID                                           SAMPLE
004630             FROM TABLE1, TABLE3                                  SAMPLE
004640             ORDER BY C_COMPANY                                   SAMPLE
004650                      ,I_AREA                                     SAMPLE
004660                      ,I_REGION                                   SAMPLE
004670                      ,I_ZONE                                     SAMPLE
004680                      ,I_BRAND_ID                                    CL**7
004690                      ,C_GL_ACCOUNT                                  CL**7
004700     END-EXEC                                                     SAMPLE
004710                                                                  SAMPLE
004720     EXEC SQL                                                     SAMPLE
004730         DECLARE ZDERDLR_CURSOR CURSOR FOR                        SAMPLE
004740             SELECT                                               SAMPLE
004750             C_COMPANY                                            SAMPLE
004760            ,I_AREA                                               SAMPLE
004770            ,I_REGION                                             SAMPLE
004780            ,I_ZONE                                               SAMPLE
004790            ,I_DLR                                                SAMPLE
004800            ,I_DLR_SUFF                                           SAMPLE
004810            ,I_BRAND_ID                                           SAMPLE
004820            ,CHAR(D_RPT,USA)                                      SAMPLE
004830            ,C_DLR_NAME                                           SAMPLE
004840            ,C_STATE                                              SAMPLE
004850            ,X_ADDR                                               SAMPLE
004860            ,N_CITY                                               SAMPLE
004870            ,I_ZIP                                                SAMPLE
004880            ,A_RSVR_REGLR                                         SAMPLE
004890            ,A_RSVR_SPCL                                          SAMPLE
004900            ,A_RSVR_REPRCH                                        SAMPLE
004910            ,A_OTSTD_AUTO                                         SAMPLE
004920            ,Q_OTSTD_AUTO                                         SAMPLE
004930            ,A_OTSTD_TRK                                          SAMPLE
004940            ,Q_OTSTD_TRK                                          SAMPLE
004950            ,A_OTSTD_OTHR                                         SAMPLE
004960            ,Q_OTSTD_OTHR                                         SAMPLE
004970            ,A_VOL_AUTO                                           SAMPLE
004980            ,Q_VOL_AUTO                                           SAMPLE
004990            ,A_VOL_YTD                                            SAMPLE
005000            ,Q_VOL_YTD                                            SAMPLE
005010            ,A_OFF_CHRG_PREV                                      SAMPLE
005020            ,A_OFF_CHRG_CURR                                      SAMPLE
005030            ,CHAR(D_SHEET_PLAN,USA)                               SAMPLE
005040            ,C_FINC_RETL                                          SAMPLE
005050            ,C_FREQ_RSVR_REGLR                                    SAMPLE
005060            ,P_RSVR_REGLR_HOLD                                    SAMPLE
005070            ,A_RSVR_REGLR_HOLD                                    SAMPLE
005080            ,C_FREQ_RSVR_SPCL                                     SAMPLE
005090            ,P_RSVR_SPCL_HOLD                                     SAMPLE
005100            ,A_RSVR_SPCL_HOLD                                     SAMPLE
005110            ,C_FREQ_RSVR_REPRCH                                   SAMPLE
005120            ,P_RSVR_REPRCH_HOLD                                   SAMPLE
005130            ,A_RSVR_REPRCH_HOLD                                   SAMPLE
005140            ,C_RSVR_HOLD                                          SAMPLE
005150            ,C_OFF_WRT                                            SAMPLE
005160            ,C_RPT_DELQ_IND                                       SAMPLE
005170            ,A_OTSTD_RECRS_WTHT                                   SAMPLE
005180            ,A_OTSTD_REPRCH                                       SAMPLE
005190            ,A_OTSTD_RECRS                                        SAMPLE
005200            ,C_PRTMAIL_DLR_IND                                    SAMPLE
005210            ,C_OUT_OF_BUS                                         SAMPLE
005220            ,I_UPD                                                SAMPLE
005230            ,I_LAST_JOB_UPD                                       SAMPLE
005240            ,I_LAST_PGM_UPD                                       SAMPLE
005250            ,T_STMP_UPD                                           SAMPLE
005260            ,A_RSVR_REG_UNSOLD                                       CL**4
005270            ,A_RSVR_REG_SOLD                                         CL**4
005280             FROM TABLE2 A                                        SAMPLE
005290             ORDER BY A.I_DLR                                     SAMPLE
005300                     ,A.I_DLR_SUFF                                SAMPLE
005310                     ,A.I_BRAND_ID                                SAMPLE
005320     END-EXEC                                                     SAMPLE
005330                                                                  SAMPLE
005340******************************************************************SAMPLE
005350**                ERROR REPORT STARTS HERE                       *SAMPLE
005360******************************************************************SAMPLE
005370                                                                  SAMPLE
005380 01  ERROR-HDR1.                                                  SAMPLE
005390     05  FILLER                 PIC X(31) VALUE                   SAMPLE
005400                                ' JOB NAME     : SAMPLE'.       SAMPLE
005410     05  FILLER                 PIC X(16) VALUE SPACES.              CL**5
005420     05  HDR1-CORP-NAME         PIC X(60) VALUE SPACES.              CL**5
005430     05  FILLER                 PIC X(07) VALUE SPACES.              CL**5
005440     05  FILLER                 PIC X(6) VALUE 'DATE: '.          SAMPLE
005450     05  ERROR-DATE.                                              SAMPLE
005460         10 ERROR-MM            PIC XX.                           SAMPLE
005470         10 FILLER              PIC X VALUE '/'.                  SAMPLE
005480         10 ERROR-DD            PIC XX.                           SAMPLE
005490         10 FILLER              PIC X VALUE '/'.                  SAMPLE
005500         10 ERROR-CCYY          PIC X(04).                        SAMPLE
005510     05  FILLER                 PIC X(01) VALUE SPACES.           SAMPLE
005520                                                                  SAMPLE
005530 01  ERROR-HDR2.                                                  SAMPLE
005540     05  FILLER                 PIC X(31) VALUE                   SAMPLE
005550                                ' PROGRAM NAME : SAMPLE'.       SAMPLE
005560     05  FILLER                 PIC X(21) VALUE SPACES.           SAMPLE
005570     05  FILLER                 PIC X(42) VALUE                   SAMPLE
005580             'DEALER RESERVE DEALER SUMMARY ERROR REPORT'.        SAMPLE
005590     05  FILLER                 PIC X(21) VALUE SPACES.           SAMPLE
005600     05  FILLER                 PIC X(6)  VALUE 'TIME: '.         SAMPLE
005610     05  ERROR-TIME.                                              SAMPLE
005620         10  ERROR-HR           PIC XX.                           SAMPLE
005630         10  FILLER             PIC X VALUE '.'.                  SAMPLE
005640         10  ERROR-MIN          PIC XX.                           SAMPLE
005650         10  FILLER             PIC X VALUE '.'.                  SAMPLE
005660         10  ERROR-SEC          PIC XX.                           SAMPLE
005670     05  FILLER                 PIC X(03) VALUE SPACES.           SAMPLE
005680                                                                  SAMPLE
005690 01  ERROR-HDR3.                                                  SAMPLE
005700     05  FILLER                 PIC X(31) VALUE                   SAMPLE
005710                                ' REPORT ID    : ZDE020R2 - B1NR'.SAMPLE
005720     05  FILLER                 PIC X(84) VALUE SPACES.           SAMPLE
005730     05  FILLER                 PIC X(06) VALUE 'PAGE: '.         SAMPLE
005740     05  ERROR-PAGE             PIC ZZ9.                          SAMPLE
005750     05  FILLER                 PIC X(08) VALUE SPACES.           SAMPLE
005760                                                                  SAMPLE
005770 01  ERROR-HDR4.                                                  SAMPLE
005780     05 FILLER           PIC X(02) VALUE 'CO'.                    SAMPLE
005790     05 FILLER           PIC X(06) VALUE 'ZONE'.                  SAMPLE
005800     05 FILLER           PIC X(07) VALUE 'DEALER'.                SAMPLE
005810     05 FILLER           PIC X(07) VALUE 'BRAND'.                 SAMPLE
005820     05 FILLER           PIC X(24) VALUE 'DEALER NAME'.           SAMPLE
005830     05 FILLER           PIC X(18) VALUE '           REGULAR'.    SAMPLE
005840     05 FILLER           PIC X(18) VALUE '           SPECIAL'.    SAMPLE
005850     05 FILLER           PIC X(19) VALUE '           REPURCH'.    SAMPLE
005860     05 FILLER           PIC X(30) VALUE 'ERROR DESCRIPTION '.    SAMPLE
005870                                                                  SAMPLE
005880 01  ERROR-HDR-DASHES.                                            SAMPLE
005890     05  FILLER              PIC X(01) VALUE SPACES.              SAMPLE
005900     05  ERROR-DASH          PIC X(131) VALUE ALL '-'.            SAMPLE
005910                                                                  SAMPLE
005920 01  ERROR-DET1.                                                  SAMPLE
005930     05  FILLER                PIC X(01) VALUE SPACES.            SAMPLE
005940     05  FILLER                PIC X(131) VALUE SPACES.           SAMPLE
005950                                                                  SAMPLE
005960 01  WORKING-STORAGE-END.                                         SAMPLE
005970     05  FILLER                 PIC X(31)                         SAMPLE
005980         VALUE '** WORKING STORAGE ENDS HERE **'.                 SAMPLE
005990                                                                  SAMPLE
006000 PROCEDURE DIVISION.                                              SAMPLE
006010 0000-MAIN.                                                       SAMPLE
006020******************************************************************SAMPLE
006030*    MAIN BODY                                                   *SAMPLE
006040******************************************************************SAMPLE
006050     MOVE '0000-MAIN       '    TO WORK-PARAGRAPH                 SAMPLE
006060                                                                  SAMPLE
006070     PERFORM 1000-HOUSEKEEPING  THRU 1000-EXIT                    SAMPLE
006080     PERFORM 2000-PROCESS       THRU 2000-EXIT                    SAMPLE
006090                                UNTIL ZDERDLR-EOF                 SAMPLE
006100                                  AND HIER-SERV-EOF               SAMPLE
006110     PERFORM 9999-FINISH        THRU 9999-EXIT                    SAMPLE
006120     .                                                            SAMPLE
006130 0000-EXIT.                                                       SAMPLE
006140     STOP RUN.                                                    SAMPLE
006150                                                                  SAMPLE
006160 1000-HOUSEKEEPING.                                               SAMPLE
006170******************************************************************SAMPLE
006180*    INITIALIZATION PROCESS                                      *SAMPLE
006190******************************************************************SAMPLE
006200     MOVE '1000-HOUSEKEEPING'    TO WORK-PARAGRAPH                SAMPLE
006210                                                                  SAMPLE
006220     OPEN INPUT   RUN-DATE-FILE                                   SAMPLE
006230                  PROFILE-VALID-DLR                               SAMPLE
006240                  COMPANY-PARM                                    SAMPLE
006250     OPEN OUTPUT  DLRSUMM-CARMB                                   SAMPLE
006260                  DLRSUMM-TRKBVF                                  SAMPLE
006270                  DLRSUMM-BYPASS                                  SAMPLE
006280                  ERROR-REPORT                                    SAMPLE
006290                                                                  SAMPLE
006300     ACCEPT SYSTEM-DATE FROM DATE YYYYMMDD                        SAMPLE
006310                                                                  SAMPLE
006320     MOVE SYSTEM-DATE-MM         TO ERROR-MM                      SAMPLE
006330     MOVE SYSTEM-DATE-DD         TO ERROR-DD                      SAMPLE
006340     MOVE SYSTEM-DATE-CCYY       TO ERROR-CCYY                    SAMPLE
006350                                                                  SAMPLE
006360     ACCEPT SYSTEM-TIME FROM TIME                                 SAMPLE
006370                                                                  SAMPLE
006380     MOVE SYSTEM-TIME-HH         TO ERROR-HR                      SAMPLE
006390     MOVE SYSTEM-TIME-MM         TO ERROR-MIN                     SAMPLE
006400     MOVE SYSTEM-TIME-SS         TO ERROR-SEC                     SAMPLE
006410                                                                  SAMPLE
006420     INITIALIZE                    ZPCMAST                        SAMPLE
006430                                   ZPCRETL                        SAMPLE
006440                                   ZDERDLR                        SAMPLE
006450                                   ZDEBRND                        SAMPLE
006460*                                                                 SAMPLE
006470     PERFORM 7000-OPEN-XYZXYZA  THRU 7000-EXIT                    SAMPLE
006480*                                                                 SAMPLE
006490     MOVE HIGH-VALUES     TO WS-BRAND-TABLE                       SAMPLE
006500*                                                                 SAMPLE
006510     MOVE C-N             TO WS-BRAND-EOF                         SAMPLE
006520     SET BRND-INDX        TO +1                                   SAMPLE
006530                                                                  SAMPLE
006540     PERFORM 1010-BUILD-BRAND-TABLE THRU 1010-EXIT                SAMPLE
006550       VARYING CNTR-SUB  FROM 1 BY 1                              SAMPLE
006560           UNTIL BRAND-EOF                                        SAMPLE
006570*                                                                 SAMPLE
006580     PERFORM 7400-GET-RUN-DATE  THRU 7400-EXIT                    SAMPLE
006590*                                                                 SAMPLE
006600     PERFORM 7450-GET-COMP-PARMTR THRU 7450-EXIT                    SAMPLE
006610     MOVE WS-CORPNAME-LEGAL-NAME  TO HDR1-CORP-NAME               SAMPLE
006620*                                                                 SAMPLE
006630     PERFORM 7500-GET-VALID-DLR THRU 7500-EXIT                    SAMPLE
006640*                                                                 SAMPLE
006650     PERFORM 7010-OPEN-TABLE  THRU 7010-EXIT                    SAMPLE
006660*                                                                 SAMPLE
006670     PERFORM 7100-FETCH-ZDERDLR THRU 7100-EXIT                    SAMPLE
006680     .                                                            SAMPLE
006690 1000-EXIT.                                                       SAMPLE
006700     EXIT.                                                        SAMPLE
006710 1010-BUILD-BRAND-TABLE.                                          SAMPLE
006720******************************************************************SAMPLE
006730*    BUILD BRAND TABLE                                           *SAMPLE
006740******************************************************************SAMPLE
006750*                                                                 SAMPLE
006760     MOVE '1010-BUILD-BRAND-TABLE '  TO WORK-PARAGRAPH            SAMPLE
006770*                                                                 SAMPLE
006780     EXEC SQL                                                     SAMPLE
006790         FETCH ZDEBRND_CURSOR                                     SAMPLE
006800         INTO                                                     SAMPLE
006810             :ZDEBRND.C-COMPANY                                   SAMPLE
006820            ,:ZDEBRND.I-AREA                                      SAMPLE
006830            ,:ZDEBRND.I-REGION                                    SAMPLE
006840            ,:ZDEBRND.I-ZONE                                      SAMPLE
006850            ,:ZDEBRND.C-GL-ACCOUNT                                SAMPLE
006860            ,:ZDEBRND.I-BRAND-ID                                  SAMPLE
006870     END-EXEC                                                     SAMPLE
006880*                                                                 SAMPLE
006890     EVALUATE SQLCODE                                             SAMPLE
006900         WHEN +0                                                  SAMPLE
006910              CONTINUE                                            SAMPLE
006920         WHEN +100                                                SAMPLE
006930              MOVE 'Y'           TO WS-BRAND-EOF                  SAMPLE
006940         WHEN OTHER                                               SAMPLE
006950              PERFORM 9960-DB2-ERROR THRU 9960-EXIT               SAMPLE
006960     END-EVALUATE                                                 SAMPLE
006970*                                                                 SAMPLE
006980     IF WS-BRAND-EOF NOT = 'Y'                                    SAMPLE
006990      IF I-BRAND-ID OF ZDEBRND NOT =  C-MBBVF                        CL*10
007000       IF C-GL-ACCOUNT OF ZDEBRND =  C-2631020 OR                 SAMPLE
007010                                     C-2270800                       CL**7
007020        MOVE C-COMPANY    OF ZDEBRND TO BRND-COMPANY   (BRND-INDX)SAMPLE
007030        MOVE I-AREA       OF ZDEBRND TO BRND-AREA      (BRND-INDX)SAMPLE
007040        MOVE I-REGION     OF ZDEBRND TO BRND-REGION    (BRND-INDX)SAMPLE
007050        MOVE I-ZONE       OF ZDEBRND TO BRND-ZONE      (BRND-INDX)SAMPLE
007060        MOVE C-GL-ACCOUNT OF ZDEBRND TO BRND-GL-ACCT   (BRND-INDX)SAMPLE
007070        MOVE I-BRAND-ID   OF ZDEBRND TO BRND-BRAND-ID  (BRND-INDX)SAMPLE
007080        ADD +1                       TO MAX-BRAND-COUNTER         SAMPLE
007090        DISPLAY 'BRND-KEY ' BRND-KEY (BRND-INDX)                     CL*10
007100                ' BRND-GL-ACCT ' BRND-GL-ACCT (BRND-INDX)            CL*10
007110                ' MAX-BRAND-COUNTER  ' MAX-BRAND-COUNTER             CL*10
007120        SET  BRND-INDX    UP BY +1                                SAMPLE
007130        IF MAX-BRAND-COUNTER > C-REACHING-MAX-BRANDS              SAMPLE
007140            DISPLAY ' BRAND ID TABLE REACHING MAX, INCREASE SIZE' SAMPLE
007150            DISPLAY ' # OF ENTRIES : ' MAX-BRAND-COUNTER          SAMPLE
007160                    ' SAFETY CNTR  : ' C-REACHING-MAX-BRANDS      SAMPLE
007170                    ' PGRM WILL ABEND WHEN >: ' C-MAXED-BRANDS    SAMPLE
007180        ELSE                                                      SAMPLE
007190            IF MAX-BRAND-COUNTER > C-MAXED-BRANDS                 SAMPLE
007200                DISPLAY ' BRAND ID TABLE REACHED MAX, INCREASE '  SAMPLE
007210                        ' INCREASE SIZE NOW, PROGRAM ABENDED'     SAMPLE
007220                PERFORM 9900-ABEND THRU 9900-EXIT                 SAMPLE
007230            END-IF                                                SAMPLE
007240        END-IF                                                    SAMPLE
007250       END-IF                                                     SAMPLE
007260      END-IF                                                         CL*10
007270     END-IF                                                       SAMPLE
007280     .                                                            SAMPLE
007290 1010-EXIT.                                                       SAMPLE
007300     EXIT.                                                        SAMPLE
007310                                                                  SAMPLE
007320 2000-PROCESS.                                                    SAMPLE
007330******************************************************************SAMPLE
007340*    MAIN PROCESS LOOP, PROCESS CURSOR                           *SAMPLE
007350******************************************************************SAMPLE
007360*                                                                 SAMPLE
007370     MOVE '2000-PROCESS           '  TO WORK-PARAGRAPH            SAMPLE
007380*                                                                 SAMPLE
007390     MOVE 'N'              TO WS-TABLE-ENTRY-FOUND-SW             SAMPLE
007400     MOVE 'N'              TO NEW-FZCOMMON-SW                     SAMPLE
007410     MOVE 'N'              TO VALID-DEALER-SW                     SAMPLE
007420     MOVE ' '              TO WS-DEALER-ONLY                      SAMPLE
007430*                                                                 SAMPLE
007440* CBD = COMPANY/BRANCH/DEALER                                     SAMPLE
007450*                                                                 SAMPLE
007460     IF WS-VALID-DEALER  > WS-ZDERDLR-DEALER                      SAMPLE
007470         ADD +1               TO  WS-ZDERDLR-BYPASS-CNTR          SAMPLE
007480         PERFORM 2800-BYPASS-TABLE     THRU 2800-EXIT           SAMPLE
007490         PERFORM 7100-FETCH-ZDERDLR      THRU 7100-EXIT           SAMPLE
007500     ELSE                                                         SAMPLE
007510         IF WS-VALID-DEALER  = WS-ZDERDLR-DEALER                  SAMPLE
007520             PERFORM 2050-MATCHED        THRU 2050-EXIT           SAMPLE
007530         ELSE                                                     SAMPLE
007540             IF WS-VALID-DEALER  < WS-ZDERDLR-DEALER              SAMPLE
007550                 PERFORM 2075-LESS-THAN  THRU 2075-EXIT           SAMPLE
007560             END-IF                                               SAMPLE
007570         END-IF                                                   SAMPLE
007580     END-IF                                                       SAMPLE
007590     .                                                            SAMPLE
007600 2000-EXIT.                                                       SAMPLE
007610     EXIT.                                                        SAMPLE
007620                                                                  SAMPLE
007630 2050-MATCHED.                                                    SAMPLE
007640******************************************************************SAMPLE
007650*    ROUTINE WILL:                                               *SAMPLE
007660*      1 - GET DATA FROM PROFILE TABLES                          *SAMPLE
007670*          A - PUT TO ERROR REPORT IF SQLCODE NOT +0             *SAMPLE
007680*      2 - BUILD KEY TO SEARCH FOR BRAND                         *SAMPLE
007690*          PERFORM BRAND SEARCH UP TO 3 TIMES                    *SAMPLE
007700*            1 - EXACT MATCH COMPANY,AREA,REGION,ZONE            *SAMPLE
007710*            2 - MATCH COMPANY,AREA,REGION,ANY ZONE              *SAMPLE
007720*            3 - MATCH COMPANY,AREA,ANY REGION,ANY ZONE          *SAMPLE
007730******************************************************************SAMPLE
007740*                                                                 SAMPLE
007750     MOVE '2050-MATCHED           '  TO WORK-PARAGRAPH            SAMPLE
007760                                                                  SAMPLE
007770     PERFORM 4000-DB2-TABLES THRU 4000-EXIT                       SAMPLE
007780                                                                  SAMPLE
007790     IF SQLCODE NOT = +0                                          SAMPLE
007800         ADD +1               TO WS-ZDERDLR-ERR-CNTR              SAMPLE
007810         PERFORM 3000-ERROR-COMMONS    THRU 3000-EXIT           SAMPLE
007820         PERFORM 7100-FETCH-ZDERDLR      THRU 7100-EXIT           SAMPLE
007830         PERFORM 2058-CHECK-DEALER       THRU 2058-EXIT           SAMPLE
007840     ELSE                                                         SAMPLE
007850         MOVE WS-VALID-COMPANY           TO WS-COMPANY            SAMPLE
007860         MOVE WS-VALID-AREA              TO WS-AREA               SAMPLE
007870         MOVE WS-VALID-REGION            TO WS-REGION             SAMPLE
007880         MOVE WS-VALID-ZONE              TO WS-ZONE               SAMPLE
007890         MOVE I-BRAND-ID OF ZDERDLR      TO WS-BRAND-ID           SAMPLE
007900                                                                  SAMPLE
007910* SEARCH WILL GO THRU 3 REVOLUTIONS,EXACT MATCH,ANY ZONE,ANY REG  SAMPLE
007920                                                                  SAMPLE
007930         MOVE C-NO-N                     TO WS-SEARCH-EXHAUSTED   SAMPLE
007940         PERFORM 2053-SEARCH-3-TIMES     THRU 2053-EXIT           SAMPLE
007950             UNTIL SEARCH-EXHAUSTED                               SAMPLE
007960     END-IF                                                       SAMPLE
007970     .                                                            SAMPLE
007980 2050-EXIT.                                                       SAMPLE
007990     EXIT.                                                        SAMPLE
008000                                                                  SAMPLE
008010 2053-SEARCH-3-TIMES.                                             SAMPLE
008020******************************************************************SAMPLE
008030*    ROUTINE WILL:                                               *SAMPLE
008040*      1 - SEARCH WITH KEYS AND SET ZONE THEN REGION TO SPLAT    *SAMPLE
008050*            1 - EXACT MATCH COMPANY,AREA,REGION,ZONE            *SAMPLE
008060*            2 - MATCH COMPANY,AREA,REGION,ANY ZONE              *SAMPLE
008070*            3 - MATCH COMPANY,AREA,ANY REGION,ANY ZONE          *SAMPLE
008080*      2 - PERFORM TABLE ENTRY FOUND ROUTINE                     *SAMPLE
008090******************************************************************SAMPLE
008100*                                                                 SAMPLE
008110     MOVE '2053-SEARCH-3-TIMES    '  TO WORK-PARAGRAPH            SAMPLE
008120                                                                  SAMPLE
008130     PERFORM 2180-SEARCH-BRAND-TABLE THRU 2180-EXIT               SAMPLE
008140     PERFORM 2055-TABLE-ENTRY-FOUND  THRU 2055-EXIT               SAMPLE
008150                                                                  SAMPLE
008160     IF NOT SEARCH-EXHAUSTED                                      SAMPLE
008170         IF WS-ZONE = C-3-SPLATS                                  SAMPLE
008180             IF WS-REGION = C-1-SPLAT                             SAMPLE
008190                 MOVE C-3       TO WS-ERROR-CODE                  SAMPLE
008200                 ADD +1           TO WS-ZDERDLR-ERR-CNTR          SAMPLE
008210                 PERFORM 3000-ERROR-COMMONS THRU 3000-EXIT      SAMPLE
008220                 PERFORM 7100-FETCH-ZDERDLR   THRU 7100-EXIT      SAMPLE
008230                 PERFORM 2058-CHECK-DEALER    THRU 2058-EXIT      SAMPLE
008240                 MOVE C-YES-Y   TO WS-SEARCH-EXHAUSTED            SAMPLE
008250             ELSE                                                 SAMPLE
008260                 MOVE C-1-SPLAT TO WS-REGION                      SAMPLE
008270             END-IF                                               SAMPLE
008280         ELSE                                                     SAMPLE
008290             MOVE C-3-SPLATS    TO WS-ZONE                        SAMPLE
008300         END-IF                                                   SAMPLE
008310     END-IF                                                       SAMPLE
008320     .                                                            SAMPLE
008330 2053-EXIT.                                                       SAMPLE
008340     EXIT.                                                        SAMPLE
008350                                                                  SAMPLE
008360 2055-TABLE-ENTRY-FOUND.                                          SAMPLE
008370******************************************************************SAMPLE
008380*    ROUTINE WILL CHECK:                                         *SAMPLE
008390*      1 - ENTRY ON BRAND TABLE                                  *SAMPLE
008400*      2 - BVF BRAND AND DEALER IS A BVF DEALER                  *SAMPLE
008410*      3 - PERFORM ROUTINE TO VERIFY BRAND AGAINST Z.ZDERDLR TABLESAMPLE
008420******************************************************************SAMPLE
008430*                                                                 SAMPLE
008440     MOVE '2055-TABLE-ERNTY-FOUND '  TO WORK-PARAGRAPH            SAMPLE
008450                                                                  SAMPLE
008460     IF TABLE-ENTRY-FOUND                                         SAMPLE
008470**       IF BRND-BRAND-CFCBVF (BRND-INDX) = C-BVF OR              SAMPLE
008480         IF BRND-BRAND-MBBVF  (BRND-INDX) = C-BVF                 SAMPLE
008490             IF L-CMRC-FINC-OPT OF ZPCMAST = C-1                  SAMPLE
008500                 PERFORM 2057-CHECK-BRAND-ID THRU 2057-EXIT       SAMPLE
008510             ELSE                                                 SAMPLE
008520                 IF BRND-BRAND-ID (BRND-INDX) = WS-BRAND-ID       SAMPLE
008530                     MOVE C-6         TO WS-ERROR-CODE            SAMPLE
008540                     ADD +1           TO WS-ZDERDLR-ERR-CNTR      SAMPLE
008550                     PERFORM 3000-ERROR-COMMONS  THRU 3000-EXIT SAMPLE
008560                     PERFORM 7100-FETCH-ZDERDLR    THRU 7100-EXIT SAMPLE
008570                     PERFORM 2058-CHECK-DEALER     THRU 2058-EXIT SAMPLE
008580                     MOVE C-YES-Y     TO WS-SEARCH-EXHAUSTED      SAMPLE
008590                 END-IF                                           SAMPLE
008600             END-IF                                               SAMPLE
008610         ELSE                                                     SAMPLE
008620             PERFORM 2057-CHECK-BRAND-ID THRU 2057-EXIT           SAMPLE
008630         END-IF                                                   SAMPLE
008640     END-IF                                                       SAMPLE
008650     .                                                            SAMPLE
008660 2055-EXIT.                                                       SAMPLE
008670     EXIT.                                                        SAMPLE
008680                                                                  SAMPLE
008690 2057-CHECK-BRAND-ID.                                             SAMPLE
008700******************************************************************SAMPLE
008710*    ROUTINE WILL CHECK:                                         *SAMPLE
008720*      1 - TABLE BRAND ID TO FZCOMMON BRAND ID                   *SAMPLE
008730*      2 - IF BRAND ID IS EQUAL, THEN A FZCOMMON REC IS CREATED  *SAMPLE
008740*      3 - IF BRAND ID LESS, THEN A FZCOMMON REC IS CREATED      *SAMPLE
008750*      3 - IF BRAND ID GREATER, THEN A REC COULD EXIST ON ZDERDLR*SAMPLE
008760*          AND NO ACTION WILL BE TAKEN                           *SAMPLE
008770******************************************************************SAMPLE
008780*                                                                 SAMPLE
008790     MOVE '2057-CHECK-BRAND-ID    '  TO WORK-PARAGRAPH            SAMPLE
008800                                                                  SAMPLE
008810     IF BRND-BRAND-ID (BRND-INDX) = WS-BRAND-ID                   SAMPLE
008820         MOVE C-NO-N      TO NEW-FZCOMMON-SW                      SAMPLE
008830         PERFORM 2200-CREATE-FZCOMMON  THRU 2200-EXIT             SAMPLE
008840         PERFORM 7100-FETCH-ZDERDLR    THRU 7100-EXIT             SAMPLE
008850         PERFORM 2058-CHECK-DEALER     THRU 2058-EXIT             SAMPLE
008860         MOVE C-YES-Y     TO WS-SEARCH-EXHAUSTED                  SAMPLE
008870         PERFORM 2057A-IDENTIFY-BVF    THRU 2057A-EXIT            SAMPLE
008880     ELSE                                                         SAMPLE
008890         IF BRND-BRAND-ID (BRND-INDX) < WS-BRAND-ID               SAMPLE
008900             IF BVF-DEALER-YES                                    SAMPLE
008910                IF VALID-DLR-DEALER  = WS-BVF-DEALER              SAMPLE
008920                    NEXT SENTENCE                                 SAMPLE
008930                ELSE                                              SAMPLE
008940                    MOVE C-YES-Y     TO NEW-FZCOMMON-SW           SAMPLE
008950                    PERFORM 2200-CREATE-FZCOMMON  THRU 2200-EXIT  SAMPLE
008960             ELSE                                                 SAMPLE
008970                MOVE C-YES-Y     TO NEW-FZCOMMON-SW               SAMPLE
008980                PERFORM 2200-CREATE-FZCOMMON  THRU 2200-EXIT      SAMPLE
008990         END-IF                                                   SAMPLE
009000     END-IF                                                       SAMPLE
009010     .                                                            SAMPLE
009020 2057-EXIT.                                                       SAMPLE
009030     EXIT.                                                        SAMPLE
009040                                                                  SAMPLE
009050 2057A-IDENTIFY-BVF.                                              SAMPLE
009060******************************************************************SAMPLE
009070*    ROUTINE WILL IDENTIFY WHEN A PREVIOUS RECORD IS A BVF, SO   *SAMPLE
009080*      A DUMMY ONE WILL NOT BE CREATED.                          *SAMPLE
009090******************************************************************SAMPLE
009100*                                                                 SAMPLE
009110     MOVE '2057-CHECK-BRAND-ID    '  TO WORK-PARAGRAPH            SAMPLE
009120                                                                  SAMPLE
009130**   IF WS-BRAND-ID = C-CFCBVF OR                                 SAMPLE
009140     IF WS-BRAND-ID = C-MBBVF                                     SAMPLE
009150        MOVE C-YES-Y           TO WS-BVF-DEALER-SW                SAMPLE
009160        MOVE VALID-DLR-DEALER  TO WS-BVF-DEALER                   SAMPLE
009170     END-IF                                                       SAMPLE
009180     .                                                            SAMPLE
009190 2057A-EXIT.                                                      SAMPLE
009200     EXIT.                                                        SAMPLE
009210**                                                                SAMPLE
009220 2058-CHECK-DEALER.                                               SAMPLE
009230******************************************************************SAMPLE
009240*    ROUTINE WILL CHECK:                                         *SAMPLE
009250*      1 - AFTER ZDERDLR RECORD IS READ, IT WILL CHECK THE DEALER*SAMPLE
009260*          TO DETERMINE IF                                       *SAMPLE
009270*          A - THERE ARE ANY MORE ZDERDLR RECORDS THAT NEED TO   *SAMPLE
009280*              PROCESS AGAINST THIS DEALER                       *SAMPLE
009290*          B - REST OF THE TABLE NEEDS TO BE REVIEWED FOR MORE   *SAMPLE
009300*              RECORDS TO ADD TO THE FZCOMMON                    *SAMPLE
009310*      2 - WHEN THESE CONDITIONS ARE MET, THEN GET NEXT VALID DLR*SAMPLE
009320******************************************************************SAMPLE
009330*                                                                 SAMPLE
009340     MOVE '2058-CHECK-DEALER      '  TO WORK-PARAGRAPH            SAMPLE
009350**                                                                SAMPLE
009360** CORRELATING SEQUENCE, CFCBVF OR MBBVF WILL COME BEFORE THESE   SAMPLE
009370** DEALERS, THUS IF THERE ARE MULTIPLE RECORDS, THE CFCBVF/MBBVF  SAMPLE
009380** WILL NOT CAUSE A READ TO THE 7500-GET-VALID-DLR PARA, THUS     SAMPLE
009390** ADVANCING TO THE NEXT DEALER.                                  SAMPLE
009400**                                                                SAMPLE
009410**  SO WHEN A CFCBVF OR MBBVF ENTRY IS CREATED, WE NEED TO SET A  SAMPLE
009420**  SWITCH AND STORE THE DEALER NUMBER.                           SAMPLE
009430**  IF THE SWITCH IS ON AND THE DEALER NUMBER IS THE SAME AS THE OSAMPLE
009440**  WE ARE TRYING TO CREATE, DO NOT CREATE THE RECORD, BUT BYPASS SAMPLE
009450**                                                                SAMPLE
009460     IF WS-VALID-DEALER = WS-ZDERDLR-DEALER                       SAMPLE
009470**       IF WS-BRAND-ID = C-CFCCAN OR                             SAMPLE
009480         IF WS-BRAND-ID = C-MBCAN  OR                             SAMPLE
009490                          C-TRKCAN OR                             SAMPLE
009500                          C-TRKUS                                 SAMPLE
009510             PERFORM 7500-GET-VALID-DLR THRU 7500-EXIT            SAMPLE
009520         ELSE                                                     SAMPLE
009530**           IF WS-BRAND-ID = C-CFCUS OR                          SAMPLE
009540             IF WS-BRAND-ID = C-MBUS                              SAMPLE
009550                 PERFORM 7500-GET-VALID-DLR THRU 7500-EXIT        SAMPLE
009560             END-IF                                               SAMPLE
009570         END-IF                                                   SAMPLE
009580     ELSE                                                         SAMPLE
009590         PERFORM 7500-GET-VALID-DLR   THRU 7500-EXIT              SAMPLE
009600     END-IF                                                       SAMPLE
009610     .                                                            SAMPLE
009620 2058-EXIT.                                                       SAMPLE
009630     EXIT.                                                        SAMPLE
009640                                                                  SAMPLE
009650 2075-LESS-THAN.                                                  SAMPLE
009660******************************************************************SAMPLE
009670*    DEALER IS A VALID DEALER, BUT NO ZDERDLR ROW FOUND          *SAMPLE
009680*      VERIFY WITH BRAND TABLE A RECORD NEEDS TO BE CREATED      *SAMPLE
009690*    ROUTINE WILL:                                               *SAMPLE
009700*      1 - GET DATA FROM PROFILE TABLES                          *SAMPLE
009710*          A - PUT TO ERROR REPORT IF SQLCODE NOT +0             *SAMPLE
009720*      2 - BUILD KEY TO SEARCH FOR BRAND                         *SAMPLE
009730*          PERFORM BRAND SEARCH UP TO 3 TIMES                    *SAMPLE
009740*            1 - EXACT MATCH COMPANY,AREA,REGION,ZONE            *SAMPLE
009750*            2 - MATCH COMPANY,AREA,REGION,ANY ZONE              *SAMPLE
009760*            3 - MATCH COMPANY,AREA,ANY REGION,ANY ZONE          *SAMPLE
009770*      IF WE GET AN EXACT MATCH, THERE COULD ALSO BE ANOTHER     *SAMPLE
009780*      BRAND ID THAT IS GENERIC THAT WOULD HAVE TO BE SET UP     *SAMPLE
009790*      SO KEEP CHECKING AFTER FIRST HIT                          *SAMPLE
009800******************************************************************SAMPLE
009810*                                                                 SAMPLE
009820     MOVE '2075-LESS-THAN         '  TO WORK-PARAGRAPH            SAMPLE
009830                                                                  SAMPLE
009840     PERFORM 4000-DB2-TABLES THRU 4000-EXIT                       SAMPLE
009850                                                                  SAMPLE
009860     IF SQLCODE NOT = +0                                          SAMPLE
009870         MOVE C-YES-Y                    TO WS-DEALER-ONLY        SAMPLE
009880         PERFORM 3000-ERROR-COMMONS    THRU 3000-EXIT           SAMPLE
009890     ELSE                                                         SAMPLE
009900         MOVE WS-VALID-COMPANY           TO WS-COMPANY            SAMPLE
009910         MOVE WS-VALID-AREA              TO WS-AREA               SAMPLE
009920         MOVE WS-VALID-REGION            TO WS-REGION             SAMPLE
009930         MOVE WS-VALID-ZONE              TO WS-ZONE               SAMPLE
009940         MOVE SPACES                     TO WS-BRAND-ID           SAMPLE
009950                                                                  SAMPLE
009960* SEARCH WILL GO THRU 3 REVOLUTIONS, EXACT MATCH, ANY ZONE, ANY RESAMPLE
009970                                                                  SAMPLE
009980         MOVE C-NO-N                     TO WS-SEARCH-EXHAUSTED   SAMPLE
009990         MOVE C-NO-N                     TO WS-AT-LEAST-1-FOUND   SAMPLE
010000                                                                  SAMPLE
010010         PERFORM 2078-THREE-TIME-SEARCH  THRU 2078-EXIT           SAMPLE
010020             UNTIL SEARCH-EXHAUSTED                               SAMPLE
010030                                                                  SAMPLE
010040         IF NOT TABLE-ENTRY-FOUND                                 SAMPLE
010050             IF NOT AT-LEAST-1-FOUND                              SAMPLE
010060                 MOVE C-4                     TO WS-ERROR-CODE    SAMPLE
010070                 PERFORM 3000-ERROR-COMMONS THRU 3000-EXIT      SAMPLE
010080             END-IF                                               SAMPLE
010090         END-IF                                                   SAMPLE
010100     END-IF                                                       SAMPLE
010110                                                                  SAMPLE
010120     PERFORM 7500-GET-VALID-DLR THRU 7500-EXIT                    SAMPLE
010130     .                                                            SAMPLE
010140 2075-EXIT.                                                       SAMPLE
010150     EXIT.                                                        SAMPLE
010160                                                                  SAMPLE
010170 2078-THREE-TIME-SEARCH.                                          SAMPLE
010180******************************************************************SAMPLE
010190*    ROUTINE WILL:                                               *SAMPLE
010200*      1 - SEARCH WITH KEYS AND SET ZONE THEN REGION TO SPLAT    *SAMPLE
010210*            1 - EXACT MATCH COMPANY,AREA,REGION,ZONE            *SAMPLE
010220*            2 - MATCH COMPANY,AREA,REGION,ANY ZONE              *SAMPLE
010230*            3 - MATCH COMPANY,AREA,ANY REGION,ANY ZONE          *SAMPLE
010240*      2 - PERFORM TABLE ENTRY FOUND ROUTINE                     *SAMPLE
010250******************************************************************SAMPLE
010260*                                                                 SAMPLE
010270     MOVE '2078-THREE-TIME-SEARCH  ' TO WORK-PARAGRAPH            SAMPLE
010280                                                                  SAMPLE
010290     PERFORM 2180-SEARCH-BRAND-TABLE THRU 2180-EXIT               SAMPLE
010300     PERFORM 2080-CHECK-TABLE-ENTRY  THRU 2080-EXIT               SAMPLE
010310                                                                  SAMPLE
010320     IF NOT SEARCH-EXHAUSTED                                      SAMPLE
010330         IF WS-ZONE = C-3-SPLATS                                  SAMPLE
010340             IF WS-REGION  = C-1-SPLAT                            SAMPLE
010350                 MOVE C-YES-Y   TO WS-SEARCH-EXHAUSTED            SAMPLE
010360             ELSE                                                 SAMPLE
010370                 MOVE C-1-SPLAT TO WS-REGION                      SAMPLE
010380             END-IF                                               SAMPLE
010390         ELSE                                                     SAMPLE
010400             MOVE C-3-SPLATS    TO WS-ZONE                        SAMPLE
010410         END-IF                                                   SAMPLE
010420     END-IF                                                       SAMPLE
010430     .                                                            SAMPLE
010440 2078-EXIT.                                                       SAMPLE
010450     EXIT.                                                        SAMPLE
010460                                                                  SAMPLE
010470 2080-CHECK-TABLE-ENTRY.                                          SAMPLE
010480******************************************************************SAMPLE
010490*    ROUTINE WILL CHECK:                                         *SAMPLE
010500*      1 - ENTRY ON BRAND TABLE                                  *SAMPLE
010510*      2 - BVF BRAND AND DEALER IS A BVF DEALER                  *SAMPLE
010520*          A - CREATE FZCOMMON RECORD                            *SAMPLE
010530*      3 - OR ANY NON-BVF ENTRY ON BRAND TABLE                   *SAMPLE
010540*          A - CREATE FZCOMMON RECORD                            *SAMPLE
010550******************************************************************SAMPLE
010560*                                                                 SAMPLE
010570     MOVE '2080-CHECK-TABLE-ENTRY '  TO WORK-PARAGRAPH            SAMPLE
010580                                                                  SAMPLE
010590     IF TABLE-ENTRY-FOUND                                         SAMPLE
010600**       IF BRND-BRAND-CFCBVF (BRND-INDX) = C-BVF OR              SAMPLE
010610**       IF BRND-BRAND-MBBVF  (BRND-INDX) = C-BVF                    CL*11
010620**           IF L-CMRC-FINC-OPT OF ZPCMAST = C-1                     CL*11
010630**               MOVE C-YES-Y     TO NEW-FZCOMMON-SW                 CL*11
010640**               MOVE C-YES-Y     TO WS-AT-LEAST-1-FOUND             CL*11
010650**               PERFORM 2200-CREATE-FZCOMMON  THRU 2200-EXIT        CL*11
010660**           END-IF                                                  CL*11
010670**       ELSE                                                        CL*11
010680             MOVE C-YES-Y     TO NEW-FZCOMMON-SW                  SAMPLE
010690             MOVE C-YES-Y     TO WS-AT-LEAST-1-FOUND              SAMPLE
010700             PERFORM 2200-CREATE-FZCOMMON  THRU 2200-EXIT         SAMPLE
010710**       END-IF                                                      CL*11
010720     END-IF                                                       SAMPLE
010730     .                                                            SAMPLE
010740 2080-EXIT.                                                       SAMPLE
010750     EXIT.                                                        SAMPLE
010760                                                                  SAMPLE
010770 2180-SEARCH-BRAND-TABLE.                                         SAMPLE
010780******************************************************************SAMPLE
010790*     SEARCH BRAND TABLE                                          SAMPLE
010800******************************************************************SAMPLE
010810*                                                                 SAMPLE
010820     MOVE '2180-SEARCH-BRAND-TABLE'  TO WORK-PARAGRAPH            SAMPLE
010830**                                                                SAMPLE
010840      SET BRND-INDX TO +1.                                        SAMPLE
010850      SEARCH ALL BRAND-TABLE                                      SAMPLE
010860         AT END                                                   SAMPLE
010870            MOVE C-NO-N  TO WS-TABLE-ENTRY-FOUND-SW               SAMPLE
010880         WHEN                                                     SAMPLE
010890            BRND-KEY(BRND-INDX) = WS-KEY-CO-BRANCH                   CL*11
010900            MOVE C-YES-Y TO WS-TABLE-ENTRY-FOUND-SW               SAMPLE
010910     .                                                               CL**8
010920         DISPLAY 'VALID-DLR-DEALERB ' VALID-DLR-DEALER               CL*10
010930         DISPLAY 'BRND-KEYB         ' BRND-KEY (BRND-INDX)           CL*10
010940         DISPLAY 'WS-KEY-CO-BRANCHB ' WS-KEY-CO-BRANCH               CL*10
010950     .                                                            SAMPLE
010960**   IF WS-TABLE-ENTRY-FOUND-SW = C-YES-Y                            CL**3
010970**       IF WS-BRAND-ID = SPACES                                     CL**3
010980**           MOVE BRND-BRAND-ID (BRND-INDX) TO WS-BRAND-ID           CL**3
010990**       END-IF                                                      CL**3
011000**   END-IF                                                          CL**3
011010**   .                                                               CL**3
011020 2180-EXIT.                                                       SAMPLE
011030     EXIT.                                                        SAMPLE
011040                                                                  SAMPLE
011050 2200-CREATE-FZCOMMON.                                            SAMPLE
011060******************************************************************SAMPLE
011070*    CREATE FZCOMMON RECORD                                      *SAMPLE
011080******************************************************************SAMPLE
011090*                                                                 SAMPLE
011100     MOVE '2200-CREATE-FZCOMMON   '  TO WORK-PARAGRAPH            SAMPLE
011110                                                                  SAMPLE
011120     MOVE SPACES                   TO WS-DEALER-REC               SAMPLE
011130                                                                  SAMPLE
011140     MOVE C-DEALER-D               TO WS-DEALER-RECORD-TYPE       SAMPLE
011150                                                                  SAMPLE
011160* ALWAYS USE THE COMPANY,AREA/REG/ZONE FROM HEIRSERV FILE         SAMPLE
011170                                                                  SAMPLE
011180     MOVE VALID-DLR-COMPANY        TO WS-DEALER-COMPANY           SAMPLE
011190     MOVE VALID-DLR-BRANCH         TO WS-DEALER-BRANCH            SAMPLE
011200     MOVE VALID-DLR-DEALER         TO WS-DEALER-DEALER            SAMPLE
011210                                                                  SAMPLE
011220     PERFORM 2300-CLEAR-RECORD     THRU 2300-EXIT                 SAMPLE
011230                                                                  SAMPLE
011240     IF NOT NEW-FZCOMMON                                          SAMPLE
011250         PERFORM 2400-BUILD-FROM-TABLE  THRU 2400-EXIT          SAMPLE
011260     END-IF                                                       SAMPLE
011270*                                                                 SAMPLE
011280     MOVE BRND-BRAND-ID (BRND-INDX)     TO WS-DEALER-BRAND-ID     SAMPLE
011290*                                                                 SAMPLE
011300     MOVE N-DLR-LEGL-FULL    OF ZPCMAST TO WS-DEALER-DEAL-NAME    SAMPLE
011310     MOVE VALID-DLR-STATE-CD            TO WS-DEALER-STATE-CD     SAMPLE
011320     MOVE VALID-DLR-ADDRESS             TO WS-DEALER-DEAL-ADDRESS SAMPLE
011330     MOVE VALID-DLR-CITY                TO WS-DEALER-CITY-STATE   SAMPLE
011340*    MOVE VALID-DLR-CITY                TO WS-CITY                SAMPLE
011350*    MOVE VALID-DLR-STATE-CD            TO WS-STATE               SAMPLE
011360*    MOVE WS-CITY-STATE                 TO WS-DEALER-CITY-STATE   SAMPLE
011370     MOVE VALID-DLR-ZIP                 TO WS-DEALER-ZIP          SAMPLE
011380*                                                                 SAMPLE
011390     MOVE C-FINC-RETL        OF ZPCRETL TO WS-DEALER-DLR-PLAN     SAMPLE
011400     MOVE C-FREQ-RSVR-REGLR  OF ZPCRETL TO WS-DEALER-REG-P-FREQ   SAMPLE
011410     MOVE P-RSVR-REGLR-HOLD  OF ZPCRETL TO                        SAMPLE
011420                                       WS-DEALER-REG-P-PERC-RDF   SAMPLE
011430     MOVE A-RSVR-REGLR-HOLD  OF ZPCRETL TO WS-DEALER-REG-P-MIN    SAMPLE
011440     MOVE C-FREQ-RSVR-SPCL   OF ZPCRETL TO WS-DEALER-SPEC-P-FREQ  SAMPLE
011450     MOVE P-RSVR-SPCL-HOLD   OF ZPCRETL TO                        SAMPLE
011460                                       WS-DEALER-SPEC-P-PERC-RDF  SAMPLE
011470     MOVE A-RSVR-SPCL-HOLD   OF ZPCRETL TO WS-DEALER-SPEC-P-MIN   SAMPLE
011480     MOVE C-FREQ-RSVR-REPRCH OF ZPCRETL TO WS-DEALER-REPUR-P-FREQ SAMPLE
011490     MOVE P-RSVR-REPRCH-HOLD OF ZPCRETL TO                        SAMPLE
011500                                       WS-DEALER-REPUR-P-PERC-RDF SAMPLE
011510     MOVE A-RSVR-REPRCH-HOLD OF ZPCRETL TO WS-DEALER-REPUR-P-MIN  SAMPLE
011520     MOVE C-RSVR-HOLD        OF ZPCRETL TO WS-DEALER-HOLD-REL-IND SAMPLE
011530     MOVE C-OFF-WRT          OF ZPCRETL TO WS-DEALER-WRITEOFF-IND SAMPLE
011540     MOVE C-RPT-DELQ-RETL    OF ZPCRETL TO WS-DEALER-DELQ-IND     SAMPLE
011550     IF L-VFY-RETL OF   ZPCRETL > 0                               SAMPLE
011560         MOVE C-1                  TO WS-DEALER-PRTMAIL-TO-DLR-INDSAMPLE
011570     ELSE                                                         SAMPLE
011580         MOVE C-0                  TO WS-DEALER-PRTMAIL-TO-DLR-INDSAMPLE
011590     END-IF                                                       SAMPLE
011600*                                                                 SAMPLE
011610     MOVE C-OUT-OF-BUS   OF ZPCRETL TO WS-DEALER-SUM-OUT-OF-BUS-CDSAMPLE
011620     MOVE D-SHEET-PLAN OF ZPCRETL            TO CONVERT-DATE      SAMPLE
011630     MOVE 2 TO DATE-CMP-SW                                        SAMPLE
011640     PERFORM 5000-CONVERT-YEARMD THRU 5000-EXIT                   SAMPLE
011650     MOVE WORK2-PART-DATE                    TO WS-DEALER-PLAN-SD SAMPLE
011660     EVALUATE C-RPT-DELQ-RETL                                     SAMPLE
011670         WHEN 0                                                   SAMPLE
011680             CONTINUE                                             SAMPLE
011690         WHEN 1                                                   SAMPLE
011700             MOVE 'A'                        TO WS-DEALER-DELQ-INDSAMPLE
011710         WHEN 2                                                   SAMPLE
011720             MOVE 'B'                        TO WS-DEALER-DELQ-INDSAMPLE
011730         WHEN 3                                                   SAMPLE
011740             MOVE 'C'                        TO WS-DEALER-DELQ-INDSAMPLE
011750         WHEN 4                                                   SAMPLE
011760             MOVE 'D'                        TO WS-DEALER-DELQ-INDSAMPLE
011770     END-EVALUATE                                                 SAMPLE
011780*                                                                 SAMPLE
011790     IF BRND-GL-ACCT    (BRND-INDX) = C-2270800                      CL**7
011800         PERFORM 7600-WRITE-FZCOMMON-TRUCK THRU 7600-EXIT         SAMPLE
011810     ELSE                                                         SAMPLE
011820         PERFORM 7700-WRITE-FZCOMMON-CAR   THRU 7700-EXIT         SAMPLE
011830     .                                                            SAMPLE
011840 2200-EXIT.                                                       SAMPLE
011850     EXIT.                                                        SAMPLE
011860                                                                  SAMPLE
011870 2300-CLEAR-RECORD.                                               SAMPLE
011880******************************************************************SAMPLE
011890*    SET DATA FIELDS TO ZEROES                                   *SAMPLE
011900******************************************************************SAMPLE
011910*                                                                 SAMPLE
011920     MOVE '2300-CLEAR-RECORD      '  TO WORK-PARAGRAPH            SAMPLE
011930                                                                  SAMPLE
011940     MOVE ZEROES               TO WS-DEALER-REGULAR               SAMPLE
011950     MOVE ZEROES               TO WS-DEALER-SPECIAL               SAMPLE
011960     MOVE ZEROES               TO WS-DEALER-REPURCH               SAMPLE
011970     MOVE ZEROES               TO WS-DEALER-AUTO                  SAMPLE
011980     MOVE ZEROES               TO WS-DEALER-ACCTS-AUTO            SAMPLE
011990     MOVE ZEROES               TO WS-DEALER-TRUCK                 SAMPLE
012000     MOVE ZEROES               TO WS-DEALER-ACCTS-TRUCK           SAMPLE
012010     MOVE ZEROES               TO WS-DEALER-OTHER                 SAMPLE
012020     MOVE ZEROES               TO WS-DEALER-ACCTS-OTHER           SAMPLE
012030     MOVE ZEROES               TO WS-DEALER-AUTOVOL               SAMPLE
012040     MOVE ZEROES               TO WS-DEALER-AUTOVOLCNT            SAMPLE
012050     MOVE ZEROES               TO WS-DEALER-AUTOYTD               SAMPLE
012060     MOVE ZEROES               TO WS-DEALER-AUTOYTDCNT            SAMPLE
012070     MOVE ZEROES               TO WS-DEALER-PRIOR-CHARGEOFF       SAMPLE
012080     MOVE ZEROES               TO WS-DEALER-CURR-CHARGEOFF        SAMPLE
012090     MOVE ZEROES               TO WS-DEALER-OUT-WOR-OS            SAMPLE
012100     MOVE ZEROES               TO WS-DEALER-REP                   SAMPLE
012110     MOVE ZEROES               TO WS-DEALER-REC1                  SAMPLE
012120     MOVE ZEROES               TO WS-DEALER-BRANCH-DATE           SAMPLE
012130     MOVE ZEROES               TO WS-DEALER-REGULAR-UNSOLD           CL**4
012140     MOVE ZEROES               TO WS-DEALER-REGULAR-SOLD             CL**4
012150     .                                                            SAMPLE
012160 2300-EXIT.                                                       SAMPLE
012170     EXIT.                                                        SAMPLE
012180                                                                  SAMPLE
012190 2400-BUILD-FROM-TABLE.                                         SAMPLE
012200******************************************************************SAMPLE
012210*    DATA FIELDS FROM TABLE ZDERDLR MOVE TO FZCOMMON OUT         *SAMPLE
012220******************************************************************SAMPLE
012230*                                                                 SAMPLE
012240     MOVE '2400-BUILD-FROM-ZDERDLR'  TO WORK-PARAGRAPH            SAMPLE
012250                                                                  SAMPLE
012260     MOVE I-DLR              OF ZDERDLR TO WS-DEALER-DEALER       SAMPLE
012270     MOVE I-DLR-SUFF         OF ZDERDLR TO WS-DEALER-DEALER-SUFFIXSAMPLE
012280     MOVE A-RSVR-REGLR       OF ZDERDLR TO WS-DEALER-REGULAR      SAMPLE
012290     MOVE A-RSVR-REG-UNSOLD OF ZDERDLR TO WS-DEALER-REGULAR-UNSOLD   CL**4
012300     MOVE A-RSVR-REG-SOLD   OF ZDERDLR TO WS-DEALER-REGULAR-SOLD     CL**4
012310     MOVE A-RSVR-SPCL        OF ZDERDLR TO WS-DEALER-SPECIAL      SAMPLE
012320     MOVE A-RSVR-REPRCH      OF ZDERDLR TO WS-DEALER-REPURCH      SAMPLE
012330*    MOVE A-OTSTD-AUTO       OF ZDERDLR TO WS-DEALER-AUTO         SAMPLE
012340*    MOVE Q-OTSTD-AUTO       OF ZDERDLR TO WS-DEALER-ACCTS-AUTO   SAMPLE
012350*    MOVE A-OTSTD-TRK        OF ZDERDLR TO WS-DEALER-TRUCK        SAMPLE
012360*    MOVE Q-OTSTD-TRK        OF ZDERDLR TO WS-DEALER-ACCTS-TRUCK  SAMPLE
012370*    MOVE A-OTSTD-OTHR       OF ZDERDLR TO WS-DEALER-OTHER        SAMPLE
012380*    MOVE Q-OTSTD-OTHR       OF ZDERDLR TO WS-DEALER-ACCTS-OTHER  SAMPLE
012390*    MOVE A-VOL-AUTO         OF ZDERDLR TO WS-DEALER-AUTOVOL      SAMPLE
012400*    MOVE Q-VOL-AUTO         OF ZDERDLR TO WS-DEALER-AUTOVOLCNT   SAMPLE
012410*    MOVE A-VOL-YTD          OF ZDERDLR TO WS-DEALER-AUTOYTD      SAMPLE
012420*    MOVE Q-VOL-YTD          OF ZDERDLR TO WS-DEALER-AUTOYTDCNT   SAMPLE
012430     MOVE A-OFF-CHRG-PREV    OF ZDERDLR TO                        SAMPLE
012440                                         WS-DEALER-PRIOR-CHARGEOFFSAMPLE
012450     MOVE A-OFF-CHRG-CURR    OF ZDERDLR TO                        SAMPLE
012460                                         WS-DEALER-CURR-CHARGEOFF SAMPLE
012470     MOVE D-RPT              OF ZDERDLR TO CONVERT-DATE           SAMPLE
012480     PERFORM 5000-CONVERT-YEARMD THRU 5000-EXIT                   SAMPLE
012490     MOVE WORK-PART-DATE                TO WS-DEALER-BRANCH-DATE  SAMPLE
012500     .                                                            SAMPLE
012510*                                                                 SAMPLE
012520 2400-EXIT.                                                       SAMPLE
012530     EXIT.                                                        SAMPLE
012540                                                                  SAMPLE
012550 2800-BYPASS-TABLE.                                             SAMPLE
012560******************************************************************SAMPLE
012570* CREATE DLRSUMM RECORD FOR ZDERDLR RECORDS THAT ARE NOT ACTIVE   SAMPLE
012580* DEALERS ON THE HEIRSERV FILE                                    SAMPLE
012590******************************************************************SAMPLE
012600*                                                                 SAMPLE
012610     MOVE '2800-BYPASS-TABLE    '  TO WORK-PARAGRAPH            SAMPLE
012620                                                                  SAMPLE
012630     MOVE SPACES                   TO WS-DEALER-REC               SAMPLE
012640                                                                  SAMPLE
012650     MOVE C-DEALER-D               TO WS-DEALER-RECORD-TYPE       SAMPLE
012660                                                                  SAMPLE
012670* ALWAYS USE THE COMPANY,AREA/REG/ZONE FROM HEIRSERV FILE         SAMPLE
012680                                                                  SAMPLE
012690     PERFORM 2300-CLEAR-RECORD           THRU 2300-EXIT           SAMPLE
012700                                                                  SAMPLE
012710     PERFORM 2400-BUILD-FROM-TABLE     THRU 2400-EXIT           SAMPLE
012720                                                                  SAMPLE
012730     MOVE C-COMPANY          OF ZDERDLR TO WS-DEALER-COMPANY      SAMPLE
012740     MOVE I-AREA             OF ZDERDLR TO WS-DEALER-AREA         SAMPLE
012750     MOVE I-REGION           OF ZDERDLR TO WS-DEALER-REGION       SAMPLE
012760     MOVE I-ZONE             OF ZDERDLR TO WS-DEALER-ZONE         SAMPLE
012770     MOVE I-BRAND-ID         OF ZDERDLR TO WS-DEALER-BRAND-ID     SAMPLE
012780     MOVE C-DLR-NAME         OF ZDERDLR TO WS-DEALER-DEAL-NAME    SAMPLE
012790     MOVE C-STATE            OF ZDERDLR TO WS-DEALER-STATE-CD     SAMPLE
012800     MOVE X-ADDR             OF ZDERDLR TO WS-DEALER-DEAL-ADDR    SAMPLE
012810     MOVE N-CITY             OF ZDERDLR TO WS-DEALER-CITY-STATE   SAMPLE
012820     MOVE I-ZIP              OF ZDERDLR TO WS-DEALER-ZIP          SAMPLE
012830     MOVE D-SHEET-PLAN       OF ZDERDLR TO CONVERT-DATE           SAMPLE
012840     MOVE 2 TO DATE-CMP-SW                                        SAMPLE
012850     PERFORM 5000-CONVERT-YEARMD THRU 5000-EXIT                   SAMPLE
012860     MOVE WORK2-PART-DATE               TO WS-DEALER-PLAN-SD      SAMPLE
012870     MOVE C-FINC-RETL        OF ZDERDLR TO WS-DEALER-DLR-PLAN     SAMPLE
012880     MOVE C-FREQ-RSVR-REGLR  OF ZDERDLR TO WS-DEALER-REG-P-FREQ   SAMPLE
012890     MOVE P-RSVR-REGLR-HOLD  OF ZDERDLR TO WS-DEALER-REG-P-PERC   SAMPLE
012900     MOVE A-RSVR-REGLR-HOLD  OF ZDERDLR TO WS-DEALER-REG-P-MIN    SAMPLE
012910     MOVE C-FREQ-RSVR-SPCL   OF ZDERDLR TO WS-DEALER-SPEC-P-FREQ  SAMPLE
012920     MOVE P-RSVR-SPCL-HOLD   OF ZDERDLR TO WS-DEALER-SPEC-P-PERC  SAMPLE
012930     MOVE A-RSVR-SPCL-HOLD   OF ZDERDLR TO WS-DEALER-SPEC-P-MIN   SAMPLE
012940     MOVE C-FREQ-RSVR-REPRCH OF ZDERDLR TO WS-DEALER-REPUR-P-FREQ SAMPLE
012950     MOVE P-RSVR-REPRCH-HOLD OF ZDERDLR TO WS-DEALER-REPUR-P-PERC SAMPLE
012960     MOVE A-RSVR-REPRCH-HOLD OF ZDERDLR TO WS-DEALER-REPUR-P-MIN  SAMPLE
012970     MOVE C-RSVR-HOLD        OF ZDERDLR TO WS-DEALER-HOLD-REL-IND SAMPLE
012980     MOVE C-OFF-WRT          OF ZDERDLR TO WS-DEALER-WRITEOFF-IND SAMPLE
012990     IF   C-PRTMAIL-DLR-IND  OF ZDERDLR > 0                       SAMPLE
013000         MOVE C-1                  TO WS-DEALER-PRTMAIL-TO-DLR-INDSAMPLE
013010     ELSE                                                         SAMPLE
013020         MOVE C-0                  TO WS-DEALER-PRTMAIL-TO-DLR-INDSAMPLE
013030     END-IF                                                       SAMPLE
013040*                                                                 SAMPLE
013050     MOVE C-OUT-OF-BUS   OF ZPCRETL TO WS-DEALER-SUM-OUT-OF-BUS-CDSAMPLE
013060     MOVE C-RPT-DELQ-IND     OF ZDERDLR TO WS-DEALER-DELQ-IND     SAMPLE
013070     EVALUATE C-RPT-DELQ-IND                                      SAMPLE
013080         WHEN 0                                                   SAMPLE
013090             CONTINUE                                             SAMPLE
013100         WHEN 1                                                   SAMPLE
013110             MOVE 'A'                        TO WS-DEALER-DELQ-INDSAMPLE
013120         WHEN 2                                                   SAMPLE
013130             MOVE 'B'                        TO WS-DEALER-DELQ-INDSAMPLE
013140         WHEN 3                                                   SAMPLE
013150             MOVE 'C'                        TO WS-DEALER-DELQ-INDSAMPLE
013160         WHEN 4                                                   SAMPLE
013170             MOVE 'D'                        TO WS-DEALER-DELQ-INDSAMPLE
013180     END-EVALUATE                                                 SAMPLE
013190                                                                  SAMPLE
013200     PERFORM 7800-WRITE-COMMON-BYPASS  THRU 7800-EXIT           SAMPLE
013210     .                                                            SAMPLE
013220*                                                                 SAMPLE
013230 2800-EXIT.                                                       SAMPLE
013240     EXIT.                                                        SAMPLE
013250                                                                  SAMPLE
013260 3000-ERROR-COMMONS.                                            SAMPLE
013270******************************************************************SAMPLE
013280* ERROR REASONS                                                   SAMPLE
013290*  BYPASSED                                                       SAMPLE
013300*            88  ERR-NO-ZPCMAST         VALUE '1'.                SAMPLE
013310*            88  ERR-NO-ZPCRETL         VALUE '2'.                SAMPLE
013320*            88  ERR-NOBRAND            VALUE '3'.                SAMPLE
013330*            88  ERR-NORDLR-NOBRND      VALUE '4'.                SAMPLE
013340******************************************************************SAMPLE
013350*                                                                 SAMPLE
013360     MOVE '3000-ERROR-COMMONS   '  TO WORK-PARAGRAPH            SAMPLE
013370                                                                  SAMPLE
013380     IF ERROR-LINE-CNTR    > 54                                   SAMPLE
013390         PERFORM 3500-ERROR-HEADING  THRU 3500-EXIT               SAMPLE
013400     END-IF                                                       SAMPLE
013410                                                                  SAMPLE
013420     MOVE SPACES           TO WS-ERR1                             SAMPLE
013430     ADD +1                TO WS-ZDERDLR-ERR-CNTR                 SAMPLE
013440                                                                  SAMPLE
013450**  VALID DEALER ON ZPCSERV, VALID-DLR MATCHES ZDERDLR            SAMPLE
013460**    NO CUST PROFILE ZPCMAST                                     SAMPLE
013470     IF ERR-NO-ZPCMAST                                            SAMPLE
013480         IF DEALER-ONLY-YES                                       SAMPLE
013490             MOVE ERR-NO-ZPCMAST-DESC1A     TO WS-ERR1-DESC       SAMPLE
013500             MOVE WS-VALID-DLR-COMPANY      TO WS-ERR1-COMPANY    SAMPLE
013510             MOVE WS-VALID-DLR-AREA         TO WS-ERR1-AREA       SAMPLE
013520             MOVE WS-VALID-DLR-REG          TO WS-ERR1-REGION     SAMPLE
013530             MOVE WS-VALID-DLR-ZONE         TO WS-ERR1-ZONE       SAMPLE
013540             MOVE WS-VALID-DLR-DEALER       TO WS-ERR1-DLR        SAMPLE
013550         ELSE                                                     SAMPLE
013560             MOVE ERR-NO-ZPCMAST-DESC1      TO WS-ERR1-DESC       SAMPLE
013570             MOVE C-COMPANY      OF ZDERDLR TO WS-ERR1-COMPANY    SAMPLE
013580             MOVE I-AREA         OF ZDERDLR TO WS-ERR1-AREA       SAMPLE
013590             MOVE I-REGION       OF ZDERDLR TO WS-ERR1-REGION     SAMPLE
013600             MOVE I-ZONE         OF ZDERDLR TO WS-ERR1-ZONE       SAMPLE
013610             MOVE I-DLR          OF ZDERDLR TO WS-ERR1-DEALER     SAMPLE
013620             MOVE I-DLR-SUFF     OF ZDERDLR TO WS-ERR1-DEALER-SUFFSAMPLE
013630             MOVE I-BRAND-ID     OF ZDERDLR TO WS-ERR1-BRAND-ID   SAMPLE
013640             MOVE C-DLR-NAME     OF ZDERDLR TO WS-ERR1-DEAL-NAME  SAMPLE
013650             MOVE A-RSVR-REGLR   OF ZDERDLR TO WS-ERR1-REGULAR    SAMPLE
013660             MOVE A-RSVR-SPCL    OF ZDERDLR TO WS-ERR1-SPECIAL    SAMPLE
013670             MOVE A-RSVR-REPRCH  OF ZDERDLR TO WS-ERR1-REPURCH    SAMPLE
013680         END-IF                                                   SAMPLE
013690     END-IF                                                       SAMPLE
013700                                                                  SAMPLE
013710**  VALID DEALER ON ZPCSERV, VALID-DLR MATCHES ZDERDLR            SAMPLE
013720**    WE HAVE CUST PROFILE ZPCMAST, BUT NO ZPCRETL                SAMPLE
013730     IF ERR-NO-ZPCRETL                                            SAMPLE
013740         IF DEALER-ONLY-YES                                       SAMPLE
013750             MOVE ERR-NO-ZPCRETL-DESC2A     TO WS-ERR1-DESC       SAMPLE
013760             MOVE WS-VALID-DLR-COMPANY      TO WS-ERR1-COMPANY    SAMPLE
013770             MOVE WS-VALID-DLR-AREA         TO WS-ERR1-AREA       SAMPLE
013780             MOVE WS-VALID-DLR-REG          TO WS-ERR1-REGION     SAMPLE
013790             MOVE WS-VALID-DLR-ZONE         TO WS-ERR1-ZONE       SAMPLE
013800             MOVE WS-VALID-DLR-DEALER       TO WS-ERR1-DLR        SAMPLE
013810         ELSE                                                     SAMPLE
013820             MOVE ERR-NO-ZPCRETL-DESC2        TO WS-ERR1-DESC     SAMPLE
013830             MOVE C-COMPANY      OF ZDERDLR TO WS-ERR1-COMPANY    SAMPLE
013840             MOVE I-AREA         OF ZDERDLR TO WS-ERR1-AREA       SAMPLE
013850             MOVE I-REGION       OF ZDERDLR TO WS-ERR1-REGION     SAMPLE
013860             MOVE I-ZONE         OF ZDERDLR TO WS-ERR1-ZONE       SAMPLE
013870             MOVE I-DLR          OF ZDERDLR TO WS-ERR1-DEALER     SAMPLE
013880             MOVE I-DLR-SUFF     OF ZDERDLR TO WS-ERR1-DEALER-SUFFSAMPLE
013890             MOVE I-BRAND-ID     OF ZDERDLR TO WS-ERR1-BRAND-ID   SAMPLE
013900             MOVE C-DLR-NAME     OF ZDERDLR TO WS-ERR1-DEAL-NAME  SAMPLE
013910             MOVE A-RSVR-REGLR   OF ZDERDLR TO WS-ERR1-REGULAR    SAMPLE
013920             MOVE A-RSVR-SPCL    OF ZDERDLR TO WS-ERR1-SPECIAL    SAMPLE
013930             MOVE A-RSVR-REPRCH  OF ZDERDLR TO WS-ERR1-REPURCH    SAMPLE
013940         END-IF                                                   SAMPLE
013950     END-IF                                                       SAMPLE
013960                                                                  SAMPLE
013970**  VALID DEALER, MATCHED TO ZDERDLR, BUT NO BRAND                SAMPLE
013980     IF ERR-RDLR-NOBRND                                           SAMPLE
013990         MOVE ERR-RDLR-NOBRND-DESC3       TO WS-ERR1-DESC         SAMPLE
014000         MOVE C-COMPANY      OF ZDERDLR TO WS-ERR1-COMPANY        SAMPLE
014010         MOVE I-AREA         OF ZDERDLR TO WS-ERR1-AREA           SAMPLE
014020         MOVE I-REGION       OF ZDERDLR TO WS-ERR1-REGION         SAMPLE
014030         MOVE I-ZONE         OF ZDERDLR TO WS-ERR1-ZONE           SAMPLE
014040         MOVE I-DLR          OF ZDERDLR TO WS-ERR1-DEALER         SAMPLE
014050         MOVE I-DLR-SUFF     OF ZDERDLR TO WS-ERR1-DEALER-SUFF    SAMPLE
014060         MOVE I-BRAND-ID     OF ZDERDLR TO WS-ERR1-BRAND-ID       SAMPLE
014070         MOVE C-DLR-NAME     OF ZDERDLR TO WS-ERR1-DEAL-NAME      SAMPLE
014080         MOVE A-RSVR-REGLR   OF ZDERDLR TO WS-ERR1-REGULAR        SAMPLE
014090         MOVE A-RSVR-SPCL    OF ZDERDLR TO WS-ERR1-SPECIAL        SAMPLE
014100         MOVE A-RSVR-REPRCH  OF ZDERDLR TO WS-ERR1-REPURCH        SAMPLE
014110     END-IF                                                       SAMPLE
014120                                                                  SAMPLE
014130**  VALID DEALER, NO ZDERDLR AND NO BRAND                         SAMPLE
014140     IF ERR-NORDLR-NOBRND                                         SAMPLE
014150         MOVE ERR-NORDLR-NOBRND-DESC4     TO WS-ERR1-DESC         SAMPLE
014160         MOVE VALID-DLR-COMPANY           TO WS-ERR1-COMPANY      SAMPLE
014170         MOVE VALID-DLR-AREA              TO WS-ERR1-AREA         SAMPLE
014180         MOVE VALID-DLR-REG               TO WS-ERR1-REGION       SAMPLE
014190         MOVE VALID-DLR-ZONE              TO WS-ERR1-ZONE         SAMPLE
014200         MOVE VALID-DLR-DEALER            TO WS-ERR1-DLR          SAMPLE
014210     END-IF                                                       SAMPLE
014220                                                                  SAMPLE
014230     WRITE ERROR-OUT FROM WS-ERR1    AFTER ADVANCING 2 LINE       SAMPLE
014240     ADD 2                      TO ERROR-LINE-CNTR                SAMPLE
014250     .                                                            SAMPLE
014260 3000-EXIT.                                                       SAMPLE
014270     EXIT.                                                        SAMPLE
014280                                                                  SAMPLE
014290 3500-ERROR-HEADING.                                              SAMPLE
014300******************************************************************SAMPLE
014310* ERROR HEADINGS                                                 *SAMPLE
014320******************************************************************SAMPLE
014330*                                                                 SAMPLE
014340     MOVE '3500-ERROR-HEADINGS    '  TO WORK-PARAGRAPH            SAMPLE
014350                                                                  SAMPLE
014360     ADD 1                      TO ERROR-PAGE-CNTR                SAMPLE
014370     MOVE ERROR-PAGE-CNTR       TO ERROR-PAGE                     SAMPLE
014380     MOVE 0                     TO ERROR-LINE-CNTR                SAMPLE
014390                                                                  SAMPLE
014400     WRITE ERROR-OUT FROM ERROR-HDR1 AFTER ADVANCING TOP-OF-PAGE  SAMPLE
014410     WRITE ERROR-OUT FROM ERROR-HDR2 AFTER ADVANCING 1 LINE       SAMPLE
014420     WRITE ERROR-OUT FROM ERROR-HDR3 AFTER ADVANCING 1 LINE       SAMPLE
014430     WRITE ERROR-OUT FROM ERROR-DASH AFTER ADVANCING 2 LINE       SAMPLE
014440     WRITE ERROR-OUT FROM ERROR-HDR4 AFTER ADVANCING 1 LINE       SAMPLE
014450     WRITE ERROR-OUT FROM ERROR-DASH AFTER ADVANCING 1 LINE       SAMPLE
014460     .                                                            SAMPLE
014470 3500-EXIT.                                                       SAMPLE
014480     EXIT.                                                        SAMPLE
014490                                                                  SAMPLE
014500 4000-DB2-TABLES.                                                 SAMPLE
014510******************************************************************SAMPLE
014520*    SHELL CHANGES REQUIRED                                      *SAMPLE
014530*    CHANGE THE CODE TO SELECT DATA REQUIRED FOR SPECIFIC DLR MST*SAMPLE
014540******************************************************************SAMPLE
014550*                                                                 SAMPLE
014560     MOVE '4000-DB2-TABLES        '  TO WORK-PARAGRAPH            SAMPLE
014570                                                                  SAMPLE
014580     MOVE ALL 'N'                  TO TABLE-SWITCHES              SAMPLE
014590*                                                                 SAMPLE
014600     INITIALIZE                    ZPCMAST                        SAMPLE
014610                                   ZPCRETL                        SAMPLE
014620*                                                                 SAMPLE
014630     MOVE C-0                         TO WS-COMP-POS1             SAMPLE
014640     MOVE VALID-DLR-COMPANY           TO WS-COMP-POS2             SAMPLE
014650     MOVE WS-COMP-COMPANY             TO C-CO         OF ZPCMAST  SAMPLE
014660                                         C-CO         OF ZPCRETL  SAMPLE
014670     MOVE VALID-DLR-DEALER-NMBR       TO I-DLR        OF ZPCMAST  SAMPLE
014680*                                                                 SAMPLE
014690     MOVE VALID-DLR-DEALER-NMBR       TO I-DLR        OF ZPCRETL  SAMPLE
014700     MOVE VALID-DLR-DEALER-SUFF       TO I-DLR-SUFF   OF ZPCRETL  SAMPLE
014710     MOVE 2                           TO I-STAT-APRVL OF ZPCMAST  SAMPLE
014720                                         I-STAT-APRVL OF ZPCRETL  SAMPLE
014730     MOVE 'D'                         TO C-DLR-TYP    OF ZPCMAST  SAMPLE
014740                                         C-DLR-TYP    OF ZPCRETL  SAMPLE
014750                                                                  SAMPLE
014760     PERFORM 8000-GET-MASTER      THRU 8000-EXIT                 SAMPLE
014770     IF SQLCODE NOT = +0                                          SAMPLE
014780         MOVE C-1             TO  WS-ERROR-CODE                   SAMPLE
014790         DISPLAY 'NO ZPCMAST ' VALID-DLR-DEALER-NMBR              SAMPLE
014800                               VALID-DLR-DEALER-SUFF              SAMPLE
014810     ELSE                                                         SAMPLE
014820         PERFORM 8720-GET-TBL      THRU 8720-EXIT             SAMPLE
014830         IF SQLCODE NOT = +0                                      SAMPLE
014840             MOVE C-2         TO  WS-ERROR-CODE                   SAMPLE
014850             DISPLAY 'NO ZPCRETL ' VALID-DLR-DEALER-NMBR          SAMPLE
014860                                   VALID-DLR-DEALER-SUFF          SAMPLE
014870         END-IF                                                   SAMPLE
014880     END-IF                                                       SAMPLE
014890     .                                                            SAMPLE
014900 4000-EXIT.                                                       SAMPLE
014910     EXIT.                                                        SAMPLE
014920                                                                  SAMPLE
014930 5000-CONVERT-YEARMD.                                             SAMPLE
014940******************************************************************SAMPLE
014950*    DATE CONVERSION ROUTINE                                     *SAMPLE
014960******************************************************************SAMPLE
014970*                                                                 SAMPLE
014980     MOVE '5000-CONVERT-YEARMD    '  TO WORK-PARAGRAPH            SAMPLE
014990                                                                  SAMPLE
015000     EVALUATE DATE-CMP-SW                                         SAMPLE
015010         WHEN 2                                                   SAMPLE
015020             MOVE CONVERT-MM TO WORK2-MM                          SAMPLE
015030             MOVE CONVERT-DD TO WORK2-DD                          SAMPLE
015040             MOVE CONVERT-YY TO WORK2-YY                          SAMPLE
015050             IF CONVERT-CC = '19'                                 SAMPLE
015060                 MOVE '0' TO WORK2-CC                             SAMPLE
015070             ELSE                                                 SAMPLE
015080                 MOVE '1' TO WORK2-CC                             SAMPLE
015090             END-IF                                               SAMPLE
015100             IF WORK2-PART-DATE = '111111'                        SAMPLE
015110                 MOVE ZEROS TO WORK-DATE                          SAMPLE
015120             END-IF                                               SAMPLE
015130         WHEN OTHER                                               SAMPLE
015140             MOVE CONVERT-MM TO WORK-MM                           SAMPLE
015150             MOVE CONVERT-DD TO WORK-DD                           SAMPLE
015160             MOVE CONVERT-YY TO WORK-YY                           SAMPLE
015170             IF CONVERT-CC = '19'                                 SAMPLE
015180                 MOVE '0' TO WORK-CC                              SAMPLE
015190             ELSE                                                 SAMPLE
015200                 MOVE '1' TO WORK-CC                              SAMPLE
015210             END-IF                                               SAMPLE
015220             IF WORK-PART-DATE = '111111'                         SAMPLE
015230                 MOVE ZEROS TO WORK-DATE                          SAMPLE
015240             END-IF                                               SAMPLE
015250     END-EVALUATE                                                 SAMPLE
015260     MOVE 0            TO DATE-CMP-SW                             SAMPLE
015270     .                                                            SAMPLE
015280 5000-EXIT.                                                       SAMPLE
015290     EXIT.                                                        SAMPLE
015300                                                                  SAMPLE
015310 7000-OPEN-XYZXYZA.                                               SAMPLE
015320******************************************************************SAMPLE
015330*    OPEN ZDEBRND TABLE                                          *SAMPLE
015340******************************************************************SAMPLE
015350*                                                                 SAMPLE
015360     MOVE '7000-OPEN-XYZXYZA '  TO WORK-PARAGRAPH                 SAMPLE
015370*                                                                 SAMPLE
015380     EXEC SQL                                                     SAMPLE
015390         OPEN ZDEBRND_CURSOR                                      SAMPLE
015400     END-EXEC                                                     SAMPLE
015410     EVALUATE SQLCODE                                             SAMPLE
015420         WHEN +0                                                  SAMPLE
015430             CONTINUE                                             SAMPLE
015440         WHEN OTHER                                               SAMPLE
015450             PERFORM 9960-DB2-ERROR THRU 9960-EXIT                SAMPLE
015460     END-EVALUATE                                                 SAMPLE
015470     .                                                            SAMPLE
015480 7000-EXIT.                                                       SAMPLE
015490     EXIT.                                                        SAMPLE
015500                                                                  SAMPLE
015510 7010-OPEN-TABLE.                                               SAMPLE
015520******************************************************************SAMPLE
015530*    OPEN ZDERDLR TABLE                                          *SAMPLE
015540******************************************************************SAMPLE
015550*                                                                 SAMPLE
015560     MOVE '7010-OPEN-TABLE '  TO WORK-PARAGRAPH                 SAMPLE
015570*                                                                 SAMPLE
015580     EXEC SQL                                                     SAMPLE
015590         OPEN ZDERDLR_CURSOR                                      SAMPLE
015600     END-EXEC                                                     SAMPLE
015610     EVALUATE SQLCODE                                             SAMPLE
015620         WHEN +0                                                  SAMPLE
015630             CONTINUE                                             SAMPLE
015640         WHEN OTHER                                               SAMPLE
015650             PERFORM 9960-DB2-ERROR THRU 9960-EXIT                SAMPLE
015660     END-EVALUATE                                                 SAMPLE
015670     .                                                            SAMPLE
015680 7010-EXIT.                                                       SAMPLE
015690     EXIT.                                                        SAMPLE
015700                                                                  SAMPLE
015710 7100-FETCH-ZDERDLR.                                              SAMPLE
015720******************************************************************SAMPLE
015730*    SELECT GENERIC ZDERDLR INFORMATION                          *SAMPLE
015740******************************************************************SAMPLE
015750*                                                                 SAMPLE
015760     MOVE '7100-FETCH-ZDERDLR'    TO WORK-PARAGRAPH               SAMPLE
015770*                                                                 SAMPLE
015780     EXEC SQL                                                     SAMPLE
015790         FETCH ZDERDLR_CURSOR                                     SAMPLE
015800         INTO                                                     SAMPLE
015810             :ZDERDLR.C-COMPANY                                   SAMPLE
015820            ,:ZDERDLR.I-AREA                                      SAMPLE
015830            ,:ZDERDLR.I-REGION                                    SAMPLE
015840            ,:ZDERDLR.I-ZONE                                      SAMPLE
015850            ,:ZDERDLR.I-DLR                                       SAMPLE
015860            ,:ZDERDLR.I-DLR-SUFF                                  SAMPLE
015870            ,:ZDERDLR.I-BRAND-ID                                  SAMPLE
015880            ,:ZDERDLR.D-RPT                                       SAMPLE
015890            ,:ZDERDLR.C-DLR-NAME                                  SAMPLE
015900            ,:ZDERDLR.C-STATE                                     SAMPLE
015910            ,:ZDERDLR.X-ADDR                                      SAMPLE
015920            ,:ZDERDLR.N-CITY                                      SAMPLE
015930            ,:ZDERDLR.I-ZIP                                       SAMPLE
015940            ,:ZDERDLR.A-RSVR-REGLR                                SAMPLE
015950            ,:ZDERDLR.A-RSVR-SPCL                                 SAMPLE
015960            ,:ZDERDLR.A-RSVR-REPRCH                               SAMPLE
015970            ,:ZDERDLR.A-OTSTD-AUTO                                SAMPLE
015980            ,:ZDERDLR.Q-OTSTD-AUTO                                SAMPLE
015990            ,:ZDERDLR.A-OTSTD-TRK                                 SAMPLE
016000            ,:ZDERDLR.Q-OTSTD-TRK                                 SAMPLE
016010            ,:ZDERDLR.A-OTSTD-OTHR                                SAMPLE
016020            ,:ZDERDLR.Q-OTSTD-OTHR                                SAMPLE
016030            ,:ZDERDLR.A-VOL-AUTO                                  SAMPLE
016040            ,:ZDERDLR.Q-VOL-AUTO                                  SAMPLE
016050            ,:ZDERDLR.A-VOL-YTD                                   SAMPLE
016060            ,:ZDERDLR.Q-VOL-YTD                                   SAMPLE
016070            ,:ZDERDLR.A-OFF-CHRG-PREV                             SAMPLE
016080            ,:ZDERDLR.A-OFF-CHRG-CURR                             SAMPLE
016090            ,:ZDERDLR.D-SHEET-PLAN                                SAMPLE
016100            ,:ZDERDLR.C-FINC-RETL                                 SAMPLE
016110            ,:ZDERDLR.C-FREQ-RSVR-REGLR                           SAMPLE
016120            ,:ZDERDLR.P-RSVR-REGLR-HOLD                           SAMPLE
016130            ,:ZDERDLR.A-RSVR-REGLR-HOLD                           SAMPLE
016140            ,:ZDERDLR.C-FREQ-RSVR-SPCL                            SAMPLE
016150            ,:ZDERDLR.P-RSVR-SPCL-HOLD                            SAMPLE
016160            ,:ZDERDLR.A-RSVR-SPCL-HOLD                            SAMPLE
016170            ,:ZDERDLR.C-FREQ-RSVR-REPRCH                          SAMPLE
016180            ,:ZDERDLR.P-RSVR-REPRCH-HOLD                          SAMPLE
016190            ,:ZDERDLR.A-RSVR-REPRCH-HOLD                          SAMPLE
016200            ,:ZDERDLR.C-RSVR-HOLD                                 SAMPLE
016210            ,:ZDERDLR.C-OFF-WRT                                   SAMPLE
016220            ,:ZDERDLR.C-RPT-DELQ-IND                              SAMPLE
016230            ,:ZDERDLR.A-OTSTD-RECRS-WTHT                          SAMPLE
016240            ,:ZDERDLR.A-OTSTD-REPRCH                              SAMPLE
016250            ,:ZDERDLR.A-OTSTD-RECRS                               SAMPLE
016260            ,:ZDERDLR.C-PRTMAIL-DLR-IND                           SAMPLE
016270            ,:ZDERDLR.C-OUT-OF-BUS                                SAMPLE
016280            ,:ZDERDLR.I-UPD                                       SAMPLE
016290            ,:ZDERDLR.I-LAST-JOB-UPD                              SAMPLE
016300            ,:ZDERDLR.I-LAST-PGM-UPD                              SAMPLE
016310            ,:ZDERDLR.T-STMP-UPD                                  SAMPLE
016320            ,:ZDERDLR.A-RSVR-REG-UNSOLD                              CL**4
016330            ,:ZDERDLR.A-RSVR-REG-SOLD                                CL**4
016340     END-EXEC                                                     SAMPLE
016350     EVALUATE SQLCODE                                             SAMPLE
016360         WHEN +0                                                  SAMPLE
016370             MOVE C-COMPANY  OF ZDERDLR TO WS-ZDERDLR-COMPANY     SAMPLE
016380             MOVE I-AREA     OF ZDERDLR TO WS-ZDERDLR-AREA        SAMPLE
016390             MOVE I-REGION   OF ZDERDLR TO WS-ZDERDLR-REG         SAMPLE
016400             MOVE I-ZONE     OF ZDERDLR TO WS-ZDERDLR-ZONE        SAMPLE
016410             MOVE I-DLR      OF ZDERDLR TO WS-ZDERDLR-DEALER-NMBR SAMPLE
016420             MOVE I-DLR-SUFF OF ZDERDLR TO WS-ZDERDLR-DEALER-SUFF SAMPLE
016430             ADD +1                     TO WS-ZDERDLR-COUNT-IN    SAMPLE
016440             CONTINUE                                             SAMPLE
016450         WHEN +100                                                SAMPLE
016460             MOVE 'Y'                  TO WS-ZDERDLR-SW           SAMPLE
016470             MOVE HIGH-VALUES          TO WS-ZDERDLR-COMPANY      SAMPLE
016480             MOVE HIGH-VALUES          TO WS-ZDERDLR-BRANCH       SAMPLE
016490             MOVE HIGH-VALUES          TO WS-ZDERDLR-DEALER       SAMPLE
016500             CONTINUE                                             SAMPLE
016510         WHEN OTHER                                               SAMPLE
016520             DISPLAY 'SQLCODE ' SQLCODE                           SAMPLE
016530             PERFORM 9960-DB2-ERROR THRU 9960-EXIT                SAMPLE
016540     END-EVALUATE                                                 SAMPLE
016550     .                                                            SAMPLE
016560 7100-EXIT.                                                       SAMPLE
016570     EXIT.                                                        SAMPLE
016580                                                                  SAMPLE
016590 7210-CLOSE-TABLE.                                                SAMPLE
016600******************************************************************SAMPLE
016610*   CLOSE ZDEBRND TABLE                                          *SAMPLE
016620******************************************************************SAMPLE
016630*                                                                 SAMPLE
016640     MOVE '7210-CLOSE-TABLE'  TO WORK-PARAGRAPH                   SAMPLE
016650*                                                                 SAMPLE
016660     EXEC SQL                                                     SAMPLE
016670         CLOSE ZDEBRND_CURSOR                                     SAMPLE
016680     END-EXEC                                                     SAMPLE
016690     EVALUATE SQLCODE                                             SAMPLE
016700         WHEN +0                                                  SAMPLE
016710             CONTINUE                                             SAMPLE
016720         WHEN OTHER                                               SAMPLE
016730             PERFORM 9960-DB2-ERROR THRU 9960-EXIT                SAMPLE
016740     END-EVALUATE                                                 SAMPLE
016750     .                                                            SAMPLE
016760 7210-EXIT.                                                       SAMPLE
016770     EXIT.                                                        SAMPLE
016780                                                                  SAMPLE
016790 7220-CLOSE-ABCDEFG.                                              SAMPLE
016800******************************************************************SAMPLE
016810*   CLOSE ZDERDLR TABLE                                          *SAMPLE
016820******************************************************************SAMPLE
016830*                                                                 SAMPLE
016840     MOVE '7220-CLOSE-ABCDEFG'  TO WORK-PARAGRAPH                 SAMPLE
016850*                                                                 SAMPLE
016860     EXEC SQL                                                     SAMPLE
016870         CLOSE ZDERDLR_CURSOR                                     SAMPLE
016880     END-EXEC                                                     SAMPLE
016890     EVALUATE SQLCODE                                             SAMPLE
016900         WHEN +0                                                  SAMPLE
016910             CONTINUE                                             SAMPLE
016920         WHEN OTHER                                               SAMPLE
016930             PERFORM 9960-DB2-ERROR THRU 9960-EXIT                SAMPLE
016940     END-EVALUATE                                                 SAMPLE
016950     .                                                            SAMPLE
016960 7220-EXIT.                                                       SAMPLE
016970     EXIT.                                                        SAMPLE
016980                                                                  SAMPLE
016990*                                                                 SAMPLE
017000 7400-GET-RUN-DATE.                                               SAMPLE
017010******************************************************************SAMPLE
017020*    GET RUN DATE                                                *SAMPLE
017030******************************************************************SAMPLE
017040*                                                                 SAMPLE
017050     MOVE '7400-GET-RUN-DATE '  TO WORK-PARAGRAPH                 SAMPLE
017060                                                                  SAMPLE
017070     READ RUN-DATE-FILE INTO WS-RUN-DATE                          SAMPLE
017080         AT END                                                   SAMPLE
017090            MOVE C-Y       TO RUN-DATE-SW                         SAMPLE
017100     .                                                            SAMPLE
017110     IF RUN-DATE-EOF                                              SAMPLE
017120         DISPLAY ' ** ERROR RUN DATE FILE, NO RECORD FOUND'       SAMPLE
017130         PERFORM 9900-ABEND THRU 9900-EXIT                        SAMPLE
017140     END-IF                                                       SAMPLE
017150     .                                                            SAMPLE
017160 7400-EXIT.                                                       SAMPLE
017170     EXIT.                                                        SAMPLE
017180*                                                                 SAMPLE
017190 7450-GET-COMP-PARMTR.                                            SAMPLE
017200******************************************************************SAMPLE
017210*    GET COMPANY PARM                                            *SAMPLE
017220******************************************************************SAMPLE
017230*                                                                 SAMPLE
017240     MOVE '7450-GET-COMP-PARMTR '     TO WORK-PARAGRAPH           SAMPLE
017250                                                                  SAMPLE
017260     READ COMPANY-PARM INTO WS-COMPANY-PARM                       SAMPLE
017270         AT END                                                   SAMPLE
017280            MOVE C-Y       TO COMPANY-PARM-SW                     SAMPLE
017290     .                                                            SAMPLE
017300     IF COMPANY-PARM-EOF                                          SAMPLE
017310         DISPLAY ' ** ERROR COMPANY PARM FILE, NO RECORD FOUND'   SAMPLE
017320         PERFORM 9900-ABEND THRU 9900-EXIT                        SAMPLE
017330     END-IF                                                       SAMPLE
017340     .                                                            SAMPLE
017350 7450-EXIT.                                                       SAMPLE
017360     EXIT.                                                        SAMPLE
017370                                                                  SAMPLE
017380 7500-GET-VALID-DLR.                                              SAMPLE
017390******************************************************************SAMPLE
017400*    GET VALID RETAIL DEALER                                     *SAMPLE
017410******************************************************************SAMPLE
017420*                                                                 SAMPLE
017430     MOVE '7500-GET-VALID-DLR'  TO WORK-PARAGRAPH                 SAMPLE
017440     IF WS-HIER-SERV-SW = 'Y'                                     CL**2
017450         GO TO 7500-EXIT                                          CL**2
017460     END-IF                                                       CL**2
017470     .                                                            CL**2
017480                                                                  SAMPLE
017490     READ PROFILE-VALID-DLR INTO WS-VALID-DLR-REC                 SAMPLE
017500         AT END                                                   SAMPLE
017510             MOVE 'Y'       TO WS-HIER-SERV-SW                    SAMPLE
017520     .                                                            SAMPLE
017530     MOVE C-NO-N            TO WS-BVF-DEALER-SW                   SAMPLE
017540     MOVE SPACES            TO WS-BVF-DEALER                      SAMPLE
017550     IF NOT HIER-SERV-EOF                                         SAMPLE
017560         ADD 1                      TO WS-HEIR-SERV-COUNT-IN      SAMPLE
017570         MOVE WS-VALID-DLR-COMPANY  TO WS-VALID-COMPANY           SAMPLE
017580         MOVE WS-VALID-DLR-BRANCH   TO WS-VALID-BRANCH            SAMPLE
017590         MOVE WS-VALID-DLR-DEALER   TO WS-VALID-DEALER            SAMPLE
017600     ELSE                                                         SAMPLE
017610         MOVE HIGH-VALUES           TO WS-VALID-DLR-REC           SAMPLE
017620                                       WS-VALID-DLR-KEY           SAMPLE
017630     END-IF                                                       SAMPLE
017640     .                                                            SAMPLE
017650 7500-EXIT.                                                       SAMPLE
017660     EXIT.                                                        SAMPLE
017670                                                                  SAMPLE
017680 7600-WRITE-FZCOMMON-TRUCK.                                       SAMPLE
017690******************************************************************SAMPLE
017700*    WRITE DEALER MASTER FOR TRUCK                               *SAMPLE
017710******************************************************************SAMPLE
017720*                                                                 SAMPLE
017730     MOVE '7600-WRITE-FZCOMMON-TRUCK'   TO WORK-PARAGRAPH         SAMPLE
017740                                                                  SAMPLE
017750     WRITE DEALER-TRK-REC FROM WS-DEALER-REC                      SAMPLE
017760                                                                  SAMPLE
017770     IF NEW-FZCOMMON                                              SAMPLE
017780         ADD +1             TO WS-DEALER-TRK-OUT-NEW              SAMPLE
017790     ELSE                                                         SAMPLE
017800         ADD +1             TO WS-DEALER-TRK-OUT                  SAMPLE
017810     END-IF                                                       SAMPLE
017820     .                                                            SAMPLE
017830 7600-EXIT.                                                       SAMPLE
017840     EXIT.                                                        SAMPLE
017850                                                                  SAMPLE
017860 7700-WRITE-FZCOMMON-CAR.                                         SAMPLE
017870******************************************************************SAMPLE
017880*    WRITE DEALER MASTER FOR CAR & MB                            *SAMPLE
017890******************************************************************SAMPLE
017900*                                                                 SAMPLE
017910     MOVE '7700-WRITE-FZCOMMON-CAR  '   TO WORK-PARAGRAPH         SAMPLE
017920                                                                  SAMPLE
017930     WRITE DEALER-CAR-REC FROM WS-DEALER-REC                      SAMPLE
017940                                                                  SAMPLE
017950     IF NEW-FZCOMMON                                              SAMPLE
017960         ADD +1             TO WS-DEALER-CAR-OUT-NEW              SAMPLE
017970     ELSE                                                         SAMPLE
017980         ADD +1             TO WS-DEALER-CAR-OUT                  SAMPLE
017990     END-IF                                                       SAMPLE
018000     .                                                            SAMPLE
018010 7700-EXIT.                                                       SAMPLE
018020     EXIT.                                                        SAMPLE
018030                                                                  SAMPLE
018040 7800-WRITE-COMMON-BYPASS.                                        SAMPLE
018050******************************************************************SAMPLE
018060*    WRITE DEALER MASTER FOR BYPASSED DEALERS FROM ZDERDLR       *SAMPLE
018070******************************************************************SAMPLE
018080*                                                                 SAMPLE
018090     MOVE '7800-WRITE-COMMON-BYPASS'   TO WORK-PARAGRAPH          SAMPLE
018100                                                                  SAMPLE
018110     WRITE DEALER-BYP-REC FROM WS-DEALER-REC                      SAMPLE
018120                                                                  SAMPLE
018130     ADD +1                          TO WS-DEALER-BYP-OUT         SAMPLE
018140     .                                                            SAMPLE
018150 7800-EXIT.                                                       SAMPLE
018160     EXIT.                                                        SAMPLE
018170                                                                  SAMPLE
018180 8000-GET-MASTER.                                                 SAMPLE
018190******************************************************************SAMPLE
018200*    SELECT GENERIC ZPCMAST INFORMATION                          *SAMPLE
018210******************************************************************SAMPLE
018220*                                                                 SAMPLE
018230     MOVE '8000-GET-MASTER'    TO WORK-PARAGRAPH                  SAMPLE
018240*                                                                 SAMPLE
018250     EXEC SQL                                                     SAMPLE
018260         SELECT                                                   SAMPLE
018270             C_CO                                                 SAMPLE
018280            ,I_DLR                                                SAMPLE
018290            ,C_DLR_TYP                                            SAMPLE
018300            ,CHAR(D_EFFCTV,USA)                                   SAMPLE
018310            ,I_HIST_SEQ                                           SAMPLE
018320            ,I_DLR_GR                                             SAMPLE
018330            ,CHAR(D_DLR_LAST_REVW,USA)                            SAMPLE
018340            ,CHAR(D_DLR_GR_UPD,USA)                               SAMPLE
018350            ,N_DLR_LEGL_FULL                                      SAMPLE
018360            ,N_DLR_ABRV                                           SAMPLE
018370            ,C_STMNT_FIN_FREQ                                     SAMPLE
018380            ,I_TAX_CORPT                                          SAMPLE
018390            ,I_GST                                                SAMPLE
018400            ,I_TAX_REGSTRN_CAND                                   SAMPLE
018410            ,CHAR(D_DLR_ORGN,USA)                                 SAMPLE
018420            ,CHAR(D_DLR_INCORPN,USA)                              SAMPLE
018430            ,C_STAT                                               SAMPLE
018440            ,Q_MILES_FROM_BRCH                                    SAMPLE
018450            ,Q_POPLTN_SERVD                                       SAMPLE
018460            ,N_DLR_LANDLD                                         SAMPLE
018470            ,C_RLTY_CHRYS                                         SAMPLE
018480            ,C_DLR_ZONE                                           SAMPLE
018490            ,C_INVMT_TYP                                          SAMPLE
018500            ,I_STAT_APRVL                                         SAMPLE
018510            ,I_TERMNL                                             SAMPLE
018520            ,I_UPD                                                SAMPLE
018530            ,T_STMP_UPD                                           SAMPLE
018540            ,I_STAT_APRVL                                         SAMPLE
018550            ,C_REFRL_RETL                                         SAMPLE
018560            ,C_DLR_PRMRY                                          SAMPLE
018570            ,CHAR(D_DLR_PRMRY_UPD,USA)                            SAMPLE
018580            ,C_PAYMT_DVDND_TYP                                    SAMPLE
018590            ,N_PAYMT_DVDND_PAYE                                   SAMPLE
018600            ,L_SUPVR_SCRTY                                        SAMPLE
018610            ,L_ASSTNT_SCRTY                                       SAMPLE
018620            ,L_DLR_SCRTY                                          SAMPLE
018630            ,L_ACSS_NO_SCRTY                                      SAMPLE
018640            ,CHAR(D_ACSS_UPD,USA)                                 SAMPLE
018650            ,L_DLR_PAYMT                                          SAMPLE
018660            ,CHAR(D_DLR_PAYMT_UPD,USA)                            SAMPLE
018670            ,L_CMRC_FINC_OPT                                      SAMPLE
018680            ,CHAR(D_AGRMT_CMRC_FINC,USA)                          SAMPLE
018690         INTO                                                     SAMPLE
018700             :ZPCMAST.C-CO                                        SAMPLE
018710            ,:ZPCMAST.I-DLR                                       SAMPLE
018720            ,:ZPCMAST.C-DLR-TYP                                   SAMPLE
018730            ,:ZPCMAST.D-EFFCTV                                    SAMPLE
018740            ,:ZPCMAST.I-HIST-SEQ                                  SAMPLE
018750            ,:ZPCMAST.I-DLR-GR                                    SAMPLE
018760            ,:ZPCMAST.D-DLR-LAST-REVW                             SAMPLE
018770            ,:ZPCMAST.D-DLR-GR-UPD                                SAMPLE
018780            ,:ZPCMAST.N-DLR-LEGL-FULL                             SAMPLE
018790            ,:ZPCMAST.N-DLR-ABRV                                  SAMPLE
018800            ,:ZPCMAST.C-STMNT-FIN-FREQ                            SAMPLE
018810            ,:ZPCMAST.I-TAX-CORPT                                 SAMPLE
018820            ,:ZPCMAST.I-GST                                       SAMPLE
018830            ,:ZPCMAST.I-TAX-REGSTRN-CAND                          SAMPLE
018840            ,:ZPCMAST.D-DLR-ORGN                                  SAMPLE
018850            ,:ZPCMAST.D-DLR-INCORPN                               SAMPLE
018860            ,:ZPCMAST.C-STAT                                      SAMPLE
018870            ,:ZPCMAST.Q-MILES-FROM-BRCH                           SAMPLE
018880            ,:ZPCMAST.Q-POPLTN-SERVD                              SAMPLE
018890            ,:ZPCMAST.N-DLR-LANDLD                                SAMPLE
018900            ,:ZPCMAST.C-RLTY-CHRYS                                SAMPLE
018910            ,:ZPCMAST.C-DLR-ZONE                                  SAMPLE
018920            ,:ZPCMAST.C-INVMT-TYP                                 SAMPLE
018930            ,:ZPCMAST.I-STAT-APRVL                                SAMPLE
018940            ,:ZPCMAST.I-TERMNL                                    SAMPLE
018950            ,:ZPCMAST.I-UPD                                       SAMPLE
018960            ,:ZPCMAST.T-STMP-UPD                                  SAMPLE
018970            ,:ZPCMAST.I-STAT-APRVL                                SAMPLE
018980            ,:ZPCMAST.C-REFRL-RETL                                SAMPLE
018990            ,:ZPCMAST.C-DLR-PRMRY                                 SAMPLE
019000            ,:ZPCMAST.D-DLR-PRMRY-UPD                             SAMPLE
019010            ,:ZPCMAST.C-PAYMT-DVDND-TYP                           SAMPLE
019020            ,:ZPCMAST.N-PAYMT-DVDND-PAYE                          SAMPLE
019030            ,:ZPCMAST.L-SUPVR-SCRTY                               SAMPLE
019040            ,:ZPCMAST.L-ASSTNT-SCRTY                              SAMPLE
019050            ,:ZPCMAST.L-DLR-SCRTY                                 SAMPLE
019060            ,:ZPCMAST.L-ACSS-NO-SCRTY                             SAMPLE
019070            ,:ZPCMAST.D-ACSS-UPD                                  SAMPLE
019080            ,:ZPCMAST.L-DLR-PAYMT                                 SAMPLE
019090            ,:ZPCMAST.D-DLR-PAYMT-UPD                             SAMPLE
019100            ,:ZPCMAST.L-CMRC-FINC-OPT                             SAMPLE
019110            ,:ZPCMAST.D-AGRMT-CMRC-FINC                           SAMPLE
019120         FROM Z.ZPCMAST                                           SAMPLE
019130         WHERE C_CO         = :ZPCMAST.C-CO      AND              SAMPLE
019140               I_DLR        = :ZPCMAST.I-DLR     AND              SAMPLE
019150               C_DLR_TYP    = :ZPCMAST.C-DLR-TYP AND              SAMPLE
019160               I_STAT_APRVL = :ZPCMAST.I-STAT-APRVL               SAMPLE
019170     END-EXEC                                                     SAMPLE
019180     EVALUATE SQLCODE                                             SAMPLE
019190         WHEN +0                                                  SAMPLE
019200             MOVE 'Y'               TO ZPCMAST-SW                 SAMPLE
019210         WHEN +100                                                SAMPLE
019220             MOVE 'N'               TO ZPCMAST-SW                 SAMPLE
019230         WHEN OTHER                                               SAMPLE
019240             PERFORM 9960-DB2-ERROR THRU 9960-EXIT                SAMPLE
019250     END-EVALUATE                                                 SAMPLE
019260     .                                                            SAMPLE
019270 8000-EXIT.                                                       SAMPLE
019280     EXIT.                                                        SAMPLE
019290                                                                  SAMPLE
019300 8720-GET-TBL.                                                    SAMPLE
019310******************************************************************SAMPLE
019320*    SELECT GENERIC ZPCRETL INFORMATION                          *SAMPLE
019330******************************************************************SAMPLE
019340*                                                                 SAMPLE
019350     MOVE '8720-GET-TBL'    TO WORK-PARAGRAPH                     SAMPLE
019360*                                                                 SAMPLE
019370         EXEC SQL                                                 SAMPLE
019380             SELECT                                               SAMPLE
019390                 C_CO                                             SAMPLE
019400                ,I_DLR                                            SAMPLE
019410                ,CHAR(D_EFFCTV,USA)                               SAMPLE
019420                ,I_DLR_SUFF                                       SAMPLE
019430                ,C_DLR_TYP                                        SAMPLE
019440                ,I_HIST_SEQ                                       SAMPLE
019450                ,C_FINC_RETL                                      SAMPLE
019460                ,C_RPT_DELQ_RETL                                  SAMPLE
019470                ,C_OVERID_RETL                                    SAMPLE
019480                ,CHAR(D_REAS_HOLD_RETL,USA)                       SAMPLE
019490                ,C_REAS_HOLD_RETL                                 SAMPLE
019500                ,C_PGM_GDRP                                       SAMPLE
019510                ,C_DLR_METRO                                      SAMPLE
019520                ,A_ADJ_BRCH                                       SAMPLE
019530                ,P_RSVR_REGLR                                     SAMPLE
019540                ,P_RSVR_SPCL                                      SAMPLE
019550                ,C_METHD_CALCN_RSVR                               SAMPLE
019560                ,P_DAYS_ODD_RSVR                                  SAMPLE
019570                ,C_PLAN_SPCL                                      SAMPLE
019580                ,L_RSVR_ADVC                                      SAMPLE
019590                ,L_TAX_OTHR                                       SAMPLE
019600                ,L_FEE_FILNG                                      SAMPLE
019610                ,L_FEE_LIC                                        SAMPLE
019620                ,L_FEE_TI                                         SAMPLE
019630                ,L_FEE_REGSTRN                                    SAMPLE
019640                ,L_FEE_INSP_VHCL                                  SAMPLE
019650                ,L_TAX_STMP_DOCU                                  SAMPLE
019660                ,L_FEE_DOCU_OTHR                                  SAMPLE
019670                ,L_FEE_NOTRY                                      SAMPLE
019680                ,L_INS_PD_1                                       SAMPLE
019690                ,L_INS_PD_2                                       SAMPLE
019700                ,L_BRKDN_MECH                                     SAMPLE
019710                ,L_SC_PD_1                                        SAMPLE
019720                ,L_SC_PD_2                                        SAMPLE
019730                ,L_VFY_RETL                                       SAMPLE
019740                ,A_ADVC_ADJ                                       SAMPLE
019750                ,L_OPT_WLKAWY                                     SAMPLE
019760                ,C_FREQ_RSVR_REGLR                                SAMPLE
019770                ,A_RSVR_REGLR_HOLD                                SAMPLE
019780                ,P_RSVR_REGLR_HOLD                                SAMPLE
019790                ,C_FREQ_RSVR_SPCL                                 SAMPLE
019800                ,A_RSVR_SPCL_HOLD                                 SAMPLE
019810                ,P_RSVR_SPCL_HOLD                                 SAMPLE
019820                ,C_FREQ_RSVR_REPRCH                               SAMPLE
019830                ,A_RSVR_REPRCH_HOLD                               SAMPLE
019840                ,P_RSVR_REPRCH_HOLD                               SAMPLE
019850                ,I_STAT_APRVL                                     SAMPLE
019860                ,C_RETL_STAT                                      SAMPLE
019870                ,I_TERMNL                                         SAMPLE
019880                ,I_UPD                                            SAMPLE
019890                ,T_STMP_UPD                                       SAMPLE
019900                ,CHAR(D_SHEET_PLAN,USA)                           SAMPLE
019910                ,C_RSVR_HOLD                                      SAMPLE
019920                ,C_OFF_WRT                                        SAMPLE
019930                ,C_OPT_WLKAWY                                     SAMPLE
019940                ,C_OUT_OF_BUS                                     SAMPLE
019950                ,C_ACTVY                                          SAMPLE
019960         INTO                                                     SAMPLE
019970                 :ZPCRETL.C-CO                                    SAMPLE
019980                ,:ZPCRETL.I-DLR                                   SAMPLE
019990                ,:ZPCRETL.D-EFFCTV                                SAMPLE
020000                ,:ZPCRETL.I-DLR-SUFF                              SAMPLE
020010                ,:ZPCRETL.C-DLR-TYP                               SAMPLE
020020                ,:ZPCRETL.I-HIST-SEQ                              SAMPLE
020030                ,:ZPCRETL.C-FINC-RETL                             SAMPLE
020040                ,:ZPCRETL.C-RPT-DELQ-RETL                         SAMPLE
020050                ,:ZPCRETL.C-OVERID-RETL                           SAMPLE
020060                ,:ZPCRETL.D-REAS-HOLD-RETL                        SAMPLE
020070                ,:ZPCRETL.C-REAS-HOLD-RETL                        SAMPLE
020080                ,:ZPCRETL.C-PGM-GDRP                              SAMPLE
020090                ,:ZPCRETL.C-DLR-METRO                             SAMPLE
020100                ,:ZPCRETL.A-ADJ-BRCH                              SAMPLE
020110                ,:ZPCRETL.P-RSVR-REGLR                            SAMPLE
020120                ,:ZPCRETL.P-RSVR-SPCL                             SAMPLE
020130                ,:ZPCRETL.C-METHD-CALCN-RSVR                      SAMPLE
020140                ,:ZPCRETL.P-DAYS-ODD-RSVR                         SAMPLE
020150                ,:ZPCRETL.C-PLAN-SPCL                             SAMPLE
020160                ,:ZPCRETL.L-RSVR-ADVC                             SAMPLE
020170                ,:ZPCRETL.L-TAX-OTHR                              SAMPLE
020180                ,:ZPCRETL.L-FEE-FILNG                             SAMPLE
020190                ,:ZPCRETL.L-FEE-LIC                               SAMPLE
020200                ,:ZPCRETL.L-FEE-TI                                SAMPLE
020210                ,:ZPCRETL.L-FEE-REGSTRN                           SAMPLE
020220                ,:ZPCRETL.L-FEE-INSP-VHCL                         SAMPLE
020230                ,:ZPCRETL.L-TAX-STMP-DOCU                         SAMPLE
020240                ,:ZPCRETL.L-FEE-DOCU-OTHR                         SAMPLE
020250                ,:ZPCRETL.L-FEE-NOTRY                             SAMPLE
020260                ,:ZPCRETL.L-INS-PD-1                              SAMPLE
020270                ,:ZPCRETL.L-INS-PD-2                              SAMPLE
020280                ,:ZPCRETL.L-BRKDN-MECH                            SAMPLE
020290                ,:ZPCRETL.L-SC-PD-1                               SAMPLE
020300                ,:ZPCRETL.L-SC-PD-2                               SAMPLE
020310                ,:ZPCRETL.L-VFY-RETL                              SAMPLE
020320                ,:ZPCRETL.A-ADVC-ADJ                              SAMPLE
020330                ,:ZPCRETL.L-OPT-WLKAWY                            SAMPLE
020340                ,:ZPCRETL.C-FREQ-RSVR-REGLR                       SAMPLE
020350                ,:ZPCRETL.A-RSVR-REGLR-HOLD                       SAMPLE
020360                ,:ZPCRETL.P-RSVR-REGLR-HOLD                       SAMPLE
020370                ,:ZPCRETL.C-FREQ-RSVR-SPCL                        SAMPLE
020380                ,:ZPCRETL.A-RSVR-SPCL-HOLD                        SAMPLE
020390                ,:ZPCRETL.P-RSVR-SPCL-HOLD                        SAMPLE
020400                ,:ZPCRETL.C-FREQ-RSVR-REPRCH                      SAMPLE
020410                ,:ZPCRETL.A-RSVR-REPRCH-HOLD                      SAMPLE
020420                ,:ZPCRETL.P-RSVR-REPRCH-HOLD                      SAMPLE
020430                ,:ZPCRETL.I-STAT-APRVL                            SAMPLE
020440                ,:ZPCRETL.C-RETL-STAT                             SAMPLE
020450                ,:ZPCRETL.I-TERMNL                                SAMPLE
020460                ,:ZPCRETL.I-UPD                                   SAMPLE
020470                ,:ZPCRETL.T-STMP-UPD                              SAMPLE
020480                ,:ZPCRETL.D-SHEET-PLAN                            SAMPLE
020490                ,:ZPCRETL.C-RSVR-HOLD                             SAMPLE
020500                ,:ZPCRETL.C-OFF-WRT                               SAMPLE
020510                ,:ZPCRETL.C-OPT-WLKAWY                            SAMPLE
020520                ,:ZPCRETL.C-OUT-OF-BUS                            SAMPLE
020530                ,:ZPCRETL.C-ACTVY                                 SAMPLE
020540             FROM Z.ZPCRETL                                       SAMPLE
020550             WHERE C_CO         = :ZPCRETL.C-CO       AND         SAMPLE
020560                   I_DLR        = :ZPCRETL.I-DLR      AND         SAMPLE
020570                   I_DLR_SUFF   = :ZPCRETL.I-DLR-SUFF AND         SAMPLE
020580                   C_DLR_TYP    = :ZPCRETL.C-DLR-TYP AND          SAMPLE
020590                   I_STAT_APRVL = :ZPCRETL.I-STAT-APRVL           SAMPLE
020600         END-EXEC                                                 SAMPLE
020610     EVALUATE SQLCODE                                             SAMPLE
020620         WHEN +0                                                  SAMPLE
020630             MOVE 'Y'               TO ZPCRETL-SW                 SAMPLE
020640         WHEN +100                                                SAMPLE
020650             MOVE 'N'               TO ZPCRETL-SW                 SAMPLE
020660         WHEN OTHER                                               SAMPLE
020670             PERFORM 9960-DB2-ERROR THRU 9960-EXIT                SAMPLE
020680     END-EVALUATE                                                 SAMPLE
020690     .                                                            SAMPLE
020700 8720-EXIT.                                                       SAMPLE
020710     EXIT.                                                        SAMPLE
020720                                                                  SAMPLE
020730 9960-DB2-ERROR.                                                  SAMPLE
020740******************************************************************SAMPLE
020750*    BASIC DB2 ERROR DISPLAY                                     *SAMPLE
020760******************************************************************SAMPLE
020770*                                                                 SAMPLE
020780     DISPLAY '*** SAMPLE ON PARAGRAPH ' WORK-PARAGRAPH            SAMPLE
020790*                                                                 SAMPLE
020800     CALL 'DSNTIAR' USING SQLCA, ERROR-MESSAGE, ERROR-TEXT-LENGTH SAMPLE
020810     IF RETURN-CODE = ZERO                                        SAMPLE
020820         DISPLAY ERROR-TEXT (1)                                   SAMPLE
020830         DISPLAY ERROR-TEXT (2)                                   SAMPLE
020840         DISPLAY ERROR-TEXT (3)                                   SAMPLE
020850         DISPLAY ERROR-TEXT (4)                                   SAMPLE
020860         DISPLAY ERROR-TEXT (5)                                   SAMPLE
020870         DISPLAY ERROR-TEXT (6)                                   SAMPLE
020880         DISPLAY ERROR-TEXT (7)                                   SAMPLE
020890         DISPLAY ERROR-TEXT (8)                                   SAMPLE
020900     ELSE                                                         SAMPLE
020910        DISPLAY 'DB2 ERROR '                                      SAMPLE
020920                ' RETURN-CODE ' RETURN-CODE                       SAMPLE
020930     END-IF                                                       SAMPLE
020940                                                                  SAMPLE
020950     PERFORM 9900-ABEND  THRU 9900-EXIT                           SAMPLE
020960     .                                                            SAMPLE
020970 9960-EXIT.                                                       SAMPLE
020980     EXIT.                                                        SAMPLE
020990                                                                  SAMPLE
021000 9900-ABEND.                                                      SAMPLE
021010******************************************************************SAMPLE
021020*    COMMON ABEND ROUTINE                                        *SAMPLE
021030******************************************************************SAMPLE
021040*                                                                 SAMPLE
021050     MOVE '9900-ABEND       '   TO WORK-PARAGRAPH                 SAMPLE
021060                                                                  SAMPLE
021070     CALL 'MISABEND' USING ABEND-PARM                             SAMPLE
021080     .                                                            SAMPLE
021090 9900-EXIT.                                                       SAMPLE
021100     EXIT.                                                        SAMPLE
021110                                                                  SAMPLE
021120 9988-TOTALS.                                                     SAMPLE
021130******************************************************************SAMPLE
021140*    DISPLAY FILE TOTALS                                         *SAMPLE
021150******************************************************************SAMPLE
021160*                                                                 SAMPLE
021170     MOVE '9988-TOTALS      '   TO WORK-PARAGRAPH                 SAMPLE
021180                                                                  SAMPLE
021190     MOVE SPACES                 TO DISPLAY-TOTALS                SAMPLE
021200     MOVE C-IN                   TO DISPLAY-IN-OUT                SAMPLE
021210     MOVE C-HEIR-SERV-DESC       TO DISPLAY-DESC                  SAMPLE
021220     MOVE WS-HEIR-SERV-COUNT-IN  TO DISPLAY-ACCUMULATOR           SAMPLE
021230     DISPLAY DISPLAY-TOTALS                                       SAMPLE
021240                                                                  SAMPLE
021250     MOVE SPACES                 TO DISPLAY-TOTALS                SAMPLE
021260     MOVE C-IN                   TO DISPLAY-IN-OUT                SAMPLE
021270     MOVE C-ZDERDLR-DESC         TO DISPLAY-DESC                  SAMPLE
021280     MOVE WS-ZDERDLR-COUNT-IN    TO DISPLAY-ACCUMULATOR           SAMPLE
021290     DISPLAY DISPLAY-TOTALS                                       SAMPLE
021300                                                                  SAMPLE
021310     MOVE SPACES                 TO DISPLAY-TOTALS                SAMPLE
021320     MOVE C-IN                   TO DISPLAY-IN-OUT                SAMPLE
021330     MOVE C-ZDERDLR-BYPASS-DESC  TO DISPLAY-DESC                  SAMPLE
021340     MOVE WS-ZDERDLR-BYPASS-CNTR TO DISPLAY-ACCUMULATOR           SAMPLE
021350     MOVE WS-DEALER-BYP-OUT      TO DISPLAY-ACCUMULATOR2          SAMPLE
021360     DISPLAY DISPLAY-TOTALS                                       SAMPLE
021370                                                                  SAMPLE
021380     MOVE SPACES                 TO DISPLAY-TOTALS                SAMPLE
021390     MOVE C-IN                   TO DISPLAY-IN-OUT                SAMPLE
021400     MOVE C-ZDERDLR-ERR-DESC     TO DISPLAY-DESC                  SAMPLE
021410     MOVE WS-ZDERDLR-ERR-CNTR    TO DISPLAY-ACCUMULATOR           SAMPLE
021420     DISPLAY DISPLAY-TOTALS                                       SAMPLE
021430                                                                  SAMPLE
021440     MOVE SPACES                 TO DISPLAY-TOTALS                SAMPLE
021450     MOVE C-OUT                  TO DISPLAY-IN-OUT                SAMPLE
021460     MOVE C-CAR-OUT-DESC         TO DISPLAY-DESC                  SAMPLE
021470     MOVE WS-DEALER-CAR-OUT      TO DISPLAY-ACCUMULATOR           SAMPLE
021480     DISPLAY DISPLAY-TOTALS                                       SAMPLE
021490                                                                  SAMPLE
021500     MOVE SPACES                 TO DISPLAY-TOTALS                SAMPLE
021510     MOVE C-OUT                  TO DISPLAY-IN-OUT                SAMPLE
021520     MOVE C-CAR-OUT-DESC-NEW     TO DISPLAY-DESC                  SAMPLE
021530     MOVE WS-DEALER-CAR-OUT-NEW  TO DISPLAY-ACCUMULATOR           SAMPLE
021540     DISPLAY DISPLAY-TOTALS                                       SAMPLE
021550                                                                  SAMPLE
021560     MOVE SPACES                 TO DISPLAY-TOTALS                SAMPLE
021570     MOVE C-OUT                  TO DISPLAY-IN-OUT                SAMPLE
021580     MOVE C-TRUCK-OUT-DESC       TO DISPLAY-DESC                  SAMPLE
021590     MOVE WS-DEALER-TRK-OUT      TO DISPLAY-ACCUMULATOR           SAMPLE
021600     DISPLAY DISPLAY-TOTALS                                       SAMPLE
021610                                                                  SAMPLE
021620     MOVE SPACES                  TO DISPLAY-TOTALS               SAMPLE
021630     MOVE C-OUT                   TO DISPLAY-IN-OUT               SAMPLE
021640     MOVE C-TRUCK-OUT-DESC-NEW    TO DISPLAY-DESC                 SAMPLE
021650     MOVE WS-DEALER-TRK-OUT-NEW TO DISPLAY-ACCUMULATOR            SAMPLE
021660     DISPLAY DISPLAY-TOTALS                                       SAMPLE
021670     .                                                            SAMPLE
021680 9988-EXIT.                                                       SAMPLE
021690     EXIT.                                                        SAMPLE
021700                                                                  SAMPLE
021710 9999-FINISH.                                                     SAMPLE
021720******************************************************************SAMPLE
021730*    CLOSING PROCESS                                             *SAMPLE
021740******************************************************************SAMPLE
021750*                                                                 SAMPLE
021760     MOVE '9999-FINISH      '   TO WORK-PARAGRAPH                 SAMPLE
021770                                                                  SAMPLE
021780     PERFORM 9988-TOTALS        THRU 9988-EXIT                    SAMPLE
021790                                                                  SAMPLE
021800     PERFORM 7210-CLOSE-TABLE THRU 7210-EXIT                      SAMPLE
021810*                                                                 SAMPLE
021820     PERFORM 7220-CLOSE-ABCDEFG THRU 7220-EXIT                    SAMPLE
021830*                                                                 SAMPLE
021840     CLOSE        RUN-DATE-FILE                                   SAMPLE
021850                  PROFILE-VALID-DLR                               SAMPLE
021860                  COMPANY-PARM                                    SAMPLE
021870                  DLRSUMM-CARMB                                   SAMPLE
021880                  DLRSUMM-TRKBVF                                  SAMPLE
021890                  DLRSUMM-BYPASS                                  SAMPLE
021900                  ERROR-REPORT                                    SAMPLE
021910     .                                                            SAMPLE
021920 9999-EXIT.                                                       SAMPLE
021930     EXIT.                                                        SAMPLE
