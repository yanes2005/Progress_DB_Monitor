/* ------------------------------------------------------------------------- * 
 * ------------------------------------------------------------------------- */


/*        --------========<<<<<<<< Variables >>>>>>>>========--------        */
DEFINE VARIABLE inConnectUsr          AS INTEGER NO-UNDO.
DEFINE VARIABLE lTableMode            AS LOGICAL NO-UNDO INITIAL TRUE.

 
/*        --------========<<<<<<<< Temp-Tables >>>>>>>>========--------      */
DEFINE TEMP-TABLE ttTableStat NO-UNDO
  FIELD TableID       AS INT64
  FIELD TableName     AS CHARACTER
  FIELD StatType      AS CHARACTER
  FIELD LastValue     AS INT64
  FIELD CurrentValue  AS INT64
  FIELD DeltaValue    AS INT64
  FIELD AllSessValue  AS INT64
  INDEX IDX-Main      IS UNIQUE
          TableID
          StatType
  INDEX IDX-Sort
          CurrentValue
          DeltaValue  DESCENDING
          LastValue   DESCENDING.

DEFINE TEMP-TABLE ttIndexStat NO-UNDO
  FIELD Index_ID      AS INT64
  FIELD TableName     AS CHARACTER
  FIELD IndexName     AS CHARACTER
  FIELD StatType      AS CHARACTER
  FIELD LastValue     AS INT64
  FIELD CurrentValue  AS INT64
  FIELD DeltaValue    AS INT64
  FIELD AllSessValue  AS INT64
  INDEX IDX-Main      IS UNIQUE
          Index_ID
          StatType
  INDEX IDX-Sort
          CurrentValue
          DeltaValue  DESCENDING
          LastValue   DESCENDING.

DEFINE TEMP-TABLE ttConnection NO-UNDO
  FIELD ConnName   LIKE DICTDB._Connect._Connect-Name
  FIELD ConnUsr    LIKE DICTDB._Connect._Connect-Usr
  FIELD ConnPID    LIKE DICTDB._Connect._Connect-PID
  FIELD ConnType   LIKE DICTDB._Connect._Connect-Type
  FIELD ConnDevice LIKE DICTDB._Connect._Connect-Device
  FIELD ConnTime   LIKE DICTDB._Connect._Connect-Time
  FIELD ConnCType  LIKE DICTDB._Connect._Connect-ClientType
  INDEX Idx-Main   IS UNIQUE
          ConnUsr
  INDEX IE_Search  IS PRIMARY
          ConnName.
 

/*         --------========<<<<<<<< Buttons >>>>>>>>========--------         */
DEFINE BUTTON btnStack LABEL '&Stack trace' SIZE 13 BY 1.
DEFINE BUTTON btnMode  LABEL '&Index view'  SIZE 12 BY 1.
DEFINE BUTTON btnZero  LABEL '&Zero stats'  SIZE 12 BY 1.
DEFINE BUTTON btnConn  LABEL '&Connections' SIZE 13 BY 1.


/*   --------========<<<<<<<< Function Prototypes >>>>>>>>========--------   */
FUNCTION fnSetTableStat   RETURNS LOGICAL   ( INPUT INT64,
                                              INPUT CHARACTER,
                                              INPUT CHARACTER,
                                              INPUT INT64      ) FORWARD.
FUNCTION fnGetTableStats  RETURNS LOGICAL   (                  ) FORWARD.
FUNCTION fnGetCallStack   RETURNS CHARACTER (                  ) FORWARD.
FUNCTION fnGetIndexStats  RETURNS LOGICAL   (                  ) FORWARD.
FUNCTION fnSetIndexStat   RETURNS LOGICAL   ( INPUT INT64,
                                              INPUT CHARACTER,
                                              INPUT INT64      ) FORWARD.


/*         --------========<<<<<<<< Queries >>>>>>>>========--------         */
DEFINE QUERY qTableStat FOR ttTableStat SCROLLING.
DEFINE QUERY qIndexStat FOR ttIndexStat SCROLLING.


/*         --------========<<<<<<<< Browses >>>>>>>>========--------         */
DEFINE BROWSE bTableStat QUERY qTableStat DISPLAY
  ttTableStat.TableName  FORMAT          'X(25)' LABEL 'Table'
  ttTableStat.StatType   FORMAT           'X(8)' LABEL 'Operation'
  ttTableStat.DeltaValue FORMAT    '>>>,>>>,>>9' LABEL 'Last Sample'
  ttTableStat.LastValue  FORMAT '>>,>>>,>>>,>>9' LABEL 'Session Total' 
  WITH NO-BOX 17 DOWN.

DEFINE BROWSE bIndexStat QUERY qIndexStat DISPLAY
  ttIndexStat.TableName  FORMAT          'X(18)' LABEL 'Table'
  ttIndexStat.IndexName  FORMAT          'X(18)' LABEL 'Index'
  ttIndexStat.StatType   FORMAT           'X(8)' LABEL 'Operation'
  ttIndexStat.DeltaValue FORMAT    '>>>,>>>,>>9' LABEL 'Last Sample'
  ttIndexStat.LastValue  FORMAT '>>,>>>,>>>,>>9' LABEL 'Session Total'
  WITH NO-BOX 17 DOWN.


/*         --------========<<<<<<<< Frames >>>>>>>>========--------          */
DEFINE FRAME fMain
  bTableStat AT ROW  1 COL 7
             HELP 'Scroll/Page Up/Down, SPACE=Refresh, F4=Exit' 
  bIndexStat AT ROW  1 COL 1
             HELP 'Scroll/Page Up/Down, SPACE=Refresh, F4=Exit' 
  btnStack   AT ROW 21 COLUMN  6
             HELP 'View the Stack Trace for this Connection'
  btnMode    AT ROW 21 COLUMN 25
             HELP 'Switch to Index View'
  btnZero    AT ROW 21 COLUMN 43
             HELP 'Reset All Statistics to Zero'
  btnConn    AT ROW 21 COLUMN 61
             HELP 'Select a Different Connection to Monitor'
  WITH NO-BOX CENTERED.


/*        --------========<<<<<<<< Triggers >>>>>>>>========--------         */

/*---Show Stack Trace---*/
ON 'S':U, 's':U OF bTableStat, bIndexStat IN FRAME fMain
OR 'CHOOSE':U   OF btnStack               IN FRAME fMain
DO:
  DEFINE VARIABLE cStackTrace AS CHARACTER NO-UNDO.

  ASSIGN cStackTrace = fnGetCallStack().

  IF cStackTrace = ''
  THEN MESSAGE 'Statement Caching Not Enabled'
          VIEW-AS ALERT-BOX INFO.
  ELSE MESSAGE fnGetCallStack()
          VIEW-AS ALERT-BOX BUTTONS OK TITLE 'Call Stack'.

  RETURN.
END.

/*---Switch User / Connection---*/
ON 'C':U, 'c':U OF bTableStat, bIndexStat IN FRAME fMain
OR 'CHOOSE':U   OF btnConn                IN FRAME fMain
DO:
  DEFINE VARIABLE inNewConnUsr AS INTEGER NO-UNDO.

  RUN pChooseConnection ( OUTPUT inNewConnUsr ).

  IF inNewConnUsr = 0
  THEN RETURN.

  ASSIGN inConnectUsr = inNewConnUsr.
  EMPTY TEMP-TABLE ttTableStat. /* Clear */
  EMPTY TEMP-TABLE ttIndexStat. /* Data  */
  APPLY 'U1' TO FRAME fMain. /* & Refresh */
  RETURN.
END.

/*---Switch Table / Index Mode---*/
ON 'I':U, 'i':U OF bTableStat IN FRAME fMain
OR 'T':U, 't':U OF bIndexStat IN FRAME fMain
OR 'CHOOSE':U   OF btnMode    IN FRAME fMain
DO:
  ASSIGN lTableMode = NOT lTableMode.
  APPLY 'U1' TO FRAME fMain.
  RETURN.
END.

/*---Reset all Stats to Zero---*/
ON 'Z':U, 'z':U OF bTableStat, bIndexStat IN FRAME fMain
OR 'CHOOSE':U   OF btnZero                IN FRAME fMain
DO:
  FOR EACH ttTableStat:
    ASSIGN ttTableStat.AllSessValue = ttTableStat.AllSessValue
                                    + ttTableStat.CurrentValue
           ttTableStat.LastValue    = 0.
  END.
  FOR EACH ttIndexStat:
    ASSIGN ttIndexStat.AllSessValue = ttIndexStat.AllSessValue
                                    + ttIndexStat.CurrentValue
           ttIndexStat.LastValue    = 0.
  END.
  APPLY 'U1' TO FRAME fMain.
  RETURN.
END.

/*  ---------=========<<<<<<<<< DEFINITIONS END >>>>>>>>>=========---------  */

/*  ---------=========<<<<<<<<< MAIN CODE BEGIN >>>>>>>>>=========---------  */

PAUSE 0 BEFORE-HIDE.

RUN pChooseConnection ( OUTPUT inConnectUsr ).

IF inConnectUsr = 0
THEN RETURN.

MAIN-BLOCK:
REPEAT ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
       ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  fnGetTableStats().
  fnGetIndexStats().

  ENABLE btnStack btnMode btnZero btnConn WITH FRAME fMain.

  ASSIGN
    bTableStat:VISIBLE   IN FRAME fMain =     lTableMode
    bTableStat:SENSITIVE IN FRAME fMain =     lTableMode
    bIndexStat:VISIBLE   IN FRAME fMain = NOT lTableMode
    bIndexStat:SENSITIVE IN FRAME fMain = NOT lTableMode
    btnMode   :LABEL     IN FRAME fMain = IF  lTableMode
                                          THEN '&Index view'
                                          ELSE '&Table view'
    btnMode   :HELP      IN FRAME fMain = 'Switch to '
  + btnMode   :LABEL     IN FRAME fMain.

  IF lTableMode
  THEN OPEN QUERY qTableStat
        FOR EACH ttTableStat
           WHERE ttTableStat.CurrentValue > 0 
              BY ttTableStat.DeltaValue   DESCENDING
              BY ttTableStat.LastValue    DESCENDING.
  ELSE OPEN QUERY qIndexStat
        FOR EACH ttIndexStat 
           WHERE ttIndexStat.CurrentValue > 0 
              BY ttIndexStat.DeltaValue   DESCENDING
              BY ttIndexStat.LastValue    DESCENDING.

  WAIT-FOR 'U1', ' ', 'END-ERROR' OF FRAME fMain PAUSE 60.

  IF KEYFUNCTION( LASTKEY ) = 'END-ERROR' 
  THEN LEAVE.

END. /*--- REPEAT ---*/

CLOSE QUERY qTableStat.
CLOSE QUERY qIndexStat.
HIDE FRAME fMain.


/*        --------========<<<<<<<< Procedures >>>>>>>>========--------       */
PROCEDURE pChooseConnection:

  /*--- Define Pre-Processors ---*/
  &SCOPED-DEFINE OpenConnQuery OPEN QUERY qConn FOR EACH ttConnection ~
     WHERE ttConnection.ConnName BEGINS fiFilterUser.
  
  /*--- Define Parameters ---*/
  DEFINE OUTPUT PARAMETER opConnectionUsr AS INTEGER NO-UNDO.

  /*--- Define Variables ---*/
  DEFINE VARIABLE fiFilterUser AS CHARACTER FORMAT 'X(256)':U 
    LABEL 'Filter for UID' VIEW-AS FILL-IN SIZE 14 BY 1 NO-UNDO.

  /*--- Define Queries ---*/
  DEFINE QUERY qConn FOR ttConnection SCROLLING.

  /*--- Define Browsers ---*/
  DEFINE BROWSE bConn QUERY qConn DISPLAY
    ttConnection.ConnName   FORMAT      'x(9)' COLUMN-LABEL 'UID'
    ttConnection.ConnUsr    FORMAT      '>>>9' COLUMN-LABEL 'Con#'
    ttConnection.ConnPID  
    ttConnection.ConnType
    ttConnection.ConnCType                     COLUMN-LABEL 'Clnt'
    ttConnection.ConnDevice
    ttConnection.ConnTime 
    WITH NO-BOX 14 DOWN.

  /*--- Define Frames ---*/
  DEFINE FRAME dgSelConn
    bConn HELP
      'Start Typing to Filter by UID, Return to Select, F4 to Cancel'       
    fiFilterUser AT ROW 18 COL 40 COLON-ALIGNED 
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS TITLE
 '                     Select Database Connection to Monitor                ' 
         ROW 2 CENTERED

/* I keep getting this while doing a syntax check under windows....
 *   **BROWSE bConn will not fit in FRAME dgSelConn in PROGRAM . (4028)
 */
&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
    WIDTH 84
&ENDIF
    .
 
  /*---Triggers---*/
  ON DEFAULT-ACTION OF bConn IN FRAME dgSelConn
  DO:
    IF AVAILABLE ttConnection 
    THEN ASSIGN opConnectionUsr = ttConnection.ConnUsr.
    APPLY 'GO' TO FRAME dgSelConn.
    RETURN.
  END.

  ON 'ANY-PRINTABLE':U OF bConn IN FRAME dgSelConn
  DO:
    ASSIGN fiFilterUser = fiFilterUser + KEYFUNCTION( LASTKEY ).
    DISPLAY fiFilterUser WITH FRAME dgSelConn. 
    {&OpenConnQuery}
    RETURN.
  END.

  ON 'BACKSPACE':U OF bConn IN FRAME dgSelConn
  DO:
    IF     LENGTH( fiFilterUser ) >= 1
    THEN ASSIGN    fiFilterUser
      = SUBSTRING( fiFilterUser, 1,
           LENGTH( fiFilterUser ) - 1 ).
    DISPLAY fiFilterUser WITH FRAME dgSelConn.
    {&OpenConnQuery}
    RETURN.
  END.

  ON 'END-ERROR':U OF bConn IN FRAME dgSelConn
  DO:
    APPLY 'GO' TO FRAME dgSelConn.
    RETURN.
  END.

  /*---MAIN---*/
  RUN pLoadConnections.

  SEL-CONN-BLOCK:
  DO ON ERROR   UNDO SEL-CONN-BLOCK, LEAVE SEL-CONN-BLOCK
     ON END-KEY UNDO SEL-CONN-BLOCK, LEAVE SEL-CONN-BLOCK:

    DISPLAY fiFilterUser WITH FRAME dgSelConn.
    ENABLE  bConn        WITH FRAME dgSelConn.
    VIEW FRAME Dialog-Frame.
    {&OpenConnQuery}
    WAIT-FOR GO OF FRAME dgSelConn.
  END.

  CLOSE QUERY qConn.
  HIDE FRAME dgSelConn.

END PROCEDURE. /* pChooseConnection */
  

PROCEDURE pLoadConnections:

  EMPTY TEMP-TABLE ttConnection.

  FOR EACH DICTDB._Connect  NO-LOCK
     WHERE DICTDB._Connect._Connect-Type <> 'SERV'
       AND DICTDB._Connect._Connect-Type <> 'APW'
       AND DICTDB._Connect._Connect-Type <> 'BIW'
       AND DICTDB._Connect._Connect-Type <> 'AIW'
       AND DICTDB._Connect._Connect-Type <> 'BROK'
       AND DICTDB._Connect._Connect-Type <> 'WDOG':

    CREATE ttConnection.
    ASSIGN ttConnection.ConnName   = DICTDB._Connect._Connect-Name 
           ttConnection.ConnUsr    = DICTDB._Connect._Connect-Usr
           ttConnection.ConnPID    = DICTDB._Connect._Connect-PID
           ttConnection.ConnType   = DICTDB._Connect._Connect-Type
           ttConnection.ConnDevice = DICTDB._Connect._Connect-Device
           ttConnection.ConnTime   = DICTDB._Connect._Connect-Time
           ttConnection.ConnCType  = DICTDB._Connect._Connect-ClientType.
  END.
END PROCEDURE. /* pLoadConnections */


/*        --------========<<<<<<<< Functions >>>>>>>>========--------        */
FUNCTION fnSetTableStat RETURNS LOGICAL
  ( INPUT ipiID     AS INT64,
    INPUT ipcTable  AS CHARACTER,
    INPUT ipcType   AS CHARACTER,
    INPUT ipiValue  AS INT64 ):
  
  IF ipiValue = 0
  THEN RETURN TRUE.
   
  FIND  ttTableStat
  WHERE ttTableStat.TableID  = ipiID
    AND ttTableStat.StatType = ipcType
                               NO-ERROR.

  IF AVAILABLE ttTableStat
  THEN ASSIGN ttTableStat.CurrentValue = ipiValue
                                       - ttTableStat.AllSessValue
              ttTableStat.DeltaValue   = ttTableStat.CurrentValue
                                       - ttTableStat.LastValue
              ttTableStat.LastValue    = ttTableStat.CurrentValue. 
  ELSE DO:

    CREATE ttTableStat.
    ASSIGN ttTableStat.TableID      = ipiID   
           ttTableStat.TableName    = ipcTable
           ttTableStat.StatType     = ipcType
           ttTableStat.LastValue    = ipiValue
           ttTableStat.CurrentValue = ipiValue.
  END.

  RETURN TRUE.
  
END FUNCTION. /*--- fnSetTableStat ---*/


FUNCTION fnGetTableStats RETURNS LOGICAL ():

  /* Based on George Potemkin's PEG post "Re: vst tables" 23/01/2013 */
  DEFINE VARIABLE inTableRangeSize  AS INT64  NO-UNDO.
  DEFINE VARIABLE inHighestTableId  AS INT64  NO-UNDO.
  DEFINE VARIABLE inFirstStatID     AS INT64  NO-UNDO.
  DEFINE VARIABLE inLastStatID      AS INT64  NO-UNDO.

  FOR LAST DICTDB._TableStat NO-LOCK:
    ASSIGN inTableRangeSize = RECID( DICTDB._TableStat ).
  END.

  FOR EACH DICTDB._File NO-LOCK
        BY DICTDB._File._File-Number DESCENDING:
    ASSIGN inHighestTableId = DICTDB._File._File-Number.
    LEAVE.
  END.

  ASSIGN inFirstStatID = inTableRangeSize * inConnectUsr
         inLastStatID  = inTableRangeSize + inFirstStatID
         inFirstStatID = inFirstStatID    + 1. 

  FOR EACH DICTDB._UserTableStat  NO-LOCK
  /* WHERE DICTDB._UserTableStat._UserTableStat-Conn = inConnectUsr, UNINDEXED!!! */
     WHERE DICTDB._UserTableStat._UserTableStat-ID  >= inFirstStatID
       AND DICTDB._UserTableStat._UserTableStat-ID  <= inLastStatID
     WHILE DICTDB._UserTableStat._UserTableStat-Num <= inHighestTableId:

    FOR FIRST DICTDB._File                NO-LOCK
        WHERE DICTDB._File._File-Number = DICTDB._UserTableStat._UserTableStat-Num:
 
      fnSetTableStat( DICTDB._File._File-Number,
                      DICTDB._File._File-Name,
                      'Read',
                      DICTDB._UserTableStat._UserTableStat-Read ).
      fnSetTableStat( DICTDB._File._File-Number,
                      DICTDB._File._File-Name,
                      'Create',
                      DICTDB._UserTableStat._UserTableStat-Create ).
      fnSetTableStat( DICTDB._File._File-Number,
                      DICTDB._File._File-Name,
                      'Update',
                      DICTDB._UserTableStat._UserTableStat-Update ).
      fnSetTableStat( DICTDB._File._File-Number,
                      DICTDB._File._File-Name,
                      'Delete',
                      DICTDB._UserTableStat._UserTableStat-Delete ).

    END. /*--- FOR FIRST File ---*/
  END. /*--- FOR EACH UserTableStat ---*/

  RETURN TRUE.
 
END FUNCTION. /*--- fnGetTableStats ---*/


FUNCTION fnGetCallStack RETURNS CHARACTER ():

  DEFINE VARIABLE cStackList  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iStackCount AS INTEGER   NO-UNDO INITIAL  1.

  /* _Connect._Connect-ID = _Connect._Connect-Usr + 1 !!! */
  FIND  DICTDB._Connect
  WHERE DICTDB._Connect._Connect-ID = inConnectUsr + 1
                                      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE DICTDB._Connect
  THEN RETURN ''.

  DO WHILE DICTDB._Connect._Connect-CacheInfo[ iStackCount ] <> ?:

    ASSIGN cStackList
         = cStackList
         + SUBSTITUTE( '[&1] &2 (&3)~n',
             iStackCount,
             DICTDB._Connect._Connect-CacheInfo[ iStackCount ],
             DICTDB._Connect._Connect-CacheLineNumber[ iStackCount ] )

           iStackCount
         = iStackCount
         + 1.
  END.

  RETURN cStackList.

END FUNCTION. /*--- fnGetCallStack ---*/


FUNCTION fnSetIndexStat RETURNS LOGICAL
  ( INPUT ipiID     AS INT64,
    INPUT ipcType   AS CHARACTER,
    INPUT ipiValue  AS INT64 ):

  IF ipiValue = 0
  THEN RETURN TRUE.

  FIND  ttIndexStat
  WHERE ttIndexStat.Index_ID = ipiID
    AND ttIndexStat.StatType = ipcType
                               NO-ERROR.

  IF AVAILABLE ttIndexStat
  THEN ASSIGN ttIndexStat.CurrentValue = ipiValue
                                       - ttIndexStat.AllSessValue
              ttIndexStat.DeltaValue   = ttIndexStat.CurrentValue
                                       - ttIndexStat.LastValue
              ttIndexStat.LastValue    = ttIndexStat.CurrentValue.
  ELSE DO:

    CREATE ttIndexStat.
    ASSIGN ttIndexStat.Index_ID     = ipiID   
           ttIndexStat.StatType     = ipcType
           ttIndexStat.LastValue    = ipiValue
           ttIndexStat.CurrentValue = ipiValue
           ttIndexStat.TableName    = DICTDB._File._File-Name
           ttIndexStat.IndexName    = DICTDB._Index._Index-Name.
  END.

  RETURN TRUE.
   
END FUNCTION. /*--- fnSetIndexStat ---*/


FUNCTION fnGetIndexStats RETURNS LOGICAL ():

  DEFINE VARIABLE inIndexRangeSize  AS INT64  NO-UNDO.
  DEFINE VARIABLE inHighestIndexId  AS INT64  NO-UNDO.
  DEFINE VARIABLE inFirstStatID     AS INT64  NO-UNDO.
  DEFINE VARIABLE inLastStatID      AS INT64  NO-UNDO.

  FOR LAST DICTDB._IndexStat NO-LOCK:
    ASSIGN inIndexRangeSize = RECID( DICTDB._IndexStat ).
  END.

  FOR EACH DICTDB._Index NO-LOCK
        BY DICTDB._Index._Idx-Num DESCENDING:
    ASSIGN inHighestIndexId = DICTDB._Index._Idx-Num.
    LEAVE.
  END.

  ASSIGN inFirstStatID = inIndexRangeSize * inConnectUsr
         inLastStatID  = inIndexRangeSize + inFirstStatID
         inFirstStatID = inFirstStatID    + 1. 

  FOR EACH DICTDB._UserIndexStat  NO-LOCK
  /* WHERE DICTDB._UserIndexStat._UserIndexStat-Conn = inConnectUsr, UNINDEXED!!! */
     WHERE DICTDB._UserIndexStat._UserIndexStat-ID  >= inFirstStatID
       AND DICTDB._UserIndexStat._UserIndexStat-ID  <= inLastStatID
     WHILE DICTDB._UserIndexStat._UserIndexStat-Num <= inHighestIndexId:

    FOR FIRST DICTDB._Index            NO-LOCK
        WHERE DICTDB._Index._Idx-Num = DICTDB._UserIndexStat._UserIndexStat-Num,
        FIRST DICTDB._File             NO-LOCK
        WHERE RECID( DICTDB._File )  = DICTDB._Index._File-Recid
          AND DICTDB._File._Hidden   = FALSE:
         
      fnSetIndexStat( DICTDB._UserIndexStat._UserIndexStat-Num,
                      'Read',
                      DICTDB._UserIndexStat._UserIndexStat-Read ).
      fnSetIndexStat( DICTDB._UserIndexStat._UserIndexStat-Num,
                      'Create',
                      DICTDB._UserIndexStat._UserIndexStat-Create ).
      fnSetIndexStat( DICTDB._UserIndexStat._UserIndexStat-Num,
                      'Delete',
                      DICTDB._UserIndexStat._UserIndexStat-Delete ).
      fnSetIndexStat( DICTDB._UserIndexStat._UserIndexStat-Num,
                      'BlockDelete',
                      DICTDB._UserIndexStat._UserIndexStat-BlockDelete ).
      fnSetIndexStat( DICTDB._UserIndexStat._UserIndexStat-Num,
                      'Split',
                      DICTDB._UserIndexStat._UserIndexStat-Split ).
    END.
  END.

  RETURN TRUE.

END FUNCTION. /*--- fnGetIndexStats ---*/
