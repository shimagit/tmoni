Imports System.Drawing.Text
Imports System.Security.Cryptography.Pkcs

'Contec add start
Imports VB = Microsoft.VisualBasic
Imports System.Text
Imports System.Runtime.CompilerServices
Imports Microsoft.VisualBasic.ApplicationServices



Public Class Form1
    '==========================================
    ' 変数の宣言
    '==========================================
    Dim Id As Short                              ' デバイスID
    Dim Ret As Integer                           ' 戻り値
    Dim szError As New StringBuilder("", 256)       ' エラー文字列
    Dim szText As New String("", 100)
    Dim UpCount(19) As Short                      ' アップカウンタ
    Dim DownCount(19) As Short                    ' ダウンカウンタ
    Dim Check(19) As CheckBox
    Dim Edit_Down(19) As TextBox
    Dim Edit_Up(19) As TextBox

    Dim DebugMode As Boolean = True                'デバッグモード
    Dim ParaAim(20) As Single                      '目標値
    Dim ParaMulti(20) As Short                     '1サイクル辺りの取れ数

    Dim AveGrobal(20) As Double                    '現在の平均値 グローバル変数

    Dim db1 As Boolean = True
    Dim db2 As Integer = 0
    Dim db3(90) As Integer
    Dim pushCount As Integer = 0


    ' Me.AutoScrollPosition = New Point(-Me.AutoScrollPosition.X + 10, -Me.AutoScrollPosition.Y + 10)

    '---------------------------------------------------------
    ' ダイアログ初期化
    '---------------------------------------------------------
    Private Sub Interrupt_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load

        LoadParameta()

        Dim i As Short

        Me.Left = (Screen.PrimaryScreen.Bounds.Width - Me.Width) / 2
        Me.Top = (Screen.PrimaryScreen.Bounds.Height - Me.Height) / 2

        Check(0) = _Check_0
        Edit_Down(0) = _Edit_Down_0
        Edit_Up(0) = _Edit_Up_0

        ' 画面アイテムの初期化
        Edit_DeviceName.Text = "DIO000"

        For i = 0 To 0
            Check(i).Checked = True
        Next
        ' アップカウンタ
        For i = 0 To 0
            Edit_Up(i).Text = ""
        Next i

        ' ダウンカウンタ
        For i = 0 To 0
            Edit_Down(i).Text = ""
        Next i

        Edit_ReturnCode.Text = ""


        'Check(0) = Check_0

        Edit_Up(0) = Edit_Up_0
        Edit_Up(1) = Edit_Up_1
        Edit_Up(2) = Edit_Up_2
        Edit_Up(3) = Edit_Up_3
        Edit_Up(4) = Edit_Up_4
        Edit_Up(5) = Edit_Up_5
        Edit_Up(6) = Edit_Up_6
        Edit_Up(7) = Edit_Up_7
        Edit_Up(8) = Edit_Up_8
        Edit_Up(9) = Edit_Up_9
        Edit_Up(10) = Edit_Up_10
        Edit_Up(11) = Edit_Up_11
        Edit_Up(12) = Edit_Up_12
        Edit_Up(13) = Edit_Up_13
        Edit_Up(14) = Edit_Up_14
        Edit_Up(15) = Edit_Up_15
        Edit_Up(16) = Edit_Up_16
        Edit_Up(17) = Edit_Up_17
        Edit_Up(18) = Edit_Up_18

        Edit_Down(0) = Edit_Down_0
        Edit_Down(1) = Edit_Down_1
        Edit_Down(2) = Edit_Down_2
        Edit_Down(3) = Edit_Down_3
        Edit_Down(4) = Edit_Down_4
        Edit_Down(5) = Edit_Down_5
        Edit_Down(6) = Edit_Down_6
        Edit_Down(7) = Edit_Down_7
        Edit_Down(8) = Edit_Down_8
        Edit_Down(9) = Edit_Down_9
        Edit_Down(10) = Edit_Down_10
        Edit_Down(11) = Edit_Down_11
        Edit_Down(12) = Edit_Down_12
        Edit_Down(13) = Edit_Down_13
        Edit_Down(14) = Edit_Down_14
        Edit_Down(15) = Edit_Down_15
        Edit_Down(16) = Edit_Down_16
        Edit_Down(17) = Edit_Down_17
        Edit_Down(18) = Edit_Down_18

        '--------------------------------------
        ' 画面表示の初期化
        '--------------------------------------
        Edit_DeviceName.Text = "DIO000"
        Edit_ReturnCode.Text = ""
        'アップカウント
        For i = 0 To 18
            Edit_Up(i).Text = ""
        Next i
        'ダウンカウント
        For i = 0 To 18
            Edit_Down(i).Text = ""
        Next i
        'Radio_Rise.Checked = True
        Edit_ReturnCode.Text = ""

    End Sub

    '---------------------------------------------------------
    '  DioInit()
    '---------------------------------------------------------
    Private Sub Button_DioInit_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Button_DioInit.Click
        ContecDioInit()
    End Sub

    Private Sub ContecDioInit()
        Dim szDeviceName As String

        '--------------------------------------
        ' デバイス名取得
        '--------------------------------------
        szDeviceName = Edit_DeviceName.Text
        '--------------------------------------
        ' 初期化処理
        '--------------------------------------
        Ret = DioInit(szDeviceName, Id)
        '--------------------------------------
        ' エラー処理
        '--------------------------------------
        Call DioGetErrorString(Ret, szError)
        Edit_ReturnCode.Text = "DioInit Ret = " & Ret & ":" & szError.ToString()
    End Sub

    '---------------------------------------------------------
    ' DioExit()
    '---------------------------------------------------------
    Private Sub Button_DioExit_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Button_DioExit.Click

        '--------------------------------------
        ' DioExit()
        '--------------------------------------
        Ret = DioExit(Id)
        '--------------------------------------
        ' エラー処理
        '--------------------------------------
        Call DioGetErrorString(Ret, szError)
        Edit_ReturnCode.Text = "DioExit Ret = " & Ret & ":" & szError.ToString()

    End Sub

    '---------------------------------------------------------
    ' ダイアログExit
    '---------------------------------------------------------
    Private Sub Button_Exit_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Button_Exit.Click

        '--------------------------------------
        ' DioExit()
        '--------------------------------------
        Ret = DioExit(Id)
        '--------------------------------------
        ' ダイアログExit
        '--------------------------------------
        End
    End Sub

    '---------------------------------------------------------
    ' トリガ監視開始
    '---------------------------------------------------------
    Private Sub Button_Trigger_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Button_Trigger.Click
        ContecTriggerStart()
    End Sub

    Private Sub ContecTriggerStart()

        Dim i As Short
        Dim BitNo As Short
        Dim Kind As Short
        Dim Tim As Integer

        ' 変数を初期化する
        For i = 0 To 19
            UpCount(i) = 0                      ' アップカウンタ
            DownCount(i) = 0                    ' ダウンカウンタ
        Next i

        ' チェックを見て
        Tim = 100                               ' 100ms周期で監視
        Kind = DIO_TRG_RISE Or DIO_TRG_FALL     ' アップダウン両方を監視
        For BitNo = 0 To 19

            ' トリガ開始
            'If Check(BitNo).CheckState = 1 Then
            'If Check(0).CheckState = 1 Then
            Ret = DioNotifyTrg(Id, BitNo, Kind, Tim, Me.Handle)
            If (Ret <> DIO_ERR_SUCCESS) Then
                Exit For
            End If

            ' またはトリガ停止
            'Else
            'Ret = DioStopNotifyTrg(Id, BitNo)
            'If (Ret <> DIO_ERR_SUCCESS) Then
            'Exit For
            'End If
            'End If
        Next BitNo

        ' エラー処理
        Call DioGetErrorString(Ret, szError)
        Edit_ReturnCode.Text = "Ret = " & Ret & ":" & szError.ToString()
    End Sub
    '---------------------------------------------------------
    ' サブルーチン：チェック処理
    '---------------------------------------------------------
    Function CheckProcess(ByRef BitNo As Short) As Object

        Dim Tim As Integer              ' 100ms周期で監視
        Dim Kind As Short               ' アップダウン両方を監視

        Tim = 100
        Kind = DIO_TRG_RISE Or DIO_TRG_FALL

        ' トリガ開始\
        'If Check(BitNo).CheckState = 1 Then
        'If Check(0).CheckState = 1 Then

        Ret = DioNotifyTrg(Id, BitNo, Kind, Tim, Me.Handle)

        ' またはトリガ停止
        'Else
        'Ret = DioStopNotifyTrg(Id, BitNo)
        'End If

        ' エラー処理
        Call DioGetErrorString(Ret, szError)
        Edit_ReturnCode.Text = "Ret = " & Ret & ":" & szError.ToString()
        Return 0
    End Function

    '---------------------------------------------------------
    ' チェック処理
    '---------------------------------------------------------
    Private Sub _Check_0_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Check_0.CheckedChanged
        Call CheckProcess(0)
    End Sub

    '---------------------------------------------------------
    ' サブルーチン：HIWORD,LOWORDを取得する
    '---------------------------------------------------------
    Function HiLoWord(ByRef Number As Integer, ByRef HiLo As Short) As Integer

        Dim Hi As Integer
        Dim Lo As Integer
        Dim StrData As String
        Dim i As Short

        StrData = Hex(Number)
        If Len(StrData) < 8 Then
            For i = 1 To 8 - Len(StrData)
                StrData = "0" & StrData
            Next i
        End If

        Lo = Val("&h" & VB.Right(StrData, 4))
        Hi = Val("&h" & VB.Left(StrData, 4))
        If HiLo = 0 Then
            HiLoWord = Lo
        ElseIf HiLo = 1 Then
            HiLoWord = Hi
        End If

        Return HiLoWord

    End Function

    '---------------------------------------------------------
    ' メッセージハンドラ
    '---------------------------------------------------------
    Protected Overrides Sub WndProc(ByRef m As Message)
        Dim BitNo As Short
        Dim Id As Short
        Dim Kind As Short

        '--------------------------------------
        ' トリガイベントであれば
        '--------------------------------------
        If m.Msg = DIOM_TRIGGER Then

            Id = HiLoWord(m.WParam.ToInt32(), 0)             'DioInit関数で取得したID
            BitNo = HiLoWord(m.LParam.ToInt32(), 0)          'トリガビット番号
            Kind = HiLoWord(m.LParam.ToInt32(), 1)           '立ち上がり：1、もしくは立ち下がり：2、両方：3

            'If (Kind And DIO_INT_RISE) Then
            '    UpCount(BitNo) = UpCount(BitNo) + 1          'アップカウント
            '    StackIn(BitNo)
            'End If
            If (Kind And DIO_INT_FALL) Then
                DownCount(BitNo) = DownCount(BitNo) + 1      'ダウンカウント
                StackIn(BitNo)
            End If
            '--------------------------------------
            ' カウント値表示
            '--------------------------------------
            Edit_Up(BitNo).Text = CStr(UpCount(BitNo))
            Edit_Down(BitNo).Text = CStr(DownCount(BitNo))

            'StackIn(BitNo)

            UpCount(BitNo) = 0
            DownCount(BitNo) = 0

        End If

        MyBase.WndProc(m)
    End Sub


    'Contec add end


    Dim mil1 As Integer
    Dim mil2 As Integer

    Dim counter1(18) As Integer
    Dim counter2(18) As Integer

    Dim stk0 As New Stack(Of String)()
    Dim stk1 As New Stack(Of String)()
    Dim stk2 As New Stack(Of String)()
    Dim stk3 As New Stack(Of String)()
    Dim stk4 As New Stack(Of String)()
    Dim stk5 As New Stack(Of String)()
    Dim stk6 As New Stack(Of String)()
    Dim stk7 As New Stack(Of String)()
    Dim stk8 As New Stack(Of String)()
    Dim stk9 As New Stack(Of String)()
    Dim stk10 As New Stack(Of String)()
    Dim stk11 As New Stack(Of String)()
    Dim stk12 As New Stack(Of String)()
    Dim stk13 As New Stack(Of String)()
    Dim stk14 As New Stack(Of String)()
    Dim stk15 As New Stack(Of String)()
    Dim stk16 As New Stack(Of String)()
    Dim stk17 As New Stack(Of String)()
    Dim stk18 As New Stack(Of String)()
    Dim stk As Stack(Of String)() = Enumerable.Range(1, 20).Select(Function(i) New Stack(Of String)()).ToArray()


    Dim aveHour0 As New Stack(Of String)()
    Dim aveHour1 As New Stack(Of String)()
    Dim aveHour2 As New Stack(Of String)()
    Dim aveHour3 As New Stack(Of String)()
    Dim aveHour4 As New Stack(Of String)()
    Dim aveHour5 As New Stack(Of String)()
    Dim aveHour6 As New Stack(Of String)()
    Dim aveHour7 As New Stack(Of String)()
    Dim aveHour8 As New Stack(Of String)()
    Dim aveHour9 As New Stack(Of String)()
    Dim aveHour10 As New Stack(Of String)()
    Dim aveHour11 As New Stack(Of String)()
    Dim aveHour12 As New Stack(Of String)()
    Dim aveHour13 As New Stack(Of String)()
    Dim aveHour14 As New Stack(Of String)()
    Dim aveHour15 As New Stack(Of String)()
    Dim aveHour16 As New Stack(Of String)()
    Dim aveHour17 As New Stack(Of String)()
    Dim aveHour18 As New Stack(Of String)()
    Dim aveHour As Stack(Of String)() = Enumerable.Range(1, 20).Select(Function(i) New Stack(Of String)()).ToArray()
    'Stackクラスの配列化（別の方法）
    'Dim ix As Integer
    'Dim ListOfStack As New List(Of Stack(Of String))

    'For ix = 0 To 19
    '        ListOfStack.Add(New Stack(Of String))
    '    Next ix
    'For ix = 0 To 29
    '        ListOfStack(0).Push(ix.ToString)
    '        ListOfStack(1).Push(ix * 2.ToString)
    'Next ix
    'Dim arr1 = ListOfStack(0).ToArray
    'Dim arr2 = ListOfStack(1).ToArray
    'Dim arr3 = ListOfStack(2).ToArray

    Dim r As New System.Random(1000)

    Private testButtons() As System.Windows.Forms.Button    'ボタンコントロール配列のフィールドを作成
    Private lblTitle() As System.Windows.Forms.Label        '項目名のラベルコントロール配列のフィールドを作成
    Private lblMcNames() As System.Windows.Forms.Label      '装置名のラベルコントロール配列のフィールドを作成
    Private lblTimeAim() As System.Windows.Forms.Label      '平均時間のラベルコントロール配列のフィールドを作成
    Private lblTimeAve() As System.Windows.Forms.Label      '平均時間のラベルコントロール配列のフィールドを作成
    Private lblTimeMin() As System.Windows.Forms.Label      '最小時間のラベルコントロール配列のフィールドを作成
    Private lblTimeNow() As System.Windows.Forms.Label      '現在時間のラベルコントロール配列のフィールドを作成
    Private lblTimeAvx() As System.Windows.Forms.Label      '1日の平均時間のラベルコントロール配列のフィールドを作成
    Private lblTimeStk0() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk1() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk2() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk3() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk4() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk5() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk6() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk7() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk8() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk9() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk10() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk11() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk12() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk13() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk14() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk15() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk16() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk17() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk18() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeStk19() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour0() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour1() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour2() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour3() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour4() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour5() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour6() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour7() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour8() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour9() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour10() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour11() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour12() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour13() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour14() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour15() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour16() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour17() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    Private lblTimeAveHour18() As System.Windows.Forms.Label      'スタックのラベルコントロール配列のフィールドを作成
    'Private timDummy() As System.Windows.Forms.Timer       'デバッグ用のタイマーコントロール配列のフィールドを作成

    Public Const STACK_AREA = 30
    Public ReadOnly NameMc() As String = {"Magカシメ", "M&Aカシメ", "M&A圧入", "特性検査", "カバー挿入", "STターミナル圧入", "Magカシメ", "M&Aカシメ", "M&A圧入", "特性検査", "カバー挿入", "STターミナル圧入", "シール", "遠赤炉", "半田付け", "封止", "特性選別", "マーキング", "xxx"}




    Private Sub Timer99_Tick(sender As Object, e As EventArgs) Handles Timer99.Tick
        'Hist()
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        'LoadParameta()

        Const WIDTH_MC As Integer = 80 * 1.2
        Const HEIGHT_MC As Integer = 25 * 2
        Dim j As Integer

        Me.ActiveControl = Me.Label21   '初期フォーカスするコントロールを設定（起動時画面を一番上/右）

        Me.lblTitle = New System.Windows.Forms.Label(19) {}
        Me.lblMcNames = New System.Windows.Forms.Label(NameMc.Length) {}
        Me.lblTimeAim = New System.Windows.Forms.Label(NameMc.Length) {}
        Me.lblTimeAve = New System.Windows.Forms.Label(NameMc.Length) {}
        Me.lblTimeMin = New System.Windows.Forms.Label(NameMc.Length) {}
        Me.lblTimeNow = New System.Windows.Forms.Label(NameMc.Length) {}
        Me.lblTimeAvx = New System.Windows.Forms.Label(NameMc.Length) {}
        Me.lblTimeStk0 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk1 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk2 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk3 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk4 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk5 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk6 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk7 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk8 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk9 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk10 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk11 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk12 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk13 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk14 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk15 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk16 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk17 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk18 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeStk19 = New System.Windows.Forms.Label(STACK_AREA) {}
        Me.lblTimeAveHour0 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour1 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour2 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour3 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour4 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour5 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour6 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour7 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour8 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour9 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour10 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour11 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour12 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour13 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour14 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour15 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour16 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour17 = New System.Windows.Forms.Label(24) {}
        Me.lblTimeAveHour18 = New System.Windows.Forms.Label(24) {}
        'Me.timDummy = New System.Windows.Forms.Timer(NameMc.Length) {}

        For j = 0 To 18
            Me.lblTitle(j) = New System.Windows.Forms.Label
            Me.lblTitle(j).Name = "lblTitle" + j.ToString
            Me.lblTitle(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTitle(j).TextAlign = ContentAlignment.MiddleCenter
            Me.lblTitle(j).Size = New Size(WIDTH_MC, HEIGHT_MC)
            Me.lblTitle(j).Font = New Font("ＭＳゴシック", 24, FontStyle.Bold, GraphicsUnit.Pixel)

            AddHandler Me.lblTitle(j).Click, AddressOf Me.lblTitle_Click
        Next j
        For j = 0 To 2
            Me.lblTitle(0 + j * 6).Text = "装置名"
            Me.lblTitle(0 + j * 6).BackColor = Color.LightBlue
            Me.lblTitle(1 + j * 6).Text = "目標"
            Me.lblTitle(1 + j * 6).BackColor = Color.SandyBrown
            Me.lblTitle(2 + j * 6).Text = "平均"
            Me.lblTitle(2 + j * 6).BackColor = Color.Blue
            Me.lblTitle(2 + j * 6).ForeColor = Color.White
            Me.lblTitle(3 + j * 6).Text = "Min"
            Me.lblTitle(3 + j * 6).BackColor = Color.GreenYellow
            Me.lblTitle(4 + j * 6).Text = "現在"
            Me.lblTitle(4 + j * 6).BackColor = Color.LightGreen
            Me.lblTitle(5 + j * 6).Text = "日平均"
            Me.lblTitle(5 + j * 6).BackColor = Color.LightSteelBlue

        Next j
        Me.lblTitle(0).Location = New Point(100 - WIDTH_MC, 55)
        Me.lblTitle(1).Location = New Point(100 - WIDTH_MC, 55 + HEIGHT_MC)
        Me.lblTitle(2).Location = New Point(100 - WIDTH_MC, 55 + HEIGHT_MC * 2)
        Me.lblTitle(3).Location = New Point(100 - WIDTH_MC, 55 + HEIGHT_MC * 3)
        Me.lblTitle(4).Location = New Point(100 - WIDTH_MC, 55 + HEIGHT_MC * 4)
        Me.lblTitle(5).Location = New Point(100 - WIDTH_MC, 55 + HEIGHT_MC * 5)


        Me.lblTitle(6).Location = New Point(100 - WIDTH_MC, 405)
        Me.lblTitle(7).Location = New Point(100 - WIDTH_MC, 405 + HEIGHT_MC)
        Me.lblTitle(8).Location = New Point(100 - WIDTH_MC, 405 + HEIGHT_MC * 2)
        Me.lblTitle(9).Location = New Point(100 - WIDTH_MC, 405 + HEIGHT_MC * 3)
        Me.lblTitle(10).Location = New Point(100 - WIDTH_MC, 405 + HEIGHT_MC * 4)
        Me.lblTitle(11).Location = New Point(100 - WIDTH_MC, 405 + HEIGHT_MC * 5)


        Me.lblTitle(12).Location = New Point(5 * WIDTH_MC + 200, 155)
        Me.lblTitle(13).Location = New Point(5 * WIDTH_MC + 200, 155 + HEIGHT_MC)
        Me.lblTitle(14).Location = New Point(5 * WIDTH_MC + 200, 155 + HEIGHT_MC * 2)
        Me.lblTitle(15).Location = New Point(5 * WIDTH_MC + 200, 155 + HEIGHT_MC * 3)
        Me.lblTitle(16).Location = New Point(5 * WIDTH_MC + 200, 155 + HEIGHT_MC * 4)
        Me.lblTitle(17).Location = New Point(5 * WIDTH_MC + 200, 155 + HEIGHT_MC * 5)


        For j = 0 To NameMc.Length - 2
            counter1(j) = 0
            counter2(j) = 0
            'インスタンス作成
            Me.lblMcNames(j) = New System.Windows.Forms.Label
            Me.lblTimeAim(j) = New System.Windows.Forms.Label
            Me.lblTimeAve(j) = New System.Windows.Forms.Label
            Me.lblTimeMin(j) = New System.Windows.Forms.Label
            Me.lblTimeNow(j) = New System.Windows.Forms.Label
            Me.lblTimeAvx(j) = New System.Windows.Forms.Label


            'プロパティ設定
            Me.lblMcNames(j).Name = "lblMcName" + j.ToString 'NameMc(j)
            Me.lblMcNames(j).Text = NameMc(j)
            Me.lblMcNames(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblMcNames(j).TextAlign = ContentAlignment.MiddleCenter
            Me.lblMcNames(j).Size = New Size(WIDTH_MC, HEIGHT_MC)
            Me.lblMcNames(j).Font = New Font("ＭＳゴシック", 16, FontStyle.Bold, GraphicsUnit.Pixel)
            Me.lblMcNames(j).BackColor = Color.LightBlue

            Me.lblTimeAim(j).Name = "lblTimeAim" + j.ToString 'NameMc(j)
            Me.lblTimeAim(j).Text = "目標" + j.ToString
            Me.lblTimeAim(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAim(j).TextAlign = ContentAlignment.MiddleRight
            Me.lblTimeAim(j).Size = New Size(WIDTH_MC, HEIGHT_MC)
            Me.lblTimeAim(j).Font = New Font("ＭＳゴシック", 28, FontStyle.Bold, GraphicsUnit.Pixel)
            Me.lblTimeAim(j).BackColor = Color.SandyBrown

            Me.lblTimeAve(j).Name = "lblTimeAve" + j.ToString 'NameMc(j)
            Me.lblTimeAve(j).Text = "Ave " + j.ToString
            Me.lblTimeAve(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAve(j).TextAlign = ContentAlignment.MiddleRight
            Me.lblTimeAve(j).Size = New Size(WIDTH_MC, HEIGHT_MC)
            Me.lblTimeAve(j).Font = New Font("ＭＳゴシック", 28, FontStyle.Bold, GraphicsUnit.Pixel)
            Me.lblTimeAve(j).ForeColor = Color.White
            Me.lblTimeAve(j).BackColor = Color.Blue

            Me.lblTimeMin(j).Name = "lblTimeMin" + j.ToString 'NameMc(j)
            Me.lblTimeMin(j).Text = "Min " + j.ToString
            Me.lblTimeMin(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeMin(j).TextAlign = ContentAlignment.MiddleRight
            Me.lblTimeMin(j).Size = New Size(WIDTH_MC, HEIGHT_MC)
            Me.lblTimeMin(j).Font = New Font("ＭＳゴシック", 28, FontStyle.Bold, GraphicsUnit.Pixel)
            Me.lblTimeMin(j).BackColor = Color.GreenYellow

            Me.lblTimeNow(j).Name = "lblTimeNow" + j.ToString 'NameMc(j)
            Me.lblTimeNow(j).Text = "Now " + j.ToString
            Me.lblTimeNow(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeNow(j).TextAlign = ContentAlignment.MiddleRight
            Me.lblTimeNow(j).Size = New Size(WIDTH_MC, HEIGHT_MC)
            Me.lblTimeNow(j).Font = New Font("ＭＳゴシック", 28, FontStyle.Bold, GraphicsUnit.Pixel)
            Me.lblTimeNow(j).BackColor = Color.LightGreen

            Me.lblTimeAvx(j).Name = "lblTimeAvx" + j.ToString 'NameMc(j)
            Me.lblTimeAvx(j).Text = "Day " + j.ToString
            Me.lblTimeAvx(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAvx(j).TextAlign = ContentAlignment.MiddleRight
            Me.lblTimeAvx(j).Size = New Size(WIDTH_MC, HEIGHT_MC)
            Me.lblTimeAvx(j).Font = New Font("ＭＳゴシック", 28, FontStyle.Bold, GraphicsUnit.Pixel)
            Me.lblTimeAvx(j).BackColor = Color.LightSteelBlue



            If j <= 5 Then
                Me.lblMcNames(j).Location = New Point(j * WIDTH_MC + 100, 55)
                Me.lblTimeAim(j).Location = New Point(j * WIDTH_MC + 100, 55 + HEIGHT_MC)
                Me.lblTimeAve(j).Location = New Point(j * WIDTH_MC + 100, 55 + HEIGHT_MC * 2)
                Me.lblTimeMin(j).Location = New Point(j * WIDTH_MC + 100, 55 + HEIGHT_MC * 3)
                Me.lblTimeNow(j).Location = New Point(j * WIDTH_MC + 100, 55 + HEIGHT_MC * 4)
                Me.lblTimeAvx(j).Location = New Point(j * WIDTH_MC + 100, 55 + HEIGHT_MC * 5)

            ElseIf j > 5 And j <= 11 Then
                Me.lblMcNames(j).Location = New Point((j - 6) * WIDTH_MC + 100, 405)
                Me.lblTimeAim(j).Location = New Point((j - 6) * WIDTH_MC + 100, 405 + HEIGHT_MC)
                Me.lblTimeAve(j).Location = New Point((j - 6) * WIDTH_MC + 100, 405 + HEIGHT_MC * 2)
                Me.lblTimeMin(j).Location = New Point((j - 6) * WIDTH_MC + 100, 405 + HEIGHT_MC * 3)
                Me.lblTimeNow(j).Location = New Point((j - 6) * WIDTH_MC + 100, 405 + HEIGHT_MC * 4)
                Me.lblTimeAvx(j).Location = New Point((j - 6) * WIDTH_MC + 100, 405 + HEIGHT_MC * 5)
            Else
                Me.lblMcNames(j).Location = New Point((j - 6) * WIDTH_MC + 200, 155)
                Me.lblTimeAim(j).Location = New Point((j - 6) * WIDTH_MC + 200, 155 + HEIGHT_MC)
                Me.lblTimeAve(j).Location = New Point((j - 6) * WIDTH_MC + 200, 155 + HEIGHT_MC * 2)
                Me.lblTimeMin(j).Location = New Point((j - 6) * WIDTH_MC + 200, 155 + HEIGHT_MC * 3)
                Me.lblTimeNow(j).Location = New Point((j - 6) * WIDTH_MC + 200, 155 + HEIGHT_MC * 4)
                Me.lblTimeAvx(j).Location = New Point((j - 6) * WIDTH_MC + 200, 155 + HEIGHT_MC * 5)
            End If
            'イベントハンドラに関連付け
            AddHandler Me.lblMcNames(j).Click, AddressOf Me.lblMcNames_Click
            AddHandler Me.lblTimeAim(j).Click, AddressOf Me.lblTimeAim_Click
            AddHandler Me.lblTimeAve(j).Click, AddressOf Me.lblTimeAve_Click
            AddHandler Me.lblTimeMin(j).Click, AddressOf Me.lblTimeMin_Click
            AddHandler Me.lblTimeNow(j).Click, AddressOf Me.lblTimeNow_Click
        Next

        For j = 0 To STACK_AREA - 1
            'インスタンス作成
            Me.lblTimeStk0(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk0(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk0(j).Text = "" + j.ToString
            Me.lblTimeStk0(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk0(j).Size = New Size(40, 20)
            Me.lblTimeStk0(j).Location = New Point(40 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk0(j).Click, AddressOf Me.lblTimeStk0_Click

            'インスタンス作成
            Me.lblTimeStk1(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk1(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk1(j).Text = "" + j.ToString
            Me.lblTimeStk1(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk1(j).Size = New Size(40, 20)
            Me.lblTimeStk1(j).Location = New Point(80 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk1(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeStk2(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk2(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk2(j).Text = "" + j.ToString
            Me.lblTimeStk2(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk2(j).Size = New Size(40, 20)
            Me.lblTimeStk2(j).Location = New Point(120 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk2(j).Click, AddressOf Me.lblTimeStk2_Click

            'インスタンス作成
            Me.lblTimeStk3(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk3(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk3(j).Text = "" + j.ToString
            Me.lblTimeStk3(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk3(j).Size = New Size(40, 20)
            Me.lblTimeStk3(j).Location = New Point(160 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk3(j).Click, AddressOf Me.lblTimeStk3_Click

            'インスタンス作成
            Me.lblTimeStk4(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk4(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk4(j).Text = "" + j.ToString
            Me.lblTimeStk4(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk4(j).Size = New Size(40, 20)
            Me.lblTimeStk4(j).Location = New Point(200 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk4(j).Click, AddressOf Me.lblTimeStk4_Click

            'インスタンス作成
            Me.lblTimeStk5(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk5(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk5(j).Text = "" + j.ToString
            Me.lblTimeStk5(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk5(j).Size = New Size(40, 20)
            Me.lblTimeStk5(j).Location = New Point(240 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk5(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeStk6(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk6(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk6(j).Text = "" + j.ToString
            Me.lblTimeStk6(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk6(j).Size = New Size(40, 20)
            Me.lblTimeStk6(j).Location = New Point(280 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk6(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeStk7(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk7(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk7(j).Text = "" + j.ToString
            Me.lblTimeStk7(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk7(j).Size = New Size(40, 20)
            Me.lblTimeStk7(j).Location = New Point(320 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk7(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeStk8(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk8(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk8(j).Text = "" + j.ToString
            Me.lblTimeStk8(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk8(j).Size = New Size(40, 20)
            Me.lblTimeStk8(j).Location = New Point(360 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk8(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeStk9(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk9(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk9(j).Text = "" + j.ToString
            Me.lblTimeStk9(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk9(j).Size = New Size(40, 20)
            Me.lblTimeStk9(j).Location = New Point(400 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk9(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeStk10(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk10(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk10(j).Text = "" + j.ToString
            Me.lblTimeStk10(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk10(j).Size = New Size(40, 20)
            Me.lblTimeStk10(j).Location = New Point(440 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk10(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeStk11(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk11(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk11(j).Text = "" + j.ToString
            Me.lblTimeStk11(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk11(j).Size = New Size(40, 20)
            Me.lblTimeStk11(j).Location = New Point(480 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk11(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeStk12(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk12(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk12(j).Text = "" + j.ToString
            Me.lblTimeStk12(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk12(j).Size = New Size(40, 20)
            Me.lblTimeStk12(j).Location = New Point(520 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk12(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeStk13(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk13(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk13(j).Text = "" + j.ToString
            Me.lblTimeStk13(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk13(j).Size = New Size(40, 20)
            Me.lblTimeStk13(j).Location = New Point(560 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk13(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeStk14(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk14(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk14(j).Text = "" + j.ToString
            Me.lblTimeStk14(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk14(j).Size = New Size(40, 20)
            Me.lblTimeStk14(j).Location = New Point(600 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk14(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeStk15(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk15(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk15(j).Text = "" + j.ToString
            Me.lblTimeStk15(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk15(j).Size = New Size(40, 20)
            Me.lblTimeStk15(j).Location = New Point(640 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk15(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeStk16(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk16(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk16(j).Text = "" + j.ToString
            Me.lblTimeStk16(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk16(j).Size = New Size(40, 20)
            Me.lblTimeStk16(j).Location = New Point(680 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk16(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeStk17(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk17(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk17(j).Text = "" + j.ToString
            Me.lblTimeStk17(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk17(j).Size = New Size(40, 20)
            Me.lblTimeStk17(j).Location = New Point(720 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk17(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeStk18(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeStk18(j).Name = "lblTimeStk" + j.ToString 'NameMc(j)
            Me.lblTimeStk18(j).Text = "" + j.ToString
            Me.lblTimeStk18(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeStk18(j).Size = New Size(40, 20)
            Me.lblTimeStk18(j).Location = New Point(760 + 800, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk18(j).Click, AddressOf Me.lblTimeStk1_Click
        Next


        For j = 0 To 23
            'インスタンス作成
            Me.lblTimeAveHour0(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour0(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour0(j).Text = "" + j.ToString
            Me.lblTimeAveHour0(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour0(j).Size = New Size(40, 20)
            Me.lblTimeAveHour0(j).Location = New Point(40 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk0(j).Click, AddressOf Me.lblTimeStk0_Click

            'インスタンス作成
            Me.lblTimeAveHour1(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour1(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour1(j).Text = "" + j.ToString
            Me.lblTimeAveHour1(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour1(j).Size = New Size(40, 20)
            Me.lblTimeAveHour1(j).Location = New Point(80 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk1(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeAveHour2(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour2(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour2(j).Text = "" + j.ToString
            Me.lblTimeAveHour2(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour2(j).Size = New Size(40, 20)
            Me.lblTimeAveHour2(j).Location = New Point(120 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk2(j).Click, AddressOf Me.lblTimeStk2_Click

            'インスタンス作成
            Me.lblTimeAveHour3(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour3(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour3(j).Text = "" + j.ToString
            Me.lblTimeAveHour3(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour3(j).Size = New Size(40, 20)
            Me.lblTimeAveHour3(j).Location = New Point(160 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk3(j).Click, AddressOf Me.lblTimeStk3_Click

            'インスタンス作成
            Me.lblTimeAveHour4(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour4(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour4(j).Text = "" + j.ToString
            Me.lblTimeAveHour4(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour4(j).Size = New Size(40, 20)
            Me.lblTimeAveHour4(j).Location = New Point(200 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk4(j).Click, AddressOf Me.lblTimeStk4_Click

            'インスタンス作成
            Me.lblTimeAveHour5(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour5(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour5(j).Text = "" + j.ToString
            Me.lblTimeAveHour5(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour5(j).Size = New Size(40, 20)
            Me.lblTimeAveHour5(j).Location = New Point(240 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk5(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeAveHour6(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour6(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour6(j).Text = "" + j.ToString
            Me.lblTimeAveHour6(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour6(j).Size = New Size(40, 20)
            Me.lblTimeAveHour6(j).Location = New Point(280 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk6(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeAveHour7(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour7(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour7(j).Text = "" + j.ToString
            Me.lblTimeAveHour7(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour7(j).Size = New Size(40, 20)
            Me.lblTimeAveHour7(j).Location = New Point(320 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk7(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeAveHour8(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour8(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour8(j).Text = "" + j.ToString
            Me.lblTimeAveHour8(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour8(j).Size = New Size(40, 20)
            Me.lblTimeAveHour8(j).Location = New Point(360 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk8(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeAveHour9(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour9(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour9(j).Text = "" + j.ToString
            Me.lblTimeAveHour9(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour9(j).Size = New Size(40, 20)
            Me.lblTimeAveHour9(j).Location = New Point(400 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk9(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeAveHour10(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour10(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour10(j).Text = "" + j.ToString
            Me.lblTimeAveHour10(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour10(j).Size = New Size(40, 20)
            Me.lblTimeAveHour10(j).Location = New Point(440 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk10(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeAveHour11(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour11(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour11(j).Text = "" + j.ToString
            Me.lblTimeAveHour11(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour11(j).Size = New Size(40, 20)
            Me.lblTimeAveHour11(j).Location = New Point(480 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk11(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeAveHour12(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour12(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour12(j).Text = "" + j.ToString
            Me.lblTimeAveHour12(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour12(j).Size = New Size(40, 20)
            Me.lblTimeAveHour12(j).Location = New Point(520 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk12(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeAveHour13(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour13(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour13(j).Text = "" + j.ToString
            Me.lblTimeAveHour13(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour13(j).Size = New Size(40, 20)
            Me.lblTimeAveHour13(j).Location = New Point(560 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk13(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeAveHour14(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour14(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour14(j).Text = "" + j.ToString
            Me.lblTimeAveHour14(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour14(j).Size = New Size(40, 20)
            Me.lblTimeAveHour14(j).Location = New Point(600 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk14(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeAveHour15(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour15(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour15(j).Text = "" + j.ToString
            Me.lblTimeAveHour15(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour15(j).Size = New Size(40, 20)
            Me.lblTimeAveHour15(j).Location = New Point(640 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk15(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeAveHour16(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour16(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour16(j).Text = "" + j.ToString
            Me.lblTimeAveHour16(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour16(j).Size = New Size(40, 20)
            Me.lblTimeAveHour16(j).Location = New Point(680 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk16(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeAveHour17(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour17(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour17(j).Text = "" + j.ToString
            Me.lblTimeAveHour17(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour17(j).Size = New Size(40, 20)
            Me.lblTimeAveHour17(j).Location = New Point(720 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk17(j).Click, AddressOf Me.lblTimeStk1_Click

            'インスタンス作成
            Me.lblTimeAveHour18(j) = New System.Windows.Forms.Label
            'プロパティ設定
            Me.lblTimeAveHour18(j).Name = "lblTimeAveHour" + j.ToString 'NameMc(j)
            Me.lblTimeAveHour18(j).Text = "" + j.ToString
            Me.lblTimeAveHour18(j).BorderStyle = BorderStyle.FixedSingle
            Me.lblTimeAveHour18(j).Size = New Size(40, 20)
            Me.lblTimeAveHour18(j).Location = New Point(760 + 1600, 750 + 20 * j)
            'イベントハンドラに関連付け
            'AddHandler Me.lblTimeStk18(j).Click, AddressOf Me.lblTimeStk1_Click
        Next

        Me.Controls.AddRange(Me.lblTitle)
        Me.Controls.AddRange(Me.lblMcNames)
        Me.Controls.AddRange(Me.lblTimeAim)
        Me.Controls.AddRange(Me.lblTimeAve)
        Me.Controls.AddRange(Me.lblTimeMin)
        Me.Controls.AddRange(Me.lblTimeNow)
        Me.Controls.AddRange(Me.lblTimeAvx)
        Me.Controls.AddRange(Me.lblTimeStk0)
        Me.Controls.AddRange(Me.lblTimeStk1)
        Me.Controls.AddRange(Me.lblTimeStk2)
        Me.Controls.AddRange(Me.lblTimeStk3)
        Me.Controls.AddRange(Me.lblTimeStk4)
        Me.Controls.AddRange(Me.lblTimeStk5)
        Me.Controls.AddRange(Me.lblTimeStk6)
        Me.Controls.AddRange(Me.lblTimeStk7)
        Me.Controls.AddRange(Me.lblTimeStk8)
        Me.Controls.AddRange(Me.lblTimeStk9)
        Me.Controls.AddRange(Me.lblTimeStk10)
        Me.Controls.AddRange(Me.lblTimeStk11)
        Me.Controls.AddRange(Me.lblTimeStk12)
        Me.Controls.AddRange(Me.lblTimeStk13)
        Me.Controls.AddRange(Me.lblTimeStk14)
        Me.Controls.AddRange(Me.lblTimeStk15)
        Me.Controls.AddRange(Me.lblTimeStk16)
        Me.Controls.AddRange(Me.lblTimeStk17)
        Me.Controls.AddRange(Me.lblTimeStk18)
        Me.Controls.AddRange(Me.lblTimeAveHour0)
        Me.Controls.AddRange(Me.lblTimeAveHour1)
        Me.Controls.AddRange(Me.lblTimeAveHour2)
        Me.Controls.AddRange(Me.lblTimeAveHour3)
        Me.Controls.AddRange(Me.lblTimeAveHour4)
        Me.Controls.AddRange(Me.lblTimeAveHour5)
        Me.Controls.AddRange(Me.lblTimeAveHour6)
        Me.Controls.AddRange(Me.lblTimeAveHour7)
        Me.Controls.AddRange(Me.lblTimeAveHour8)
        Me.Controls.AddRange(Me.lblTimeAveHour9)
        Me.Controls.AddRange(Me.lblTimeAveHour10)
        Me.Controls.AddRange(Me.lblTimeAveHour11)
        Me.Controls.AddRange(Me.lblTimeAveHour12)
        Me.Controls.AddRange(Me.lblTimeAveHour13)
        Me.Controls.AddRange(Me.lblTimeAveHour14)
        Me.Controls.AddRange(Me.lblTimeAveHour15)
        Me.Controls.AddRange(Me.lblTimeAveHour16)
        Me.Controls.AddRange(Me.lblTimeAveHour17)
        Me.Controls.AddRange(Me.lblTimeAveHour18)
        Me.ResumeLayout(False)

        'ボタンコントロール配列の作成（５個）
        'Me.testButtons = New System.Windows.Forms.Button(4) {}
        'ボタンコントロールのインスタンスを作成し、プロパティを設定する
        'Me.SuspendLayout()
        'Dim i As Integer
        'For i = 0 To Me.testButtons.Length - 1
        'インスタンス作成
        'Me.testButtons(i) = New System.Windows.Forms.Button
        'プロパティ設定
        'Me.testButtons(i).Name = "Button" + i.ToString()
        'Me.testButtons(i).Text = i.ToString()
        'Me.testButtons(i).Size = New Size(30, 30)
        'Me.testButtons(i).Location = New Point(i * 30, 10)
        'イベントハンドラに関連付け
        'AddHandler Me.testButtons(i).Click, AddressOf Me.testButtons_Click
        'Next
        'フォームにコントロールを追加
        'Me.Controls.AddRange(Me.testButtons)
        'Me.ResumeLayout(False)

        'Hist()
    End Sub

    Private Sub LoadParameta()
        Dim cReader As New System.IO.StreamReader("c:\takto\Parameta.txt", System.Text.Encoding.Default)
        Dim stResult As String = String.Empty
        While (cReader.Peek() >= 0)
            Dim stBuffer1 As String = cReader.ReadLine()
            Dim z1() As String = Split(stBuffer1, ",")
            For i As Integer = 0 To z1.Length - 1
                ParaAim(i) = CSng(Val(z1(i)))
            Next

            Dim stBuffer2 As String = cReader.ReadLine()
            Dim z2() As String = Split(stBuffer2, ",")
            For i As Integer = 0 To z2.Length - 1
                ParaMulti(i) = CSng(Val(z2(i)))
            Next
            'stResult &= stBuffer & System.Environment.NewLine
            stResult = stBuffer1 + "*" + stBuffer2
        End While
        cReader.Close()
    End Sub

    Private Sub SaveParameta()
        Dim cWriter As New System.IO.StreamWriter("c:\takto\Parameta.txt", False, System.Text.Encoding.Default)
        Dim stResult1 As String = String.Empty
        Dim stResult2 As String = String.Empty
        stResult1 = str(ParaAim(0))
        For i As Integer = 1 To ParaAim.length - 1
            stResult1 += "," + str(ParaAim(i))
        Next
        stResult2 = str(ParaMulti(0))
        For i As Integer = 1 To ParaMulti.length - 1
            stResult2 += "," + str(ParaMulti(i))
        Next
        cWriter.WriteLine(stResult1)
        cWriter.WriteLine(stResult2)
        cWriter.close()
    End Sub

    Private Sub Hist()
        If stk(0).Count < 30 Then Exit Sub
        Dim arry = stk(0).ToArray

        Dim His(20) As Integer
        For i As Integer = 0 To 29
            For j As Integer = 1 To 20
                If 1.1 + 0.01 * j > Single.Parse(arry(i)) Then
                    His(j) += 1
                    Exit For
                ElseIf j = 20 Then
                    His(j) += 1
                End If
            Next
        Next
        PicHist.Width = 400
        PicHist.Height = 240
        Dim currentContext As BufferedGraphicsContext
        Dim myBuffer As BufferedGraphics
        currentContext = BufferedGraphicsManager.Current
        myBuffer = currentContext.Allocate(PicHist.CreateGraphics(), New Rectangle(0, 0, 603, 295))
        Dim g As Graphics = myBuffer.Graphics

        Dim Rx As Single = CSng(PicHist.Size.Width / 1000)
        Dim Ry As Single = CSng(PicHist.Size.Height / 1000)
        Dim Pen1 As Pen = New Pen(Color.Black, 1)
        g.Clear(Color.White)
        '外枠
        g.DrawRectangle(Pen1, 0, 0, Rx * 1000 - 1, Ry * 1000 - 1)
        g.DrawRectangle(Pen1, Rx * 100, Ry * 100, Rx * 800, Ry * 800)
        'X軸分割線
        For i As Integer = 1 To 19
            Dim x1 As Single = Rx * 100 + (Rx * 800 / 20) * i
            g.DrawLine(Pen1, x1, Ry * 870, x1, Ry * 900)
        Next
        'X軸分割数値を表示
        Dim ds As String = "abc"
        Dim df As New Font("ＭＳゴシック", 8)
        Dim db As New SolidBrush(Color.Black)
        g.DrawString("1.10", df, db, Rx * 70, Ry * 910)
        g.DrawString("1.15", df, db, Rx * 270, Ry * 910)
        g.DrawString("1.20", df, db, Rx * 470, Ry * 910)
        g.DrawString("1.25", df, db, Rx * 670, Ry * 910)
        g.DrawString("1.30", df, db, Rx * 870, Ry * 910)
        g.DrawString("[Sec]", df, db, Rx * 925, Ry * 935)
        '一番大きいセグメントを調べる
        Dim HistMax As Integer = His.Max()
        'Y軸の分割数を決める
        Dim tmp0 As Single = 0
        Dim tmp1 As Single = 0
        Dim tmp2 As Single = 0
        tmp1 = CSng(HistMax / 30)
        Dim YaxisUpper As Integer = 0
        If tmp1 > 0.8 Then
            YaxisUpper = 100
        ElseIf tmp1 > 0.6 Then
            YaxisUpper = 80
        ElseIf tmp1 > 0.4 Then
            YaxisUpper = 60
        ElseIf tmp1 > 0.2 Then
            YaxisUpper = 40
        Else
            YaxisUpper = 20
        End If
        g.DrawLine(Pen1, Rx * 100, Ry * 300, Rx * 900, Ry * 300)
        g.DrawLine(Pen1, Rx * 100, Ry * 500, Rx * 900, Ry * 500)
        g.DrawLine(Pen1, Rx * 100, Ry * 700, Rx * 900, Ry * 700)
        If YaxisUpper > 90 Then
            g.DrawString(Str(YaxisUpper), df, db, Rx * 32, Ry * 90)
        Else
            g.DrawString(Str(YaxisUpper), df, db, Rx * 50, Ry * 90)
        End If
        g.DrawString("[%]", df, db, Rx * 55, Ry * 35)
        For i As Integer = 1 To 3
            Dim tmp3 As Single = CSng((YaxisUpper / 4) * i)
            If tmp3 < 10 Then
                g.DrawString(Str(Math.Abs(tmp3)), df, db, Rx * 72, Ry * (800 - 200 * i))
            Else
                g.DrawString(Str(Math.Abs(tmp3)), df, db, Rx * 50, Ry * (800 - 200 * i))
            End If
        Next
        'ヒストデータ表示
        Dim Digit As Single = CSng(HistMax * (YaxisUpper / 100))
        Dim PenH As Pen = New Pen(Color.DarkOliveGreen, Rx * 30)
        For i As Integer = 1 To 20
            Dim a1 As Integer = His(i)
            If His(i) > 0 Then
                Dim x1 As Single = CSng(100 + (800 / 20) * (i - 1) + 10)
                g.DrawLine(PenH, Rx * x1, Ry * 899, Rx * x1, Ry * (800 - (His(i) / Digit) * 800 + 99))
            End If
        Next

        myBuffer.Render()
    End Sub

    'ラベルのクリックイベントハンドラ
    Private Sub lblTitle_Click(ByVal sender As Object, ByVal e As EventArgs)
        'クリックされたボタンのnameを表示する
        MessageBox.Show(CType(sender, Label).Name)
    End Sub

    Private Sub lblMcNames_Click(ByVal sender As Object, ByVal e As EventArgs)
        'クリックされたボタンのnameを表示する
        'MessageBox.Show(CType(sender, Label).Name)
        Dim zx As String = InputBox(“入力してください。”, “タイトル”, “”, 0, 0)
        MsgBox(zx)
    End Sub

    Private Sub lblTimeAim_Click(ByVal sender As Object, ByVal e As EventArgs)
        'クリックされたボタンのnameを表示する
        'MessageBox.Show(CType(sender, System.Windows.Forms.Label).Name)
        Dim ax As String = Val(Replace(CType(sender, System.Windows.Forms.Label).Name, "lblTimeAim", ""))
        'MessageBox.Show(ax)
        Dim tmp As String = InputBox(“目標値を入力してください。”, "目標値設定", ParaAim(ax), 0, 0)
        ParaAim(ax) = Val(tmp)
        SaveParameta()
    End Sub

    Private Sub lblTimeAve_Click(ByVal sender As Object, ByVal e As EventArgs)
        'クリックされたボタンのnameを表示する
        'MessageBox.Show(CType(sender, System.Windows.Forms.Label).Name)
        Dim ax As String = Val(Replace(CType(sender, System.Windows.Forms.Label).Name, "lblTimeAve", ""))
        Dim tmp As String = InputBox(“取れ数を入力してください。”, "取れ数設定", ParaMulti(ax), 0, 0)
        ParaMulti(ax) = Val(tmp)
    End Sub
    '
    Private Sub lblTimeMin_Click(ByVal sender As Object, ByVal e As EventArgs)
        'クリックされたボタンのnameを表示する
        MessageBox.Show(CType(sender, System.Windows.Forms.Label).Name)
    End Sub

    Private Sub lblTimeNow_Click(ByVal sender As Object, ByVal e As EventArgs)
        'クリックされたボタンのnameを表示する
        MessageBox.Show(CType(sender, System.Windows.Forms.Label).Name)
    End Sub
    '
    Private Sub lblTimeStk0_Click(ByVal sender As Object, ByVal e As EventArgs)
        'クリックされたボタンのnameを表示する
        MessageBox.Show(CType(sender, System.Windows.Forms.Label).Name)
    End Sub
    '
    Private Sub lblTimeStk1_Click(ByVal sender As Object, ByVal e As EventArgs)
        'クリックされたボタンのnameを表示する
        MessageBox.Show(CType(sender, System.Windows.Forms.Label).Name)
    End Sub

    Private Sub testButtons_Click(ByVal sender As Object, ByVal e As EventArgs)
        'クリックされたボタンのnameを表示する
        MessageBox.Show(CType(sender, System.Windows.Forms.Button).Name)
    End Sub

    Public Function DummyTimer(TimerNo As Integer) As String
        counter1(TimerNo) = Environment.TickCount
        Dim i1 As Long = (counter1(TimerNo) - counter2(TimerNo)) / ParaMulti(TimerNo)
        Dim i2 As Long = r.Next(-100, 100)
        Dim countDelta As String = (i1 + i2).ToString
        Dim countLength As Integer = Len(countDelta)
        If countLength = 3 Then
            DummyTimer = "0." + countDelta
        ElseIf countLength = 4 Then
            DummyTimer = countDelta.Insert(1, ".")
        Else
            DummyTimer = "-.---"
        End If
        counter2(TimerNo) = counter1(TimerNo)
    End Function

    Public Sub StackIn(n As Integer)
        Dim TimeX As String = DummyTimer(n)
        Dim Ave As Single = 0
        Dim x(20) As Single
        For i As Integer = 0 To ParaAim.Length - 1
            x(i) = ParaAim(i)
        Next


        lblTimeAim(n).Text = (Math.Round(ParaAim(n), 2)).ToString("0.00")
        lblTimeNow(n).Text = (Math.Round(Val(TimeX), 2)).ToString("0.00")
        If TimeX = "-.---" Then Exit Sub

        Dim sx As New Stack(Of String)()
        sx.Clear()
        stk(n).Push(TimeX)
        If stk(n).Count >= STACK_AREA + 1 Then
            For i As Integer = 1 To STACK_AREA
                sx.Push(stk(n).Pop())
            Next
            stk(n).Clear()
            For i As Integer = 1 To STACK_AREA
                stk(n).Push(sx.Pop())
            Next
        End If

        Dim values(29) As Single
        Dim j As Integer = 0
        For Each item As String In stk(n)
            Select Case n
                Case 0
                    lblTimeStk0(j).Text = item
                Case 1
                    lblTimeStk1(j).Text = item
                Case 2
                    lblTimeStk2(j).Text = item
                Case 3
                    lblTimeStk3(j).Text = item
                Case 4
                    lblTimeStk4(j).Text = item
                Case 5
                    lblTimeStk5(j).Text = item
                Case 6
                    lblTimeStk6(j).Text = item
                Case 7
                    lblTimeStk7(j).Text = item
                Case 8
                    lblTimeStk8(j).Text = item
                Case 9
                    lblTimeStk9(j).Text = item
                Case 10
                    lblTimeStk10(j).Text = item
                Case 11
                    lblTimeStk11(j).Text = item
                Case 12
                    lblTimeStk12(j).Text = item
                Case 13
                    lblTimeStk13(j).Text = item
                Case 14
                    lblTimeStk14(j).Text = item
                Case 15
                    lblTimeStk15(j).Text = item
                Case 16
                    lblTimeStk16(j).Text = item
                Case 17
                    lblTimeStk17(j).Text = item
                Case 18
                    lblTimeStk18(j).Text = item
                Case Else
                    lblTimeStk19(j).Text = item
            End Select

            values(j) = Val(item)
            j += 1
            Ave += Val(item)
        Next

        Dim tmpAve As Single = values.Average()
        Dim tmpAim As Single = Single.Parse(lblTimeAim(n).Text)
        If stk(n).Count = STACK_AREA Then
            If tmpAve < (tmpAim * 1.1) Then
                lblTimeAve(n).BackColor = Color.Blue
                lblTimeAve(n).ForeColor = Color.White
            ElseIf tmpAve < (tmpAim * 1.2) Then
                lblTimeAve(n).BackColor = Color.Yellow
                lblTimeAve(n).ForeColor = Color.Red
            Else
                lblTimeAve(n).BackColor = Color.Red
                lblTimeAve(n).ForeColor = Color.Yellow
            End If
            lblTimeAve(n).Text = (Math.Round(values.Average(), 2)).ToString("0.00")
            AveGrobal(n) = Math.Round(values.Average(), 2)
            'lblTimeAvx(n).Text = AveGrobal(n)
            lblTimeMin(n).Text = (Math.Round(values.Min(), 2)).ToString("0.00")
        End If

    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        If DebugMode Then StackIn(0)
    End Sub

    Private Sub Timer2_Tick(sender As Object, e As EventArgs) Handles Timer2.Tick
        If DebugMode Then StackIn(1)
    End Sub

    Private Sub Timer3_Tick(sender As Object, e As EventArgs) Handles Timer3.Tick
        If DebugMode Then StackIn(2)
    End Sub

    Private Sub Timer4_Tick(sender As Object, e As EventArgs) Handles Timer4.Tick
        If DebugMode Then StackIn(3)
    End Sub

    Private Sub Timer5_Tick(sender As Object, e As EventArgs) Handles Timer5.Tick
        If DebugMode Then StackIn(4)
    End Sub

    Private Sub Timer6_Tick(sender As Object, e As EventArgs) Handles Timer6.Tick
        If DebugMode Then StackIn(5)
    End Sub

    Private Sub Timer7_Tick(sender As Object, e As EventArgs) Handles Timer7.Tick
        If DebugMode Then StackIn(6)
    End Sub

    Private Sub Timer8_Tick(sender As Object, e As EventArgs) Handles Timer8.Tick
        If DebugMode Then StackIn(7)
    End Sub

    Private Sub Timer9_Tick(sender As Object, e As EventArgs) Handles Timer9.Tick
        If DebugMode Then StackIn(8)
    End Sub

    Private Sub Timer10_Tick(sender As Object, e As EventArgs) Handles Timer10.Tick
        If DebugMode Then StackIn(9)
    End Sub

    Private Sub Timer11_Tick(sender As Object, e As EventArgs) Handles Timer11.Tick
        If DebugMode Then StackIn(10)
    End Sub

    Private Sub Timer12_Tick(sender As Object, e As EventArgs) Handles Timer12.Tick
        If DebugMode Then StackIn(11)
    End Sub

    Private Sub Timer13_Tick(sender As Object, e As EventArgs) Handles Timer13.Tick
        If DebugMode Then StackIn(12)
    End Sub

    Private Sub Timer14_Tick(sender As Object, e As EventArgs) Handles Timer14.Tick
        If DebugMode Then StackIn(13)
    End Sub

    Private Sub Timer15_Tick(sender As Object, e As EventArgs) Handles Timer15.Tick
        If DebugMode Then StackIn(14)
    End Sub

    Private Sub Timer16_Tick(sender As Object, e As EventArgs) Handles Timer16.Tick
        If DebugMode Then StackIn(15)
    End Sub

    Private Sub Timer17_Tick(sender As Object, e As EventArgs) Handles Timer17.Tick
        If DebugMode Then StackIn(16)
    End Sub

    Private Sub Timer18_Tick(sender As Object, e As EventArgs) Handles Timer18.Tick
        If DebugMode Then StackIn(17)
    End Sub

    Private Sub Timer19_Tick(sender As Object, e As EventArgs) Handles Timer18.Tick
        'If DebugMode Then StackIn(18)
    End Sub

    Private Sub Timer19_Tick_1(sender As Object, e As EventArgs) Handles Timer19.Tick
        Timer1.Enabled = True
        Timer2.Enabled = True
        Timer3.Enabled = True
        Timer4.Enabled = True
        Timer5.Enabled = True
        Timer6.Enabled = True
        Timer7.Enabled = True
        Timer8.Enabled = True
        Timer9.Enabled = True
        Timer10.Enabled = True
        Timer11.Enabled = True
        Timer12.Enabled = True
        Timer13.Enabled = True
        Timer14.Enabled = True
        Timer15.Enabled = True
        Timer16.Enabled = True
        Timer17.Enabled = True
        Timer18.Enabled = True
        Timer20.Enabled = True
        ContecDioInit()
        ContecTriggerStart()
    End Sub

    Private Sub Timer20_Tick(sender As Object, e As EventArgs) Handles Timer20.Tick
        Dim dtNow As DateTime = DateTime.Now
        Dim NowSec As Integer
        Dim NowMin As Integer

        NowMin = dtNow.Minute
        NowSec = dtNow.Second

        'NowMin = 59

        If NowMin = 59 And NowSec = 59 Then
            System.Threading.Thread.Sleep(2000)

            '平均値をスタックエリアにＰＵＳＨ
            For i As Integer = 0 To 19
                Dim sx As New Stack(Of String)()
                sx.Clear()
                aveHour(i).Push(AveGrobal(i))
                If aveHour(i).Count >= 24 Then
                    For j As Integer = 1 To 24
                        sx.Push(aveHour(i).Pop())
                    Next
                    aveHour(i).Clear()
                    For j As Integer = 1 To 24
                        aveHour(i).Push(sx.Pop())
                    Next
                End If
            Next i


            'デバッグ用スタック内容表示
            For n As Integer = 0 To 19
                Dim values(29) As Single
                Dim j As Integer = 0
                For Each item As String In aveHour(n)
                    Select Case n
                        Case 0
                            lblTimeAveHour0(j).Text = item
                        Case 1
                            lblTimeAveHour1(j).Text = item
                        Case 2
                            lblTimeAveHour2(j).Text = item
                        Case 3
                            lblTimeAveHour3(j).Text = item
                        Case 4
                            lblTimeAveHour4(j).Text = item
                        Case 5
                            lblTimeAveHour5(j).Text = item
                        Case 6
                            lblTimeAveHour6(j).Text = item
                        Case 7
                            lblTimeAveHour7(j).Text = item
                        Case 8
                            lblTimeAveHour8(j).Text = item
                        Case 9
                            lblTimeAveHour9(j).Text = item
                        Case 10
                            lblTimeAveHour10(j).Text = item
                        Case 11
                            lblTimeAveHour11(j).Text = item
                        Case 12
                            lblTimeAveHour12(j).Text = item
                        Case 13
                            lblTimeAveHour13(j).Text = item
                        Case 14
                            lblTimeAveHour14(j).Text = item
                        Case 15
                            lblTimeAveHour15(j).Text = item
                        Case 16
                            lblTimeAveHour16(j).Text = item
                        Case 17
                            lblTimeAveHour17(j).Text = item
                        Case Else
                            lblTimeAveHour18(j).Text = item
                    End Select
                    j += 1
                Next
            Next n

            '一日平均表示
            For i As Integer = 0 To 17
                Dim tmp0 As Single = 0
                Dim tmp1 As Single = Single.Parse(lblTimeAim(i).Text)
                Dim tmp2 As Single = 0
                Dim Count As Integer = 0
                For Each item As String In aveHour(i)
                    tmp0 = Single.Parse(item)
                    If tmp0 > (tmp1 * 0.8) And tmp0 < (tmp1 * 1.2) Then
                        tmp2 += tmp0
                        Count += 1
                    End If
                Next
                If Count > 0 Then
                    lblTimeAvx(i).Text = (Math.Round((tmp2 / Count), 2)).ToString("0.00")
                End If
            Next i
        End If
    End Sub

End Class
