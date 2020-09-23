VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H80000009&
   ClientHeight    =   11160
   ClientLeft      =   165
   ClientTop       =   165
   ClientWidth     =   15240
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   MousePointer    =   99  'Custom
   ScaleHeight     =   11160
   ScaleWidth      =   15240
   StartUpPosition =   3  'Windows Default
   WindowState     =   2  'Maximized
   Begin VB.Timer Timer25 
      Interval        =   100
      Left            =   6840
      Top             =   3120
   End
   Begin VB.Timer Timer24 
      Enabled         =   0   'False
      Interval        =   100
      Left            =   7080
      Top             =   5400
   End
   Begin VB.Timer Timer23 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   0
      Top             =   3600
   End
   Begin VB.Timer Timer22 
      Enabled         =   0   'False
      Interval        =   100
      Left            =   14520
      Top             =   6960
   End
   Begin VB.Timer Timer21 
      Enabled         =   0   'False
      Interval        =   100
      Left            =   14880
      Top             =   6960
   End
   Begin VB.Timer tmrball 
      Interval        =   50
      Left            =   5
      Top             =   3240
   End
   Begin VB.Timer Timer20 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer19 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer18 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer17 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer16 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer15 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer14 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer13 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer12 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer11 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer10 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer9 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer8 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer7 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer6 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer5 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer4 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer3 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer2 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Timer Timer1 
      Interval        =   50
      Left            =   600
      Top             =   0
   End
   Begin VB.Shape Shape26 
      Height          =   495
      Left            =   0
      Shape           =   3  'Circle
      Top             =   2760
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Shape Shape25 
      FillStyle       =   0  'Solid
      Height          =   1215
      Left            =   2880
      Top             =   5880
      Visible         =   0   'False
      Width           =   1695
   End
   Begin VB.Line Line4 
      BorderWidth     =   10
      Visible         =   0   'False
      X1              =   2640
      X2              =   4800
      Y1              =   7080
      Y2              =   7080
   End
   Begin VB.Shape Shape24 
      FillColor       =   &H000080FF&
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   3720
      Shape           =   3  'Circle
      Top             =   10920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Line Line3 
      BorderWidth     =   5
      Visible         =   0   'False
      X1              =   3600
      X2              =   4080
      Y1              =   10920
      Y2              =   10920
   End
   Begin VB.Shape Shape23 
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   3120
      Shape           =   3  'Circle
      Top             =   10920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape Shape22 
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   4080
      Shape           =   3  'Circle
      Top             =   10920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape Shape21 
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   3360
      Shape           =   3  'Circle
      Top             =   10920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape Shape20 
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   3600
      Shape           =   3  'Circle
      Top             =   10920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape Shape19 
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   3960
      Shape           =   3  'Circle
      Top             =   10920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape Shape18 
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   4320
      Shape           =   3  'Circle
      Top             =   10920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Line Line2 
      BorderWidth     =   10
      Visible         =   0   'False
      X1              =   2640
      X2              =   4800
      Y1              =   5760
      Y2              =   5760
   End
   Begin VB.Shape Shape17 
      FillStyle       =   0  'Solid
      Height          =   1215
      Left            =   2880
      Top             =   4560
      Visible         =   0   'False
      Width           =   1695
   End
   Begin VB.Line Line1 
      BorderWidth     =   5
      Visible         =   0   'False
      X1              =   3480
      X2              =   3960
      Y1              =   6840
      Y2              =   6840
   End
   Begin VB.Shape Shape16 
      FillColor       =   &H000080FF&
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   3600
      Shape           =   3  'Circle
      Top             =   6360
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape Shape15 
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   4080
      Shape           =   3  'Circle
      Top             =   6000
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape Shape14 
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   3120
      Shape           =   3  'Circle
      Top             =   6000
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape Shape13 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  'Solid
      Height          =   1815
      Left            =   720
      Shape           =   3  'Circle
      Top             =   9240
      Visible         =   0   'False
      Width           =   1935
   End
   Begin VB.Shape Shape12 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  'Solid
      Height          =   1815
      Left            =   720
      Shape           =   3  'Circle
      Top             =   5400
      Visible         =   0   'False
      Width           =   1935
   End
   Begin VB.Shape Shape11 
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   3600
      Shape           =   3  'Circle
      Top             =   8400
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape Shape10 
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   3600
      Shape           =   3  'Circle
      Top             =   8040
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape Shape9 
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   3600
      Shape           =   3  'Circle
      Top             =   7680
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape Shape8 
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   3600
      Shape           =   3  'Circle
      Top             =   7320
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape Shape7 
      Height          =   495
      Left            =   0
      Shape           =   3  'Circle
      Top             =   2760
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Shape Shape6 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  'Solid
      Height          =   1815
      Left            =   2760
      Shape           =   3  'Circle
      Top             =   5400
      Visible         =   0   'False
      Width           =   1935
   End
   Begin VB.Shape Shape5 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  'Solid
      Height          =   2295
      Left            =   2400
      Shape           =   3  'Circle
      Top             =   6720
      Visible         =   0   'False
      Width           =   2655
   End
   Begin VB.Shape Shape4 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  'Solid
      Height          =   2775
      Left            =   2040
      Shape           =   3  'Circle
      Top             =   8280
      Visible         =   0   'False
      Width           =   3375
   End
   Begin VB.Shape Shape3 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  'Solid
      Height          =   615
      Left            =   12000
      Shape           =   3  'Circle
      Top             =   8040
      Visible         =   0   'False
      Width           =   615
   End
   Begin VB.Shape Shape2 
      Height          =   495
      Left            =   0
      Shape           =   3  'Circle
      Top             =   2760
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label lblloading 
      Alignment       =   2  'Center
      BackColor       =   &H80000009&
      Caption         =   "Loading..."
      BeginProperty Font 
         Name            =   "Book Antiqua"
         Size            =   27.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   735
      Left            =   6120
      TabIndex        =   0
      Top             =   4680
      Visible         =   0   'False
      Width           =   2655
   End
   Begin VB.Shape Shape1 
      Height          =   495
      Left            =   0
      Shape           =   3  'Circle
      Top             =   2760
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Image Image20 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":0000
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image19 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":14E2
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image18 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":29C4
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image17 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":3EA6
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image16 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":5388
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image15 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":686A
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image14 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":7D4C
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image13 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":922E
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image12 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":A710
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image11 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":BBF2
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image10 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":D0D4
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image9 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":E5B6
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image8 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":FA98
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image7 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":10F7A
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image6 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":1245C
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image5 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":1393E
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image4 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":14E20
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image3 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":16302
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image2 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":177E4
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image1 
      Height          =   660
      Left            =   0
      Picture         =   "Form1.frx":18CC6
      Top             =   0
      Width           =   600
   End
   Begin VB.Image Image23 
      Height          =   3705
      Left            =   10920
      Picture         =   "Form1.frx":1A1A8
      Top             =   7440
      Visible         =   0   'False
      Width           =   3240
   End
   Begin VB.Image Image22 
      Height          =   3600
      Left            =   10920
      Picture         =   "Form1.frx":41322
      Top             =   7560
      Visible         =   0   'False
      Width           =   3240
   End
   Begin VB.Image Image21 
      Height          =   3705
      Left            =   10800
      Picture         =   "Form1.frx":672E4
      Top             =   7440
      Visible         =   0   'False
      Width           =   4485
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Sub Main()
If App.PrevInstance Then
End
Else
Form1.Show
End If
End Sub
Private Sub cmdexit_click()
End
End Sub
Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
End
End Sub

Private Sub Form_KeyPress(KeyAscii As Integer)
End
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
End
End Sub

Private Sub Form_Load()
KeyPreview = True
If App.PrevInstance Then
End
End If
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
Static count As Integer
If count > 2 Then
End
Else
count = count + 1
End If
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Static count As Integer
If count > 2 Then
End
Else
count = count + 1
End If
End Sub

Private Sub Form_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
Static count As Integer
If count > 2 Then
End
Else
count = count + 1
End If
End Sub

Private Sub Timer1_Timer()
Randomize
Image1.Visible = True
Image1.Top = Image1.Top + 50
If Image1.Top > 10200 Then
Image1.Top = Rnd * 50
Image1.Left = Rnd(10) * 15000
End If
If Image1.Top = 600 Then
Timer2.Enabled = True
End If
If Image1.Left = 0 And Image1.Top = 0 Then
Timer24.Enabled = False
End If
End Sub

Private Sub Timer10_Timer()
Randomize
Image10.Visible = True
Image10.Top = Image10.Top + 50
If Image10.Top > 10200 Then
Image10.Top = Rnd * 50
Image10.Left = Rnd(10) * 15000
End If
If Image10.Top = 600 Then
Timer11.Enabled = True
End If
End Sub

Private Sub Timer11_Timer()
Randomize
Image11.Visible = True
Image11.Top = Image11.Top + 50
If Image11.Top > 10200 Then
Image11.Top = Rnd * 50
Image11.Left = Rnd(10) * 15000
End If
If Image11.Top = 600 Then
Timer12.Enabled = True
End If
End Sub

Private Sub Timer12_Timer()
Randomize
Image12.Visible = True
Image12.Top = Image12.Top + 50
If Image12.Top > 10200 Then
Image12.Top = Rnd * 50
Image12.Left = Rnd(10) * 15000
End If
If Image12.Top = 600 Then
Timer13.Enabled = True
End If
End Sub

Private Sub Timer13_Timer()
Randomize
Image13.Visible = True
Image13.Top = Image13.Top + 50
If Image13.Top > 10200 Then
Image13.Top = Rnd * 50
Image13.Left = Rnd(10) * 15000
End If
If Image13.Top = 600 Then
Timer14.Enabled = True
End If
End Sub

Private Sub Timer14_Timer()
Randomize
Image14.Visible = True
Image14.Top = Image14.Top + 50
If Image14.Top > 10200 Then
Image14.Top = Rnd * 50
Image14.Left = Rnd(10) * 15000
End If
If Image14.Top = 600 Then
Timer15.Enabled = True
End If
End Sub

Private Sub Timer15_Timer()
Randomize
Image15.Visible = True
Image15.Top = Image15.Top + 50
If Image15.Top > 10200 Then
Image15.Top = Rnd * 50
Image15.Left = Rnd(10) * 15000
End If
If Image15.Top = 600 Then
Timer16.Enabled = True
End If
End Sub

Private Sub Timer16_Timer()
Randomize
Image16.Visible = True
Image16.Top = Image16.Top + 50
If Image16.Top > 10200 Then
Image16.Top = Rnd * 50
Image16.Left = Rnd(10) * 15000
End If
If Image16.Top = 600 Then
Timer17.Enabled = True
End If
End Sub

Private Sub Timer17_Timer()
Randomize
Image17.Visible = True
Image17.Top = Image17.Top + 50
If Image17.Top > 10200 Then
Image17.Top = Rnd * 50
Image17.Left = Rnd(10) * 15000
End If
If Image17.Top = 600 Then
Timer18.Enabled = True
End If
End Sub

Private Sub Timer18_Timer()
Randomize
Image18.Visible = True
Image18.Top = Image18.Top + 50
If Image18.Top > 10200 Then
Image18.Top = Rnd * 50
Image18.Left = Rnd(10) * 15000
End If
If Image18.Top = 600 Then
Timer19 = True
End If
End Sub

Private Sub Timer19_Timer()
Randomize
Image19.Visible = True
Image19.Top = Image19.Top + 50
If Image19.Top > 10200 Then
Image19.Top = Rnd * 50
Image19.Left = Rnd(10) * 15000
End If
If Image19.Top = 600 Then
Timer20 = True
End If
End Sub

Private Sub Timer2_Timer()
Randomize
Image2.Visible = True
Image2.Top = Image2.Top + 50
If Image2.Top > 10200 Then
Image2.Top = Rnd * 50
Image2.Left = Rnd(10) * 15000
End If
If Image2.Top = 600 Then
Timer3.Enabled = True
End If
End Sub

Private Sub Timer20_Timer()
Randomize
Image20.Visible = True
Image20.Top = Image20.Top + 50
If Image20.Top > 10200 Then
Image20.Top = Rnd * 50
Image20.Left = Rnd(10) * 15000
End If
End Sub

Private Sub Timer21_Timer()
Shape2.Left = Shape2.Left + 150
Image21.Visible = True
If Shape2.Left > 480 Then
Image22.Visible = True
Image21.Visible = False
End If
If Shape2.Left > 1200 Then
Image23.Visible = True
Image22.Visible = False
End If
If Image23.Visible = True Then
Timer22.Enabled = True
End If
End Sub

Private Sub Timer22_Timer()
Shape3.Visible = True
Shape3.Left = Shape3.Left - 80
Shape3.Top = Shape3.Top - 20
If Shape3.Left < 4560 And Shape3.Top < 6240 Then
Shape6.Left = Shape6.Left - 50
End If
If Shape6.Left < 720 Then
Shape6.Visible = False
Timer23.Enabled = False
Shape11.Top = Shape11.Top + 50
Shape10.Top = Shape10.Top + 50
Shape9.Top = Shape9.Top + 50
Shape8.Top = Shape8.Top + 50
Line1.Y1 = Line1.Y1 + 50
Line1.Y2 = Line1.Y2 + 50
Shape16.Top = Shape16.Top + 50
Shape14.Top = Shape14.Top + 50
Shape15.Top = Shape15.Top + 50
Shape17.Top = Shape17.Top + 50
Line2.Y1 = Line2.Y1 + 50
Line2.Y2 = Line2.Y2 + 50
Shape12.Visible = True
Shape12.Top = Shape12.Top + 50
End If
If Shape12.Top > 9240 Then
Shape12.Visible = False
Shape13.Visible = True
End If
If Line2.Y1 > 7080 And Line2.Y2 > 7080 Then
Line2.Visible = False
Shape17.Visible = False
Line4.Visible = True
Shape25.Visible = True
End If
If Line1.Y1 > 10920 And Line1.Y2 > 10920 Then
Shape14.Visible = False
Shape15.Visible = False
Shape16.Visible = False
Line1.Visible = False
Shape22.Visible = True
Shape23.Visible = True
Shape24.Visible = True
Line3.Visible = True
End If
If Shape11.Top > 10920 Then
Shape11.Visible = False
Shape18.Visible = True
End If
If Shape10.Top > 10920 Then
Shape10.Visible = False
Shape19.Visible = True
End If
If Shape9.Top > 10920 Then
Shape9.Visible = False
Shape20.Visible = True
End If
If Shape8.Top > 10920 Then
Shape8.Visible = False
Shape21.Visible = True
End If
If Shape24.Visible = True Then
Timer24.Enabled = True
End If
End Sub

Private Sub Timer23_Timer()
Shape7.Left = Shape7.Left + 50
If Shape7.Left > 1080 Then
Shape4.Visible = True
End If
If Shape7.Left > 2160 Then
Shape5.Visible = True
End If
If Shape7.Left > 3240 Then
Shape6.Visible = True
End If
If Shape7.Left > 4320 Then
Shape11.Visible = True
End If
If Shape7.Left > 5400 Then
Shape10.Visible = True
End If
If Shape7.Left > 6480 Then
Shape9.Visible = True
End If
If Shape7.Left > 7560 Then
Shape8.Visible = True
End If
If Shape7.Left > 8640 Then
Line1.Visible = True
End If
If Shape7.Left > 9720 Then
Shape14.Visible = True
End If
If Shape7.Left > 10800 Then
Shape15.Visible = True
End If
If Shape7.Left > 11880 Then
Shape16.Visible = True
End If
If Shape7.Left > 12960 Then
Line2.Visible = True
End If
If Shape7.Left > 14040 Then
Shape17.Visible = True
End If
If Shape17.Visible = True Then
Timer21.Enabled = True
End If
If Shape6.Left < 720 Then
Shape6.Visible = False
End If
End Sub

Private Sub Timer24_Timer()
Timer1.Enabled = False
Timer2.Enabled = False
Timer3.Enabled = False
Timer4.Enabled = False
Timer5.Enabled = False
Timer6.Enabled = False
Timer7.Enabled = False
Timer8.Enabled = False
Timer9.Enabled = False
Timer10.Enabled = False
Timer11.Enabled = False
Timer12.Enabled = False
Timer13.Enabled = False
Timer14.Enabled = False
Timer15.Enabled = False
Timer16.Enabled = False
Timer17.Enabled = False
Timer18.Enabled = False
Timer19.Enabled = False
Timer20.Enabled = False
Timer21.Enabled = False
Timer22.Enabled = False
Timer23.Enabled = False
tmrball.Enabled = False
Image1.Visible = False
Image2.Visible = False
Image3.Visible = False
Image4.Visible = False
Image5.Visible = False
Image6.Visible = False
Image7.Visible = False
Image8.Visible = False
Image9.Visible = False
Image10.Visible = False
Image11.Visible = False
Image12.Visible = False
Image13.Visible = False
Image14.Visible = False
Image15.Visible = False
Image16.Visible = False
Image17.Visible = False
Image18.Visible = False
Image19.Visible = False
Image20.Visible = False
Image21.Visible = False
Image22.Visible = False
Image23.Visible = False
Shape1.Visible = False
Shape2.Visible = False
Shape3.Visible = False
Shape4.Visible = False
Shape5.Visible = False
Shape6.Visible = False
Shape7.Visible = False
Shape8.Visible = False
Shape9.Visible = False
Shape10.Visible = False
Shape11.Visible = False
Shape12.Visible = False
Shape13.Visible = False
Shape14.Visible = False
Shape15.Visible = False
Shape16.Visible = False
Shape17.Visible = False
Shape18.Visible = False
Shape19.Visible = False
Shape20.Visible = False
Shape21.Visible = False
Shape22.Visible = False
Shape23.Visible = False
Shape24.Visible = False
Shape25.Visible = False
Line1.Visible = False
Line2.Visible = False
Line3.Visible = False
Line4.Visible = False
Shape1.Left = 0
Shape2.Left = 0
Shape7.Left = 0
Shape3.Left = 12000
Shape3.Top = 8040
Image1.Left = 0
Image2.Left = 0
Image3.Left = 0
Image4.Left = 0
Image5.Left = 0
Image6.Left = 0
Image7.Left = 0
Image8.Left = 0
Image9.Left = 0
Image10.Left = 0
Image11.Left = 0
Image12.Left = 0
Image13.Left = 0
Image14.Left = 0
Image15.Left = 0
Image16.Left = 0
Image17.Left = 0
Image18.Left = 0
Image19.Left = 0
Image20.Left = 0
Image1.Top = 0
Image2.Top = 0
Image3.Top = 0
Image4.Top = 0
Image5.Top = 0
Image6.Top = 0
Image7.Top = 0
Image8.Top = 0
Image9.Top = 0
Image10.Top = 0
Image11.Top = 0
Image12.Top = 0
Image13.Top = 0
Image14.Top = 0
Image15.Top = 0
Image16.Top = 0
Image17.Top = 0
Image18.Top = 0
Image19.Top = 0
Image20.Top = 0
Shape12.Left = 720
Shape12.Top = 5400
Shape17.Left = 2880
Shape17.Top = 4560
Shape14.Left = 3120
Shape14.Top = 6000
Shape15.Left = 4080
Shape15.Top = 6000
Shape16.Left = 3600
Shape16.Top = 6360
Shape6.Left = 2760
Shape6.Top = 5400
Shape8.Left = 3600
Shape8.Top = 7320
Shape9.Left = 3600
Shape9.Top = 7680
Shape10.Left = 3600
Shape10.Top = 8040
Shape11.Left = 3600
Shape11.Top = 8400
Line1.Y1 = 6840
Line1.Y2 = 6840
Line2.Y1 = 5760
Line2.Y2 = 5760
Timer1.Enabled = True
Timer23.Enabled = True
End Sub

Private Sub Timer3_Timer()
Randomize
Image3.Visible = True
Image3.Top = Image3.Top + 50
If Image3.Top > 10200 Then
Image3.Top = Rnd * 50
Image3.Left = Rnd(10) * 15000
End If
If Image3.Top = 600 Then
Timer4.Enabled = True
End If
End Sub

Private Sub Timer4_Timer()
Randomize
Image4.Visible = True
Image4.Top = Image4.Top + 50
If Image4.Top > 10200 Then
Image4.Top = Rnd * 50
Image4.Left = Rnd(10) * 15000
End If
If Image4.Top = 600 Then
Timer5.Enabled = True
End If
End Sub

Private Sub Timer5_Timer()
Randomize
Image5.Visible = True
Image5.Top = Image5.Top + 50
If Image5.Top > 10200 Then
Image5.Top = Rnd * 50
Image5.Left = Rnd(10) * 15000
End If
If Image5.Top = 600 Then
Timer6.Enabled = True
End If
End Sub

Private Sub Timer6_Timer()
Randomize
Image6.Visible = True
Image6.Top = Image6.Top + 50
If Image6.Top > 10200 Then
Image6.Top = Rnd * 50
Image6.Left = Rnd(10) * 15000
End If
If Image6.Top = 600 Then
Timer7.Enabled = True
End If
End Sub

Private Sub Timer7_Timer()
Randomize
Image7.Visible = True
Image7.Top = Image7.Top + 50
If Image7.Top > 10200 Then
Image7.Top = Rnd * 50
Image7.Left = Rnd(10) * 15000
End If
If Image7.Top = 600 Then
Timer8.Enabled = True
End If
End Sub

Private Sub Timer8_Timer()
Randomize
Image8.Visible = True
Image8.Top = Image8.Top + 50
If Image8.Top > 10200 Then
Image8.Top = Rnd * 50
Image8.Left = Rnd(10) * 15000
End If
If Image8.Top = 600 Then
Timer9.Enabled = True
End If
End Sub

Private Sub Timer9_Timer()
Randomize
Image9.Visible = True
Image9.Top = Image9.Top + 50
If Image9.Top > 10200 Then
Image9.Top = Rnd * 50
Image9.Left = Rnd(10) * 15000
End If
If Image9.Top = 600 Then
Timer10.Enabled = True
End If
End Sub

Private Sub tmrball_Timer()
Shape1.Left = Shape1.Left + 30
If Shape1.Left = 120 Then
lblloading.Visible = True
End If
If Shape1.Left = 360 Then
lblloading.Visible = False
End If
If Shape1.Left = 600 Then
lblloading.Visible = True
End If
If Shape1.Left = 840 Then
lblloading.Visible = False
End If
If Shape1.Left = 1080 Then
lblloading.Visible = True
End If
If Shape1.Left = 1320 Then
lblloading.Visible = False
End If
If Shape1.Left = 1560 Then
lblloading.Visible = True
End If
If Shape1.Left = 1800 Then
lblloading.Visible = False
End If
If Shape1.Left = 2040 Then
lblloading.Visible = True
End If
If Shape1.Left = 2280 Then
lblloading.Visible = False
End If
If Shape1.Left = 2520 Then
lblloading.Visible = True
End If
If Shape1.Left = 2760 Then
lblloading.Visible = False
End If
If Shape1.Left = 3000 Then
lblloading.Visible = True
End If
If Shape1.Left = 3240 Then
lblloading.Visible = False
End If
If Shape1.Left = 3480 Then
lblloading.Visible = True
End If
If Shape1.Left = 3720 Then
lblloading.Visible = False
End If
If Shape1.Left = 3960 Then
lblloading.Visible = True
End If
If Shape1.Left = 4200 Then
lblloading.Visible = False
End If
If Shape1.Left = 4440 Then
lblloading.Visible = True
End If
If Shape1.Left = 4680 Then
lblloading.Visible = False
End If
If Shape1.Left = 4920 Then
lblloading.Visible = True
End If
If Shape1.Left = 5160 Then
lblloading.Visible = False
End If
If Shape1.Left = 5400 Then
lblloading.Visible = True
End If
If Shape1.Left = 5640 Then
lblloading.Visible = False
End If
If Shape1.Left = 5880 Then
lblloading.Visible = True
End If
If Shape1.Left = 6120 Then
lblloading.Visible = False
End If
If Shape1.Left = 6360 Then
lblloading.Visible = True
End If
If Shape1.Left = 6600 Then
lblloading.Visible = False
End If
If Shape1.Left = 6840 Then
lblloading.Visible = True
End If
If Shape1.Left = 7080 Then
lblloading.Visible = False
End If
If Shape1.Left = 7320 Then
lblloading.Visible = True
End If
If Shape1.Left = 7560 Then
lblloading.Visible = False
End If
If Shape1.Left = 7800 Then
lblloading.Visible = True
End If
If Shape1.Left = 8040 Then
lblloading.Visible = False
End If
If Shape1.Left = 8280 Then
lblloading.Visible = True
End If
If Shape1.Left = 8520 Then
lblloading.Visible = False
End If
If Shape1.Left = 8760 Then
lblloading.Visible = True
End If
If Shape1.Left = 9000 Then
lblloading.Visible = False
End If
If Shape1.Left = 9240 Then
lblloading.Visible = True
End If
If Shape1.Left = 9480 Then
lblloading.Visible = False
End If
If Shape1.Left = 9720 Then
lblloading.Visible = True
End If
If Shape1.Left = 9960 Then
lblloading.Visible = False
End If
If Shape1.Left = 10200 Then
lblloading.Visible = True
End If
If Shape1.Left = 10440 Then
lblloading.Visible = False
End If
If Shape1.Left = 10680 Then
lblloading.Visible = True
End If
If Shape1.Left = 10920 Then
lblloading.Visible = False
End If
If Shape1.Left = 11160 Then
lblloading.Visible = True
End If
If Shape1.Left = 11400 Then
lblloading.Visible = False
End If
If Shape1.Left = 11640 Then
lblloading.Visible = True
End If
If Shape1.Left = 12000 Then
lblloading.Visible = False
End If
If Shape1.Left = 12240 Then
lblloading.Visible = True
End If
If Shape1.Left = 12480 Then
lblloading.Visible = False
End If
If Shape1.Left = 12720 Then
lblloading.Visible = True
End If
If Shape1.Left = 12960 Then
lblloading.Visible = False
Timer23.Enabled = True
End If
End Sub


