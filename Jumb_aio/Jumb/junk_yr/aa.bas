
' Trig Explorer
'
#Include "vbcompat.bi"

#Define Rad2Deg(r)   r*(180/pi)
#Define Fmt(f)       Format(f,"0.00000")

Const pi     = Atn(1)*4
Const dx     = 640
Const dy     = 480
Const dx2    = dx/2
Const dy2    = dy/2
Const radius = 1

ScreenRes dx,dy,32
WindowTitle "Trig Explorer"

Dim As Single ox,oy,legx,legy,ang,sine,cosine,tangent
Dim As Integer mx,my,btn,wheel,clip,tanerr

Do
   GetMouse mx,my,wheel,btn,clip
   
   ang     = Atan2(my-dy2,mx-dx2)
   sine    = Sin(ang)
   cosine  = Cos(ang)
   
   If Abs(cosine)>.000001 Then
      tangent = sine/cosine
      tanerr = 0
   Else
      tanerr = 1
   End If
   
   legx    = cosine*radius
   legy    = sine*radius
     
   ScreenLock
      Cls
     
      Window Screen (-2,2)-(2,-2)     
      Line (-2,0)-(2,0),&hFFFF00
      Line (0,-2)-(0,2),&hFFFF00
     
      Circle (0,0),radius,,,,dy/dx
      Line (0,0)-(legx,legy),&hFFFFFF
      Line (0,0)-(legx,0),&hFF
      Line (legx,0)-(legx,legy),&hFF0000
             
      Window Screen (0,0)-(dx,dy)
      Draw String (5,5)  ,"Angle = " & fmt(ang) & " | " & fmt(Rad2Deg(ang))
      Draw String (5,15) ,"Leg   = " & fmt(legy), &hFF0000
      Draw String (5,25) ,"Leg   = " & fmt(legx), &hFF
      Draw String (5,35) ,"Sin   = " & fmt(sine), &h00FF00
      Draw String (5,45) ,"Cos   = " & fmt(cosine), &h00FF00
      If tanerr Then
         Draw String (5,55) ,"Tan   = " & "#UNDEFINED", &h00FF00
      Else       
         Draw String (5,55) ,"Tan   = " & fmt(tangent), &h00FF00
      Endif   
   ScreenUnlock
   
   If btn Then
      Draw String (5,dy-20),"Paused...Press any key to resume"
      Do
         Sleep 1         
      Loop While Inkey=""
   Else
      Draw String (5,dy-20),"Click Mouse to Pause"
   Endif
   
   Sleep 1
   
Loop Until Inkey=Chr(27)