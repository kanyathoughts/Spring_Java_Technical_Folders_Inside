VERSION 6.00
Begin VB.UserControl UserControl1 
   BorderStyle     =   1  'Fixed Single
End
Attribute VB_Name = "UserControl1"
Private Sub UserControl_ReadProperties(PropBag As PropertyBag)
    On Error Resume Next
    Set ParentForm = Parent
End Sub
' This entire line is also a comment.