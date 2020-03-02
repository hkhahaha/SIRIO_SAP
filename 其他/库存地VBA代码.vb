Sub Macro1()
'
' Macro1 Macro
'
Dim rng As Range, n
n = 0
Dim A, B, col, row
For Each rng In [J3:R4]
    If rng.Value = "√" Then
        n = n + 1
        MsgBox (n & "哈哈" & rng.row & "列" & rng.Column)
    End If
Next rng
MsgBox (n)
End Sub