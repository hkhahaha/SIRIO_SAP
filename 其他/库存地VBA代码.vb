Sub Macro1()
'
' Macro1 Macro
'
Dim rng As Range, n
n = 0
Dim A, B, col, row
For Each rng In [J3:R102]
    If rng.Value = "√" Then
        n = n + 1
        'MsgBox (n & "哈哈" & rng.row & "列" & rng.Column)
        A = rng.row
        B = rng.Column
        Worksheets(2).Cells(n, 1).Value = Worksheets(1).Cells(A, 2).Value
        'Worksheets(2).Cells(n, 1).Value = Worksheets(1).Cells(A, 2).Value
        Worksheets(2).Cells(n, 3).Value = Worksheets(1).Cells(A, 6).Value
        Worksheets(2).Cells(n, 4).Value = Worksheets(1).Cells(A, 7).Value
        If B = 10 Then
        Worksheets(2).Cells(n, 2).Value = "Z010"
        ElseIf B = 11 Then
        Worksheets(2).Cells(n, 2).Value = "Z020"
        ElseIf B = 12 Then
        Worksheets(2).Cells(n, 2).Value = "Z030"
        ElseIf B = 13 Then
        Worksheets(2).Cells(n, 2).Value = "Z040"
        ElseIf B = 14 Then
        Worksheets(2).Cells(n, 2).Value = "Z050"
        ElseIf B = 15 Then
        Worksheets(2).Cells(n, 2).Value = "Z060"
        ElseIf B = 16 Then
        Worksheets(2).Cells(n, 2).Value = "Z070"
        ElseIf B = 17 Then
        Worksheets(2).Cells(n, 2).Value = "Z090"
        ElseIf B = 18 Then
        Worksheets(2).Cells(n, 2).Value = "Z080"
        End If
        
    End If
Next rng
MsgBox (n)
End Sub

