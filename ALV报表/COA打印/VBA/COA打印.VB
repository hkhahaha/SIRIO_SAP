 Sub COA生成()
'
' COA生成 宏
''
Dim i, j, countinspM, temp, countinspS, CM, CS


countCOA = 11
COASTARTROW = 18
SourceStartRow = 24
SourceStartColumn = 2
indexQA = countCOA + 2
indexQC = countCOA + 3
indexEN = countCOA + 4
indexENA = countCOA + 6
KEN = indexEN + 1
KENA = indexENA + 1

countinspM = Sheets(1).Range("A24").End(xlDown).Row - SourceStartRow - 1
countinspS = Sheets(1).Range("T24").End(xlDown).Row - SourceStartRow - 1
countinspo = Sheets(1).Range("AM24").End(xlDown).Row - SourceStartRow - 1
If countinspM + countinspS + countinspo > 0 Then
Sheets(2).Activate
If countinspM > 0 Then
Range(Sheets(2).Cells(1, 1), Sheets(2).Cells(countinspM, 10)).Copy
Range(Sheets(2).Cells(1, 26), Sheets(2).Cells(countinspM, 35)).PasteSpecial Paste:=xlPasteValues
End If
If countinspS > 0 Then
Range(Sheets(2).Cells(1, 11), Sheets(2).Cells(countinspS, 20)).Copy
Range(Sheets(2).Cells(1, 36), Sheets(2).Cells(countinspS, 45)).PasteSpecial Paste:=xlPasteValues
End If
CM = countinspM
CS = countinspS

j = 1
For i = 1 To countinspM
If Sheets(2).Cells(j, 34).Text = "" Then
j = j + 1
Else
 Range(Sheets(2).Cells(j, 26), Sheets(2).Cells(j, 35)).Select
 Selection.Delete Shift:=xlUp
 CM = CM - 1
End If
Next


j = 1
For i = 1 To countinspS
If Sheets(2).Cells(j, 44).Text = "" Then
j = j + 1
Else
 Range(Sheets(2).Cells(j, 36), Sheets(2).Cells(j, 45)).Select
 Selection.Delete Shift:=xlUp
 CS = CS - 1
End If
Next




For j = 1 To countCOA + 2
For i = 1 To CM + CS
Sheets(j + 3).Activate
Sheets(j + 3).Rows(COASTARTROW + 1).Insert
Next
Next

For i = 1 To countCOA
If CM > 0 Then
Sheets(2).Activate
Range(Sheets(2).Cells(1, 26), Sheets(2).Cells(CM, 33)).Copy
Sheets(i + 3).Activate
Range(Sheets(i + 3).Cells(COASTARTROW, 1), Sheets(i + 3).Cells(COASTARTROW + CM - 1, 8)).PasteSpecial Paste:=xlPasteValues
End If

If CS > 0 Then
Sheets(2).Activate
Range(Sheets(2).Cells(1, 36), Sheets(2).Cells(CS, 43)).Copy
Sheets(i + 3).Activate
Range(Sheets(i + 3).Cells(COASTARTROW + CM, 1), Sheets(i + 3).Cells(COASTARTROW + CM + CS - 1, 8)).PasteSpecial Paste:=xlPasteValues
End If
Next


Sheets(indexQC).Activate  'QC页筛选
CMB = CM
CSB = CS
j = 1
i = 1
For i = 1 To CMB

If Sheets(2).Cells(i, 35).Text = "QC" Then
j = j + 1
Else
 Range(Sheets(indexQC).Cells(j + COASTARTROW - 1, 1), Sheets(indexQC).Cells(j + COASTARTROW - 1, 10)).Select
 Selection.Delete Shift:=xlUp
 CMB = CMB - 1
End If
Next

j = 1
i = 1
For i = 1 To CSB
If Sheets(2).Cells(i, 45).Text = "QC" Then
j = j + 1
Else
 Range(Sheets(indexQC).Cells(j + COASTARTROW + CMB - 1, 1), Sheets(indexQC).Cells(j + COASTARTROW + CMB - 1, 10)).Select
 Selection.Delete Shift:=xlUp
End If
Next




Sheets(indexQA).Activate 'QA页筛选
j = 1
i = 1
CMB = CM
CSB = CS
For i = 1 To CMB
If Sheets(2).Cells(i, 35).Text = "QA" Then
j = j + 1
Else
 Range(Sheets(indexQA).Cells(j + COASTARTROW - 1, 1), Sheets(indexQA).Cells(j + COASTARTROW - 1, 10)).Select
 Selection.Delete Shift:=xlUp
 CMB = CMB - 1
End If
Next

j = 1
i = 1
For i = 1 To CSB
If Sheets(2).Cells(i, 45).Text = "QA" Then
j = j + 1
Else
 Range(Sheets(indexQA).Cells(j + COASTARTROW + CMB - 1, 1), Sheets(indexQA).Cells(j + COASTARTROW + CMB - 1, 10)).Select
 Selection.Delete Shift:=xlUp
End If
Next

'英文
If countinspM + countinspS > 0 Then
Sheets(2).Activate
If countinspM > 0 Then
Range(Sheets(2).Cells(1, 49), Sheets(2).Cells(countinspM, 58)).Copy
Range(Sheets(2).Cells(1, 74), Sheets(2).Cells(countinspM, 83)).PasteSpecial Paste:=xlPasteValues
End If
If countinspS > 0 Then
Range(Sheets(2).Cells(1, 59), Sheets(2).Cells(countinspS, 68)).Copy
Range(Sheets(2).Cells(1, 84), Sheets(2).Cells(countinspS, 93)).PasteSpecial Paste:=xlPasteValues
End If
CM = countinspM
CS = countinspS

j = 1
For i = 1 To countinspM
If Sheets(2).Cells(j, 82).Text = "" Then
j = j + 1
Else
 Range(Sheets(2).Cells(j, 74), Sheets(2).Cells(j, 83)).Select
 Selection.Delete Shift:=xlUp
 CM = CM - 1
End If
Next


j = 1
For i = 1 To countinspS
If Sheets(2).Cells(j, 92).Text = "" Then
j = j + 1
Else
 Range(Sheets(2).Cells(j, 84), Sheets(2).Cells(j, 93)).Select
 Selection.Delete Shift:=xlUp
 CS = CS - 1
End If
Next


For i = indexEN To KEN
If CM > 0 Then
Range(Sheets(2).Cells(1, 74), Sheets(2).Cells(CM, 81)).Copy
Sheets(i).Activate
Range(Sheets(i).Cells(COASTARTROW, 1), Sheets(i).Cells(COASTARTROW + CM - 1, 8)).PasteSpecial Paste:=xlPasteValues
End If
If CS > 0 Then
Range(Sheets(2).Cells(1, 84), Sheets(2).Cells(CS, 91)).Copy
Sheets(i).Activate
Range(Sheets(i).Cells(COASTARTROW + CM, 1), Sheets(i).Cells(COASTARTROW + CM + CS - 1, 8)).PasteSpecial Paste:=xlPasteValues
End If
Next
End If

'英文附页
If countinspo > 0 Then
Sheets(2).Activate
Range(Sheets(2).Cells(1, 94), Sheets(2).Cells(countinspo, 102)).Select
Range(Sheets(2).Cells(1, 94), Sheets(2).Cells(countinspo, 102)).Copy
Range(Sheets(2).Cells(1, 104), Sheets(2).Cells(countinspo, 112)).PasteSpecial Paste:=xlPasteValues

CO = countinspo

j = 1
For i = 1 To countinspo
If Sheets(2).Cells(j, 102).Text = "" Then
j = j + 1
Else
 Range(Sheets(2).Cells(j, 94), Sheets(2).Cells(j, 102)).Select
 Selection.Delete Shift:=xlUp
 CO = CO - 1
End If
Next

For j = indexENA To KENA
For i = 1 To CO
Sheets(j).Activate
Sheets(j).Rows(COASTARTROW + 1).Insert
Next
Next


For i = indexENA To KENA
If CO > 0 Then
Range(Sheets(2).Cells(1, 94), Sheets(2).Cells(CO, 102)).Copy
Sheets(i).Activate
Range(Sheets(i).Cells(COASTARTROW, 2), Sheets(i).Cells(COASTARTROW + CO - 1, 9)).PasteSpecial Paste:=xlPasteValues
End If
Next
End If

MsgBox ("succeed")

Else
MsgBox ("Empty")
End If
Sheets(4).Activate
End Sub


