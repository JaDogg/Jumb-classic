Type variable
  As Double varValue
  As String varName
End Type

Type variableSpace
  Public:
    Declare Constructor (parentSpace As Any Ptr = 0)
    Declare Destructor ()

    Declare Sub addVariable (vname As String, initialVal As Double)
    Declare Function getVariable (vname As String) As Double
    Declare Sub setVariable (vname As String, newVal As Double)
    Declare Function accessVariable (vname As String) As Double Ptr

    Declare Sub destroyAllParents ()

  Private:
    As variable Ptr _variables
    As uInteger _num_variables

    As variableSpace Ptr _parent_space

    As Any Ptr _info_mutex
End Type

Constructor variableSpace (parentSpace As Any Ptr = 0)
  this._num_variables = 0
  this._variables = Allocate(1)
  this._parent_space = CPtr(variableSpace Ptr, parentSpace)
  this._info_mutex = MutexCreate()
End Constructor

Destructor variableSpace ()
  MutexLock(this._info_mutex)
  this._num_variables = 0
  DeAllocate(this._variables)
  MutexDestroy(this._info_mutex)
End Destructor

Sub variableSpace.addVariable (vname As String, initialVal As Double)
  
  vname = LCase(vname)
  
  'Thread safety, as usual
  MutexLock(this._info_mutex)

  'Add a new variable
  this._num_variables += 1
  this._variables = ReAllocate(this._variables, (this._num_variables+1)*SizeOf(variable))
  this._variables[this._num_variables-1].varValue = initialVal
  this._variables[this._num_variables-1].varName = vname

  MutexUnlock(this._info_mutex)
End Sub

Function variableSpace.getVariable (vname As String) As Double
Dim As uInteger found = 0

  If this._num_variables = 0 Then Return 0

  vname = LCase(vname)
  
  'Special variables - RND and TIMER
  'If vname = "rnd" Then Return Rnd
  'If vname = "timer" Then Return Timer

  'Thread safety, as usual
  MutexLock(this._info_mutex)
  
  'Default value
  Function = 0

  'Check each variable and see if it has the right value
  For i As uInteger = 0 To this._num_variables-1
    'If we find it, return its value
    If this._variables[i].varName = vname Then
      Function = this._variables[i].varValue
      found = Not 0
    End If
  Next i

  'If we didn't find it, check our parent space if we have one...
  If found = 0 Then
    If this._parent_space <> 0 Then
      Function = this._parent_space->getVariable(vname)
    End If
  End If

  MutexUnlock(this._info_mutex)
End Function

Sub variableSpace.setVariable (vname As String, newVal As Double)
Dim As uInteger found = 0
Dim As Double Ptr dblPtr

  If this._num_variables = 0 Then Exit Sub

  vname = LCase(vname)
  
  'If trying to set RND, set the seed
  'If vname = "rnd" Then Randomize newVal

  'Thread safety, as usual
  MutexLock(this._info_mutex)

  'Check each variable and see if it has the right value
  For i As uInteger = 0 To this._num_variables-1
    'If we find it, return its value
    If this._variables[i].varName = vname Then
      dblPtr = @(this._variables[i].varValue)
      found = Not 0
    End If
  Next i

  'If we didn't find it, check our parent space if we have one...
  If found = 0 Then
    If this._parent_space <> 0 Then
      dblPtr = this._parent_space->accessVariable(vname)
    End If
  End If

  If dblPtr <> 0 Then *dblPtr = newVal

  MutexUnlock(this._info_mutex)
End Sub

Function variableSpace.accessVariable (vname As String) As Double Ptr
Dim As uInteger found = 0

  If this._num_variables = 0 Then Return 0

  vname = LCase(vname)

  'Thread safety, as usual
  MutexLock(this._info_mutex)

  'Default value
  Function = 0

  'Check each variable and see if it has the right value
  For i As uInteger = 0 To this._num_variables-1
    'If we find it, return its value
    If this._variables[i].varName = vname Then
      Function = @(this._variables[i].varValue)
      found = Not 0
    End If
  Next i

  'If we didn't find it, check our parent space if we have one...
  If found = 0 Then
    If this._parent_space <> 0 Then
      Function = this._parent_space->accessVariable(vname)
    End If
  End If

  MutexUnlock(this._info_mutex)
End Function

Sub variableSpace.destroyAllParents()
  If this._parent_space <> 0 Then
    this._parent_space->destroyAllParents()
    Delete this._parent_space
  End If
End Sub

Function isNumeric (testStr As String) As uInteger
  'Check if the first character is numeric or not
  For i As uInteger = 0 To Len(testStr)-1
    Select Case testStr[i]
  	  Case Asc("0"), Asc("1"), Asc("2"), Asc("3"), Asc("4"), Asc("5"), Asc("6"), Asc("7"), Asc("8"), Asc("9")
  	    Return Not 0
  	  Case Else:
  	    Return 0
    End Select
  Next i
  Return Not 0
End Function

Function isKeyword (testStr As String) As uInteger
  If LCase(teststr) = "dim" Then Return Not 0
  If LCase(teststr) = "cls" Then Return Not 0
  If LCase(teststr) = "end" Then Return Not 0
  If LCase(teststr) = "print" Then Return Not 0
  If LCase(teststr) = "sleep" Then Return Not 0
End Function