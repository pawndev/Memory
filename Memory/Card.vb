Public Class Card

    Inherits PictureBox
    Public id As String
    Public recto As Image
    Public clicked As Boolean

    Public Sub New()
    	id = ""
    	recto = Nothing
    	clicked = False
    End Sub

End Class