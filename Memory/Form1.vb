Public Class Form1
    'declaration des variables
    Public ran As New Random 'on declare l'objet Random pour générer un nombre aléatoire
    Public rand As Integer 'on déclare cet entier pour contenir le nombre aléatoire
    Public Img1Count As Integer = 0 'compteur de nombre de fois ou la carte a été placé, 0 fois au début
    Public Img2Count As Integer = 0
    Public Img3Count As Integer = 0
    Public Img4Count As Integer = 0
    Public Img5Count As Integer = 0
    Public Img6Count As Integer = 0
    Public myDate As Date = Date.Now 'récupère date, heure,minut, seconde ou l'application a été lancé
    Public ImgClicked As Integer = 0
    Public All As Integer = 0 'variable qui contiendra le nombre de pair réussi
    Public Tour As Integer = 0
    Private CurrentlyInUse1 As New Card 'variable qui servira a dire si la carte est retourné
    Private CurrentlyInUse2 As New Card
    Private Deck(12) As Card 'Afin de manipuler les cartes d'une manière plus pratique

    Private Sub InitDeck() 'On remplie maintenant le tableau
        Deck(0) = Card1
        Deck(1) = Card2
        Deck(2) = Card3
        Deck(3) = Card4
        Deck(4) = Card5
        Deck(5) = Card6
        Deck(6) = Card7
        Deck(7) = Card8
        Deck(8) = Card9
        Deck(9) = Card10
        Deck(10) = Card11
        Deck(11) = Card12
    End Sub


    Private Sub randomm()
        rand = ran.Next(1, 7) 'fonction qui génère un nombre aléatoire compris entre 1 et 6 et mis dans la variable rand
    End Sub

    Private Sub UpdateCurrentCard(ByRef clickedCard) 'fonction qui update les cartes
        If CurrentlyInUse1 Is Nothing Then
            CurrentlyInUse1 = clickedCard
        ElseIf CurrentlyInUse2 Is Nothing Then
            CurrentlyInUse2 = clickedCard
        End If
    End Sub

    Private Function VerifyCounter(ByVal Counter As Integer) As Boolean 'fonction qui vérifie si le compteur de carte numéro x a dépassé les 2 
        If Counter < 2 Then 'si non on retourn 'true'
            Return True
        Else                'si oui on retourne 'false'
            Return False
        End If
    End Function

    Private Sub Apply(ByRef card As Card) 'fonction qui met dans les picturebox leur backgroundImage
line01:  'est une étiquette pour le 'GoTo'
        randomm() 'on appel la fonction randomm()

        Select Case rand
            Case 1  'si le nombre aléatoire est 1 et que la carte numéro 1 n'as pas été mis plus de 2 fois alors on l'affecte a la picturebox en question
                If VerifyCounter(Img1Count) = True Then
                    card.Image = My.Resources.carte
                    card.recto = My.Resources.carte1 'rajoute l'image au dos
                    Img1Count += 1 'puis on rajoute 1 au compteur pour dire qu'elle vient d'être placé 1 fois en plus
                    card.id = "carte1"
                Else
                    GoTo line01 'si la carte a été mise déjà 2 fois alors on retourne au début de la fonction
                End If

            Case 2

                If VerifyCounter(Img2Count) = True Then
                    card.Image = My.Resources.carte
                    card.recto = My.Resources.carte2
                    Img2Count += 1
                    card.id = "carte2"
                Else
                    GoTo line01
                End If

            Case 3

                If VerifyCounter(Img3Count) = True Then
                    card.Image = My.Resources.carte
                    card.recto = My.Resources.carte3
                    Img3Count += 1
                    card.id = "carte3"
                Else
                    GoTo line01

                End If
            Case 4

                If VerifyCounter(Img4Count) = True Then
                    card.Image = My.Resources.carte
                    card.recto = My.Resources.carte4
                    Img4Count += 1
                    card.id = "carte4"
                Else
                    GoTo line01

                End If
            Case 5

                If VerifyCounter(Img5Count) = True Then
                    card.Image = My.Resources.carte
                    card.recto = My.Resources.carte5
                    Img5Count += 1
                    card.id = "carte5"
                Else
                    GoTo line01

                End If
            Case 6

                If VerifyCounter(Img6Count) = True Then
                    card.Image = My.Resources.carte
                    card.recto = My.Resources.carte6
                    Img6Count += 1
                    card.id = "carte6"
                Else
                    GoTo line01
                End If
        End Select

    End Sub

    Private Sub ImgApply() 'fonction qui applique les images a chaque Card de 1 a 12
        For i As Integer = 0 To 11
            Apply(Deck(i))
        Next
    End Sub

    Private Sub DisableAll() 'Utile afin que le joueur ne puisse plus activer d'autres cartes si deux cartes ont déjà été retournées
        For i As Integer = 0 To 11
            Deck(i).Enabled = False
        Next
    End Sub

    Private Sub EnableAll() 'Utile pour réactiver les cartes une fois que 2 ont été retourné
        For i As Integer = 0 To 11
            Deck(i).Enabled = True
        Next
    End Sub

    Private Sub ResetAllClicks() 'Remet le nombre de click a 0, pour pouvoire rclicker a nouveau
        For i As Integer = 0 To 11
            Deck(i).clicked = False
        Next
    End Sub

    Private Sub DisplayImg(ByRef Picture01 As Card) 'fonction qui affiche les images, vérifie si 2 déjà activer
        If ImgClicked < 2 Then 'si il y a moins de 2 image cliqué on peut cliquer et afficher le verso, dire qu'il est retourné dans la variable "curentlyinuse" et mettre+1 a
            Picture01.Enabled = False
            Picture01.Image = Picture01.recto 'la variable ImgClicked
            Picture01.Update()
            ImgClicked += 1
            If ImgClicked = 2 Then
                VerifySamePicture(CurrentlyInUse1, CurrentlyInUse2)
                ImgClicked = 0
                ResetAllClicks()
            End If

        End If
    End Sub

    Private Sub VerifySamePicture(ByRef PictureBoxFirst As Card, ByRef PictureBoxSecond As Card) 'Fonction qui vérifiera si les 2 cartes retournées sont identiques
        DisableAll()
        If String.Compare(PictureBoxFirst.id, PictureBoxSecond.id) = 0 Then
            System.Threading.Thread.Sleep(1500)  ' 1000 correspond à 1 seconde
            PictureBoxFirst.Visible = False
            PictureBoxSecond.Visible = False
            PictureBoxFirst.Enabled = False
            PictureBoxSecond.Enabled = False
            All = All + 1
            Label2.Text = CStr(All)
            If All = 6 Then
                If Tour >= 6 And Tour <= 8 Then
                    MsgBox(" Nous te nommons : DIEU DU MEMORY!!!!", vbOKOnly, "The End")
                    If vbOK = 1 Then
                        Me.Close()
                    End If

                ElseIf Tour > 8 And Tour <= 11 Then
                    MsgBox("Bien joué, mais tu peux t'améliorer !", vbOKOnly, "The End")
                    If vbOK = 1 Then
                        Me.Close()
                    End If
                Else
                    MsgBox("Touche le fond mais creuse encore !", vbOKOnly, "The End")
                    If vbOK = 1 Then
                        Me.Close()
                    End If
                End If
            End If

        Else
            System.Threading.Thread.Sleep(1500) 'await Task.Delay(100) ' 1000 correspond à 1 seconde
            PictureBoxFirst.Image = My.Resources.carte
            PictureBoxSecond.Image = My.Resources.carte
        End If
        CurrentlyInUse1 = Nothing
        CurrentlyInUse2 = Nothing
        EnableAll()
        Tour = Tour + 1
        Label3.Text = "Tour : " + CStr(Tour)
    End Sub

    Private Sub Card1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Card1.Click
        If Card1.clicked = False Then
            Card1.clicked = True
            UpdateCurrentCard(Card1)
            DisplayImg(Card1)
        End If
    End Sub

    Private Sub Card2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Card2.Click
        If Card2.clicked = False Then
            Card2.clicked = True
            UpdateCurrentCard(Card2)
            DisplayImg(Card2)
        End If
    End Sub

    Private Sub Card3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Card3.Click
        If Card3.clicked = False Then
            Card3.clicked = True
            UpdateCurrentCard(Card3)
            DisplayImg(Card3)
        End If
    End Sub

    Private Sub Card4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Card4.Click
        If Card4.clicked = False Then
            Card4.clicked = True
            UpdateCurrentCard(Card4)
            DisplayImg(Card4)
        End If
    End Sub

    Private Sub Card5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Card5.Click
        If Card5.clicked = False Then
            Card5.clicked = True
            UpdateCurrentCard(Card5)
            DisplayImg(Card5)
        End If
    End Sub

    Private Sub Card6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Card6.Click
        If Card6.clicked = False Then
            Card6.clicked = True
            UpdateCurrentCard(Card6)
            DisplayImg(Card6)
        End If
    End Sub

    Private Sub Card7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Card7.Click
        If Card7.clicked = False Then
            Card7.clicked = True
            UpdateCurrentCard(Card7)
            DisplayImg(Card7)
        End If
    End Sub

    Private Sub Card8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Card8.Click
        If Card8.clicked = False Then
            Card8.clicked = True
            UpdateCurrentCard(Card8)
            DisplayImg(Card8)
        End If
    End Sub

    Private Sub Card9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Card9.Click
        If Card9.clicked = False Then
            Card9.clicked = True
            UpdateCurrentCard(Card9)
            DisplayImg(Card9)
        End If
    End Sub

    Private Sub Card10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Card10.Click
        If Card10.clicked = False Then
            Card10.clicked = True
            UpdateCurrentCard(Card10)
            DisplayImg(Card10)
        End If
    End Sub

    Private Sub Card11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Card11.Click
        If Card11.clicked = False Then
            Card11.clicked = True
            UpdateCurrentCard(Card11)
            DisplayImg(Card11)
        End If
    End Sub

    Private Sub Card12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Card12.Click
        If Card12.clicked = False Then
            Card1.clicked = True
            UpdateCurrentCard(Card12)
            DisplayImg(Card12)
        End If
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        CurrentlyInUse1 = Nothing
        CurrentlyInUse2 = Nothing
        Label1.Text = "Vous avez démarré l'application le :" + CStr(myDate) 'affiche dans le label la date, heure, minute, seconde  de l'ouverture de l'application
        InitDeck()
        ImgApply() 'on applique la fonction qui va assigner les backgroundImage
    End Sub

End Class
