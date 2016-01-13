''''''''''''''''''''''''''''''''''''''''''''''''''''''
'   Genesis2000 Visual Basic.NET 脚本接口类          ’
'   作者:褚迪   联系QQ：271314609                    ’
''''''''''''''''''''''''''''''''''''''''''''''''''''''

'2016.1.13日开放源代码！本人水平有限，业余时间写了这个接口，
'如有任何建议，请和我联系！希望高手能指点。同时也方便大家研究和学习
'欢迎您使用源代码，但请尊重我的著作权！







Imports System.IO
Imports System.Drawing

Public Class Genesis
    Private _GENESIS_DIR As String = Environment.GetEnvironmentVariable("GENESIS_DIR")
    Private _GENESIS_EDIR As String =Environment.GetEnvironmentVariable("GENESIS_EDIR")
   
    Private DRI_PREFIX = "@%#%@"    'genesis命令行必备的前缀
    Private _JOB As String
    Private _STEP As String
    Private csh_file As String
    Public STATUS As String
    Public READANS As String
    Public PAUSANS As String
    Public MOUSEANS As String
    Public COMANS As String

    Public Doinfo As Dictionary(Of String, String())


    Public ReadOnly Property Csh_file_name As String
        Get
            Return csh_file
        End Get
    End Property
    Public ReadOnly Property GetInfo(ByVal var As String) As Object
        Get
            '判断是数组类型还是值类型来返回对应的值
            If Doinfo.Item(var).Count = 1 Then
                Return Doinfo.Item(var)(0)
            Else
                Return Doinfo.Item(var)
            End If
        End Get
    End Property
    Public ReadOnly Property Genesis_Dir() As String
        Get
            Return _GENESIS_DIR
        End Get
    End Property
    Public ReadOnly Property Genesis_edir() As String
        Get
            Return _GENESIS_EDIR
        End Get
    End Property
    Public ReadOnly Property JOB_Name() As String
        Get
            Return Environment.GetEnvironmentVariable("JOB")
        End Get
    End Property
    Public ReadOnly Property STEP_Name As String
        Get
            Return Environment.GetEnvironmentVariable("STEP")
        End Get
    End Property
    
    Public Sub COM(ByVal ParamArray Values() As String)
        Dim command As String = ""
        If Values.Length = 1 Then
            command = Values(0)
            sendCommandToPipe("COM", RemoveNewLines(command))
        Else
            For Each item As String In Values
                command += item & ","
            Next
            sendCommandToPipe("COM", RemoveNewLines(command))
        End If
        STATUS = GetReply()
        READANS = GetReply()
        COMANS = READANS
    End Sub
    
    Public Sub DO_INFO(ByVal ParamArray Values() As String)
        COMANS = -1
        Dim entity_path As String = ""
        Dim data_type As String = ""
        Dim parameters As String = ""
        Dim serial_number As String = ""
        Dim options As String = ""
        Dim entity_type As String = ""
        Dim help As String = ""
        Dim units As String = ""
        Dim n As String = ""
        Dim i As String = ""
        Dim generator As New Random
        Dim randomValue As Integer
        For Each item As String In Values
            Dim parts() As String = item.Split("=")
            n = parts(0).Trim
            i = parts(1).Trim
            If n = "entity_type" Then
                entity_type = "-t " & i
            ElseIf n = "entity_path" Then
                entity_path = "-e " & i
            ElseIf n = "data_type" Then
                data_type = "-d " & i
            ElseIf n = "parameters" Then
                parameters = "-p " & i
            ElseIf n = "serial_number" Then
                serial_number = "-s " & i
            ElseIf n = "options" Then
                options = "-o " & i
            ElseIf n = "help" Then
                help = "-help"
            ElseIf n = "units" Then
                units = "units=" & i & ","
            End If
        Next
        '生成获取命令的参数
        randomValue = generator.Next
        csh_file = Genesis_Dir & "/share/tmp/info_csh.chudi" & randomValue
        Dim info_pre As String = "info,out_file=" & csh_file & ",write_mode=replace," & units & "args="
        Dim info_com As String = info_pre & " " & entity_type & " " & entity_path & " " & data_type & " " & parameters & " " & serial_number & " " & options & " " & help
        '分析信息
        Parse(info_com)
    End Sub


    Private Sub Parse(ByVal infoFileCommand As String)
        Dim lines As StreamReader
        '重新初始化doinf数组
        Doinfo = New Dictionary(Of String, String())
        COM(infoFileCommand, "")  '生成信息文件，并确定了信息文件的名字
        If File.Exists(csh_file) Then   '确定是否生成了文件，如果生成了则继续执行，否则返回false
            Dim var As String
            Dim valuetmp As String
            Dim value() As String
            Dim item() As String
            lines = File.OpenText(csh_file)
            Dim i As String
            Do Until lines.EndOfStream
                i = lines.ReadLine
                If i.Trim <> String.Empty Then
                    '忽略空行 
                    item = i.Split("=")                    '按等号分组
                    var = item(0).Replace("set", "").Trim  '删除set字符及空格
                    valuetmp = item(1)                     '获取值部分
                    valuetmp = valuetmp.Replace("(", "")   '删除左括号
                    valuetmp = valuetmp.Replace(")", "")   '删除右括号
                    valuetmp = valuetmp.Replace("''", "'none'") '扩展空属性
                    valuetmp = valuetmp.Replace("'", " ") '把逗号替换成空格
                    value = valuetmp.Split({" "}, StringSplitOptions.RemoveEmptyEntries)  '分组并删除空的属性
                    Doinfo.Add(var, value)  '添加入属性字典表中
                End If
            Loop
            lines.Close()
            File.Delete(csh_file)
        End If
    End Sub
    Public Sub MOUSE()
        sendCommandToPipe("MOUSE r", "")
        STATUS = GetReply()
        READANS = GetReply()
        MOUSEANS = GetReply()
    End Sub
    Public Sub PAUSE()
        sendCommandToPipe("PAUSE", "")
        '获取状态
        STATUS = GetReply()
        READANS = GetReply()
        PAUSANS = GetReply()

    End Sub
    Public Sub VOF()
        sendCommandToPipe("VOF", "")
    End Sub
    Public Sub VON()
        sendCommandToPipe("VON", "")
    End Sub
    Public Sub SU_ON()
        sendCommandToPipe("SU_ON", "")
    End Sub
    Public Sub SU_OFF()
        sendCommandToPipe("SU_OFF", "")

    End Sub
    Public Sub AUX(ByVal ParamArray Values() As String)
        Dim command As String = ""
        If Values.Length = 1 Then
            command = Values(0)
            sendCommandToPipe("AUX", RemoveNewLines(command))
        Else
            For Each item As String In Values
                command += item & ","
            Next
            sendCommandToPipe("AUX", RemoveNewLines(command))
        End If

        STATUS = GetReply()
        READANS = GetReply()
        COMANS = READANS
    End Sub
    Private Sub sendCommandToPipe(ByVal commandType As String, ByVal command As String)
        Console.WriteLine(DRI_PREFIX & commandType & " " & command)
    End Sub
    Public Function RemoveNewLines(ByVal command As String) As String
        Return command.Replace(Chr(13), " ")     '回车替换成空格
    End Function
    Public Function GetReply() As String
        Return Console.ReadLine()
    End Function
    Public Function GetENV(ByVal name As String) As String
        Return Environment.GetEnvironmentVariable(name)
    End Function
    Private Function GetFromatValue(ByVal value As String) As String
        value = value.Replace("'", "")
        value = value.Replace("(", "")
        value = value.Replace(")", "")
        Return value
    End Function


End Class
