'https://stackoverflow.com/questions/17466681/how-to-run-vbs-as-administrator-from-vbs
If Not WScript.Arguments.Named.Exists("elevate") Then
	CreateObject("Shell.Application").ShellExecute WScript.FullName _
				, """" & WScript.ScriptFullName & """ /elevate", "", "runas", 1
	WScript.Quit
End If

Set filesystem = CreateObject("Scripting.FileSystemObject")
scriptdir = filesystem.GetParentFolderName(WScript.ScriptFullName)
dotfilesdir = filesystem.GetParentFolderName(scriptdir)

Set shell = CreateObject("WScript.Shell")
homedir = shell.ExpandEnvironmentStrings("%USERPROFILE%")

' https://stackoverflow.com/questions/3525462/how-to-retrieve-startup-folder-location-in-64-bit-windows
startupdir = CreateObject("shell.application").namespace(&h7).self.path
' wscript.echo startupdir

Function quotestr(str1)
	quotestr = """" & str1 & """"
End Function

Function symlink(link, target)
	' linkcommand must end with space!
	If filesystem.FileExists(target) Then
		linkcommand = "%comspec% /C mklink "
	Else
		linkcommand = "%comspec% /C mklink /D "
	End If
	shell.run linkcommand & quotestr(link) & " " & quotestr(target), 0, True
End Function


' https://stackoverflow.com/questions/2309659/how-do-i-determine-if-a-path-is-relative-or-absolute-with-vb-scripting-host
Function isAbsolutePath(path)
	isAbsolutePath = True
	Dim first : first = UCase(Left(path, 1))
	Dim secondNthird : secondNthird = UCase(Mid(path, 2, 2))
	If first > "A" and first < "Z" and secondNthird = ":\" Then Exit Function
	If first = "\" Then Exit Function
	isAbsolutePath = False
End Function

Function dosymlink(target, link)
	If Not isAbsolutePath(link) Then
		fulllink = filesystem.BuildPath(homedir, link)
	Else
		fulllink = link
	End If

	fulltarget = filesystem.BuildPath(dotfilesdir, target)
	linkfolder = filesystem.GetParentFolderName(fulllink)
	exists = filesystem.FolderExists(linkfolder)
	If Not exists Then
		' Wscript.Echo linkfolder
		filesystem.CreateFolder(linkfolder)
	End If
	symlink fulllink, fulltarget
End Function


' dosymlink "", "AppData\Roaming\dotfiles"
dosymlink "emacs\.emacs", "AppData\Roaming\.emacs"
dosymlink "emacs\early-init.el", "AppData\Roaming\.emacs.d\early-init.el"

' dosymlink "emacs\packages.el", "AppData\Roaming\.emacs.d\straight\versions\default.el"
' dosymlink "vim\.vimrc", ".vimrc"

' dosymlink "latex\stylesheets", "AppData\Local\Programs\MiKTeX\tex\latex\stylesheets"

' dosymlink "vcxsrv\autorun.vbs", filesystem.BuildPath(startupdir, "vcxsrv_autorun.vbs")
' dosymlink "audiosink\autorun.vbs", filesystem.BuildPath(startupdir, "audiosink_autorun.vbs")
' dosymlink "windows\startup-deferred-apps.vbs", filesystem.BuildPath(startupdir, "startup_deferred.vbs")
' dosymlink "windows\startup.vbs", filesystem.BuildPath(startupdir, "startup.vbs")
