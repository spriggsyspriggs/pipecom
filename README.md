Pipecom is a cross-platform library (Windows & POSIX (Mac & Linux)) for QB64 that allows capturing of stdout, stderr, and the exit code of a shelled process _without_ using any temp files.

Basic usage:
```vbnet
Dim As String stdout, stderr
Dim As Long exit_code
exit_code = pipecom("dir", stdout, stderr) 'stdout and stderr are stored in the provided strings
```
There are also "lite" wrappers provided for quick usage of pipecom:
```vbnet
Print pipecom_lite("dir") 'this version of pipecom_lite returns stderr if it is not empty, or stdout if it is
pipecom_lite "dir" 'ideally, this would be a shelled command that you would not intend to read the results of
```
