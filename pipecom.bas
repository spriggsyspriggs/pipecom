'================================================================================
' Pipecom - A Cross-Platform Process Capture Library for QB64
' Version: 2.0 (Native Linux/Mac Implementation)
' Author: Zachary Spriggs
'
' This library provides a single function, pipecom&, to execute a shell
' command and capture its STDOUT, STDERR, and Exit Code on both
' Windows and POSIX (Linux/Mac) systems.
'
' This version uses native POSIX calls on Linux/Mac, removing the
' previous dependency on popen and temporary files for stderr.
'================================================================================
$IncludeOnce
Function pipecom& (cmd As String, stdout As String, stderr As String)
    ' Initialize output strings
    stdout = "": stderr = ""

    '========================================================================
    ' WINDOWS IMPLEMENTATION
    '========================================================================
    $If WINDOWS Then
            ' --- Win32 API Type Definitions ---

            ' https://learn.microsoft.com/en-us/windows/win32/api/wtypesbase/ns-wtypesbase-security_attributes
            Type SECURITY_ATTRIBUTES
                As _Unsigned Long nLength
        $If 64BIT Then
                    As String * 4 padding ' Align for 64-bit
        $End If
                As _Offset lpSecurityDescriptor
                As Long bInheritHandle
        $If 64BIT Then
                    As String * 4 padding2 ' Align for 64-bit
        $End If
            End Type

            ' https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/ns-processthreadsapi-startupinfoa
            Type STARTUPINFO
                As Long cb
        $If 64BIT Then
                    As String * 4 padding
        $End If
                As _Offset lpReserved, lpDesktop, lpTitle
                As _Unsigned Long dwX, dwY, dwXSize, dwYSize, dwXCountChars, dwYCountChars, dwFillAttribute, dwFlags
                As _Unsigned Integer wShowWindow, cbReserved2
        $If 64BIT Then
                    As String * 4 padding2
        $End If
                As _Offset lpReserved2, hStdInput, hStdOutput, hStdError
            End Type

            ' https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/ns-processthreadsapi-process_information
            Type PROCESS_INFORMATION
                As _Offset hProcess, hThread
                As _Unsigned Long dwProcessId, dwThreadId
            End Type

            ' --- Win32 API Constants ---
            Const STARTF_USESTDHANDLES = &H00000100 ' Use hStdInput, hStdOutput, hStdError
            Const CREATE_NO_WINDOW = &H8000000     ' Don't create a console window
            Const INFINITE = 4294967295             ' Wait forever
            Const WAIT_FAILED = &HFFFFFFFF          ' Return value for Wait error

            ' --- Win32 API Function Declarations ---
            Declare Dynamic Library "kernel32"
                ' https://learn.microsoft.com/en-us/windows/win32/api/namedpipeapi/nf-namedpipeapi-createpipe
                Function CreatePipe& (ByVal hReadPipe As _Offset, Byval hWritePipe As _Offset, Byval lpPipeAttributes As _Offset, Byval nSize As _Unsigned Long)
                
                ' https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-createprocessa
                Function CreateProcessA& (ByVal lpApplicationName As _Offset, Byval lpCommandLine As _Offset, Byval lpProcessAttributes As _Offset, Byval lpThreadAttributes As _Offset, Byval bInheritHandles As Long, Byval dwCreationFlags As _Unsigned Long, Byval lpEnvironment As _Offset, Byval lpCurrentDirectory As _Offset, Byval lpStartupInfo As _Offset, Byval lpProcessInformation As _Offset)
                
                ' https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-getexitcodeprocess
                Function GetExitCodeProcess& (ByVal hProcess As _Offset, Byval lpExitCode As _Offset)
                
                ' https://learn.microsoft.com/en-us/windows/win32/api/handleapi/nf-handleapi-closehandle
                Sub CloseHandle (ByVal hObject As _Offset)
                
                ' https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile
                Function ReadFile& (ByVal hFile As _Offset, Byval lpBuffer As _Offset, Byval nNumberOfBytesToRead As _Unsigned Long, Byval lpNumberOfBytesRead As _Offset, Byval lpOverlapped As _Offset)
                
                ' https://learn.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-waitforsingleobject
                Function WaitForSingleObject~& (ByVal hHandle As _Offset, Byval dwMilliseconds As _Unsigned Long)
            End Declare

            Dim As Long ok: ok = 1
            Dim As _Offset hStdOutPipeRead, hStdOutPipeWrite, hStdReadPipeError, hStdOutPipeError
            
            ' Set up Security Attributes for inheritable pipe handles
            Dim As SECURITY_ATTRIBUTES sa
            sa.nLength = Len(sa): sa.lpSecurityDescriptor = 0: sa.bInheritHandle = 1

            ' Create the pipe for STDOUT
            If CreatePipe(_Offset(hStdOutPipeRead), _Offset(hStdOutPipeWrite), _Offset(sa), 0) = 0 Then
                pipecom = -1
                Exit Function
            End If

            ' Create the pipe for STDERR
            If CreatePipe(_Offset(hStdReadPipeError), _Offset(hStdOutPipeError), _Offset(sa), 0) = 0 Then
                pipecom = -1
                Exit Function
            End If

            ' Set up STARTUPINFO to redirect the new process's std handles
            Dim As STARTUPINFO si
            si.cb = Len(si)
            si.dwFlags = STARTF_USESTDHANDLES
            si.hStdError = hStdOutPipeError    ' Redirect stderr to our pipe
            si.hStdOutput = hStdOutPipeWrite   ' Redirect stdout to our pipe
            si.hStdInput = 0
            
            Dim As PROCESS_INFORMATION procinfo
            Dim As _Offset lpApplicationName
            Dim As String lpCommandLine
            
            ' Prepend "cmd /c " to execute the command in a shell
            ' and add a null terminator for the C API.
            lpCommandLine = "cmd /c " + cmd + Chr$(0)
            
            Dim As _Offset lpProcessAttributes, lpThreadAttributes
            Dim As Long bInheritHandles: bInheritHandles = 1 ' Must be 1 to inherit pipes
            Dim As _Unsigned Long dwCreationFlags: dwCreationFlags = CREATE_NO_WINDOW
            Dim As _Offset lpEnvironment, lpCurrentDirectory

            ' Create the child process
            ok = CreateProcessA(lpApplicationName, _Offset(lpCommandLine), lpProcessAttributes, lpThreadAttributes, bInheritHandles, dwCreationFlags, lpEnvironment, lpCurrentDirectory, _Offset(si), _Offset(procinfo))

            If ok = 0 Then
                pipecom = -1
                Exit Function
            End If

            ' Close the "write" ends of the pipes in the parent process.
            ' The child process now holds the only copies.
            ' This is crucial, or ReadFile will never finish.
            CloseHandle hStdOutPipeWrite
            CloseHandle hStdOutPipeError

            ' Read loop for STDOUT
            Dim As String buf: buf = Space$(4096 + 1)
            Dim As _Unsigned Long dwRead
            While ReadFile(hStdOutPipeRead, _Offset(buf), 4096, _Offset(dwRead), 0) <> 0 And dwRead > 0
                buf = Mid$(buf, 1, dwRead)
                GoSub RemoveChr13 ' Remove carriage returns
                stdout = stdout + buf
                buf = Space$(4096 + 1)
            Wend

            ' Read loop for STDERR
            While ReadFile(hStdReadPipeError, _Offset(buf), 4096, _Offset(dwRead), 0) <> 0 And dwRead > 0
                buf = Mid$(buf, 1, dwRead)
                GoSub RemoveChr13 ' Remove carriage returns
                stderr = stderr + buf
                buf = Space$(4096 + 1)
            Wend

            ' Wait for the child process to terminate
            Dim As Long exit_code, ex_stat
            If WaitForSingleObject(procinfo.hProcess, INFINITE) <> WAIT_FAILED Then
                ' Get the process's exit code
                If GetExitCodeProcess(procinfo.hProcess, _Offset(exit_code)) Then
                    ex_stat = 1
                End If
            End If

            ' Clean up remaining handles
            CloseHandle hStdOutPipeRead
            CloseHandle hStdReadPipeError
            
            ' Return the exit code
            If ex_stat = 1 Then
                pipecom = exit_code
            Else
                pipecom = -1
            End If

            Exit Function

            ' Helper routine to strip Chr$(13) for Windows
            RemoveChr13:
            Dim As Long j
            j = InStr(buf, Chr$(13))
            Do While j
                buf = Left$(buf, j - 1) + Mid$(buf, j + 1)
                j = InStr(buf, Chr$(13))
            Loop
            Return

        '========================================================================
        ' POSIX (LINUX / MAC) IMPLEMENTATION
        '========================================================================
    $Else
        ' --- POSIX API Function Declarations ---
        Declare CustomType Library
            ' https://man7.org/linux/man-pages/man2/pipe.2.html
            Function pipe& (fildes As _Integer64)
                
            ' https://man7.org/linux/man-pages/man2/fork.2.html
            Function fork%& ()
                
            ' https://man7.org/linux/man-pages/man2/close.2.html
            Sub __close Alias "close" (ByVal fd As Long)
                
            ' https://man7.org/linux/man-pages/man2/dup2.2.html
            Sub dup2 (ByVal oldfd As Long, ByVal newfd As Long)
                
            ' https://man7.org/linux/man-pages/man3/execl.3.html
            Sub execl (path As String, arg1 As String, arg2 As String, cmd As String, ByVal nul As _Offset)
                
            ' https://man7.org/linux/man-pages/man2/select.2.html
            Function __select& Alias "select" (ByVal nfds As Long, ByVal readfds As _Offset, ByVal writefds As _Offset, ByVal exceptfds As _Offset, ByVal timeout As _Offset)
                
            ' https://man7.org/linux/man-pages/man2/read.2.html
            Function __read%& Alias "read" (ByVal fildes As Long, ByVal buf As _Offset, ByVal nbyte As _Offset)
                
            ' https://man7.org/linux/man-pages/man2/waitpid.2.html
            Sub waitpid (ByVal pid As _Offset, ByVal status As _Offset, ByVal options As Long)
        End Declare
            
        ' https://man7.org/linux/man-pages/man3/wait.3.html
        Declare Library "wait"
            Function WIFEXITED& (ByVal status As Long)
            Function WEXITSTATUS& (ByVal status As Long)
        End Declare

        ' Standard file descriptor numbers
        Const STDOUT_FILENO = 1
        Const STDERR_FILENO = 2

        ' --- fd_set macro replication constants ---
        $If 64BIT Then
            Const NFDBITS = 64
        $Else
                Const NFDBITS = 32
        $End If
        Const FD_SETSIZE = 1024
        Const FD_SET_ARRAY_MAX_INDEX = (FD_SETSIZE / NFDBITS) - 1
            
        ' QB64 doesn't have int[2] arrays as params, so we pack
        ' [read_fd, write_fd] into a single _INTEGER64
        Dim As _Integer64 stdout_pipes, stderr_pipes
        Dim As _Offset pid

        ' Create two pipes: one for stdout, one for stderr
        If pipe(stdout_pipes) = -1 Or pipe(stderr_pipes) = -1 Then
            _LogError "An error with pipe has occurred"
            pipecom = -1
            Exit Function
        End If

        ' Create the child process
        pid = fork
        If pid = -1 Then
            _LogError "An error with fork has occurred"
            pipecom = -1
            Exit Function
        End If

        '========================
        ' CHILD PROCESS
        '========================
        If pid = 0 Then
            ' We are in the child process.
            ' Close the READ ends of the pipes (child only writes)
            __close GetLowLong(stdout_pipes)
            __close GetLowLong(stderr_pipes)

            ' Redirect child's STDOUT to the WRITE end of the stdout pipe
            dup2 GetHighLong(stdout_pipes), STDOUT_FILENO
            ' Redirect child's STDERR to the WRITE end of the stderr pipe
            dup2 GetHighLong(stderr_pipes), STDERR_FILENO

            ' Close the original WRITE end descriptors (now redundant)
            __close GetHighLong(stdout_pipes)
            __close GetHighLong(stderr_pipes)

            ' Execute the command using /bin/sh -c "..."
            ' We add Chr$(0) for C-string null termination
            execl "/bin/sh" + Chr$(0), "sh" + Chr$(0), "-c" + Chr$(0), cmd + Chr$(0), 0
                
            ' If execl returns, an error occurred. Exit with 127.
            System 127
            
            '========================
            ' PARENT PROCESS
            '========================
        Else
            ' We are in the parent process.
            ' Close the WRITE ends of the pipes (parent only reads)
            __close GetHighLong(stdout_pipes)
            __close GetHighLong(stderr_pipes)

            pipecom = -1 ' Default exit code

            ' Find the highest file descriptor number for select()
            Dim As Long max_fd
            If GetLowLong(stdout_pipes) > GetLowLong(stderr_pipes) THEN
                max_fd = GetLowLong(stdout_pipes)
            Else
                max_fd = GetLowLong(stderr_pipes)
            End If

            ' This is our file descriptor set for select()
            Dim As _Integer64 read_fds(FD_SET_ARRAY_MAX_INDEX)
                
            ' Main read loop:
            ' We use select() to monitor both pipes at once.
            While 1
                Dim As String read_buf: read_buf = Space$(1024)
                Dim As _Offset bytes
                    
                ' Clear the fd_set
                FD_ZERO read_fds()
                    
                ' Flag to track if any pipes are still open
                Dim As Long fds_open: fds_open = 0

                ' Add STDOUT pipe to set if it's not closed
                ' (We flag closed pipes by setting their FD to -1)
                If GetLowLong(stdout_pipes) <> -1 Then
                    FD_SET GetLowLong(stdout_pipes), read_fds()
                    fds_open = 1
                End If
                    
                ' Add STDERR pipe to set if it's not closed
                If GetLowLong(stderr_pipes) <> -1 Then
                    FD_SET GetLowLong(stderr_pipes), read_fds()
                    fds_open = 1
                End If

                ' If no pipes are left open, exit the read loop
                If fds_open = 0 Then
                    Exit While
                End If

                ' Wait indefinitely until one or more pipes have data
                If __select(max_fd + 1, _Offset(read_fds()), 0, 0, 0) = -1 THEN
                    _LogError "An error with __select has occurred"
                    Exit While
                End If

                ' Check if STDOUT pipe has data
                If GetLowLong(stdout_pipes) <> -1 And FD_ISSET(GetLowLong(stdout_pipes), read_fds()) = -1 Then
                    bytes = __read(GetLowLong(stdout_pipes), _Offset(read_buf), Len(read_buf))
                    If bytes > 0 THEN
                        ' Append data to stdout string
                        stdout = stdout + Mid$(read_buf, 1, bytes)
                    ELSE
                        ' 0 bytes means EOF. Close the pipe.
                        __close GetLowLong(stdout_pipes)
                        ' Flag it as closed by setting the FD to -1
                        Dim As Long stdoutlow: stdoutlow = -1
                        stdout_pipes = PackLongsToInteger64(stdoutlow, GetHighLong(stdout_pipes))
                    End If
                End If

                ' Check if STDERR pipe has data
                If GetLowLong(stderr_pipes) <> -1 And FD_ISSET(GetLowLong(stderr_pipes), read_fds()) = -1 THEN
                    bytes = __read(GetLowLong(stderr_pipes), _Offset(read_buf), Len(read_buf))
                    If bytes > 0 THEN
                        ' Append data to stderr string
                        stderr = stderr + Mid$(read_buf, 1, bytes)
                    ELSE
                        ' 0 bytes means EOF. Close the pipe.
                        __close GetLowLong(stderr_pipes)
                        ' Flag it as closed by setting the FD to -1
                        Dim As Long stderrlow: stderrlow = -1
                        stderr_pipes = PackLongsToInteger64(stderrlow, GetHighLong(stderr_pipes))
                    End If
                End If
            Wend

            ' Wait for the child process to exit and get its status
            Dim As Long status
            waitpid pid, _OFFSET(status), 0

            ' Check if the process exited normally
            If WIFEXITED(status) Then
                ' Get the actual exit code
                pipecom = WEXITSTATUS(status)
            Else
                pipecom = -1 ' Process was killed or exited abnormally
            End If
        End If
    $End If
End Function

'============================================================================
' HELPER FUNCTIONS (POSIX-only)
'============================================================================

' This section is skipped on Windows
$If WINDOWS Then
$Else
    ' --- fd_set Macro Replications ---

    Sub FD_ZERO (arr() As _Integer64)
        ' Replicates: FD_ZERO(fd_set *set)
        ' Clears all bits in the set by zeroing the array.
        Dim As Integer i
        For i = 0 To UBound(arr)
            arr(i) = 0
        Next i
    End Sub

    Sub FD_SET (fd As Long, arr() As _Integer64)
        ' Replicates: FD_SET(int fd, fd_set *set)
        ' Sets the specific bit for a file descriptor.
        $If 64BIT Then
            Const NFDBITS = 64
        $Else
                Const NFDBITS = 32
        $End If
        Dim As Long index, bit_position
        Dim As _Integer64 bit_mask

        ' Find which array element holds the bit
        index = fd \ NFDBITS
        ' Find the bit's position within that element
        bit_position = fd Mod NFDBITS
        ' Create a mask for that bit (1 << bit_position)
        bit_mask = 2 ^ bit_position
        ' Set the bit
        arr(index) = arr(index) Or bit_mask
    End Sub

    Function FD_ISSET% (fd As Long, arr() As _Integer64)
        ' Replicates: int FD_ISSET(int fd, fd_set *set)
        ' Checks if a specific bit for a file descriptor is set.
        $If 64BIT THEN
            Const NFDBITS = 64
        $Else
                Const NFDBITS = 32
        $End If
        Dim As Long index, bit_position
        Dim As _Integer64 bit_mask

        index = fd \ NFDBITS
        bit_position = fd Mod NFDBITS
        bit_mask = 2 ^ bit_position

        ' Check the bit. Returns -1 (True) or 0 (False).
        If (arr(index) And bit_mask) <> 0 Then
            FD_ISSET = -1
        Else
            FD_ISSET = 0
        End If
    End Function

    ' --- 32/64-bit Packing Helper Functions ---
    ' (Used to store two 32-bit FDs in one 64-bit _INTEGER64)

    Function PackLongsToInteger64&& (lowLong As Long, highLong As Long)
        ' Packs two 32-bit LONGs into one 64-bit _INTEGER64.
        Const LOMASK = &HFFFFFFFF

        Dim As _Integer64 high_shifted, low_masked
        ' Shift high long into the upper 32 bits
        high_shifted = _Cast(_Integer64, highLong) * (2 ^ 32)
        ' Mask low long to 32 bits (to handle sign)
        low_masked = _Cast(_Integer64, lowLong) And LOMASK
        ' Combine them
        PackLongsToInteger64 = high_shifted Or low_masked
    End Function

    Function GetLowLong& (packedValue As _Integer64)
        ' Extracts the low 32-bit LONG (index 0)
        Const LOMASK = &HFFFFFFFF
        GetLowLong = (packedValue And LOMASK)
    End Function

    Function GetHighLong& (packedValue As _Integer64)
        ' Extracts the high 32-bit LONG (index 1)
        GetHighLong = packedValue \ (2 ^ 32) ' Arithmetic shift right
    End Function

$End If

'============================================================================
' LITE HELPER FUNCTIONS
'============================================================================

Function pipecom_lite$ (cmd As String)
    ' A simple wrapper that returns stderr if it exists,
    ' otherwise returns stdout.
    Dim As Long a
    Dim As String stdout, stderr
    a = pipecom(cmd, stdout, stderr)
    If stderr <> "" Then
        pipecom_lite = stderr
    Else
        pipecom_lite = stdout
    End If
End Function

Sub pipecom_lite (cmd As String)
    ' A "fire-and-forget" version that runs the command
    ' but doesn't return any output.
    Dim As Long a
    Dim As String stdout, stderr
    a = pipecom(cmd, stdout, stderr)
End Sub
